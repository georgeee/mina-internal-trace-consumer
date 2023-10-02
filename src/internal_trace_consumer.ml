open Core
open Async
module Block_tracing = Block_tracing

let current_block = ref "0"

let show_result_error = function
  | Ok _ ->
      ()
  | Error error ->
      Log.Global.error "SQL ERROR: %s" (Caqti_error.show error)

let abort_on_error = function
  | Ok _ ->
      Deferred.unit
  | Error error ->
      Log.Global.error "SQL ERROR: %s" (Caqti_error.show error) ;
      exit 1

let current_trace_id ?(source = `Unknown) () =
  match%map
    Persistent_registry.get_or_add_block_trace ~source !current_block
  with
  | Error err ->
      Log.Global.error "could not get_or_add_trace %s: %s" !current_block
        (Caqti_error.show err) ;
      0
  | Ok id ->
      id

(* These tables keep track of the context switches for blocks in the main trace when
   making calls to the prover or verifier. The mapping is from:
   [parent_call_checkpoint -> (block_id, timestamp)] *)
let verifier_calls_context_block : (string * float) list String.Table.t =
  String.Table.create ()

let prover_calls_context_block : (string * float) list String.Table.t =
  String.Table.create ()

module Pending = struct
  type t = Checkpoint of (string * float) | Control of (string * Yojson.Safe.t)

  type registry = t list Int.Table.t

  let prover_entries : registry = Int.Table.create ()

  let verifier_entries : registry = Int.Table.create ()

  (* Helper for being able to find the location in the main trace on which to attach
     verifier and prover checkpoints. Also to know when a verifier/prover trace is
     already complete (partial traces are left pending until completed) *)
  let parent_and_finish_checkpoints = function
    | "Verifier_verify_transaction_snarks" ->
        Some
          ( "Verify_transaction_snarks"
          , "Verifier_verify_transaction_snarks_done" )
    | "Verifier_verify_blockchain_snarks" ->
        Some
          ("Verify_blockchain_snarks", "Verifier_verify_blockchain_snarks_done")
    | "Verifier_verify_commands" ->
        Some ("Verify_commands", "Verifier_verify_commands_done")
    | "Prover_extend_blockchain" ->
        Some ("Produce_state_transition_proof", "Prover_extend_blockchain_done")
    | _ ->
        None

  let timestamp = function Checkpoint (_, timestamp) -> timestamp | _ -> -1.0
end

let push_pending_entry table call_id (entry : Pending.t) =
  Int.Table.update table call_id ~f:(fun entries ->
      let entries = Option.value ~default:[] entries in
      entry :: entries ) ;
  Deferred.unit

let find_nearest_block ~context_blocks timestamp =
  List.find_map context_blocks ~f:(fun (block, switch_timestamp) ->
      if Float.(timestamp >= switch_timestamp) then Some block else None )

let add_pending_entries_to_block_trace ~entries_source ~block_trace_id
    ~parent_checkpoint ~call_id pending_entries =
  let rec loop = function
    | Pending.Checkpoint (checkpoint, timestamp) :: entries ->
        let%bind result =
          Persistent_registry.push_checkpoint block_trace_id ~is_main:true
            ~call_id ~source:entries_source ~name:checkpoint ~timestamp ()
        in
        show_result_error result ; loop entries
    | Pending.Control ("metadata", data) :: entries ->
        let%bind result =
          Persistent_registry.push_control block_trace_id ~is_main:true ~call_id
            ~source:entries_source ~name:"metadata" ~metadata:data ()
        in
        show_result_error result ; loop entries
    | Pending.Control (_, _) :: entries ->
        (* printf "Ignoring control %s\n%!" other ; *)
        loop entries
    | [] ->
        return ()
  in
  let timestamp = Pending.timestamp (List.last_exn pending_entries) in
  match%bind
    Persistent_registry.nearest_trace ~prev_checkpoint:parent_checkpoint
      ~timestamp block_trace_id
  with
  | Ok `Main ->
      loop pending_entries
  | Ok `Other ->
      (* TODO: loop but with target = `Other? *)
      return ()
  | Error err ->
      Log.Global.error
        "[WARN] failure when trying to find nearest checkpoint: %s"
        (Caqti_error.show err) ;
      return ()

let handle_pending_entries ~call_id ~entries_source ~context_blocks
    ~last_pending_entry ~pending_entries =
  let open Pending in
  match pending_entries with
  | Checkpoint (first_checkpoint, first_timestamp) :: _ -> (
      match
        ( Pending.parent_and_finish_checkpoints first_checkpoint
        , last_pending_entry )
      with
      | ( Some (parent_checkpoint, end_checkpoint)
        , Some (Checkpoint (last_checkpoint, _)) )
        when String.equal end_checkpoint last_checkpoint -> (
          match String.Table.find context_blocks parent_checkpoint with
          | Some context_blocks -> (
              match find_nearest_block ~context_blocks first_timestamp with
              | Some block_id -> (
                  match Persistent_registry.block_trace_id block_id with
                  | None ->
                      Log.Global.error "[WARN] could not find trace id for %s"
                        block_id ;
                      return false
                  | Some block_trace_id ->
                      let%map () =
                        add_pending_entries_to_block_trace ~entries_source
                          ~block_trace_id ~parent_checkpoint ~call_id
                          pending_entries
                      in
                      false )
              | None ->
                  (* Could not find a block to attach although data is ready, drop *)
                  return false )
          | None ->
              (* Could not find related block because data is not ready yet,
                 wait and try again later *)
              return true )
      | _ ->
          (* Still not complete, wait and try again later *)
          return true )
  | Control (name, _) :: _ ->
      Log.Global.error
        "[WARN] unexpected non-checkpoint entry '%s' found as first element in \
         pending trace"
        name ;
      return false
  | _ ->
      Log.Global.error "[WARN] empty list in pending trace" ;
      return false

(* Pending, complete verifier/prover traces are matched to a block trace by their timestamp
   and expected parent call (a checkpoint in the main trace). Once added they are removed
   from the pending table. *)
let process_pending_entries ~entries_source ~context_blocks
    (pending_checkpoints : Pending.registry) =
  let open Deferred.Let_syntax in
  let keys = Int.Table.keys pending_checkpoints in
  Deferred.List.iter keys ~f:(fun call_id ->
      let rev_pending_entries =
        Int.Table.find_exn pending_checkpoints call_id
      in
      let pending_entries = List.rev rev_pending_entries in
      let%map should_keep =
        handle_pending_entries ~entries_source ~context_blocks
          ~last_pending_entry:(List.hd rev_pending_entries)
          ~call_id ~pending_entries
      in
      if not should_keep then Int.Table.remove pending_checkpoints call_id ;
      () )

module Main_handler = struct
  module Block_call_tag_key = struct
    type t = string * string [@@deriving hash, sexp, compare]
  end

  let current_call_id = ref 0

  let current_call_tag = ref ""

  let call_tracker = Hashtbl.create (module Block_call_tag_key)

  let handle_verifier_and_prover_call_checkpoints checkpoint timestamp =
    match checkpoint with
    | "Verify_transaction_snarks"
    | "Verify_blockchain_snarks"
    | "Verify_commands" ->
        String.Table.update verifier_calls_context_block checkpoint
          ~f:(fun entries ->
            let entries = Option.value ~default:[] entries in
            (!current_block, timestamp) :: entries )
    | "Produce_state_transition_proof" ->
        String.Table.update prover_calls_context_block checkpoint
          ~f:(fun entries ->
            let entries = Option.value ~default:[] entries in
            (!current_block, timestamp) :: entries )
    | _ ->
        ()

  let checkpoint_recording_action () =
    if String.equal !current_block "0" then
      (* Checkpoints happening outside of block production/processing contexts *)
      (* Still useful to keep around, can be used for txn/snark pool. etc *)
      `Keep
    else if String.equal !current_call_tag "" then `Keep
    else
      match Hashtbl.find call_tracker (!current_block, !current_call_tag) with
      | None ->
          `Keep
      | Some call_id when call_id = !current_call_id ->
          `Keep
      | Some _call_id ->
          (* For any given block+call_tag combination we keep only the first call *)
          (* eprintf
             "skipping checkpoint because call_id doesn't match (%s) %d <> %d\n\
              %!"
             !current_call_tag call_id !current_call_id ; *)
          `Other

  let process_checkpoint checkpoint timestamp =
    let action = checkpoint_recording_action () in
    match action with
    | `Skip ->
        (* NOTE: this branch is currently unused, storing out-of-block checkpoints is useful too *)
        Deferred.unit
    | (`Keep | `Other) as action ->
        let target_trace =
          match action with `Keep -> `Main | `Other -> `Other
        in
        let source = Block_tracing.compute_source checkpoint in
        let%bind trace_id = current_trace_id ~source () in
        let%map () =
          match target_trace with
          | `Main ->
              let status = Block_tracing.compute_status checkpoint in
              let%bind result =
                Persistent_registry.handle_status_change trace_id status
              in
              show_result_error result ;
              let%bind result =
                Persistent_registry.push_checkpoint trace_id ~is_main:true
                  ~source:`Main ~name:checkpoint ~timestamp ()
              in
              show_result_error result ; Deferred.unit
          | `Other ->
              let%bind result =
                Persistent_registry.push_checkpoint trace_id ~is_main:false
                  ~source:`Main ~name:checkpoint ~timestamp ()
              in
              show_result_error result ; Deferred.unit
        in
        handle_verifier_and_prover_call_checkpoints checkpoint timestamp ;
        ()

  let process_control ~other control data =
    match control with
    | "current_block" ->
        current_block := Yojson.Safe.Util.to_string data ;
        Deferred.unit
    | "current_call_id" -> (
        (* TODO: to avoid constant rehashing a flag can be set here about where
           to target the next checkpoints *)
        current_call_id := Yojson.Safe.Util.to_int data ;
        match List.Assoc.find ~equal:String.equal other "current_call_tag" with
        | None | Some (`String "" | `Null) ->
            (* If it is not present, must reset it *)
            current_call_tag := "" ;
            Deferred.unit
        | Some (`String tag) ->
            current_call_tag := tag ;
            let _call_id =
              Hashtbl.find_or_add call_tracker
                (!current_block, !current_call_tag) ~default:(fun () ->
                  !current_call_id )
            in
            Deferred.unit
        | Some other ->
            Log.Global.error "Got unexpected value for `current_call_tag`: %s"
              (Yojson.Safe.to_string other) ;
            Deferred.unit )
    | "metadata" ->
        let%bind trace_id = current_trace_id () in
        let%map result =
          Persistent_registry.push_control trace_id ~source:`Main ~is_main:true
            ~name:"metadata" ~metadata:data ()
        in
        show_result_error result ; ()
    | "block_metadata" ->
        let%bind trace_id = current_trace_id () in
        let%bind result =
          Persistent_registry.push_control trace_id ~source:`Main ~is_main:true
            ~name:"block_metadata" ~metadata:data ()
        in
        show_result_error result ;
        let%map result =
          Persistent_registry.push_block_metadata trace_id
            ~metadata:(Yojson.Safe.Util.to_assoc data)
        in
        show_result_error result ; ()
    | "produced_block_state_hash" ->
        let state_hash = Yojson.Safe.Util.to_string data in
        let%bind trace_id = current_trace_id () in
        let%map result =
          Persistent_registry.set_produced_block_state_hash trace_id state_hash
        in
        show_result_error result ; ()
    | "internal_tracing_enabled" ->
        Deferred.unit
    | "mina_node_metadata" ->
        (* TODO: should clear all old data here? *)
        Deferred.unit
    | another ->
        Log.Global.error "[WARN] unprocessed control: %s" another ;
        Deferred.unit

  (* TODO: reset all traces too? *)
  let file_changed () =
    current_block := "0" ;
    String.Table.clear verifier_calls_context_block ;
    String.Table.clear prover_calls_context_block

  let main_trace_synced = Ivar.create ()

  let synced () = Ivar.read main_trace_synced

  let eof_reached () =
    (*let before_prover_entries = Int.Table.length Pending.prover_entries in
      let before_verifier_entries = Int.Table.length Pending.verifier_entries in
      let before_time = Unix.gettimeofday () in*)
    (* TODO: these are not integrated into the checkpoints histogram *)
    let%bind () =
      process_pending_entries ~entries_source:`Prover
        ~context_blocks:prover_calls_context_block Pending.prover_entries
    in
    let%bind () =
      process_pending_entries ~entries_source:`Verifier
        ~context_blocks:verifier_calls_context_block Pending.verifier_entries
    in
    (*let after_prover_entries = Int.Table.length Pending.prover_entries in
      let after_verifier_entries = Int.Table.length Pending.verifier_entries in
      let after_time = Unix.gettimeofday () in
      printf "Processed pending entries: verifier=%d->%d prover=%d->%d in %fs\n%!"
        before_prover_entries after_prover_entries before_verifier_entries
        after_verifier_entries
        (after_time -. before_time) ;*)
    Ivar.fill_if_empty main_trace_synced () ;
    return ()

  let start_file_processing_iteration = function
    | `Sqlite ->
        Connection_context.start ()
    | `Postgres ->
        Deferred.unit

  let complete_file_processing_iteration = function
    | `Sqlite ->
        Connection_context.commit ()
    | `Postgres ->
        Deferred.unit
end

module Prover_handler = struct
  let current_call_id = ref 0

  let process_checkpoint checkpoint timestamp =
    push_pending_entry Pending.prover_entries !current_call_id
      (Pending.Checkpoint (checkpoint, timestamp))

  let process_control ~other:_ tag data =
    match tag with
    | "current_call_id" ->
        current_call_id := Yojson.Safe.Util.to_int data ;
        Deferred.unit
    | "internal_tracing_enabled" | "mina_node_metadata" ->
        Deferred.unit
    | tag ->
        push_pending_entry Pending.prover_entries !current_call_id
          (Pending.Control (tag, data))

  let file_changed () = Int.Table.clear Pending.prover_entries

  let eof_reached () = Deferred.unit (* Nothing to do *)

  let start_file_processing_iteration _ = Deferred.unit

  let complete_file_processing_iteration _ = Deferred.unit
end

module Verifier_handler = struct
  let current_call_id = ref 0

  let process_checkpoint checkpoint timestamp =
    push_pending_entry Pending.verifier_entries !current_call_id
      (Pending.Checkpoint (checkpoint, timestamp))

  let process_control ~other:_ tag data =
    match tag with
    | "current_call_id" ->
        current_call_id := Yojson.Safe.Util.to_int data ;
        Deferred.unit
    | "internal_tracing_enabled" | "mina_node_metadata" ->
        Deferred.unit
    | tag ->
        push_pending_entry Pending.verifier_entries !current_call_id
          (Pending.Control (tag, data))

  let file_changed () = Int.Table.clear Pending.verifier_entries

  let eof_reached () = Deferred.unit (* Nothing to do *)

  let start_file_processing_iteration _ = Deferred.unit

  let complete_file_processing_iteration _ = Deferred.unit
end

module Main_trace_processor = Trace_file_processor.Make (Main_handler)
module Prover_trace_processor = Trace_file_processor.Make (Prover_handler)
module Verifier_trace_processor = Trace_file_processor.Make (Verifier_handler)

let add_filename_prefix original_path ~prefix =
  let open Filename in
  concat (dirname original_path) (prefix ^ basename original_path)

let open_database_or_fail db_uri =
  match Caqti_async.connect_pool db_uri with
  | Error error ->
      Async.Log.Global.error "Failure when opening database: %s"
        (Caqti_error.show error) ;
      exit 1
  | Ok pool ->
      return pool

let compute_database_engine uri =
  match Uri.scheme uri with
  | Some "postgresql" ->
      `Postgres
  | Some "sqlite3" ->
      `Sqlite
  | _ ->
      eprintf "Unsupported database engine: %s\n%!" (Uri.to_string uri) ;
      Core.exit 1

let compute_engine_and_uri ~db_path ~db_uri =
  match (db_path, db_uri) with
  | None, None ->
      Core.prerr_endline
        "Must provide a path to a Sqlite3 file or an URI for PostgreSQL or \
         Sqlite3" ;
      Core.exit 1
  | Some _, Some _ ->
      Core.prerr_endline
        "Must provide either path to a Sqlite3 file or an URI for PostgreSQL \
         or Sqlite3, but noth both" ;
      Core.exit 1
  | Some path, None ->
      let db_uri = "sqlite3://" ^ path in
      let db_uri = Uri.of_string db_uri in
      (db_uri, compute_database_engine db_uri)
  | None, Some db_uri ->
      let db_uri = Uri.of_string db_uri in
      (db_uri, compute_database_engine db_uri)

let serve =
  Command.async_or_error ~summary:"Internal trace processor with GraphQL server"
    (let%map_open.Command port =
       flag "--port" ~aliases:[ "port" ]
         (optional_with_default 9080 int)
         ~doc:"PORT Port for GraphQL server to listen on (default 9080)"
     and main_trace_file_path =
       flag "--trace-file" ~aliases:[ "trace-file" ] (required string)
         ~doc:"PATH Path to main internal trace file"
     and db_path =
       flag "--db-path" ~aliases:[ "db-path" ] (optional string)
         ~doc:"PATH Persisted traces database path"
     and db_uri =
       flag "--db-uri" ~aliases:[ "db-uri" ] (optional string)
         ~doc:"URI Persisted traces database URI"
     and process_rotated_files =
       flag "--process-rotated-files"
         ~aliases:[ "process-rotated-files" ]
         (optional_with_default false bool)
         ~doc:"BOOL Process log-rotated files"
     in
     let prover_trace_file_path =
       add_filename_prefix main_trace_file_path ~prefix:"prover-"
     in
     let verifier_trace_file_path =
       add_filename_prefix main_trace_file_path ~prefix:"verifier-"
     in
     fun () ->
       let db_uri, engine = compute_engine_and_uri ~db_path ~db_uri in
       let%bind pool = open_database_or_fail db_uri in
       Connection_context.Db.set engine pool ;
       let%bind result = Store.initialize_database engine in
       let%bind () = abort_on_error result in
       let insecure_rest_server = true in
       Log.Global.info "Starting server on port %d..." port ;
       let%bind () =
         Graphql_server.create_graphql_server
           ~bind_to_address:
             Tcp.Bind_to_address.(
               if insecure_rest_server then All_addresses else Localhost)
           ~schema:Graphql_server.schema ~server_description:"GraphQL server"
           port
       in
       Log.Global.info "Consuming main trace events from file: %s"
         main_trace_file_path ;
       Log.Global.info "Consuming prover trace events from file: %s"
         prover_trace_file_path ;
       Log.Global.info "Consuming verifier trace events from file: %s"
         verifier_trace_file_path ;
       let%bind () =
         if process_rotated_files then
           Main_trace_processor.process_roated_files main_trace_file_path
         else Deferred.unit
       in
       let%bind () =
         Main_trace_processor.process_file main_trace_file_path
         >>| fun () -> eprintf "main trace file processed\n"
       and () =
         let%bind () = Main_handler.synced () in
         let%bind () =
           if process_rotated_files then
             Prover_trace_processor.process_roated_files prover_trace_file_path
           else Deferred.unit
         in
         Prover_trace_processor.process_file prover_trace_file_path
       and () =
         let%bind () = Main_handler.synced () in
         let%bind () =
           if process_rotated_files then
             Verifier_trace_processor.process_roated_files
               verifier_trace_file_path
           else Deferred.unit
         in
         Verifier_trace_processor.process_file verifier_trace_file_path
       in
       Log.Global.info "Done" ; Deferred.return (Ok ()) )

let process =
  Command.async_or_error
    ~summary:"Process a trace file and save results in database"
    (let%map_open.Command main_trace_file_path =
       flag "--trace-file" ~aliases:[ "trace-file" ] (required string)
         ~doc:"PATH Path to main internal trace file"
     and db_path =
       flag "--db-path" ~aliases:[ "db-path" ] (optional string)
         ~doc:"PATH Persisted traces database path"
     and db_uri =
       flag "--db-uri" ~aliases:[ "db-uri" ] (optional string)
         ~doc:"URI Persisted traces database URI"
     in
     let prover_trace_file_path =
       add_filename_prefix main_trace_file_path ~prefix:"prover-"
     in
     let verifier_trace_file_path =
       add_filename_prefix main_trace_file_path ~prefix:"verifier-"
     in
     fun () ->
       let db_uri, engine = compute_engine_and_uri ~db_path ~db_uri in
       let%bind pool = open_database_or_fail db_uri in
       Connection_context.Db.set engine pool ;
       let%bind result = Store.initialize_database engine in
       let%bind () = abort_on_error result in
       Log.Global.info "Consuming main trace events from file: %s"
         main_trace_file_path ;
       Log.Global.info "Consuming prover trace events from file: %s"
         prover_trace_file_path ;
       Log.Global.info "Consuming verifier trace events from file: %s"
         verifier_trace_file_path ;
       let without_rotation = true in
       let retry_open = false in
       let%bind () =
         Main_trace_processor.process_file ~without_rotation
           main_trace_file_path
       in
       let%bind () =
         Prover_trace_processor.process_file ~without_rotation ~retry_open
           prover_trace_file_path
       in
       let%bind () =
         Verifier_trace_processor.process_file ~without_rotation ~retry_open
           verifier_trace_file_path
       in
       Log.Global.info "Done" ; Deferred.return (Ok ()) )

let commands =
  [ ("serve", serve)
  ; ("process", process)
  ; ("test-storage", Store.Testing.test_storage)
  ]

let () =
  Async.Signal.handle [ Async.Signal.term; Async.Signal.int ] ~f:(fun _ ->
      Core.print_endline "SIGTERM/SIGINT received, exiting" ;
      Core.exit 0 ) ;
  Command.run
    (Command.group ~summary:"Internal trace processor"
       ~preserve_subcommand_order:() commands ) ;
  Core.exit 0
