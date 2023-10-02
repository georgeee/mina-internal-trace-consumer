open Core
open Async
module Block_tracing = Block_tracing

module Make (Handler : sig
  val process_checkpoint : string -> float -> unit Deferred.t

  val process_control :
       other:(string * Yojson.Safe.t) list
    -> string
    -> Yojson.Safe.t
    -> unit Deferred.t

  val file_changed : unit -> unit

  val eof_reached : unit -> unit Deferred.t

  val start_file_processing_iteration : [ `Postgres | `Sqlite ] -> unit Deferred.t

  val complete_file_processing_iteration : [ `Postgres | `Sqlite ] -> unit Deferred.t
end) =
struct
  let last_rotate_end_timestamp = ref 0.0

  let process_event original yojson =
    match yojson with
    | `List [ `String checkpoint; `Float timestamp ] ->
        let%map () = Handler.process_checkpoint checkpoint timestamp in
        true
    | `Assoc [ ("rotated_log_end", `Float timestamp) ] ->
        last_rotate_end_timestamp := timestamp ;
        return false
    | `Assoc ((head, data) :: other) ->
        let%map () = Handler.process_control ~other head data in
        true
    | _ ->
        Log.Global.error "[WARN] unexpected: %s" original ;
        return true

  let process_log_rotated_start original yojson =
    match yojson with
    | `Assoc [ ("rotated_log_start", `Float timestamp) ] ->
        if Float.(timestamp >= !last_rotate_end_timestamp) then true
        else (
          Log.Global.error
            "[WARN] file rotatation issued but file didn't rotate" ;
          false )
    | _ ->
        Log.Global.error "[WARN] expected rotated_log_start, but got: %s"
          original ;
        false

  (* Returns [true] if processing should continue *)
  let process_line ~rotated line =
    try
      let yojson = Yojson.Safe.from_string line in
      if rotated then return (process_log_rotated_start line yojson)
      else process_event line yojson
    with _ ->
      Log.Global.error "[ERROR] could not parse line: %s" line ;
      return true

  let file_changed inode filename =
    try
      let stat = Core.Unix.stat filename in
      inode <> stat.st_ino
    with Unix.Unix_error _ ->
      Log.Global.error "File '%s' removed" filename ;
      true

  let really_read_line ~inode ~filename ~wait_time reader =
    let pending = ref "" in
    let rec loop () =
      let%bind result =
        Reader.read_until reader (`Char '\n') ~keep_delim:false
      in
      match result with
      | `Eof ->
          if file_changed inode filename then return `File_changed
          else return `Eof_reached
      | `Eof_without_delim data ->
          pending := !pending ^ data ;
          let%bind () = Clock.after wait_time in
          loop ()
      | `Ok line ->
          let line = !pending ^ line in
          pending := "" ;
          return (`Line line)
    in
    loop ()

  let rec process_reader ~inode ~stop_on_eof ~rotated ~filename reader =
    let%bind next_line =
      really_read_line ~inode ~filename ~wait_time:(Time.Span.of_sec 0.2) reader
    in
    match next_line with
    | `Eof_reached ->
        if stop_on_eof then return `Eof_reached
        else
          let%bind () = Handler.eof_reached () in
          return (`Trampoline (Clock.after (Time.Span.of_sec 0.2), rotated))
    | `Line line ->
        if%bind process_line ~rotated line then
          process_reader ~inode ~stop_on_eof ~rotated:false ~filename reader
        else return `File_rotated
    | `File_changed ->
        return `File_changed

  type reader_loop_result = [ `Eof_reached | `File_changed | `File_rotated ]

  let rec process_reader_loop ~inode ~stop_on_eof ~rotated ~filename reader =
    let%bind result =
      Connection_context.with_connection (fun engine ->
          let%bind () = Handler.start_file_processing_iteration engine in
          let%bind result =
            process_reader ~inode ~stop_on_eof ~rotated ~filename reader
          in
          let%bind () = Handler.complete_file_processing_iteration engine in
          return (Ok result) )
    in
    match result with
    | Error err ->
        Log.Global.error "[WARN] Caqti failure in reader process iteration: %s"
          (Caqti_error.show err) ;
        return `Eof_reached
    | Ok result -> (
        match result with
        | `Trampoline (wait, rotated) ->
            let%bind () = wait in
            process_reader_loop ~inode ~stop_on_eof ~rotated ~filename reader
        | #reader_loop_result as result ->
            return result )

  let process_file ?(without_rotation = false) ?(retry_open = true) filename =
    let rec loop rotated =
      let%bind result =
        try_with (fun () ->
            Reader.with_file filename
              ~f:
                (process_reader_loop ~inode:(Core.Unix.stat filename).st_ino
                   ~stop_on_eof:without_rotation ~rotated ~filename ) )
      in
      match result with
      | Ok `File_rotated when without_rotation ->
          Log.Global.info "Done processing rotated file %s" filename ;
          Deferred.unit
      | Ok `Eof_reached ->
          Log.Global.info "Done processing rotated file %s" filename ;
          Deferred.unit
      | Ok `File_rotated ->
          Log.Global.info "File rotated, re-opening %s" filename ;
          let%bind () = Clock.after (Time.Span.of_sec 2.0) in
          loop true
      | Ok `File_changed ->
          Handler.file_changed () ;
          last_rotate_end_timestamp := 0.0 ;
          Log.Global.info "File changed, re-opening %s" filename ;
          let%bind () = Clock.after (Time.Span.of_sec 2.0) in
          loop false
      | Error exn ->
          ignore exn ;
          (*eprintf
            "File '%s' could not be opened, retrying in 20 seconds. Reason:\n\
             %s\n\
             %!"
            filename (Exn.to_string exn) ;*)
          if retry_open then
            let%bind () = Clock.after (Time.Span.of_sec 20.0) in
            loop rotated
          else Deferred.unit
    in
    Log.Global.info "Begin processing trace file: %s" filename ;
    loop false

  let process_roated_files filename =
    Deferred.List.iter ~how:`Sequential (List.range 0 100) ~f:(fun n ->
        let filename_n = sprintf "%s.%d" filename n in
        try
          let _stat = Core.Unix.stat filename_n in
          process_file ~without_rotation:true filename_n
        with Core.Unix.Unix_error _ -> Deferred.unit )
end
