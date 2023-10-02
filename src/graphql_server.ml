open Core
open Async
module Graphql_cohttp_async =
  Graphql_internal.Make (Graphql_async.Schema) (Cohttp_async.Io)
    (Cohttp_async.Body)

let show_result_error = function
  | Ok _ ->
      ()
  | Error error ->
      Log.Global.error "SQL ERROR: %s" (Caqti_error.show error)

module Queries = struct
  open Graphql_async
  open Schema

  module JSON = struct
    type t = Yojson.Basic.t

    let parse = Base.Fn.id

    let serialize = Base.Fn.id

    let typ () = scalar "JSON" ~doc:"Arbitrary JSON" ~coerce:serialize
  end

  let json_type : (unit, JSON.t option) typ = JSON.typ ()

  let order =
    Arg.enum "Results order"
      ~values:
        [ enum_value ~doc:"Ascending order" "Ascending" ~value:`Asc
        ; enum_value ~doc:"Descending order" "Descending" ~value:`Desc
        ]

  let get_block_trace =
    io_field "blockTrace" ~doc:"Block trace" ~typ:json_type
      ~args:Arg.[ arg "block_identifier" ~typ:(non_null string) ]
      ~resolve:(fun _info () block ->
        let%bind traces = Persistent_registry.get_block_traces block in
        show_result_error traces ;
        let traces = Result.ok traces |> Option.value ~default:[] in
        match List.hd traces with
        | None ->
            return (Ok None)
        | Some (block_trace_id, trace) ->
            let%bind checkpoints =
              Persistent_registry.get_block_trace_checkpoints block_trace_id
            in
            show_result_error checkpoints ;
            let checkpoints =
              Result.ok checkpoints |> Option.value ~default:[]
            in
            let trace =
              Store.Persisted_block_trace.to_block_trace ~checkpoints trace
            in
            return
            @@ Ok
                 (Some
                    ( trace |> Block_tracing.Trace.to_yojson
                    |> Yojson.Safe.to_basic ) ) )

  let get_block_structured_trace =
    io_field "blockStructuredTrace" ~doc:"Block structured trace" ~typ:json_type
      ~args:Arg.[ arg "block_identifier" ~typ:(non_null string) ]
      ~resolve:(fun _info () block ->
        let%bind traces = Persistent_registry.get_block_traces block in
        show_result_error traces ;
        let traces = Result.ok traces |> Option.value ~default:[] in
        match List.hd traces with
        | None ->
            return (Ok None)
        | Some (block_trace_id, trace) ->
            let%bind checkpoints =
              Persistent_registry.get_block_trace_checkpoints block_trace_id
            in
            show_result_error checkpoints ;
            let checkpoints =
              Result.ok checkpoints |> Option.value ~default:[]
            in
            let trace =
              Store.Persisted_block_trace.to_block_trace ~checkpoints trace
            in
            let trace = Block_tracing.Structured_trace.of_flat_trace trace in
            return
            @@ Ok
                 (Some
                    ( trace |> Block_tracing.Structured_trace.to_yojson
                    |> Yojson.Safe.to_basic ) ) )

  let list_block_traces =
    io_field "blockTraces" ~doc:"Block with traces" ~typ:(non_null json_type)
      ~args:
        Arg.
          [ arg "maxLength" ~doc:"The maximum number of block traces to return."
              ~typ:int
          ; arg "offset"
              ~doc:
                "Amount of traces to skip when producing the list of results."
              ~typ:int
          ; arg "height" ~doc:"Return traces with matching height." ~typ:int
          ; arg "global_slot" ~doc:"Return traces with matching global_slot."
              ~typ:int
          ; arg "chainLength"
              ~doc:
                "When filtering by height, this controls the chain lenght so \
                 that parent blocks can be included too."
              ~typ:int
          ; arg "order"
              ~doc:"Results order (by blockchain_length. Ascending by default)."
              ~typ:order
          ]
      ~resolve:(fun _info () max_length offset height global_slot chain_length
                    order ->
        let%bind traces =
          Persistent_registry.get_all_block_traces ?max_length ?offset ?height
            ?global_slot ?chain_length ?order ()
        in
        show_result_error traces ;
        let traces = Result.ok traces |> Option.value ~default:[] in
        let traces = { Block_tracing.Registry.traces; produced_traces = [] } in
        return
        @@ Ok
             ( Block_tracing.Registry.traces_to_yojson traces
             |> Yojson.Safe.to_basic ) )

  let get_block_traces_distribution =
    io_field "blockTracesDistribution"
      ~doc:"Block trace checkpoints distribution" ~typ:(non_null json_type)
      ~args:Arg.[]
      ~resolve:(fun _info () ->
        let open Block_tracing.Distributions in
        let%bind all = Persistent_registry.get_distributions () in
        show_result_error all ;
        let all =
          Result.ok all |> Option.value_map ~f:Hashtbl.data ~default:[]
        in
        let compare d1 d2 = Float.compare d1.total_time d2.total_time in
        let distributions = List.sort ~compare all in
        return @@ Ok (listing_to_yojson distributions |> Yojson.Safe.to_basic)
        )

  let get_block_traces_distribution_structured =
    io_field "blockTracesDistributionStructured"
      ~doc:"Block trace checkpoints distribution (structured)"
      ~typ:(non_null json_type)
      ~args:Arg.[]
      ~resolve:(fun _info () ->
        let%bind all = Persistent_registry.get_distributions () in
        show_result_error all ;
        let all =
          Result.ok all |> Option.value_map ~f:Hashtbl.data ~default:[]
        in
        let structured_distributions =
          Structured_distribution.of_flat_distributions all
        in
        return
        @@ Ok
             ( Structured_distribution.listing_to_yojson structured_distributions
             |> Yojson.Safe.to_basic ) )

  let commands =
    [ get_block_trace
    ; get_block_structured_trace
    ; list_block_traces
    ; get_block_traces_distribution
    ; get_block_traces_distribution_structured
    ]
end

let add_cors_headers (headers : Cohttp.Header.t) =
  Cohttp.Header.add_list headers
    [ ("Access-Control-Allow-Origin", "*")
    ; ("Access-Control-Allow-Methods", "GET, POST, PATCH, PUT, DELETE, OPTIONS")
    ; ("Access-Control-Allow-Headers", "Content-Type, Authorization")
    ]

let respond_with_cors body status =
  let headers = Cohttp.Header.init () |> add_cors_headers in
  Cohttp_async.Server.respond_string ~status ~headers body

let create_graphql_server ~bind_to_address ~schema ~server_description port =
  let graphql_callback =
    Graphql_cohttp_async.make_callback (fun _req -> ()) schema
  in
  Cohttp_async.(
    Server.create_expert
      ~on_handler_error:
        (`Call
          (fun _net exn ->
            Log.Global.error "Exception while handling REST server request: %s"
              (Exn.to_string_mach exn) ) )
      (Tcp.Where_to_listen.bind_to bind_to_address (On_port port))
      (fun ~body _sock req ->
        let uri = Cohttp.Request.uri req in
        let lift x = `Response x in
        let lift_with_cors_graphql action =
          match action with
          | `Response (response, body) ->
              let headers = Cohttp.Response.headers response in
              let headers = add_cors_headers headers in
              let response = { response with Cohttp.Response.headers } in
              return (`Response (response, body))
          | `Expert _ as action ->
              return action
        in
        match Cohttp.Request.meth req with
        | `OPTIONS ->
            respond_with_cors "" `OK >>| lift
        | `GET | `POST | `PUT | `DELETE | `HEAD | `CONNECT | `TRACE | `PATCH
          -> (
            match Uri.path uri with
            | "" | "/" ->
                let body =
                  "This page is intentionally left blank. The graphql endpoint \
                   can be found at `/graphql`."
                in
                respond_with_cors body `OK >>| lift
            | "/graphql" ->
                graphql_callback () req body >>= lift_with_cors_graphql
            | _ ->
                respond_with_cors "Route not found" `Not_found >>| lift )
        | `Other _ ->
            respond_with_cors "HTTP method not supported" `Method_not_allowed
            >>| lift ))
  |> Deferred.map ~f:(fun _ ->
         Log.Global.info "Created %s at: http://localhost:%i/graphql"
           server_description port )

let schema =
  Graphql_async.Schema.schema ~mutations:[] ~subscriptions:[] Queries.commands
