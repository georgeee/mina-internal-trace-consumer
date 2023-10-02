open Core_kernel
open Async

module Db = struct
  type pool_t =
    ((module Caqti_async.CONNECTION), Caqti_error.t) Caqti_async.Pool.t

  let set, get =
    let dbpool : ([ `Postgres | `Sqlite ] * pool_t) option ref = ref None in
    let set_db engine pool = dbpool := Some (engine, pool) in
    let get_db () =
      Option.value_exn ~message:"Database not initialized" !dbpool
    in
    (set_db, get_db)
end

let key = Univ_map.Key.create ~name:"connection" sexp_of_opaque

let with_connection f =
  let engine, pool = Db.get () in
  let with_connection_context f (conn : (module Caqti_async.CONNECTION)) =
    Async_kernel.Async_kernel_scheduler.with_local key (Some conn) ~f:(fun () ->
        f engine )
  in
  Caqti_async.Pool.use (with_connection_context f) pool

let get_opt () = Async_kernel.Async_kernel_scheduler.find_local key

let use_current f =
  match get_opt () with
  | Some conn ->
      f conn
  | None ->
      Caqti_async.Pool.use f (snd @@ Db.get ())

let start () =
  let (module Db) = get_opt () |> Option.value_exn in
  match%bind Db.start () with
  | Error err ->
      Log.Global.error "[WARN] error when starting transaction: %s"
        (Caqti_error.show err) ;
      return ()
  | Ok () ->
      return ()

let commit () =
  let (module Db) = get_opt () |> Option.value_exn in
  match%bind Db.commit () with
  | Error err ->
      Log.Global.error "[WARN] error when comitting transaction: %s"
        (Caqti_error.show err) ;
      return ()
  | Ok () ->
      return ()
