open Core
open Async
module Block_tracing = Block_tracing

(* TODO: these state must be per log file *)
module Make (Handler : sig
  val process_checkpoint : string -> float -> unit

  val process_control : string -> Yojson.Safe.t -> unit

  val file_changed : unit -> unit

  val eof_reached : unit -> unit
end) =
struct
  let last_rotate_end_timestamp = ref 0.0

  let process_event original yojson =
    match yojson with
    | `List [ `String checkpoint; `Float timestamp ] ->
        Handler.process_checkpoint checkpoint timestamp ;
        true
    | `Assoc [ ("rotated_log_end", `Float timestamp) ] ->
        last_rotate_end_timestamp := timestamp ;
        false
    | `Assoc [ (head, data) ] ->
        Handler.process_control head data ;
        true
    | _ ->
        eprintf "[WARN] unexpected: %s\n%!" original ;
        true

  let process_log_rotated_start original yojson =
    match yojson with
    | `Assoc [ ("rotated_log_start", `Float timestamp) ] ->
        if Float.(timestamp >= !last_rotate_end_timestamp) then true
        else (
          eprintf "[WARN] file rotatation issued but file didn't rotate\n%!" ;
          false )
    | _ ->
        eprintf "[WARN] expected rotated_log_start, but got: %s\n%!" original ;
        false

  let process_line ~rotated line =
    try
      let yojson = Yojson.Safe.from_string line in
      if rotated then process_log_rotated_start line yojson
      else process_event line yojson
    with _ ->
      eprintf "[ERROR] could not parse line: %s\n%!" line ;
      true

  let file_changed inode filename =
    try
      let stat = Core.Unix.stat filename in
      inode <> stat.st_ino
    with Unix.Unix_error _ ->
      eprintf "File '%s' removed\n%!" filename ;
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

  let rec process_reader ~inode ~rotated ~filename reader =
    let%bind next_line =
      really_read_line ~inode ~filename ~wait_time:(Time.Span.of_sec 0.2) reader
    in
    match next_line with
    | `Eof_reached ->
        Handler.eof_reached () ;
        let%bind () = Clock.after (Time.Span.of_sec 0.2) in
        process_reader ~inode ~rotated ~filename reader
    | `Line line ->
        if process_line ~rotated line then
          process_reader ~inode ~rotated ~filename reader
        else return `File_rotated
    | `File_changed ->
        return `File_changed

  let process_file filename =
    let rec loop rotated =
      let%bind result =
        try_with (fun () ->
            Reader.with_file filename
              ~f:
                (process_reader ~inode:(Core.Unix.stat filename).st_ino ~rotated
                   ~filename ) )
      in
      match result with
      | Ok `File_rotated ->
          printf "File rotated, re-opening %s...\n%!" filename ;
          let%bind () = Clock.after (Time.Span.of_sec 2.0) in
          loop true
      | Ok `File_changed ->
          Handler.file_changed () ;
          last_rotate_end_timestamp := 0.0 ;
          printf "File changed, re-opening %s...\n%!" filename ;
          let%bind () = Clock.after (Time.Span.of_sec 2.0) in
          loop false
      | Error exn ->
          eprintf
            "File '%s' could not be opened, retrying after 5 seconds. Reason:\n\
             %s\n\
             %!"
            filename (Exn.to_string exn) ;
          let%bind () = Clock.after (Time.Span.of_sec 5.0) in
          loop rotated
    in
    loop false
end