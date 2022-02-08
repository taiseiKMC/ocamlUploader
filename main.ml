let reporter ppf =
  let report src level ~over k msgf =
    let k _ =
      over () ;
      k () in
    let with_metadata header _tags k ppf fmt =
      Format.kfprintf k ppf
        ("%a[%a]: " ^^ fmt ^^ "\n%!")
        Logs_fmt.pp_header (level, header)
        Fmt.(styled `Magenta string)
        (Logs.Src.name src) in
    msgf @@ fun ?header ?tags fmt -> with_metadata header tags k ppf fmt in
  { Logs.report }

let cmdparse () =
  let binname = "ocamlUploader" in
  let defaultport = 8000 in
  let port = ref defaultport in
  let usage = Format.sprintf "%s [--port PORT]" binname in
  Arg.parse
    [("--port", Int (fun p -> port := p), "port")]
    ignore
    usage;
  !port

let _ =
  let port = cmdparse () in
  if not @@ Cohttp_lwt_unix.Debug.debug_active () then (
    (* Fmt_tty.setup_std_outputs (); *)
    Logs.set_level ~all:true (Some Logs.Debug);
    Logs.set_reporter (reporter Fmt.stderr));
  Lwt_main.run (OcamlUploader.server port)
