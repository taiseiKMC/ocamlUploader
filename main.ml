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

let run_server ?loglevel ~port () =
  let loglevel = match loglevel with
    | None -> (* default *) Logs.Info
    | Some l ->
      if l = "debug" then Logs.Debug
      else if l = "info" then Logs.Info
      else if l = "warning" then Logs.Warning
      else if l = "error" then Logs.Error
      else if l = "app" then Logs.App
      else failwith ("Unexpected Loglevel: " ^ l) in
  if not @@ Cohttp_lwt_unix.Debug.debug_active () then (
    (* Fmt_tty.setup_std_outputs (); *)
    Logs.set_level ~all:true (Some loglevel);
    Logs.set_reporter (reporter Fmt.stderr));
  Lwt_main.run (OcamlUploader.server port)

let _ =
  let defaultport = 8000 in

  let open Core in
  let open Command.Param in

  let run =
    Command.basic
      ~summary:"Run server"
      (map
         (both
            (flag "--port" (optional int) ~doc:(Format.asprintf "Port port. Default is %d" defaultport))
            (flag "--loglevel" (optional string) ~doc:"Loglevel [app|error|warning|info|error]"))
         ~f:(fun (port, loglevel) ->
             let port = Option.value ~default:defaultport port in
             (run_server ?loglevel ~port))) in
  Command.run run

