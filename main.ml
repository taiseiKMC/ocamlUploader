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

let remove_record uuid =
  Lwt_main.run (OcamlUploader.Manager.remove_record uuid)
  |> Result.get_ok

let add_dummy_record descr filename =
  let (uuid, _) = OcamlUploader.Manager.add_dummy_record descr filename in
  Format.printf "Add record %s\nPut the real file in %s and this record works\n"
    uuid (OcamlUploader.Manager.content_dir ^ uuid)

let _ =
  let defaultport = 8000 in

  let open Core in
  let open Command.Param in

  let run =
    Command.basic
      ~summary:"Run server"
      (map
         (both
            (flag "--port" ~aliases:["-p"] (optional int) ~doc:(Format.asprintf "Port port. Default is %d" defaultport))
            (flag "--loglevel" (optional string) ~doc:"Loglevel [app|error|warning|info|error]"))
         ~f:(fun (port, loglevel) ->
             let port = Option.value ~default:defaultport port in
             (run_server ?loglevel ~port))) in

  let remove_record =
    Command.basic
      ~summary:"Remove the record and file of given Filename(uuid)"
      (map
         (anon ("uuid" %: string))
         ~f:(fun uuid -> (fun () -> remove_record uuid))) in

  let add_dummy_record =
    Command.basic
      ~summary:"Add dummy record to database"
      (map
         (both
            (anon ("description" %: string))
            (flag "--filename" ~aliases:["-f"] (optional string) ~doc:"Filename string"))
         ~f:(fun (descr, fn) -> (fun () -> add_dummy_record descr fn))) in


  Command.group ~summary:"OCaml Uploader" ~preserve_subcommand_order:()
    [ "run", run
    ; "remove", remove_record
    ; "add-dummy", add_dummy_record ]
  |> Command.run

