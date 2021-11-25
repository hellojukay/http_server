open Server

let () =
  let pid = Unix.getpid () in
  Log.info (Printf.sprintf "server start up with pid[%d]" pid);
  let server = new http_server 3 8080 in
  server#start ()
