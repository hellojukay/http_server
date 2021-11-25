class http_server cluster port =
  object (self)
    val mutable num = cluster

    val mutable listen_port = port

    val max_queue = 100

    method string_of_address address =
      match address with
      | Unix.ADDR_INET (net, port) ->
          Unix.string_of_inet_addr net ^ Printf.sprintf "%d" port
      | Unix.ADDR_UNIX s -> s

    method bind socket =
      (* 创建 socket *)
      (* 创建本地地址 *)
      let domain = "0.0.0.0" in
      let address = Unix.inet_addr_of_string domain in
      Log.info (Printf.sprintf "binding %s:%d" domain port);
      (* 监听本地端口 *)
      try Unix.bind socket (Unix.ADDR_INET (address, port))
      with Unix.Unix_error (_, s2, s3) as e ->
        Log.error (s2 ^ s3);
        let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
        Printf.eprintf "there was an error: %s%s\n" msg stack;
        raise e

    method wait_pid list =
      let rec wait_all pids =
        match pids with
        | [] -> ()
        | pid :: next ->
            Unix.waitpid [ Unix.WUNTRACED ] pid |> ignore;
            Log.warn (Printf.sprintf "child process %d exited" pid);
            wait_all next
      in
      wait_all list

    method listen socket = Unix.listen socket max_queue

    method accept socket =
      Unix.accept socket |> fun (tcp, address) ->
      Log.info (self#string_of_address address);
      let http = new Http.http tcp in
      http#parse_http ()

    method start () =
      let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
      self#bind socket;
      self#listen socket;

      let pids = ref [] in
      let n = ref num in
      while !n > 0 do
        n := !n - 1;
        let pid = Unix.fork () in
        if pid == 0 then self#accept socket else pids := !pids @ [ pid ]
      done;
      self#wait_pid !pids
  end
