class http_server cluster port =
  object (self)
    val mutable num = cluster

    val mutable listen_port = port

    val max_queue = 100

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
            Log.info (Printf.sprintf "child process %d exited\n" pid);
            wait_all next
      in
      wait_all list

    method listen socket = Unix.listen socket max_queue

    method accept socket = Unix.accept socket |> ignore

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
