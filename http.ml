class http tcp =
  object (self)
    method parse_http () =
      Log.info (Printf.sprintf "[%d] get message" (Unix.getpid ()));
      let first_line = self#first_line () in
      Log.info (Printf.sprintf "%s\n" first_line);
      self#write "Hello World";
      Unix.close tcp

    method first_line () =
      let fd = Unix.in_channel_of_descr tcp in
      let line = input_line fd in
      try
        while true do
          let s = input_line fd in
          if Str.string_match (Str.regexp "\r?\n?") s 0 then raise End_of_file
          else s |> Log.info
        done;
        line
      with End_of_file -> line

    method write msg =
      let fd = Unix.out_channel_of_descr tcp in
      Printf.fprintf fd "%s" msg
  end
