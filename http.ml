class http tcp =
  object
    method parse_http () =
      Log.info (Printf.sprintf "[%d] get message" (Unix.getpid ()));
      let first_line = input_line (Unix.in_channel_of_descr tcp) in
      Log.info (Printf.sprintf "%s\n" first_line)
  end
