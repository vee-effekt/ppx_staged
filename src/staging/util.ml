let run_ocamlfind_query package =
    let cmd = Printf.sprintf "ocamlfind query %s" package in
    let ic = Unix.open_process_in cmd in
    match In_channel.input_line ic with
    | Some path -> 
        ignore (Unix.close_process_in ic); 
        path
    | None -> 
        ignore (Unix.close_process_in ic); 
        failwith ("Could not find " ^ package)