let get1char () =
  let termio = Unix.tcgetattr Unix.stdin in
  let () =
    Unix.tcsetattr Unix.stdin Unix.TCSADRAIN { termio with Unix.c_icanon = false }
  in
  let res = input_char stdin in
  Unix.tcsetattr Unix.stdin Unix.TCSADRAIN termio;
  res


let () =
  let xarray = Owl.Arr.of_arrays [| [| 0.0; 0.2; 0.4; 0.6; 0.8; 1.0 |] |] in
  let yarray = Owl.Arr.of_arrays [| [| 0.3; 0.5; 0.4; 0.2; 0.6; 0.7 |] |] in
  Gr.polyline xarray yarray;
  Gr.Lowlevel.(axes (tick 0. 1.) (tick 0. 1.) 0. 0. 1 1 (-0.01));
  get1char () |> ignore;
  Gr.Lowlevel.clearws ();
  Owl.Arr.(Gr.polymarker (transpose xarray) (transpose yarray));
  Gr.Lowlevel.(axes (tick 0. 1.) (tick 0. 1.) 0. 0. 1 1 (-0.01));
  get1char () |> ignore
