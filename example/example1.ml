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
  let open Gr in
  polyline ~linetype:DASHED_DOTTED ~linewidth:0.7 ~coloridx:23 xarray yarray;
  let t = tick 0. 1. in
  axes t t;
  get1char () |> ignore;
  Workstation.clear ();
  Owl.Arr.(
    polymarker
      ~markertype:CIRCLE
      ~markersize:3.0
      ~coloridx:15
      (transpose xarray)
      (transpose yarray));
  axes t t;
  get1char () |> ignore
