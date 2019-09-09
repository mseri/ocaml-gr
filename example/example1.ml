let get1char () =
  let termio = Unix.tcgetattr Unix.stdin in
  let () =
    Unix.tcsetattr Unix.stdin Unix.TCSADRAIN
      { termio with Unix.c_icanon = false } in
  let res = input_char stdin in
  Unix.tcsetattr Unix.stdin Unix.TCSADRAIN termio;
  res

let to_carray typ x = x |> Array.to_list |> Ctypes.CArray.of_list typ
let captr = Ctypes.CArray.start

let () =
  let xarray = to_carray Ctypes.double [|0.0; 0.2; 0.4; 0.6; 0.8; 1.0|] in
  let yarray = to_carray Ctypes.double [|0.3;0.5;0.4;0.2;0.6;0.7|] in
  let open Gr.Lowlevel in
  polyline 6 (captr xarray) (captr yarray);
  axes (tick 0. 1.) (tick 0. 1.) 0. 0. 1 1 (-0.01);
  get1char () |> ignore