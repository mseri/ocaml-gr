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
  (*let xarray = to_carray Ctypes.double [|0.0; 0.2; 0.4; 0.6; 0.8; 1.0|] in
    let yarray = to_carray Ctypes.double [|0.3;0.5;0.4;0.2;0.6;0.7|] in*)
  let xs = List.init 100 (fun _ -> -2.0 +. 4.0 *. Random.float 1.) in
  let ys = List.init 100 (fun _ -> -2.0 +. 4.0 *. Random.float 1.) in
  let zs = List.fold_left2 (fun acc x y -> x *. exp(-.(x**2. +. y**2.)) :: acc ) [] xs ys |> List.rev in

  let xd = Ctypes.(CArray.of_list double xs) in
  let yd = Ctypes.(CArray.of_list double ys) in
  let zd = Ctypes.(CArray.of_list double zs) in

  let xa = Ctypes.(CArray.make double 200) in
  let ya = Ctypes.(CArray.make double 200) in
  let za = Ctypes.(CArray.make double @@ 200*200) in
  let open Gr.Lowlevel in

  setviewport 0.1 0.95 0.1 0.95;
  setwindow (-2.0)  2.0  (-2.0)  2.0;
  setspace (-0.5)  0.5  0 90 |> ignore;
  setmarkersize 1.0;
  setmarkertype (-1); (*GKS_K_MARKERTYPE_SOLID_CIRCLE*)
  setcharheight 0.024;
  settextalign 2 0;
  settextfontprec 3 0;

  gridit 100 (captr xd) (captr yd) (captr zd) 200 200 (captr xa) (captr ya) (captr za);

  let h = List.init 20 (fun i -> -0.5 +. (float_of_int i) /. 19.0) |> Ctypes.(CArray.of_list double) in

  surface 200 200 (captr xa) (captr ya) (captr za) 5;
  get1char () |> ignore;
  contour 200 200 20 (captr xa) (captr ya) (captr h) (captr za) 0;
  get1char () |> ignore;
  polymarker 100 (captr xd) (captr yd);
  get1char () |> ignore;
  axes 0.25 0.25 (-2.0) (-2.0) 2 2 0.01;
  get1char () |> ignore;
