let get1char () =
  let termio = Unix.tcgetattr Unix.stdin in
  let () =
    Unix.tcsetattr Unix.stdin Unix.TCSADRAIN { termio with Unix.c_icanon = false }
  in
  let res = input_char stdin in
  Unix.tcsetattr Unix.stdin Unix.TCSADRAIN termio;
  res


let to_carray typ x = x |> Array.to_list |> Ctypes.CArray.of_list typ
let captr = Ctypes.CArray.start

let () =
  (* This could be simplified following the example of https://github.com/jheinen/GR.jl/blob/master/src/jlgr.jl *)
  let xs = List.init 100 (fun _ -> -2.0 +. (4.0 *. Random.float 1.)) in
  let ys = List.init 100 (fun _ -> -2.0 +. (4.0 *. Random.float 1.)) in
  let zs =
    List.fold_left2
      (fun acc x y -> (x *. exp (-.((x ** 2.) +. (y ** 2.)))) :: acc)
      []
      xs
      ys
    |> List.rev
  in
  let xd = Ctypes.(CArray.of_list double xs) in
  let yd = Ctypes.(CArray.of_list double ys) in
  let zd = Ctypes.(CArray.of_list double zs) in
  let xa = Ctypes.(CArray.make double 200) in
  let ya = Ctypes.(CArray.make double 200) in
  let za = Ctypes.(CArray.make double @@ (200 * 200)) in
  let open Gr in
  Lowlevel.setviewport 0.1 0.95 0.1 0.95;
  Lowlevel.setwindow (-2.0) 2.0 (-2.0) 2.0;
  Lowlevel.setspace (-0.5) 0.5 0 90 |> ignore;
  set_markersize 1.0;
  set_markertype SOLID_CIRCLE;
  set_char_height 0.024;
  set_text_align (Some CENTER) None;
  set_text_font_prec TIMES_ROMAN;
  Lowlevel.gridit
    100
    (captr xd)
    (captr yd)
    (captr zd)
    200
    200
    (captr xa)
    (captr ya)
    (captr za);
  let h =
    List.init 20 (fun i -> -0.5 +. (float_of_int i /. 19.0))
    |> Ctypes.(CArray.of_list double)
  in
  Lowlevel.surface 200 200 (captr xa) (captr ya) (captr za) 5;
  (* get1char () |> ignore; *)
  Lowlevel.contour 200 200 20 (captr xa) (captr ya) (captr h) (captr za) 0;
  (* get1char () |> ignore; *)
  let xd' = Ctypes.(bigarray_of_array genarray Bigarray.Float64 xd) in
  let yd' = Ctypes.(bigarray_of_array genarray Bigarray.Float64 yd) in
  polymarker xd' yd';
  (* get1char () |> ignore; *)
  axes ~origin:(-2.0, -2.0) ~major:(2, 2) ~tick_size:0.01 0.25 0.25;
  (* Lowlevel.axes 0.25 0.25 (-2.0) (-2.0) 2 2 0.01; *)
  (* get1char () |> ignore; *)
  Lowlevel.mathtex 0.5 0.91 {|\mbox{Attempt to plot tex stuff, e.g. } \int_0^1\sin(x)|};
  get1char () |> ignore
