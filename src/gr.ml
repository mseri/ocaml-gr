module Lowlevel = Lowlevel

exception Unimplemented

type workstation_type =
  | WISS (** Workstation Independent Segment ptr Storage *)
  | WinGDI (** Windows ptr GDI *)
  | PS_1 (**PostScript (b/w @-> color) *)
  | PS_2 (**PostScript (b/w @-> color) *)
  | PS_3 (**PostScript (b/w @-> color) *)
  | PS_4 (**PostScript (b/w @-> color) *)
  | PDFPlain (** Portable Document Format ptr plain *)
  | PDFCompressed (** Portable Document Format ptr compressed *)
  | X_1 (** X ptr Windows *)
  | X_2 (** X ptr Windows *)
  | X_3 (** X ptr Windows *)
  | X_4 (** X ptr Windows *)
  | SunRF (** Sun Raster file (RF) *)
  | GIF87 (** Graphics Interchange Format ptr GIF87 *)
  | GIF89 (**Graphics Interchange Format ptr GIF89 *)
  | MotifUIL (** Motif User Interface Language (UIL) *)
  | BMP (** Windows Bitmap (BMP) *)
  | JPEG (** JPEG image ptr file *)
  | PNG (** Portable Network Graphics file (PNG) *)
  | TIFF (** Tagged Image File Format (TIFF) *)
  | Gtk (** ptr Gtk *)
  | Wx (** ptr wxWidgets *)
  | Qt4 (** ptr Qt4 *)
  | SVG (** Scaleable Vector Graphics (SVG) *)
  | WMF (** Windows ptr Metafile *)
  | Quartz (** ptr Quartz *)
  | Sock (** Socket ptr driver *)
  | ZMQ (** 0MQ ptr driver *)
  | OGL (** ptr OpenGL *)

let int_of_workstation_type = function
  | WISS -> 5
  | WinGDI -> 41
  | PS_1 -> 61
  | PS_2 -> 62
  | PS_3 -> 63
  | PS_4 -> 64
  | PDFPlain -> 101
  | PDFCompressed -> 102
  | X_1 -> 210
  | X_2 -> 211
  | X_3 -> 213
  | X_4 -> 212
  | SunRF -> 214
  | GIF87 -> 215
  | GIF89 -> 218
  | MotifUIL -> 216
  | BMP -> 320
  | JPEG -> 321
  | PNG -> 322
  | TIFF -> 323
  | Gtk -> 371
  | Wx -> 380
  | Qt4 -> 381
  | SVG -> 382
  | WMF -> 390
  | Quartz -> 400
  | Sock -> 410
  | ZMQ -> 415
  | OGL -> 420


type linetype =
  | SOLID (** Solid line *)
  | DASHED (** Dashed line *)
  | DOTTED (** Dotted line *)
  | DASHED_DOTTED (** Dashed-dotted line *)
  | DASH_2_DOT (** Sequence of one dash followed by two dots *)
  | DASH_3_DOT (** Sequence of one dash followed by three dots *)
  | LONG_DASH (** Sequence of long dashes *)
  | LONG_SHORT_DASH (** Sequence of a long dash followed by a short dash *)
  | SPACED_DASH (** Sequence of dashes double spaced *)
  | SPACED_DOT (** Sequence of dots double spaced *)
  | DOUBLE_DOT (** Sequence of pairs of dots *)
  | TRIPLE_DOT (** Sequence of groups of three dots *)

let int_of_linetype = function
  | SOLID -> 1
  | DASHED -> 2
  | DOTTED -> 3
  | DASHED_DOTTED -> 4
  | DASH_2_DOT -> -1
  | DASH_3_DOT -> -2
  | LONG_DASH -> -3
  | LONG_SHORT_DASH -> -4
  | SPACED_DASH -> -5
  | SPACED_DOT -> -6
  | DOUBLE_DOT -> -7
  | TRIPLE_DOT -> -8


let linetype_of_int = function
  | 1 -> SOLID
  | 2 -> DASHED
  | 3 -> DOTTED
  | 4 -> DASHED_DOTTED
  | -1 -> DASH_2_DOT
  | -2 -> DASH_3_DOT
  | -3 -> LONG_DASH
  | -4 -> LONG_SHORT_DASH
  | -5 -> SPACED_DASH
  | -6 -> SPACED_DOT
  | -7 -> DOUBLE_DOT
  | -8 -> TRIPLE_DOT
  | d -> failwith @@ "Error when inferring line type. Got " ^ string_of_int d


type markertype =
  | DOT (** Smallest displayable dot *)
  | PLUS (** Plus sign *)
  | ASTERISK (** Asterisk *)
  | CIRCLE (** Hollow circle *)
  | DIAGONAL_CROSS (** Diagonal cross *)
  | SOLID_CIRCLE (** Filled circle *)
  | TRIANGLE_UP (** Hollow triangle pointing upward *)
  | SOLID_TRI_UP (** Filled triangle pointing upward *)
  | TRIANGLE_DOWN (** Hollow triangle pointing downward *)
  | SOLID_TRI_DOWN (** Filled triangle pointing downward *)
  | SQUARE (** Hollow square *)
  | SOLID_SQUARE (** Filled square *)
  | BOWTIE (** Hollow bowtie *)
  | SOLID_BOWTIE (** Filled bowtie *)
  | HGLASS (** Hollow hourglass *)
  | SOLID_HGLASS (** Filled hourglass *)
  | DIAMOND (** Hollow diamond *)
  | SOLID_DIAMOND (** Filled Diamond *)
  | STAR (** Hollow star *)
  | SOLID_STAR (** Filled Star *)
  | TRI_UP_DOWN (** Hollow triangles pointing up and down overlaid *)
  | SOLID_TRI_RIGHT (** Filled triangle point right *)
  | SOLID_TRI_LEFT (** Filled triangle pointing left *)
  | HOLLOW_PLUS (** Hollow plus sign *)
  | SOLID_PLUS (** Solid plus sign *)
  | PENTAGON (** Pentagon *)
  | HEXAGON (** Hexagon *)
  | HEPTAGON (** Heptagon *)
  | OCTAGON (** Octagon *)
  | STAR_4 (** 4-pointed star *)
  | STAR_5 (** 5-pointed star (pentagram) *)
  | STAR_6 (** 6-pointed star (hexagram) *)
  | STAR_7 (** 7-pointed star (heptagram) *)
  | STAR_8 (** 8-pointed star (octagram) *)
  | VLINE (** verical line *)
  | HLINE (** horizontal line *)
  | OMARK (** o-mark *)

let int_of_markertype = function
  | DOT -> 1
  | PLUS -> 2
  | ASTERISK -> 3
  | CIRCLE -> 4
  | DIAGONAL_CROSS -> 5
  | SOLID_CIRCLE -> -1
  | TRIANGLE_UP -> -2
  | SOLID_TRI_UP -> -3
  | TRIANGLE_DOWN -> -4
  | SOLID_TRI_DOWN -> -5
  | SQUARE -> -6
  | SOLID_SQUARE -> -7
  | BOWTIE -> -8
  | SOLID_BOWTIE -> -9
  | HGLASS -> -10
  | SOLID_HGLASS -> -11
  | DIAMOND -> -12
  | SOLID_DIAMOND -> -13
  | STAR -> -14
  | SOLID_STAR -> -15
  | TRI_UP_DOWN -> -16
  | SOLID_TRI_RIGHT -> -17
  | SOLID_TRI_LEFT -> -18
  | HOLLOW_PLUS -> -19
  | SOLID_PLUS -> -20
  | PENTAGON -> -21
  | HEXAGON -> -22
  | HEPTAGON -> -23
  | OCTAGON -> -24
  | STAR_4 -> -25
  | STAR_5 -> -26
  | STAR_6 -> -27
  | STAR_7 -> -28
  | STAR_8 -> -29
  | VLINE -> -30
  | HLINE -> -31
  | OMARK -> -32


let markertype_of_int = function
  | 1 -> DOT
  | 2 -> PLUS
  | 3 -> ASTERISK
  | 4 -> CIRCLE
  | 5 -> DIAGONAL_CROSS
  | -1 -> SOLID_CIRCLE
  | -2 -> TRIANGLE_UP
  | -3 -> SOLID_TRI_UP
  | -4 -> TRIANGLE_DOWN
  | -5 -> SOLID_TRI_DOWN
  | -6 -> SQUARE
  | -7 -> SOLID_SQUARE
  | -8 -> BOWTIE
  | -9 -> SOLID_BOWTIE
  | -10 -> HGLASS
  | -11 -> SOLID_HGLASS
  | -12 -> DIAMOND
  | -13 -> SOLID_DIAMOND
  | -14 -> STAR
  | -15 -> SOLID_STAR
  | -16 -> TRI_UP_DOWN
  | -17 -> SOLID_TRI_RIGHT
  | -18 -> SOLID_TRI_LEFT
  | -19 -> HOLLOW_PLUS
  | -20 -> SOLID_PLUS
  | -21 -> PENTAGON
  | -22 -> HEXAGON
  | -23 -> HEPTAGON
  | -24 -> OCTAGON
  | -25 -> STAR_4
  | -26 -> STAR_5
  | -27 -> STAR_6
  | -28 -> STAR_7
  | -29 -> STAR_8
  | -30 -> VLINE
  | -31 -> HLINE
  | -32 -> OMARK
  | d -> failwith @@ "Error when inferring marker type. Got " ^ string_of_int d


type scale_options =
  | OPTION_X_LOG (** Logarithmic X-axis *)
  | OPTION_Y_LOG (** Logarithmic Y-axis *)
  | OPTION_Z_LOG (** Logarithmic Z-axis *)
  | OPTION_FLIP_X (** Flip X-axis *)
  | OPTION_FLIP_Y (** Flip Y-axis *)
  | OPTION_FLIP_Z (** Flip Z-axis *)

let int_of_scale_options opts =
  let int_of = function
    | OPTION_X_LOG -> 1
    | OPTION_Y_LOG -> 2
    | OPTION_Z_LOG -> 4
    | OPTION_FLIP_X -> 8
    | OPTION_FLIP_Y -> 16
    | OPTION_FLIP_Z -> 32
  in
  List.fold_left (fun acc s -> acc + int_of s) 0 opts


let clearws = Lowlevel.clearws
let updatews = Lowlevel.updatews
let set_linetype lt = lt |> int_of_linetype |> Lowlevel.setlinetype

let current_linetype () =
  let open Ctypes in
  let c = allocate int 0 in
  Lowlevel.inqlinetype c;
  linetype_of_int !@c


(** [set_linewidth lw] defines the line width of subsequent polyline output primitives.

The line width is calculated as the nominal line width generated on the workstation multiplied by the line width scale factor.
This value is mapped by the workstation to the nearest available line width.
The default line width is 1.0, or 1 times the line width generated on the graphics device.
*)
let set_linewidth = Lowlevel.setlinewidth

let current_linewidth () =
  let open Ctypes in
  let c = allocate double 0.0 in
  Lowlevel.inqlinewidth c;
  !@c


(** [set_linecolorindex c] defines the color of subsequent polyline output primitives.
Note: c < 1256
*)
let set_linecolorindex = function
  | c when c >= 0 && c < 1256 -> Lowlevel.setlinecolorind c
  | c -> failwith @@ "Color index must be in the range [0, 1256]. Got " ^ string_of_int c


let current_linecolorindex () =
  let open Ctypes in
  let c = allocate int 0 in
  Lowlevel.inqlinecolorind c;
  !@c


let set_markertype mt = mt |> int_of_markertype |> Lowlevel.setmarkertype

let current_markertype () =
  let open Ctypes in
  let c = allocate int 0 in
  Lowlevel.inqmarkertype c;
  markertype_of_int !@c


(** [set_markersize ms] specify the marker size for polymarkers.

The polymarker size is calculated as the nominal size generated on the graphics device multiplied by the marker size scale factor. 
*)
let set_markersize = Lowlevel.setmarkersize

(** [set_markercolorindex c] define the color of subsequent markers output primitives.
Note: c < 1256
*)
let set_markercolorindex = function
  | c when c >= 0 && c < 1256 -> Lowlevel.setmarkercolorind c
  | c -> failwith @@ "Color index must be in the range [0, 1256]. Got " ^ string_of_int c


let current_markercolorindex () =
  let open Ctypes in
  let c = allocate int 0 in
  Lowlevel.inqmarkercolorind c;
  !@c


(* TODO: make sure to reset linetype linewidth coloridx to the original values *)

(**
[polyline ?linetype ?linewidth ?coloridx x y] draws a polyline using the current line attributes, starting from the first data point and ending at the last data point.

The values for [x] and [y] are in world coordinates.
The attributes that control the appearance of a polyline are linetype, linewidth and color index.
*)
let polyline ?linetype ?linewidth ?coloridx x y =
  Lowlevel.savestate ();
  Option.iter set_linetype linetype;
  Option.iter set_linewidth linewidth;
  Option.iter set_linecolorindex coloridx;
  let n, x', y' = Lowlevel.get_size_and_pointers x y in
  Lowlevel.polyline n x' y';
  Lowlevel.restorestate ()


(**
[polymarker ?markertype ?markersize ?coloridx x y] draws marker symbols centered at the given data points.

The values for [x] and [y] are in world coordinates.
The attributes that control the appearance of a polyline are markertype, markersize and color index.
*)
let polymarker ?markertype ?markersize ?coloridx x y =
  Lowlevel.savestate ();
  Option.iter set_markertype markertype;
  Option.iter set_markersize markersize;
  Option.iter set_markercolorindex coloridx;
  let n, x', y' = Lowlevel.get_size_and_pointers x y in
  Lowlevel.polymarker n x' y';
  Lowlevel.restorestate ()


(** [text x y content] draws a text at position [x], [y] using the current text attributes.

The values for [x] and [y] are in normalized device coordinates.
The attributes that control the appearance of text are text font and precision, character expansion factor, character spacing, text color index, character height, character up vector, text path and text alignment. 
*)
let text = Lowlevel.text

(** 
[fillarea x y] allows you to specify a polygonal shape of an area to be filled.
The vectors [x] and [y] specify the coordinates of the polygonal shape corners.

The attributes that control the appearance of fill areas are fill area interior style, fill area style index and fill area color index. 
*)
let fillarea x y =
  let n, x', y' = Lowlevel.get_size_and_pointers x y in
  Lowlevel.fillarea n x' y'


(** [cellarray (xmin, xmax) (ymin, ymax) (dimx, dimy) (scol, srow) (ncol, nrow) color] displays rasterlike images in a device-independent manner.
The cell array function partitions a rectangle given by two corner points into DIMX X DIMY cells, each of them colored individually by the corresponding color index of the given cell array.

The values for xmin, xmax, ymin and ymax are in world coordinates.

Parameters:
        xmin: X coordinate of the lower left point of the rectangle
        ymin: Y coordinate of the lower left point of the rectangle
        xmax: X coordinate of the upper right point of the rectangle
        ymax: Y coordinate of the upper right point of the rectangle
        dimx: X dimension of the color index array
        dimy: Y dimension of the color index array
        scol: number of leading columns in the color index array
        srow: number of leading rows in the color index array
        ncol: total number of columns in the color index array
        nrow: total number of rows in the color index array
        color: color index array
  
Note: gr_nonuniformcellarray and gr_polycellarray have been introduced in newer versions of gr.
*)
let cellarray (xmin, xmax) (ymin, ymax) (dimx, dimy) (scol, srow) (ncol, nrow) colors =
  let color' = Ctypes.(bigarray_start genarray colors) in
  Lowlevel.cellarray xmin xmax ymin ymax dimx dimy scol srow ncol nrow color'


(* let gdp = ... (* No idea what this does... *) *)

type spline_algo =
  | GeneralizedCrossValidatedSmoothing
  | InterpolatingNaturalCubic
  | CubicBSpline

(** [spline ?linetype ?linewidth ?coloridx x y m method_t] generates a cubic spline-fit, starting from the first data point and ending at the last data point.

    The values for [x] and [y] are in world coordinates.
    The attributes that control the appearance of a spline-fit are linetype, linewidth and color index.

    Parameters
            n: The number of points
            px: The X coordinates
            py: The Y coordinates
            m: The number of points in the polygon to be drawn (m > n)
            method: The smoothing method

    If method is > 0, then a generalized cross-validated smoothing spline is calculated. If method is 0, then an interpolating natural cubic spline is calculated. If method is < -1, then a cubic B-spline is calculated.
*)
let spline ?linetype ?linewidth ?coloridx x y m spline_algo =
  Lowlevel.savestate ();
  Option.iter set_linetype linetype;
  Option.iter set_linewidth linewidth;
  Option.iter set_linecolorindex coloridx;
  let algo =
    match spline_algo with
    | GeneralizedCrossValidatedSmoothing -> 1
    | InterpolatingNaturalCubic -> 0
    | CubicBSpline -> -1
  in
  let n, x', y' = Lowlevel.get_size_and_pointers x y in
  Lowlevel.spline n x' y' m algo;
  Lowlevel.restorestate ()


(** [gridit x y z (nx, ny)] interpolates data from arbitrary points at points on a rectangular grid.

Parameters

        nd: The number of input points
        xd: The X coordinates of the input points
        yd: The Y coordinates of the input points
        zd: The values of the points
        nx: The number of points in X direction for the output grid
        ny: The number of points in Y direction for the output grid

 *)
let gridit _x _y _z (_nx, _ny) = raise Unimplemented

(* Will return 
  x: The points in X direction for the output grid
  y: The points in Y direction for the output grid
  z: The interpolated values on the nx x ny grid points
*)

let tick = Lowlevel.tick

(*
    [axes ?scale ?linetype ?linewidth ?org:(0,0) ?major:(0,0) ?size:1 x_tick y_tick] draws X and Y coordinate axes with linearly and/or logarithmically spaced tick marks.
    Tick marks are positioned along each axis so that major tick marks fall on the axes origin (whether visible or not).
    Major tick marks are labeled with the corresponding data values.
    Axes are drawn according to the scale of the window.

    Parameters
        x_tick: The interval between minor tick marks on the X axis.
        y_tick: The interval between minor tick marks on the Y axis.
        x_org: The world coordinate of the origin (point of intersection) of the X axis.
        y_org: The world coordinate of the origin (point of intersection) of the Y axis.
        major_x: Unitless integer value specifying the number of minor tick intervals between major tick marks on the X axis. Values of 0 or 1 imply no minor ticks. Negative values specify no labels will be drawn for the associated axis.
        major_y: Unitless integer value specifying the number of minor tick intervals between major tick marks on the Y axis. Values of 0 or 1 imply no minor ticks. Negative values specify no labels will be drawn for the associated axis.
        tick_size: The length of minor tick marks specified in a normalized device coordinate unit. Major tick marks are twice as long as minor tick marks. A negative value reverses the tick marks on the axes from inward facing to outward facing (or vice versa)
    *)
let axes
    ?scale
    ?linetype
    ?linewidth
    ?coloridx
    ?(org = 0.0, 0.0)
    ?(minor = 0, 0)
    ?(tick_size = -0.01)
    x_tick
    y_tick
  =
  Lowlevel.savestate ();
  Option.iter (fun s -> Lowlevel.setscale (int_of_scale_options s) |> ignore) scale;
  Option.iter set_linetype linetype;
  Option.iter set_linewidth linewidth;
  Option.iter set_linecolorindex coloridx;
  let x_org, y_org = org in
  let major_x, major_y = minor in
  Lowlevel.axes x_tick y_tick x_org y_org major_x major_y tick_size;
  Lowlevel.restorestate ()


(* 
   let settextfontprec = foreign "gr_settextfontprec" (int @-> int @-> returning void)
   let setcharexpan = foreign "gr_setcharexpan" (double @-> returning void)
   let setcharspace = foreign "gr_setcharspace" (double @-> returning void)
   let settextcolorind = foreign "gr_settextcolorind" (int @-> returning void)
   let setcharheight = foreign "gr_setcharheight" (double @-> returning void)
   let setcharup = foreign "gr_setcharup" (double @-> double @-> returning void)
   let settextpath = foreign "gr_settextpath" (int @-> returning void)
   let settextalign = foreign "gr_settextalign" (int @-> int @-> returning void)
   let setfillintstyle = foreign "gr_setfillintstyle" (int @-> returning void)
   let setfillstyle = foreign "gr_setfillstyle" (int @-> returning void)
   let setfillcolorind = foreign "gr_setfillcolorind" (int @-> returning void)
   let setcolorrep = foreign "gr_setcolorrep" (int @-> double @-> double @-> double @-> returning void)
   let setwindow = foreign "gr_setwindow" (double @-> double @-> double @-> double @-> returning void)
   let inqwindow = foreign "gr_inqwindow" (ptr double @-> ptr double @-> ptr double @-> ptr double @-> returning void)
   let setviewport = foreign "gr_setviewport" (double @-> double @-> double @-> double @-> returning void)
   let inqviewport = foreign "gr_inqviewport" (ptr double @-> ptr double @-> ptr double @-> ptr double @-> returning void)
   let selntran = foreign "gr_selntran" (int @-> returning void)
   let setclip = foreign "gr_setclip" (int @-> returning void)
   let setwswindow = foreign "gr_setwswindow" (double @-> double @-> double @-> double @-> returning void)
   let setwsviewport = foreign "gr_setwsviewport" (double @-> double @-> double @-> double @-> returning void)
   let createseg = foreign "gr_createseg" (int @-> returning void)
   let copysegws = foreign "gr_copysegws" (int @-> returning void)
   let redrawsegws = foreign "gr_redrawsegws" (void @-> returning void)
   let setsegtran = foreign "gr_setsegtran" (int @-> double @-> double @-> double @-> double @-> double @-> double @-> double @-> returning void)
   let closeseg = foreign "gr_closeseg" (void @-> returning void)
   let emergencyclosegks = foreign "gr_emergencyclosegks" (void @-> returning void)
   let updategks = foreign "gr_updategks" (void @-> returning void)
   let setspace = foreign "gr_setspace" (double @-> double @-> int @-> int @-> returning int)
   let inqspace = foreign "gr_inqspace" (ptr double @-> ptr double @-> ptr int @-> ptr int @-> returning void)
   let setscale = foreign "gr_setscale" (int @-> returning int)
   let inqscale = foreign "gr_inqscale" (ptr int @-> returning void)
   let textext = foreign "gr_textext" (double @-> double @-> string @-> returning int)
   let inqtextext = foreign "gr_inqtextext" (double @-> double @-> string @-> ptr double @-> ptr double @-> returning void)

   (*
    void let axes = foreign "gr_axes" (double x_tick @-> double y_tick @-> double x_org @-> double y_org @-> int major_x @-> int major_y @-> double tick_size)

    Draw X and Y coordinate axes with linearly and/or logarithmically spaced tick marks.
    Tick marks are positioned along each axis so that major tick marks fall on the axes origin (whether visible or not).
    Major tick marks are labeled with the corresponding data values.
    Axes are drawn according to the scale of the window.
    Axes and tick marks are drawn using solid lines; line color and width can be modified using the let setlinetype = foreign "gr_setlinetype"  and let setlinewidth = foreign "gr_setlinewidth"  functions.
    Axes are drawn according to the linear or logarithmic transformation established by the let setscale = foreign "gr_setscale"  function.

    Parameters
        x_tick: The interval between minor tick marks on the X axis.
        y_tick: The interval between minor tick marks on the Y axis.
        x_org: The world coordinate of the origin (point of intersection) of the X axis.
        y_org: The world coordinate of the origin (point of intersection) of the Y axis.
        major_x: Unitless integer value specifying the number of minor tick intervals between major tick marks on the X axis. Values of 0 or 1 imply no minor ticks. Negative values specify no labels will be drawn for the associated axis.
        major_y: Unitless integer value specifying the number of minor tick intervals between major tick marks on the Y axis. Values of 0 or 1 imply no minor ticks. Negative values specify no labels will be drawn for the associated axis.
        tick_size: The length of minor tick marks specified in a normalized device coordinate unit. Major tick marks are twice as long as minor tick marks. A negative value reverses the tick marks on the axes from inward facing to outward facing (or vice versa)
    *)
   let axes = foreign "gr_axes" (double @-> double @-> double @-> double @-> int @-> int @-> double @-> returning void)

   (* let axeslbl = foreign "gr_axeslbl" (double @-> double @-> double @-> double @-> int @-> int @-> double @-> 
                        ptr (double @-> double @-> const char* @-> double @-> returning void) @-> 
                        ptr (double @-> double @-> const char* @-> double @-> returning void) @-> 
                        returning void) *)
   let grid = foreign "gr_grid" (double @-> double @-> double @-> double @-> int @-> int @-> returning void)
   let grid3d = foreign "gr_grid3d" (double @-> double @-> double @-> double @-> double @-> double @-> int @-> int @-> int @-> returning void)
   let verrorbars = foreign "gr_verrorbars" (int @-> ptr double @-> ptr double @-> ptr double @-> ptr double @-> returning void)
   let herrorbars = foreign "gr_herrorbars" (int @-> ptr double @-> ptr double @-> ptr double @-> ptr double @-> returning void)
   let polyline3d = foreign "gr_polyline3d" (int @-> ptr double @-> ptr double @-> ptr double @-> returning void)
   let polymarker3d = foreign "gr_polymarker3d" (int @-> ptr double @-> ptr double @-> ptr double @-> returning void)
   let axes3d = foreign "gr_axes3d" (double @-> double @-> double @-> double @-> double @-> double @-> int @-> int @-> int @-> double @-> returning void)
   let titles3d = foreign "gr_titles3d" (string @-> string @-> string @-> returning void)
   let surface = foreign "gr_surface" (int @-> int @-> ptr double @-> ptr double @-> ptr double @-> int @-> returning void)
   let contour = foreign "gr_contour" (int @-> int @-> int @-> ptr double @-> ptr double @-> ptr double @-> ptr double @-> int @-> returning void)
   let contourf = foreign "gr_contourf" (int @-> int @-> int @-> ptr double @-> ptr double @-> ptr double @-> ptr double @-> int @-> returning void)
   let tricontour = foreign "gr_tricontour" (int @-> ptr double @-> ptr double @-> ptr double @-> int @-> ptr double @-> returning void)
   let hexbin = foreign "gr_hexbin" (int @-> ptr double @-> ptr double @-> int @-> returning int)
   let setcolormap = foreign "gr_setcolormap" (int @-> returning void)
   let inqcolormap = foreign "gr_inqcolormap" (ptr int @-> returning void)
   let colorbar = foreign "gr_colorbar" (void @-> returning void)
   let inqcolor = foreign "gr_inqcolor" (int @-> ptr int @-> returning void)
   let inqcolorfromrgb = foreign "gr_inqcolorfromrgb" (double @-> double @-> double @-> returning int)
   let hsvtorgb = foreign "gr_hsvtorgb" (double @-> double @-> double @-> ptr double@-> ptr double @-> ptr double @-> returning void)
   let tick = foreign "gr_tick" (double @-> double @-> returning double)
   let validaterange = foreign "gr_validaterange" (double @-> double @-> returning int)
   let adjustlimits = foreign "gr_adjustlimits" (ptr double @-> ptr double @-> returning void)
   let adjustrange = foreign "gr_adjustrange" (ptr double @-> ptr double @-> returning void)
   let beginprint = foreign "gr_beginprint" (string @-> returning void)
   let beginprintext = foreign "gr_beginprintext" (string @-> string @-> string @-> string @-> returning void)
   let endprint = foreign "gr_endprint" (void @-> returning void)
   let ndctowc = foreign "gr_ndctowc" (ptr double @-> ptr double @-> returning void)
   let wctondc = foreign "gr_wctondc" (ptr double @-> ptr double @-> returning void)
   let wc3towc = foreign "gr_wc3towc" (ptr double @-> ptr double @-> ptr double @-> returning void)
   let drawrect = foreign "gr_drawrect" (double @-> double @-> double @-> double @-> returning void)
   let fillrect = foreign "gr_fillrect" (double @-> double @-> double @-> double @-> returning void)
   let drawarc = foreign "gr_drawarc" (double @-> double @-> double @-> double @-> int @-> int @-> returning void)
   let fillarc = foreign "gr_fillarc" (double @-> double @-> double @-> double @-> int @-> int @-> returning void)
   let drawpath = foreign "gr_drawpath" (int @-> ptr vertex @-> ptr uchar @-> int @-> returning void)
   let setarrowstyle = foreign "gr_setarrowstyle" (int @-> returning void)
   let setarrowsize = foreign "gr_setarrowsize" (double @-> returning void)
   let drawarrow = foreign "gr_drawarrow" (double @-> double @-> double @-> double @-> returning void)
   let readimage = foreign "gr_readimage" (string @-> ptr int @-> ptr int @-> ptr (ptr int) @-> returning int)
   let drawimage = foreign "gr_drawimage" (double @-> double @-> double @-> double @-> int @-> int @-> ptr int @-> int @-> returning void)
   let importgraphics = foreign "gr_importgraphics" (string @-> returning int)
   let setshadow = foreign "gr_setshadow" (double @-> double @-> double @-> returning void)
   let settransparency = foreign "gr_settransparency" (double @-> returning void)
   (* This should be an actual double[3][2] *)
   let setcoordxform = foreign "gr_setcoordxform" (ptr (ptr double) @-> returning void)
   let begingraphics = foreign "gr_begingraphics" (string @-> returning void)
   let endgraphics = foreign "gr_endgraphics" (void @-> returning void)
   let getgraphics = foreign "gr_getgraphics" (void @-> returning (string))
   let drawgraphics = foreign "gr_drawgraphics" (string @-> returning int)
   let mathtex = foreign "gr_mathtex" (double @-> double @-> string @-> returning void)
   let inqmathtex = foreign "gr_inqmathtex" (double @-> double @-> string @-> ptr double @-> ptr double @-> returning void)
   let beginselection = foreign "gr_beginselection" (int @-> int @-> returning void)
   let endselection = foreign "gr_endselection" (void @-> returning void)
   let moveselection = foreign "gr_moveselection" (double @-> double @-> returning void)
   let resizeselection = foreign "gr_resizeselection" (int @-> double @-> double @-> returning void)
   let inqbbox = foreign "gr_inqbbox" (ptr double @-> ptr double @-> ptr double @-> ptr double @-> returning void)
   let precision = foreign "gr_precision" (void @-> returning double)
   let setregenflags = foreign "gr_setregenflags" (int @-> returning void)
   let inqregenflags = foreign "gr_inqregenflags" (void @-> returning int)
   let savestate = foreign "gr_savestate" (void @-> returning void)
   let restorestate = foreign "gr_restorestate" (void @-> returning void)
   let selectcontext = foreign "gr_selectcontext" (int @-> returning void)
   let destroycontext = foreign "gr_destroycontext" (int @-> returning void)
   let uselinespec = foreign "gr_uselinespec" (string @-> returning int)
   let delaunay = foreign "gr_delaunay" (int @-> ptr double @-> ptr double @-> ptr int @-> ptr (ptr int) @-> returning void)
   let reducepoints = foreign "gr_reducepoints" (int @-> ptr double @-> ptr double @-> int @-> ptr double @-> ptr double @-> returning void)
   let trisurface = foreign "gr_trisurface" (int @-> ptr double @-> ptr double @-> ptr double @-> returning void)
   let gradient = foreign "gr_gradient" (int @-> int @-> ptr double @-> ptr double @-> ptr double @-> ptr double @-> ptr double @-> returning void)
   let quiver = foreign "gr_quiver" (int @-> int @-> ptr double @-> ptr double @-> ptr double @-> ptr double @-> int @-> returning void)
   let interp2 = foreign "gr_interp2" (int @-> int @-> ptr double @-> ptr double @-> ptr double @-> int @-> int @-> ptr double @-> ptr double @-> ptr double @-> int @-> double @-> returning void)
   (* 
    (* omitting meta_args_t for now *)
    DLLEXPORT gr_meta_args_t *gr_newmeta(void);
    DLLEXPORT void gr_deletemeta(gr_meta_args_t * );
   DLLEXPORT void gr_meta_args_push_arg(gr_meta_args_t *, const char *, ..);
   DLLEXPORT void gr_meta_args_push_arg_buf(
    gr_meta_args_t *, const char *, const void *, int);
   DLLEXPORT void gr_meta_args_push_kwarg(
    gr_meta_args_t *, const char *, const char *, .. );
   DLLEXPORT void gr_meta_args_push_kwarg_buf(
    gr_meta_args_t *, const char *, const char *, const void *, int);
   DLLEXPORT void *gr_openmeta(
    int, const char *, unsigned int, const char *( * )(const char *, unsigned int),
    int ( * )(const char *, unsigned int, const char * ));
   DLLEXPORT gr_meta_args_t *gr_recvmeta(const void *p, gr_meta_args_t * );
   DLLEXPORT int gr_sendmeta(const void *, const char *, .. );
   DLLEXPORT int gr_sendmeta_buf(const void *, const char *, const void *, int);
   DLLEXPORT int gr_sendmeta_ref(const void *, const char *, char, const void *, int);
   DLLEXPORT int gr_sendmeta_args(const void *p, const gr_meta_args_t * );
   DLLEXPORT void gr_closemeta(const void * );
   DLLEXPORT int gr_plotmeta(const gr_meta_args_t * );
   DLLEXPORT int gr_readmeta(gr_meta_args_t *, const char * );
   #ifndef NDEBUG
    DLLEXPORT void gr_dumpmeta(const gr_meta_args_t *, FILE * );
   DLLEXPORT void gr_dumpmeta_json(const gr_meta_args_t *, FILE * );
   #endif
  *)
   let version = foreign "gr_version" (void @-> returning (string))
   let shade = foreign "gr_shade" (int @-> ptr double @-> ptr double @-> int @-> int @-> ptr double @-> int @-> int @-> ptr int @-> returning void)
   let shadepoints = foreign "gr_shadepoints" (int @-> ptr double @-> ptr double @-> int @-> int @-> int @-> returning void)
   let shadelines = foreign "gr_shadelines" (int @-> ptr double @-> ptr double @-> int @-> int @-> int @-> returning void)
   let panzoom = foreign "gr_panzoom" (double @-> double @-> double @-> ptr double @-> ptr double @-> ptr double @-> ptr double @-> returning void) *)

let with_ws ?(typ = PNG) plot =
  let id = Random.int 1024 in
  try
    Lowlevel.openws id ("plot" ^ string_of_int id) (int_of_workstation_type typ);
    plot id;
    Lowlevel.closews id
  with
  | exn ->
    (try Lowlevel.closews id with
    | _ -> ());
    raise exn
