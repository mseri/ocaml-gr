(** {0: OCaml Bindings for the GR Framework} 

    GR is essentially based on an implementation of a Graphical Kernel System (GKS) and OpenGL. As a self-contained system it can quickly and easily be integrated into existing applications.

    The GR framework can be used in imperative programming systems or integrated into modern object-oriented systems, in particular those based on GUI toolkits.
    GR is characterized by its high interoperability and can be used with modern web technologies and mobile devices.
    The GR framework is especially suitable for real-time environments.
*)

open Stdcompat
module Lowlevel = Lowlevel

exception Unimplemented

(** Available workstation types, see also {{: (** Available line types, see also {{: https://gr-framework.org/workstations.html} GR Workstation Types} *)} GR Line Types} *)
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


(** Available line types, see also {{: https://gr-framework.org/linetypes.html} GR Line Types} *)
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


(* let linetype_of_int = function
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
   | d -> failwith @@ "Error when inferring line type. Got " ^ string_of_int d *)

(** Available marker types, see also {{: https://gr-framework.org/markertypes.html} GR Marker Types} *)
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


(* let markertype_of_int = function
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
   | d -> failwith @@ "Error when inferring marker type. Got " ^ string_of_int d *)

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


type spline_algo =
  | GeneralizedCrossValidatedSmoothing
  | InterpolatingNaturalCubic
  | CubicBSpline

let int_of_spline_algo = function
  | GeneralizedCrossValidatedSmoothing -> 1
  | InterpolatingNaturalCubic -> 0
  | CubicBSpline -> -1


(** Available fonts, see also {{: https://gr-framework.org/fonts.html} GR Font list} *)
type font =
  | TIMES_ROMAN
  | TIMES_ITALIC
  | TIMES_BOLD
  | TIMES_BOLDITALIC
  | HELVETICA
  | HELVETICA_OBLIQUE
  | HELVETICA_BOLD
  | HELVETICA_BOLDOBLIQUE
  | COURIER
  | COURIER_OBLIQUE
  | COURIER_BOLD
  | COURIER_BOLDOBLIQUE
  | SYMBOL
  | BOOKMAN_LIGHT
  | BOOKMAN_LIGHTITALIC
  | BOOKMAN_DEMI
  | BOOKMAN_DEMIITALIC
  | NEWCENTURYSCHLBK_ROMAN
  | NEWCENTURYSCHLBK_ITALIC
  | NEWCENTURYSCHLBK_BOLD
  | NEWCENTURYSCHLBK_BOLDITALIC
  | AVANTGARDE_BOOK
  | AVANTGARDE_BOOKOBLIQUE
  | AVANTGARDE_DEMI
  | AVANTGARDE_DEMIOBLIQUE
  | PALATINO_ROMAN
  | PALATINO_ITALIC
  | PALATINO_BOLD
  | PALATINO_BOLDITALIC
  | ZAPFCHANCERY_MEDIUMITALIC
  | ZAPFDINGBATS

let int_of_font = function
  | TIMES_ROMAN -> 101
  | TIMES_ITALIC -> 102
  | TIMES_BOLD -> 103
  | TIMES_BOLDITALIC -> 104
  | HELVETICA -> 105
  | HELVETICA_OBLIQUE -> 106
  | HELVETICA_BOLD -> 107
  | HELVETICA_BOLDOBLIQUE -> 108
  | COURIER -> 109
  | COURIER_OBLIQUE -> 110
  | COURIER_BOLD -> 111
  | COURIER_BOLDOBLIQUE -> 112
  | SYMBOL -> 113
  | BOOKMAN_LIGHT -> 114
  | BOOKMAN_LIGHTITALIC -> 115
  | BOOKMAN_DEMI -> 116
  | BOOKMAN_DEMIITALIC -> 117
  | NEWCENTURYSCHLBK_ROMAN -> 118
  | NEWCENTURYSCHLBK_ITALIC -> 119
  | NEWCENTURYSCHLBK_BOLD -> 120
  | NEWCENTURYSCHLBK_BOLDITALIC -> 121
  | AVANTGARDE_BOOK -> 122
  | AVANTGARDE_BOOKOBLIQUE -> 123
  | AVANTGARDE_DEMI -> 124
  | AVANTGARDE_DEMIOBLIQUE -> 125
  | PALATINO_ROMAN -> 126
  | PALATINO_ITALIC -> 127
  | PALATINO_BOLD -> 128
  | PALATINO_BOLDITALIC -> 129
  | ZAPFCHANCERY_MEDIUMITALIC -> 130
  | ZAPFDINGBATS -> 131


type text_precision =
  | STRING (** String precision (higher quality) *)
  | CHAR (** Character precision (medium quality) *)
  | STROKE (** Stroke precision (lower quality) *)

let int_of_text_precision = function
  | STRING -> 0
  | CHAR -> 1
  | STROKE -> 2


type text_path_direction =
  | RIGHT (** left-to-right *)
  | LEFT (** right-to-left *)
  | UP (** downside-up *)
  | DOWN (** upside-down *)

let int_of_text_path_direction = function
  | RIGHT -> 0
  | LEFT -> 1
  | UP -> 2
  | DOWN -> 3


type text_halign =
  | NORMAL
  | LEFT (** Left justify *)
  | CENTER (** Center justify *)
  | RIGHT (** Right justify *)

let int_of_text_halign = function
  | NORMAL -> 0
  | LEFT -> 1
  | CENTER -> 2
  | RIGHT -> 3


type text_valign =
  | NORMAL
  | TOP (** Align with the top of the characters *)
  | CAP (** Aligned with the cap of the characters *)
  | HALF (** Aligned with the half line of the characters *)
  | BASE (** Aligned with the base line of the characters *)
  | BOTTOM (** Aligned with the bottom line of the characters *)

let int_of_text_valign = function
  | NORMAL -> 0
  | TOP -> 1
  | CAP -> 2
  | HALF -> 3
  | BASE -> 4
  | BOTTOM -> 5


(** Pattern style, see also {{: https://gr-framework.org/patterns.html} GR Fill Patterns and Hatches}  *)
type pattern_style = int

let pattern_style n =
  if n > 0 && n < 109 then n else failwith "pattern_style out of range"


(** Hatch style, see also {{: https://gr-framework.org/patterns.html} GR Fill Patterns and Hatches}  *)
type hatch_style = int

let hatch_style n = if n > 1 && n < 11 then n else failwith "hatch_style out of range"

type fill_style =
  | HOLLOW (** No filling. Just draw the bounding polyline *)
  | SOLID (** Fill the interior of the polygon using the fill color index *)
  | PATTERN of pattern_style
      (** Fill the interior of the polygon using the style index as a pattern index *)
  | HATCH of hatch_style
      (** Fill the interior of the polygon using the style index as a cross-hatched style *)

let int_of_fill_style = function
  | HOLLOW -> 0
  | SOLID -> 1
  | PATTERN _ -> 2
  | HATCH _ -> 3


(** Color Maps, see also {{: https://gr-framework.org/colormaps.html} GR Color Maps} *)
type color_map =
  | Uniform
  | Temperature
  | Grayscale
  | Glowing
  | Rainbowlike
  | Geologic
  | Greenscale
  | Cyanscale
  | Bluescale
  | Magentascale
  | Redscale
  | Flame
  | Brownscale
  | Pilatus
  | Autumn
  | Bone
  | Cool
  | Copper
  | Gray
  | Hot
  | Hsv
  | Jet
  | Pink
  | Spectral
  | Spring
  | Summer
  | Winter
  | Gist_Earth
  | Gist_Heat
  | Gist_Ncar
  | Gist_Rainbow
  | Gist_Stern
  | Afmhot
  | Brg
  | Bwr
  | Coolwarm
  | Cmrmap
  | Cubehelix
  | Gnuplot
  | Gnuplot2
  | Ocean
  | Rainbow
  | Seismic
  | Terrain
  | Viridis
  | Inferno
  | Plasma
  | Magma

let int_of_color_map = function
  | Uniform -> 0
  | Temperature -> 1
  | Grayscale -> 2
  | Glowing -> 3
  | Rainbowlike -> 4
  | Geologic -> 5
  | Greenscale -> 6
  | Cyanscale -> 7
  | Bluescale -> 8
  | Magentascale -> 9
  | Redscale -> 10
  | Flame -> 11
  | Brownscale -> 12
  | Pilatus -> 13
  | Autumn -> 14
  | Bone -> 15
  | Cool -> 16
  | Copper -> 17
  | Gray -> 18
  | Hot -> 19
  | Hsv -> 20
  | Jet -> 21
  | Pink -> 22
  | Spectral -> 23
  | Spring -> 24
  | Summer -> 25
  | Winter -> 26
  | Gist_Earth -> 27
  | Gist_Heat -> 28
  | Gist_Ncar -> 29
  | Gist_Rainbow -> 30
  | Gist_Stern -> 31
  | Afmhot -> 32
  | Brg -> 33
  | Bwr -> 34
  | Coolwarm -> 35
  | Cmrmap -> 36
  | Cubehelix -> 37
  | Gnuplot -> 38
  | Gnuplot2 -> 39
  | Ocean -> 40
  | Rainbow -> 41
  | Seismic -> 42
  | Terrain -> 43
  | Viridis -> 44
  | Inferno -> 45
  | Plasma -> 46
  | Magma -> 47


type surface_options =
  | LINES (** Use X Y polylines to denote the surface *)
  | MESH (** Use a wire grid to denote the surface *)
  | FILLED_MESH (** Applies an opaque grid to the surface *)
  | Z_SHADED_MESH (** Applies Z-value shading to the surface *)
  | COLORED_MESH (** Applies a colored grid to the surface *)
  | CELL_ARRAY (** Applies a grid of individually-colored cells to the surface *)
  | SHADED_MESH (** Applies light source shading to the 3-D surface *)

let int_of_surface_options = function
  | LINES -> 0
  | MESH -> 1
  | FILLED_MESH -> 2
  | Z_SHADED_MESH -> 3
  | COLORED_MESH -> 4
  | CELL_ARRAY -> 5
  | SHADED_MESH -> 6


(*

let openws = foreign "gr_openws" (int @-> string @-> int @-> returning void)
let closews = foreign "gr_closews" (int @-> returning void)
let activatews = foreign "gr_activatews" (int @-> returning void)
let deactivatews = foreign "gr_deactivatews" (int @-> returning void)

*)

module Workstation = struct
  type id = W of int

  let wid id = W id
  let open_ (W id) conn typ = Lowlevel.openws id conn (int_of_workstation_type typ)
  let close (W id) = Lowlevel.closews id
  let activate (W id) = Lowlevel.activatews id
  let deactivate (W id) = Lowlevel.deactivatews id
  let clear = Lowlevel.clearws
  let update = Lowlevel.updatews

  (** [set_window xmin xmax ymin ymax] sets the area of the NDC viewport that is to be drawn in the workstation window.

  This function defines the rectangular area of the Normalized Device Coordinate space to be output to the device.
  By default, the workstation transformation will map the range [0,1] x [0,1] in NDC onto the largest square on the workstation’s display surface.
  The aspect ratio of the workstation window is maintained at 1 to 1.

  Parameters
        xmin: The left horizontal coordinate of the workstation window (0 <= xmin < xmax).
        xmax: The right horizontal coordinate of the workstation window (xmin < xmax <= 1).
        ymin: The bottom vertical coordinate of the workstation window (0 <= ymin < ymax).
        ymax: The top vertical coordinate of the workstation window (ymin < ymax <= 1).
  *)
  let set_window = Lowlevel.setwswindow

  (** [set_viewport xmin xmax ymin ymax] defines the size of the workstation graphics window in meters.

  This function places a workstation window on the display of the specified size in meters.
  This command allows the workstation window to be accurately sized for a display or hardcopy device, and is often useful for sizing graphs for desktop publishing applications.

  Parameters
        xmin: The left horizontal coordinate of the workstation window.
        xmax: The right horizontal coordinate of the workstation window.
        ymin: The bottom vertical coordinate of the workstation window.
        ymax: The top vertical coordinate of the workstation window.
  *)
  let set_viewport = Lowlevel.setwsviewport

  let copy_segment = Lowlevel.copysegws
  let redraw_segment = Lowlevel.redrawsegws
end

module Gks = struct
  let emergency_close = Lowlevel.emergencyclosegks
  let update = Lowlevel.updategks
end

module State = struct
  let save () = Lowlevel.savestate ()
  let restore () = Lowlevel.restorestate ()

  let with_sandbox f =
    save ();
    Fun.protect ~finally:restore f
end

(** [set_window xmin xmax ymin ymax] establishes a window, or rectangular subspace, of world coordinates to be plotted. If you desire log scaling or mirror-imaging of axes, use the gr_setscale function.

This function defines the rectangular portion of the World Coordinate space (WC) to be associated with the specified normalization transformation.
The WC window and the Normalized Device Coordinates (NDC) viewport define the normalization transformation through which all output primitives are mapped.
The WC window is mapped onto the rectangular NDC viewport which is, in turn, mapped onto the display surface of the open and active workstation, in device coordinates.
By default, GR uses the range [0,1] x [0,1], in world coordinates, as the normalization transformation window.

Parameters

        xmin: The left horizontal coordinate of the window (xmin < xmax).
        xmax: The right horizontal coordinate of the window (xmin < xmax).
        ymin: The bottom vertical coordinate of the window (ymin < ymax).
        ymax: The top vertical coordinate of the window (ymin < ymax).

*)
let set_window = Lowlevel.setwindow

(** [set_viewport xmin xmax ymin ymax] establishes a rectangular subspace of normalized device coordinates.

This function defines the rectangular portion of the Normalized Device Coordinate (NDC) space to be associated with the specified normalization transformation.
The NDC viewport and World Coordinate (WC) window define the normalization transformation through which all output primitives pass.
The WC window is mapped onto the rectangular NDC viewport which is, in turn, mapped onto the display surface of the open and active workstation, in device coordinates.

Parameters

        xmin: The left horizontal coordinate of the viewport (0 <= xmin < xmax).
        xmax: The right horizontal coordinate of the viewport (xmin < xmax <= 1).
        ymin: The bottom vertical coordinate of the viewport (0 <= ymin < ymax).
        ymax: The top vertical coordinate of the viewport (ymin < ymax <= 1).

 *)
let set_viewport = Lowlevel.setviewport

(** [select_transformation transform] selects a predefined transformation from world coordinates to normalized device coordinates.

0 	Selects the identity transformation in which both the window and viewport have the range of 0 to 1
>= 1 	Selects a normalization transformation as defined by [set_window] and [set_viewport]

Parameters
        transform: A normalization transformation number.
*)
let select_transformation = Lowlevel.selntran

(** [clip indicator] sets the clipping indicator.

    false 	Clipping is off. Data outside of the window will be drawn.
    true 	Clipping is on. Data outside of the window will not be drawn.

    Parameters
            indicator: An indicator specifying whether clipping is on or off.

    This function enables or disables clipping of the image drawn in the current window.
    Clipping is defined as the removal of those portions of the graph that lie outside of the defined viewport. If clipping is on, GR does not draw generated output primitives past the viewport boundaries. If clipping is off, primitives may exceed the viewport boundaries, and they will be drawn to the edge of the workstation window.
    By default, clipping is on.
*)
let clip c = Lowlevel.setclip (if c then 1 else 2)

let create_segment = Lowlevel.createseg
let set_segment_transform = Lowlevel.setsegtran
let close_segment = Lowlevel.closeseg

(** [set_space zmin zmax rotation tilt] sets the abstract Z-space used for mapping three-dimensional output primitives into the current world coordinate space.

This function establishes the limits of an abstract Z-axis and defines the angles for rotation and for the viewing angle (tilt) of a simulated three-dimensional graph, used for mapping corresponding output primitives into the current window.
These settings are used for all subsequent three-dimensional output primitives until other values are specified.
Angles of rotation and viewing angle must be specified between 0 and 90 degrees.

Parameters
        zmin: Minimum value for the Z-axis.
        zmax: Maximum value for the Z-axis.
        rotation: Angle for the rotation of the X axis, in degrees.
        tilt: Viewing angle of the Z axis, in degrees.
*)
let set_space = Lowlevel.setspace

let set_linetype lt = lt |> int_of_linetype |> Lowlevel.setlinetype

(* let current_linetype () =
   let open Ctypes in
   let c = allocate int 0 in
   Lowlevel.inqlinetype c;
   linetype_of_int !@c *)

(** [set_linewidth lw] defines the line width of subsequent polyline output primitives.

    The line width is calculated as the nominal line width generated on the workstation multiplied by the line width scale factor.
    This value is mapped by the workstation to the nearest available line width.
    The default line width is 1.0, or 1 times the line width generated on the graphics device.
*)
let set_linewidth = Lowlevel.setlinewidth

(* let current_linewidth () =
   let open Ctypes in
   let c = allocate double 0.0 in
   Lowlevel.inqlinewidth c;
   !@c *)

(** [set_linecolorindex c] defines the color of subsequent polyline output primitives.
    Note: c < 1256
*)
let set_linecolorindex = function
  | c when c >= 0 && c < 1256 -> Lowlevel.setlinecolorind c
  | c -> failwith @@ "Color index must be in the range [0, 1256]. Got " ^ string_of_int c


(* let current_linecolorindex () =
   let open Ctypes in
   let c = allocate int 0 in
   Lowlevel.inqlinecolorind c;
   !@c *)

let set_markertype mt = mt |> int_of_markertype |> Lowlevel.setmarkertype

(* let current_markertype () =
   let open Ctypes in
   let c = allocate int 0 in
   Lowlevel.inqmarkertype c;
   markertype_of_int !@c *)

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


(* let current_markercolorindex () =
   let open Ctypes in
   let c = allocate int 0 in
   Lowlevel.inqmarkercolorind c;
   !@c *)

(** [set_text_font_prec ?precision font] specifies the text font and precision for subsequent text output primitives.

    The appearance of a font depends on the text precision value specified.
    STRING, CHARACTER or STROKE precision allows for a greater or lesser realization of the text primitives, for efficiency.
    STRING is the default precision for GR and produces the highest quality output.

    XXX: CHARACTER and STROKE precision seem to be broken (and to break the [axes] command with it...)!
*)
let set_text_font_prec ?(precision = STRING) font =
  Lowlevel.settextfontprec (int_of_font font) (int_of_text_precision precision)


(** [set_char_expand_factor factor] sets the current character expansion factor (width to height ratio).

    This function defines the width of subsequent text output primitives.
    The expansion factor alters the width of the generated characters, but not their height. The default text expansion factor is 1, or one times the normal width-to-height ratio of the text.

    Parameters
        factor: Text expansion factor applied to the nominal text width-to-height ratio
*)
let set_char_expand_factor = Lowlevel.setcharexpan

(** [set_text_colorindex color] sets the current text color index.

    This function defines the color of subsequent text output primitives.
    GR uses the default foreground color (black=1) for the default text color index.

    Parameters
            color: The text color index (COLOR < 1256)
*)
let set_text_colorindex = function
  | c when c >= 0 && c < 1256 -> Lowlevel.settextcolorind c
  | c -> failwith @@ "Color index must be in the range [0, 1256]. Got " ^ string_of_int c


(** [set_char_height height] sets the current character height.

    This function defines the height of subsequent text output primitives.
    Text height is defined as a percentage of the default window.
    GR uses the default text height of 0.027 (2.7% of the height of the default window).
*)
let set_char_height = Lowlevel.setcharheight

(** [set_char_up (x, y)
    Set the current character text angle up vector.
    This function defines the vertical rotation of subsequent text output primitives. The text up vector is initially set to (0, 1), horizontal to the baseline.

    Parameters
            x: X coordinate of the text up vector
            y: Y coordinate of the text up vector
*)
let set_char_up (x, y) = Lowlevel.setcharup x y

let set_char_space = Lowlevel.setcharspace

(** [set_text_path direction] defines the current direction in which subsequent text will be drawn. *)
let set_text_path direction = Lowlevel.settextpath (int_of_text_path_direction direction)

(** [set_text_align horizontal vertical] specifies how the characters in a text primitive will be aligned in horizontal and vertical space.
    The default text alignment indicates horizontal left alignment and vertical baseline alignment.
*)
let set_text_align : text_halign option -> text_valign option -> unit =
 fun horizontal vertical ->
  let horizontal = Option.value ~default:NORMAL horizontal in
  let vertical = Option.value ~default:NORMAL vertical in
  Lowlevel.settextalign (int_of_text_halign horizontal) (int_of_text_valign vertical)


(** [set_fill_interior_style style] sets the fill area interior style to be used for fill areas.

    This function defines the interior style for subsequent fill area output primitives.
    The default interior style is HOLLOW. 
*)
let set_fill_interior_style style =
  Lowlevel.setfillintstyle (int_of_fill_style style);
  match style with
  | PATTERN pat -> Lowlevel.setfillstyle pat
  | HATCH hat -> Lowlevel.setfillstyle hat
  | _ -> ()


(** [set_fill_colorindex color] sets the current fill area color index.

    This function defines the color of subsequent fill area output primitives.
    GR uses the default foreground color (black=1) for the default fill area color index.

    Parameters
            color: The fill area color index (COLOR < 1256)
*)
let set_fill_colorindex = function
  | c when c >= 0 && c < 1256 -> Lowlevel.setfillcolorind c
  | c -> failwith @@ "Color index must be in the range [0, 1256]. Got " ^ string_of_int c


(** [set_color_representation index (red, green, blue)] redefines an existing color index representation by specifying an RGB color triplet.

    Parameters
        index: Color index in the range 0 to 1256
        red: Red intensity in the range 0.0 to 1.0
        green: Green intensity in the range 0.0 to 1.0
        blue: Blue intensity in the range 0.0 to 1.0

*)
let set_color_representation index (red, green, blue) =
  if index < 0 || index >= 1256
  then
    failwith @@ "Color index must be in the range [0, 1256]. Got " ^ string_of_int index;
  if (red < 0.0 || red > 1.0) || (green < 0.0 || green > 1.0) || blue < 0.0 || blue > 1.0
  then
    failwith
    @@ Printf.sprintf
         "Color values must be in the range [0.0, 1.0]. Got: (%f, %f, %f)"
         red
         green
         blue;
  Lowlevel.setcolorrep index red green blue


(** [set_colormap cmap] sets the currently used colormap.

    A list of colormaps can be found at: {{: https://gr-framework.org/colormaps.html} GR Colormaps}.
*)
let set_colormap cmap = Lowlevel.setcolormap (int_of_color_map cmap)

let set_scale scale = Lowlevel.setscale (int_of_scale_options scale)

(**
   [polyline ?linetype ?linewidth ?coloridx x y] draws a polyline using the current line attributes, starting from the first data point and ending at the last data point.

   The values for [x] and [y] are in world coordinates.
   The attributes that control the appearance of a polyline are linetype, linewidth and color index.
*)
let polyline ?linetype ?linewidth ?coloridx x y =
  State.with_sandbox (fun () ->
      Option.iter set_linetype linetype;
      Option.iter set_linewidth linewidth;
      Option.iter set_linecolorindex coloridx;
      let n, x', y' = Lowlevel.get_size_and_pointers x y in
      Lowlevel.polyline n x' y')


(**
     [polymarker ?markertype ?markersize ?coloridx x y] draws marker symbols centered at the given data points.

     The values for [x] and [y] are in world coordinates.
     The attributes that control the appearance of a polyline are markertype, markersize and color index.
  *)
let polymarker ?markertype ?markersize ?coloridx x y =
  State.with_sandbox (fun () ->
      Option.iter set_markertype markertype;
      Option.iter set_markersize markersize;
      Option.iter set_markercolorindex coloridx;
      let n, x', y' = Lowlevel.get_size_and_pointers x y in
      Lowlevel.polymarker n x' y')


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
let spline ?linetype ?linewidth ?coloridx x y m algo =
  State.with_sandbox (fun () ->
      Option.iter set_linetype linetype;
      Option.iter set_linewidth linewidth;
      Option.iter set_linecolorindex coloridx;
      let n, x', y' = Lowlevel.get_size_and_pointers x y in
      Lowlevel.spline n x' y' m (int_of_spline_algo algo))


(** [gridit x y z (nx, ny)] interpolates data from arbitrary points at points on a rectangular grid.

      Parameters

          nd: The number of input points
          xd: The X coordinates of the input points
          yd: The Y coordinates of the input points
          zd: The values of the points
          nx: The number of points in X direction for the output grid
          ny: The number of points in Y direction for the output grid

      Returns the tuple (x, y, z) with

      x: The points in X direction for the output grid
      y: The points in Y direction for the output grid
      z: The interpolated values on the nx x ny grid points
  *)
let gridit x y z (nx, ny) =
  let x' = Bigarray.(Genarray.create float64 c_layout [| nx |]) in
  let y' = Bigarray.(Genarray.create float64 c_layout [| ny |]) in
  let z' = Bigarray.(Genarray.create float64 c_layout [| nx * ny |]) in
  let n, x, y = Lowlevel.get_size_and_pointers x y in
  let nz, z = Lowlevel.get_size_and_pointer z in
  if nz <> n
  then
    failwith
    @@ Printf.sprintf "Expected arrays with dimensions n, n, n. Got %d, %d, %d" n n nz;
  Lowlevel.gridit
    n
    x
    y
    z
    nx
    ny
    Ctypes.(bigarray_start genarray x')
    Ctypes.(bigarray_start genarray y')
    Ctypes.(bigarray_start genarray z');
  x', y', z'


(*
   let textext = foreign "gr_textext" (double @-> double @-> string @-> returning int)
   let inqtextext = foreign "gr_inqtextext" (double @-> double @-> string @-> ptr double @-> ptr double @-> returning void)
*)

(** [axes ?scale ?linetype ?linewidth ?origin:(0,0) ?major:(0,0) ?size:1 x_tick y_tick] draws X and Y coordinate axes with linearly and/or logarithmically spaced tick marks.
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
    ?(scale = [])
    ?linetype
    ?linewidth
    ?coloridx
    ?(origin = 0.0, 0.0)
    ?(major = 0, 0)
    ?(tick_size = -0.01)
    x_tick
    y_tick
  =
  State.with_sandbox (fun () ->
      if scale <> [] then set_scale scale |> ignore;
      Option.iter set_linetype linetype;
      Option.iter set_linewidth linewidth;
      Option.iter set_linecolorindex coloridx;
      let x_org, y_org = origin in
      let major_x, major_y = major in
      Lowlevel.axes x_tick y_tick x_org y_org major_x major_y tick_size)


(** [axeslabels] Create axes in the current workspace and supply a custom function for changing the behaviour of the tick labels.

      Similar to [axes] but allows more fine-grained control over tick labels and text positioning by supplying callback functions.
      Within the callback function you can use normal GR text primitives for performing any manipulations on the label text.

      See [axes] for more details on drawing axes.

      Parameters

          x_tick: The interval between minor tick marks on the X axis.
          y_tick: The interval between minor tick marks on the Y axis.
          x_org: The world coordinate of the origin (point of intersection) of the X axis.
          y_org: The world coordinate of the origin (point of intersection) of the Y axis.
          major_x: Unitless integer value specifying the number of minor tick intervals between major tick marks on the X axis. Values of 0 or 1 imply no minor ticks. Negative values specify no labels will be drawn for the associated axis.
          major_y: Unitless integer value specifying the number of minor tick intervals between major tick marks on the Y axis. Values of 0 or 1 imply no minor ticks. Negative values specify no labels will be drawn for the associated axis.
          tick_size: The length of minor tick marks specified in a normalized device coordinate unit. Major tick marks are twice as long as minor tick marks. A negative value reverses the tick marks on the axes from inward facing to outward facing (or vice versa).
          fpx: Function pointer to a function that returns a label for a given tick on the X axis. The callback function should have the following arguments:
            x: NDC of the label in X direction.
            y: NDC of the label in Y direction.
            svalue: Internal string representation of the text drawn by GR at (x,y).
            value: Floating point representation of the label drawn at (x,y).
          fpy: Exactly same as the fpx above, but for the the Y axis.

  *)
let axes_labels
    ?(scale = [])
    ?linetype
    ?linewidth
    ?coloridx
    ?(origin = 0.0, 0.0)
    ?(major = 0, 0)
    ?(tick_size = -0.01)
    (fpx : float -> float -> string -> float -> unit)
    (fpy : float -> float -> string -> float -> unit)
    x_tick
    y_tick
  =
  State.with_sandbox (fun () ->
      if scale <> [] then Lowlevel.setscale (int_of_scale_options scale) |> ignore;
      Option.iter set_linetype linetype;
      Option.iter set_linewidth linewidth;
      Option.iter set_linecolorindex coloridx;
      let x_org, y_org = origin in
      let major_x, major_y = major in
      Lowlevel.axeslbl x_tick y_tick x_org y_org major_x major_y tick_size fpx fpy)


(** [surface x y z ?option] draws a three-dimensional surface plot for the given data points.

        x and y define a grid.
        z is a singly dimensioned array containing at least nx * ny data points.
        z describes the surface height at each point on the grid.
        Data is ordered as shown in the following table:

        Parameters

            nx: The number of points along the X axis
            ny: The number of points along the Y axis
            px: A pointer to the X coordinates
            py: A pointer to the Y coordinates
            pz: A pointer to the Z coordinates
            option: Surface display option (see table)
    *)
let surface ?(options = LINES) x y z =
  let nx, x = Lowlevel.get_size_and_pointer x in
  let ny, y = Lowlevel.get_size_and_pointer y in
  let nz, z = Lowlevel.get_size_and_pointer z in
  if nz <> nx * ny
  then
    failwith
    @@ Printf.sprintf
         "Expected arrays with dimensions n, n', n*n'. Got %d, %d, %d"
         nx
         ny
         nz;
  Lowlevel.surface nx ny x y z (int_of_surface_options options)


(** [contour ?major_h x y h z] sraw contours of a three-dimensional data set whose values are specified over a rectangular mesh.
    Contour lines may optionally be labeled.

    Parameters

        nx: The number of points along the X axis
        ny: The number of points along the Y axis
        nh: The number of height values
        px: A pointer to the X coordinates
        py: A pointer to the Y coordinates
        h: A pointer to the height values
        pz: A pointer to the Z coordinates
        major_h: Directs GR to label contour lines. For example, a value of 3 would label every third line. A value of 1 will label every line. A value of 0 produces no labels. To produce colored contour lines, add an offset of 1000 to major_h

*)
let contour ?(major_h = 0) x y h z =
  (* TODO: validate z *)
  let nx, x = Lowlevel.get_size_and_pointer x in
  let ny, y = Lowlevel.get_size_and_pointer y in
  let nh, h = Lowlevel.get_size_and_pointer h in
  Lowlevel.contour nx ny nh x y h Ctypes.(bigarray_start genarray z) major_h


(*
   let grid = foreign "gr_grid" (double @-> double @-> double @-> double @-> int @-> int @-> returning void)
   let grid3d = foreign "gr_grid3d" (double @-> double @-> double @-> double @-> double @-> double @-> int @-> int @-> int @-> returning void)
   let verrorbars = foreign "gr_verrorbars" (int @-> ptr double @-> ptr double @-> ptr double @-> ptr double @-> returning void)
   let herrorbars = foreign "gr_herrorbars" (int @-> ptr double @-> ptr double @-> ptr double @-> ptr double @-> returning void)
   let polyline3d = foreign "gr_polyline3d" (int @-> ptr double @-> ptr double @-> ptr double @-> returning void)
   let polymarker3d = foreign "gr_polymarker3d" (int @-> ptr double @-> ptr double @-> ptr double @-> returning void)
   let axes3d = foreign "gr_axes3d" (double @-> double @-> double @-> double @-> double @-> double @-> int @-> int @-> int @-> double @-> returning void)
   let titles3d = foreign "gr_titles3d" (string @-> string @-> string @-> returning void)
   let contourf = foreign "gr_contourf" (int @-> int @-> int @-> ptr double @-> ptr double @-> ptr double @-> ptr double @-> int @-> returning void)
   let tricontour = foreign "gr_tricontour" (int @-> ptr double @-> ptr double @-> ptr double @-> int @-> ptr double @-> returning void)
   let hexbin = foreign "gr_hexbin" (int @-> ptr double @-> ptr double @-> int @-> returning int)
   let colorbar = foreign "gr_colorbar" (void @-> returning void)
   let hsvtorgb = foreign "gr_hsvtorgb" (double @-> double @-> double @-> ptr double@-> ptr double @-> ptr double @-> returning void)
   *)

let tick = Lowlevel.tick

(*
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
