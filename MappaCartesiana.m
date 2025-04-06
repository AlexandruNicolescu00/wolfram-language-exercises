(* ::Package:: *)

(* :Title: MappaCartesiana *)
(* :Context: MappaCartesiana` *)
(* :Author: GS *)
(* :Summary: a preliminary version of the ComplexMap package *)
(* :Copyright: GS 2025 *)
(* :Package Version: 3 *)
(* :Mathematica Version: 14 *)
(* :History: last modified 14/3/2025 *)
(* :Keywords: programming style, local variables *)
(* :Sources: biblio *)
(* :Limitations:
this is a preliminary version, for educational purposes only. *)
(* :Discussion: *)
(* :Requirements: *)
(* :Warning: DOCUMENTARE TUTTO il codice *)


BeginPackage["MappaCartesiana`"]


CartesianMap::usage = "CartesianMap[ f , {x0, x1, dx}, {y0, y1, dy}] plots the image of Cartesian"


Begin["`Private`"]


CartesianMap[ func_, {x0_, x1_, dx_}, {y0_, y1_, dy_}]:= Module[
{w, x, y, horizontalgrid, verticalgrid, grid},

(* Use func and the coordinated {x,y} to form the value f[x+I*y] *)

w = func[x + I y];

(* Form the table of horizontal values, with spread={x, x0, x1, dx}.
Horizontal coordinates turn out to be of the type {Re[y], Im[y], Im[y]}
thus ParametricPlot (called by Curves[]) has bouns={y, y0, y1, dy} *)

horizontalgrid = Curves[ w, {x, x0, x1, dx}, {y, y0, y1}];

(* Form the table of vertical values, with spread=\:f01dy, y0, y1, dy\:f028.
Vertical coordinates turn out to be of the type \:f01d Re[x] , Im[x] \:f028
thus ParametricPlot \:f001called by Curves[]\:f007 has bounds=\:f01dx, x0, x1\:f028 *)

verticalgrid = Curves[ w, {y, y0, y1, dy}, {x, x0, x1}];

Show[ horizontalgrid, verticalgrid, 
	AspectRatio->Automatic,
	Axes->True,
	ImageSize->Small]
];


Curves[ w_, spread_, bounds_ ] := Module[{curves},

curves = Table[ { Re[w], Im[w] }, spread ];

(* ParametricPlot[ { fx , fy \:f028 , \:f01d u, umin, umax\:f028 ] 
plots a curve in parametric form, i.e. \:f01d x\:2a75fx[u] , y\:2a75fy[u] \:f028,
that is a curve with coordinate pairs given by \:f01dfx, fy\:f028 ,
where fx = fx[u] , fy = fy[u] *)

ParametricPlot[ curves, bounds ]
];


End[]


EndPackage[]
