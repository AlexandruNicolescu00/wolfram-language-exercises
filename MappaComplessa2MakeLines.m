(* ::Package:: *)

(* :Title: MappaComplessa2MakeLines *)
(* :Context: MappaComplessa2MakeLines` *)
(* :Author: GS *)
(* :Summary: variante di MappaComplessa2, pi\[UGrave] veloce, ma pi\[UGrave] grossolana;
qui lavoriamo in MachinePrecision, in B/W invece che a colori,
ed usiamo Line invece che Parametric Plot *)
(* :Copyright: GS 2025 *)
(* :Package Version: 3 *)
(* :Mathematica Version: 14 *)
(* :History: last modified 14/3/2025 *)
(* :Sources: biblio *)
(* :Limitations:
this is a preliminary version, for educational purposes only. *)
(* :Discussion: USES LINE *)
(* :Requirements: *)
(* :Warning: DOCUMENTATE TUTTO il codice *)


BeginPackage["MappaComplessa2MakeLines`"]


CartesianMap::usage = "CartesianMap[f, {x0, x1, (dx)}, {y0, y1, (dy)}]
	plots the image of the cartesian coordinate lines under the function f.
	The default values of dx and dy are chosen so that the number of lines
	is equal to the value of the option PlotPoints of Plot3D[ ]."


PolarMap::usage = "PolarMap[f, {r0:0, r1, (dr)}, {p0, p1, (dp)}]
	plots the image of the polar coordinate lines under the function f.
	The default values of dr and dphi are chosen so that the number of lines
	is equal to the value of the option PlotPoints of Plot3D[ ]."


Begin["`Private`"]


(* default increments *)
CartesianMap[ func_, {x0_, x1_}, {y0_, y1_} ] :=
Module[ {dx, dy, myplotpoints},

 (* Di default, PlotPoints/.Options[Plot] e' Automatic *)
 (* La Option PlotPoints puo' essere modificata 
    con SetOptions[ Plot, PlotPoints->num]; *)
 (* Qui, pertanto, collego myplotpoints ad una Option di Plot
    il cui default puo' essere variato *)
    
    myplotpoints = PlotPoints /. Options[Plot];
	(* Print["current value of plotpoints= ",myplotpoints\:f016; *)
	dx=(x1-x0)/(myplotpoints-1);
	dy=(y1-y0)/(myplotpoints-1);
	CartesianMap[ func, {x0, x1, dx}, {y0, y1, dy} ]
];


(* explicit increments *)
CartesianMap[ func_, {x0_, x1_, dx_}, {y0_, y1_, dy_} ] :=
Module[ {x, y, coords},
coords = Table[ N[ func[x + I y] ] , {x, x0, x1, dx}, {y, y0, y1, dy} ];
Show[ MakeLines[coords], AspectRatio -> Automatic, Axes -> Automatic, ImageSize -> Small]
];


(* default increments *)
PolarMap[ func_, {r0_:0, r1_}, {p0_, p1_} ] :=
Module[ {dr, dp, myplotpoints},

(* Idem *)
(* Collego myplotpoints ad una Option di Plot
il cui default puo' essere variato *)
myplotpoints = PlotPoints /. Options[Plot];
dr=(r1-r0)/(myplotpoints-1);
dp=(p1-p0)/(myplotpoints-1);
PolarMap[ func, {r0, r1, dr}, {p0, p1, dp} ]
];


(* explicit increments *)
PolarMap[ func_, {r0_, r1_, dr_}, {p0_, p1_, dp_} ] :=
Module[ {r, p, coords},
coords = Table[ N[ func[r Exp[I p] ] ] , {r, r0, r1, dr}, {p, p0, p1, dp} ];
Show[ MakeLines[coords], AspectRatio -> Automatic, Axes -> Automatic, ImageSize -> Small]
];


(* auxiliary function *)
MakeLines[points_] :=
Module[ {coords, lines},
coords = Map[ {Re[#], Im[#]}& , points, {2} ] ;
lines = Map[ Line, Join[ coords, Transpose[coords] ] ] ;
Graphics[ lines ]
];


End[]


EndPackage[]
