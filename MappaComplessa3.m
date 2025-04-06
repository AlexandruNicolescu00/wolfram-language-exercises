(* ::Package:: *)

(* :Title: MappaComplessa3 *)
(* :Context: MappaComplessa3` *)
(* :Author: GS *)
(* :Summary: versione modificata di MappaComplessa2MakeLines *)
(* :Copyright: GS 2025 *)
(* :Package Version: 3 *)
(* :Mathematica Version: 14 *)
(* :History: last modified 14/3/2025 *)
(* :Sources: bblio *)
(* :Limitations: educational purposes *)
(* :Discussion: *)
(* :Requirements: *)
(* :Warning: DOCUMENTATE TUTTO il codice *)

BeginPackage["MappaComplessa3`"]

CartesianMap::usage = "CartesianMap[f, {x0, x1, (dx)}, {y0, y1, (dy)}]
plots the image of the cartesian coordinate lines under the function f.
The default values of dx and dy are chosen so that the number of lines
is equal to the value of the option PlotPoints of Plot."

PolarMap::usage = "PolarMap[f, {r0:0, r1, (dr)}, {p0, p1, (dp)}]
plots the image of the polar coordinate lines under the function f.
The default values of dr and dp are chosen so that the number of lines
is equal to the value of the option PlotPoints of Plot."


Begin["`Private`"]


CartesianMap[ func_, {x0_, x1_, dx_:Automatic}, {y0_, y1_, dy_:Automatic}] :=
Module[ {x, y, coords, myplotpoints, ndx=dx, ndy=dy},

myplotpoints = PlotPoints /. Options[Plot];

If[ dx === Automatic, ndx = (x1-x0) / (myplotpoints - 1) ];
If[ dy === Automatic, ndy = (y1-y0) / (myplotpoints - 1) ];

coords = Table[ N[ func[ x+I y] ], {x, x0, x1, ndx}, {y,  y0, y1, ndy} ];

Show[MakeLines[coords], AspectRation->Automatic, Axes->Automatic, ImageSize->Small]
];


PolarMap[ func_, {r0_:0, r1_, dr_:Automatic}, {p0_, p1_, dp_:Automatic}] :=
Module[ {r, p, coords, myplotpoints, ndr=dr, ndp=dp},

myplotpoints = PlotPoints /. Options[Plot];

If[ dr === Automatic, ndr = (r1 - r0) / (myplotpoints - 1) ];
If[ dp === Automatic, ndp = (p1 - p0) / (myplotpoints - 1) ];

coords = Table[ N[ func[ r Exp[ I p ] ] ], {r, r0, r1,ndr}, {p, p0, p1, ndp} ];

Show[MakeLines[coords], AspectRatio->Automatic, Axes->Automatic, ImageSize->Small]
];


(* auxiliary function *)
MakeLines[points_] :=
Module[ {coords, lines},
coords = Map[ {Re[#], Im[#]}&, points, {2} ];
lines = Map[ Line, Join[ coords, Transpose[coords] ] ];
Graphics[ lines ]
];


End[]


EndPackage[]
