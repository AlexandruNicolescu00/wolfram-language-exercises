(* ::Package:: *)

(* :Title: MappaComplessa0 *)
(* :Context: MappaComplessa0` *)
(* :Author: GS *)
(* :Summary: rispetto a MappaCartesiana.m,
qui viene aggiunta una funzione per plottare
non solo in coordinate cartesiane, ma anche polari *)
(* :Copyright: GS 2025 *)
(* :Package Version: 3 *)
(* :Mathematica Version: 14 *)
(* :History: last modified 14/3/2025 *)
(* :Keywords: Cartesian coordinates, polar coordinates *)
(* :Sources: biblio *)
(* :Limitations:
this is a preliminary version, for educational purposes only. *)
(* :Discussion: *)
(* :Requirements: *)
(* :Warning: DOCUMENTATE TUTTO il codice *)


BeginPackage["MappaComplessa0`"]


CartesianMap::usage = 
	"CartesianMap[f, {x0, x1, dx}, {y0, y1, dy}] plots the image
	of the Cartesian coordinate lines under the function f."


PolarMap::usage =
	"PolarMap[f, {r0, r1, dr}, {p0, p1, dp}] plots the image
	of the polar coordinate lines under the function f."


Begin["`Private`"]


CartesianMap[ func_, {x0_, x1_, dx_}, {y0_, y1_, dy_} ] :=
	Module[ {x, y},
	
		(* Use func and the cartesian coordinates {x,y} to form the value f[x+I*y] *)
		
		Picture[ func[x + I y], {x, x0, x1, dx}, {y, y0, y1, dy} ]
	]


PolarMap[ func_, {r0_, r1_, dr_}, {p0_, p1_, dp_} ] :=
	Module[ {r, p},
	 
		(* Use func and the polar coordinates {r,p} to form the value f[r*Exp[I*p]] *)
		
		Picture[ func[r Exp[I p]], {r, r0, r1, dr}, {p, p0, p1, dp} ]
	]


Picture[ v_, {s_, s0_, s1_, ds_}, {t_, t0_, t1_, dt_} ] :=
	Module[ {horiz, vert},
	
		(* Form the table of horizontal values, with spread=\:f01ds, s0, s1, ds\:f028.
		Horizontal coordinates are of the type \:f01d Re[y] , Im[y] \:f028 or \:f01d Re[p] , Im[p] \:f028
		thus ParametricPlot \:f001called by Curves[]\:f007 has bounds=\:f01dt, t0, t1\:f028 *)
		
		horiz = Curves[ v, {s, s0, s1, ds}, {t, t0, t1}];
		
		(* Form the table of vertical values, with spread=\:f01dt, t0, t1, dt\:f028.
		Vertical coordinates are of the type \:f01d Re[x] , Im[x] \:f028 or \:f01d Re[r] , Im[r] \:f028
		thus ParametricPlot \:f001called by Curves[]\:f007 has bounds=\:f01ds, s0, s1\:f028 *)
		
		vert = Curves[ v, {t, t0, t1, dt}, {s, s0, s1} ];
		
		Show[ horiz, vert,
			AspectRatio->Automatic,
			Axes->True,
			PlotRange->All,
			ImageSize->Small
		]
	];


Curves[ w_, spread_, bounds_ ] :=
	Module[ {curves},
		curves = Table[ { Re[w], Im[w] }, spread ];
		
		(* ParametricPlot\:f010 \:f01d fx , fy \:f028 , \:f01d u, umin, umax\:f028 \:f016
		plots a curve in parametric form,
		i.e. \:f01d x\:2a75fx[u] , y\:2a75fy[u] \:f028,
		that is a curve with coordinate pairs given by \:f01dfx, fy\:f028 ,
		where fx = fx[u] , fy = fy[u] *)
		
		ParametricPlot[curves, bounds]
	];


End[]


EndPackage[]
