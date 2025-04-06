(* ::Package:: *)

(* :Title: MappaComplessa2 *)
(* :Context: MappaComplessa2` *)
(* :Author: GS *)
(* :Summary: rispetto a MappaComplessa0.m,
qui viene aggiunta una seconda Rule
per dare valori di default agli incrementi dx , dy *)
(* :Copyright: GS 2025 *)
(* :Package Version: 3 *)
(* :Mathematica Version: 14 *)
(* :History: last modified 14/3/2025 *)
(* :Keywords: default values *)
(* :Sources: biblio *)
(* :Limitations:
this is a preliminary version, for educational purposes only. *)
(* :Discussion: *)
(* :Requirements: *)
(* :Warning: DOCUMENTATE TUTTO il codice *)


BeginPackage["MappaComplessa2`"]


CartesianMap::usage = 
	"CartesianMap[f, {x0, x1, (dx)}, {y0, y1, (dy)}]
	plots the image of the cartesian coordinate lines under the function f.
	The default values of dx and dy are chosen so that the number of lines
	is equal to the value of the option PlotPoints of Plot3D[ ]."


PolarMap::usage =
	"PolarMap[f, {r0:0, r1, (dr)}, {p0, p1, (dp)}]
	plots the image of the polar coordinate lines under the function f.
	The default values of dr and dphi are chosen so that the number of lines
	is equal to the value of the option PlotPoints of Plot3D[ ]."


Begin["`Private`"]


(* default increments *)
CartesianMap[ func_, {x0_, x1_}, {y0_, y1_} ] :=
	Module[ {dx, dy, myplotpoints},
	
		(* Di default, PlotPoints/.Options[Plot] e' Automatic *)
		(* La Option PlotPoints puo' essere modificata
		con SetOptions\:f010 Plot, PlotPoints\[RightArrow]num\:f016; *)
		(* Qui, pertanto, collego myplotpoints ad una Option di Plot
		il cui default puo' essere variato *)
		
		myplotpoints = PlotPoints/. Options[Plot];
		Print["current value of plotpoints= ", myplotpoints];
		
		dx=(x1-x0)/(myplotpoints-1);
		dy=(y1-y0)/(myplotpoints-1);
		CartesianMap[ func, {x0, x1, dx}, {y0, y1, dy} ]
	];


CartesianMap[ func_, {x0_, x1_, dx_}, {y0_, y1_, dy_} ] :=
	Module[ {x, y},
		Picture[ func[x + I y], {x, x0, x1, dx}, {y, y0, y1, dy} ]
	];


(* default increments *)
PolarMap[ func_, {r0_:0, r1_}, {p0_, p1_} ] :=
	Module[ {dr, dp, myplotpoints},
	
		(* Collego myplotpoints ad una Option di Plot
		il cui default puo' essere variato *)
		
		myplotpoints = PlotPoints /. Options[Plot];
		
		dr=(r1-r0)/(myplotpoints-1);
		dp=(p1-p0)/(myplotpoints-1);
		PolarMap[ func, {r0, r1, dr}, {p0, p1, dp} ]
	];


PolarMap[ func_, {r0_, r1_, dr_}, {p0_, p1_, dp_} ] :=
	Module[ {r, p},
	 
		(* Use func and the polar coordinates {r,p} to form the value f[r*Exp[I*p]] *)
		
		Picture[ func[r Exp[I p]], {r, r0, r1, dr}, {p, p0, p1, dp} ]
	]


(* auxiliary functions *)
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
