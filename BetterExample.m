(* ::Package:: *)

(* :Title: BetterExample *)
(* :Context: BetterExample` *)
(* :Author: GS *)
(* :Summary: an example of poor programming style. *)
(* :Copyright: GS 2025 *)
(* :Package Version: 1 *)
(* :Mathematica Version: 14 *)
(* :History: last modified 14/3/2025 *)
(* :Keywords: template, skeleton, package *)
(* :Sources: biblio *)
(* :Limitations:
this is a preliminary version, for educational purposes only. *)
(* :Discussion: *)
(* :Requirements: *)
(* Warning : i simboli i, n, x sono visibili nel contesto Globale *)
(* PowerSum::usage = "PowerSum[x, n] returns the sum of the first n powers of x." *)
PowerSum[x_, n_] :=
Module[{i},
Sum[ x^i, {i, 1, n}]
]
