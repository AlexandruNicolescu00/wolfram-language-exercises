(* ::Package:: *)

(* :Title: BestExample *)
(* :Context: BestExample` *)
(* :Author: GS *)
(* :Summary: an example of good programming style *)
(* :Copyright: GS 2025 *)
(* :Package Version: 1 *)
(* :Mathematica Version: 14 *)
(* :History: last modified 14/3/2025 *)
(* :Keywords: programming style, local variables *)
(* :Sources: biblio *)
(* :Limitations: this is for educational purposes only. *)
(* :Discussion: *)
(* :Requirements: *)
(* :Warning: package Context is not defined *)
PowerSum::usage = "PowerSum[x, n] returns the sum of the first n powers of x."
Begin["Private`"]
PowerSum[x_, n_] :=
Module[{i},
Sum[x^i, {i, 1, n}]
]
End[]
