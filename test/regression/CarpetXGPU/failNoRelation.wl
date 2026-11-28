(* ::Package:: *)

(* failNoRelation.wl *)
(* A run that must fail at the check of the program: a variable is loaded with "Derivs" and declares no DerivativeOf *)

(* (c) Liwei Ji, 11/2026 *)

Needs["xAct`xCoba`", FileNameJoin[{Environment["GENERATO"], "src/Generato.wl"}]]

SetPVerbose[False];

SetPrintDate[False];

SetGridPointIndex[""];

SetTempVariableType["vreal"];

DefManifold[M3, 3, IndexRange[a, z]];

DefChart[cart, M3, {1, 2, 3}, {X[], Y[], Z[]}, ChartColor -> Blue];

DefMetric[1, euclid[-i, -j], CD];

MetricInBasis[euclid, -cart, DiagonalMatrix[{1, 1, 1}]];

MetricInBasis[euclid, cart, DiagonalMatrix[{1, 1, 1}]];

EvolVarlist = GridTensors[{phi[], PrintAs -> "phi"}];

(*
    dphi is named like a derivative of phi and does not declare that it is
    one. Nothing is read out of a name, so the check of the program finds
    that the "Derivs" load reads a relation dphi does not declare, and
    nothing is computed.
*)

DerivVarlist = TempTensors[{dphi[-i], PrintAs -> "dphi"}];

TempVarlist = TempTensors[{psi[], PrintAs -> "psi"}];

(* Simple equation using the derivative *)
SetEQN[psi[], euclid[i, j] dphi[-i] dphi[-j]];

Generate["CarpetXGPU", "failNoRelation.hxx", {
  "",
  Load[EvolVarlist, "In"],
  "",
  Load[DerivVarlist, "Derivs"],
  "",
  Store[TempVarlist, "Temp"]
}];
