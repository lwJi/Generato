(* ::Package:: *)

(* failNoRelation.wl *)
(* A run that must fail during generation: a variable is loaded with Mode->Derivs and declares no DerivativeOf *)

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
    one. Nothing is read out of a name, so the run stops when dphi is loaded
    with Mode->Derivs, after the header and the first initialization have
    been built.
*)

DerivVarlist = GridTensors[{dphi[-i], PrintAs -> "dphi"}];

TempVarlist = TempTensors[{psi[], PrintAs -> "psi"}];

(* Simple equation using the derivative *)
SetEQN[psi[], euclid[i, j] dphi[-i] dphi[-j]];

SetOutputFile[FileNameJoin[{Directory[], "failNoRelation.hxx"}]];

SetMainPrint[
  pr[];
  PrintInitializations[{Mode -> "MainIn"}, EvolVarlist];
  pr[];
  PrintInitializations[{Mode -> "Derivs"}, DerivVarlist];
  pr[];
  PrintEquations[{Mode -> "Temp"}, TempVarlist];
];

Import[FileNameJoin[{Environment["GENERATO"], "codes/CarpetXGPU.wl"}]];
