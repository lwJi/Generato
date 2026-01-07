(* ::Package:: *)

(* testDerivs.wl *)
(* Integration test for Mode->Derivs with finite difference stencils *)

(* (c) Liwei Ji, 01/2026 *)

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

(* Variables for derivative test *)
EvolVarlist = GridTensors[{phi[], PrintAs -> "phi"}];

DerivVarlist = GridTensors[{dphi[i], PrintAs -> "dphi"}];

TempVarlist = TempTensors[{psi[], PrintAs -> "psi"}];

(* Simple equation using the derivative *)
SetEQN[psi[], euclid[i, j] dphi[-i] dphi[-j]];

SetOutputFile[FileNameJoin[{Directory[], "testDerivs.hxx"}]];

SetMainPrint[
  pr[];
  PrintInitializations[{Mode -> "MainIn"}, EvolVarlist];
  pr[];
  (* Test Mode->Derivs with DerivsOrder and AccuracyOrder *)
  PrintInitializations[{Mode -> "Derivs", TensorType -> "Vect", DerivsOrder -> 1, AccuracyOrder -> 4}, DerivVarlist];
  pr[];
  PrintEquations[{Mode -> "Temp", ChartName -> cart}, TempVarlist];
];

Import[FileNameJoin[{Environment["GENERATO"], "codes/CarpetXGPU.wl"}]];
