(* ::Package:: *)

(* failBadVarSpec.wl *)
(* A run that must fail before generation: a var spec that cannot be parsed *)

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

EvolVarlist = GridTensors[{uU[i], PrintAs -> "u"}];

(* The indices of rU are written as two groups, which is not a tensor. The run stops here. *)

dtEvolVarlist = GridTensors[{rU[i][j], PrintAs -> "r"}];

SetEQN[rU[i_], uU[i]];

SetOutputFile[FileNameJoin[{Directory[], "failBadVarSpec.hxx"}]];

SetMainPrint[
  pr[];
  PrintInitializations[{Mode -> "MainOut"}, dtEvolVarlist];
  PrintInitializations[{Mode -> "MainIn"}, EvolVarlist];
  pr[];
  PrintEquations[{Mode -> "MainOut"}, dtEvolVarlist];
];

Import[FileNameJoin[{Environment["GENERATO"], "codes/CarpetXGPU.wl"}]];
