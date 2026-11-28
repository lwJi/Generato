(* ::Package:: *)

(* testModes.wl *)
(* Integration test for the "Temp" initialization and the "AddToMainOut" equation *)

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

dtEvolVarlist = GridTensors[{rU[i], PrintAs -> "r"}];

EvolVarlist = GridTensors[{uU[i], PrintAs -> "u"}];

MoreInVarlist = GridTensors[{MDD[-i, -j], Symmetric[{-i, -j}], PrintAs -> "M"}];

TempVarlist = TempTensors[{vU[i], PrintAs -> "v"}];

(*
    v^i  = M^i_j u^j     if Msqr
    v^i  = u^i           otherwise
    r^i += v^i
*)

SetEQN[{SuffixName -> "Msqr"}, vU[i_], euclid[i, k] MDD[-k, -j] uU[j]];

SetEQN[{SuffixName -> "otherwise"}, vU[i_], uU[i]];

SetEQN[rU[i_], vU[i]];

SetOutputFile[FileNameJoin[{Directory[], "testModes.hxx"}]];

SetMainPrint[
  pr[];
  PrintInitializations[{Mode -> "MainOut"}, dtEvolVarlist];
  PrintInitializations[{Mode -> "MainIn"}, EvolVarlist];
  PrintInitializations[{Mode -> "MainIn"}, MoreInVarlist];
  pr[];
  (* Declare the temporaries, then assign them in a branch *)
  PrintInitializations[{Mode -> "Temp"}, TempVarlist];
  pr[];
  pr["if(Msqr)"];
  pr["{"];
  PrintEquations[{Mode -> "MainOut", SuffixName -> "Msqr"}, TempVarlist];
  pr["}"];
  pr["else"];
  pr["{"];
  PrintEquations[{Mode -> "MainOut", SuffixName -> "otherwise"}, TempVarlist];
  pr["}"];
  pr[];
  PrintEquations[{Mode -> "AddToMainOut"}, dtEvolVarlist];
];

Import[FileNameJoin[{Environment["GENERATO"], "codes/CarpetXGPU.wl"}]];
