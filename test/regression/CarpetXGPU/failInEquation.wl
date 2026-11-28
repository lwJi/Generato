(* ::Package:: *)

(* failInEquation.wl *)
(* A run that must fail during generation: a right-hand side that cannot be evaluated *)

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
    v^i = M^i_j u^j     for i = 1, 2
    The right-hand side of v^3 ends the way an xAct error does: a message,
    then a throw that Generato did not raise. It is evaluated when the
    equation is printed, after the header, the initializations and the first
    two components have been built.
*)

vU::nocomp = "No right-hand side for component `1` of vU.";

SetEQNDelayed[vU[i_],
  If[i[[1]] < 3,
    euclid[i, k] MDD[-k, -j] uU[j]
    ,
    Throw[Message[vU::nocomp, i[[1]]]]
  ]
];

SetEQN[rU[i_], vU[i]];

SetOutputFile[FileNameJoin[{Directory[], "failInEquation.hxx"}]];

SetMainPrint[
  pr[];
  PrintInitializations[{Mode -> "MainOut"}, dtEvolVarlist];
  PrintInitializations[{Mode -> "MainIn"}, EvolVarlist];
  PrintInitializations[{Mode -> "MainIn"}, MoreInVarlist];
  pr[];
  PrintEquations[{Mode -> "Temp"}, TempVarlist];
  pr[];
  PrintEquations[{Mode -> "MainOut"}, dtEvolVarlist];
];

Import[FileNameJoin[{Environment["GENERATO"], "codes/CarpetXGPU.wl"}]];
