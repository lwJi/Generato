(* ::Package:: *)

(* failOldSurface.wl *)
(* A run that must fail at its first call of the surface that was removed: a script that calls SetMainPrint *)

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
    v^i = M^i_j u^j
    r^i = v^i
*)

SetEQN[vU[i_], euclid[i, k] MDD[-k, -j] uU[j]];

SetEQN[rU[i_], vU[i]];

(*
    From here the script is written as scripts were before a program was a
    list handed to Generate: the main content is held and printed, the output
    file is set apart from it, and the Import of the backend file starts the
    write. Every one of these names was removed. The run stops at the first
    of them, which here is SetMainPrint, and its message says what to write
    in its place. Left undefined, the calls would be inert, and the script
    would run to its end, write nothing and report success.
*)

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

SetOutputFile[FileNameJoin[{Directory[], "failOldSurface.hxx"}]];

Import[FileNameJoin[{Environment["GENERATO"], "codes/CarpetXGPU.wl"}]];
