(* ::Package:: *)

(* testModes.wl *)
(* Integration test for the "Temp" load and the "AddTo" store *)

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

Generate["CarpetXGPU", "testModes.hxx", {
  "",
  Load[dtEvolVarlist, "Out"],
  Load[EvolVarlist, "In"],
  Load[MoreInVarlist, "In"],
  "",
  (* Declare the temporaries, then assign them in a branch *)
  Load[TempVarlist, "Temp"],
  "",
  "if(Msqr)",
  "{",
  Store[TempVarlist, "Out", SuffixName -> "Msqr"],
  "}",
  "else",
  "{",
  Store[TempVarlist, "Out", SuffixName -> "otherwise"],
  "}",
  "",
  Store[dtEvolVarlist, "AddTo"]
}];
