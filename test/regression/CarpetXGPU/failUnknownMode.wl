(* ::Package:: *)

(* failUnknownMode.wl *)
(* A run that must fail at the check of the program: a load asks for a mode the backend does not have *)

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
    "MoreInOut" is a load of another backend: CarpetXGPU has no such key. The
    check of the program finds it before a right-hand side is evaluated, and
    the message names the mode, the backend and the modes the backend accepts.
*)

Generate["CarpetXGPU", "failUnknownMode.hxx", {
  "",
  Load[dtEvolVarlist, "Out"],
  Load[EvolVarlist, "In"],
  Load[MoreInVarlist, "MoreInOut"],
  "",
  Store[TempVarlist, "Temp"],
  "",
  Store[dtEvolVarlist, "Out"]
}];
