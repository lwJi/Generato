(* ::Package:: *)

(* testModule.wl *)
(* Integration test for a module: module/ADM/ADM_var.wl declares the ADM variables and names no storage, *)
(* and this script binds the ones it uses *)

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

(* The module declares ADMVars, ADMdVars and ADMdtVars *)

Get[FileNameJoin[{Environment["GENERATO"], "module/ADM/ADM_var.wl"}]];

(* A binding takes the list a declaration returned. ADMdVars is not used here, and stays declared and not bound *)

ADMVarlist = GridTensors[ADMVars];

ADMdtVarlist = GridTensors[ADMdtVars];

(* A variable of the script is declared and bound in one call *)

TempVarlist = TempTensors[{trK[], PrintAs -> "K"}];

(*
    K            = K_ij delta^ij     the metric is taken as flat in the trace, to keep the test small
    dt alpha     = -2 alpha K
    dt beta^i    = -K beta^i
*)

SetEQN[trK[], euclid[i, j] ADMexK[-i, -j]];

SetEQN[ADMdtalpha[], -2 ADMalpha[] trK[]];

SetEQN[ADMdtbeta[i_], -trK[] ADMbeta[i]];

Generate["CarpetXGPU", "testModule.hxx", {
  "",
  Load[ADMdtVarlist, "Out"],
  Load[ADMVarlist, "In"],
  "",
  Store[TempVarlist, "Temp"],
  "",
  Store[ADMdtVarlist, "Out"]
}];
