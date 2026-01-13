(* ::Package:: *)

(* test.wl *)

(* (c) Liwei Ji, 01/2024 *)

Needs["xAct`xCoba`", FileNameJoin[{Environment["GENERATO"], "src/Generato.wl"}]]

$CurrentContext = SetPVerbose[$CurrentContext, False];

$CurrentContext = SetPrintDate[$CurrentContext, False];

$CurrentContext = SetGridPointIndex[$CurrentContext, ""];

$CurrentContext = SetTempVariableType[$CurrentContext, "vreal"];

DefManifold[M3, 3, IndexRange[a, z]];

DefChart[cart, M3, {1, 2, 3}, {X[], Y[], Z[]}, ChartColor -> Blue];

(*DefChart[spnr, M3, {1, 2, 3}, {r[], th[], ph[]}, ChartColor -> Green];*)

DefMetric[1, euclid[-i, -j], CD];

MetricInBasis[euclid, -cart, DiagonalMatrix[{1, 1, 1}]];

MetricInBasis[euclid, cart, DiagonalMatrix[{1, 1, 1}]];

dtEvolVarlist = GridTensors[{rU[i], PrintAs -> "r"}];

EvolVarlist = GridTensors[{uU[i], PrintAs -> "u"}];

MoreInVarlist = GridTensors[{MDD[-i, -j], Symmetric[{-i, -j}], PrintAs -> "M"}];

TempVarlist = TempTensors[{vU[i], PrintAs -> "v"}];

(*
    r^i = M^i_j M^j_k u^k     if ADM_ConstraintNorm = Msqr
    r^i = M^i_j u^j           otherwise
*)

SetEQN[vU[i_], euclid[i, k] MDD[-k, -j] uU[j]];

SetEQN[{SuffixName -> "Msqr"}, rU[i_], euclid[i, k] MDD[-k, -j] vU[j]];

SetEQN[{SuffixName -> "otherwise"}, rU[i_], vU[i]];

$CurrentContext = SetOutputFile[$CurrentContext, FileNameJoin[{Directory[], "test.hxx"}]];

(* SetProject["CarpetX"]; *)

$MainPrint[] := (
  pr[];
  PrintInitializations[{Mode -> "MainOut"}, dtEvolVarlist];
  PrintInitializations[{Mode -> "MainIn"}, EvolVarlist];
  PrintInitializations[{Mode -> "MainIn"}, MoreInVarlist];
  pr[];
  PrintEquations[{Mode -> "Temp"}, TempVarlist];
  pr[];
  pr["if(Msqr)"];
  pr["{"];
  PrintEquations[{Mode -> "MainOut", SuffixName -> "Msqr", ChartName -> cart}, dtEvolVarlist];
  pr["}"];
  pr["else"];
  pr["{"];
  PrintEquations[{Mode -> "MainOut", SuffixName -> "otherwise"}, dtEvolVarlist];
  pr["}"];
);

Import[FileNameJoin[{Environment["GENERATO"], "codes/CarpetX.wl"}]];
