(* ::Package:: *)

(* testVariant.wl *)
(* Integration test for a backend the script registers: it extends CarpetXGPU and states one key *)
(* The declarations, the equations and the program are those of test.wl *)

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
    r^i = M^i_j M^j_k u^k     if ADM_ConstraintNorm = Msqr
    r^i = M^i_j u^j           otherwise
*)

SetEQN[vU[i_], euclid[i, k] MDD[-k, -j] uU[j]];

SetEQN[{SuffixName -> "Msqr"}, rU[i_], euclid[i, k] MDD[-k, -j] vU[j]];

SetEQN[{SuffixName -> "otherwise"}, rU[i_], vU[i]];

(*
    The backend of this script is CarpetXGPU, except that a grid function
    that is read is loaded as its value at the point. That is the one key it
    states: the "Out" load, the tile loads and the stores are the ones
    CarpetXGPU has. A formatter takes one statement and returns its lines.
*)

DefineBackend["CarpetXPoint", <|
  "Extends" -> "CarpetXGPU",
  "Load" -> <|
    "In" -> <|
      "Grid" ->
        Function[statement,
          {
            "const " <> statement["Config", "TempVariableType"] <> " " <> statement["Component", "Name"]
            <> " = gf_" <> statement["Variable", "Name"]
            <> IndexSuffix[statement["Variable"], statement["Component"], statement["Base"]] <> "(p.I);"
          }
        ]
    |>
  |>
|>];

Generate["CarpetXPoint", "testVariant.hxx", {
  "",
  Load[dtEvolVarlist, "Out"],
  Load[EvolVarlist, "In"],
  Load[MoreInVarlist, "In"],
  "",
  Store[TempVarlist, "Temp"],
  "",
  "if(Msqr)",
  "{",
  Store[dtEvolVarlist, "Out", SuffixName -> "Msqr", ChartName -> cart],
  "}",
  "else",
  "{",
  Store[dtEvolVarlist, "Out", SuffixName -> "otherwise"],
  "}"
}];
