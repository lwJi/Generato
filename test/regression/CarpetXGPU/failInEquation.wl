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
    then a throw that Generato did not raise. The program passes its check,
    since vU has an equation; the throw comes when the right-hand sides are
    evaluated, after those of the first two components.
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

Generate["CarpetXGPU", "failInEquation.hxx", {
  "",
  Load[dtEvolVarlist, "Out"],
  Load[EvolVarlist, "In"],
  Load[MoreInVarlist, "In"],
  "",
  Store[TempVarlist, "Temp"],
  "",
  Store[dtEvolVarlist, "Out"]
}];
