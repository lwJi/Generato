(* ::Package:: *)

(* testTileDerivs.wl *)
(* Integration test for derivatives read from tile storage, at order 1 and 2, of a scalar, a vector and a symmetric matrix *)
(* Declarations follow SpacetimeX Z4cowGPU/wolfram/wl/Z4c_vars.wl, the program Z4cowGPU_set_rhs_GF3D5.wl *)

(* (c) Liwei Ji, 11/2026 *)

Needs["xAct`xCoba`", FileNameJoin[{Environment["GENERATO"], "src/Generato.wl"}]]

SetPVerbose[False];

SetPrintDate[False];

SetGridPointIndex["[[ijk]]"];

SetTilePointIndex["[[ijk5]]"];

SetTempVariableType["vreal"];

DefManifold[M3, 3, IndexRange[a, z]];

DefChart[cart, M3, {1, 2, 3}, {X[], Y[], Z[]}, ChartColor -> Blue];

DefMetric[1, euclid[-i, -j], CD];

MetricInBasis[euclid, -cart, DiagonalMatrix[{1, 1, 1}]];

MetricInBasis[euclid, cart, DiagonalMatrix[{1, 1, 1}]];

(* A scalar, a symmetric matrix and a vector, with their time derivatives *)
dtEvolVarlist =
  GridTensors[
    {dtW[], PrintAs -> "dtW"},
    {dtbeta[i], PrintAs -> "dtbeta"}
  ];

EvolVarlist =
  GridTensors[
    {W[], PrintAs -> "W"},
    {gamt[-i, -j], Symmetric[{-i, -j}], PrintAs -> "gamt"},
    {beta[i], PrintAs -> "beta"}
  ];

(* Derivatives stored on a tile, read at the tile point index *)
dEvolVarlist =
  TileTensors[
    {dW[-k], DerivativeOf -> W, PrintAs -> "dW"},
    {dgamt[-k, -i, -j], Symmetric[{-i, -j}], DerivativeOf -> gamt, PrintAs -> "dgamt"},
    {dbeta[-k, i], DerivativeOf -> beta, PrintAs -> "dbeta"}
  ];

ddEvolVarlist =
  TileTensors[
    {ddW[-i, -j], Symmetric[{-i, -j}], DerivativeOf -> W, PrintAs -> "ddW"},
    {ddgamt[-l, -m, -i, -j], GenSet[Cycles[{1, 2}], Cycles[{3, 4}]], DerivativeOf -> gamt, PrintAs -> "ddgamt"},
    {ddbeta[-i, -j, k], Symmetric[{-i, -j}], DerivativeOf -> beta, PrintAs -> "ddbeta"}
  ];

TempVarlist =
  TempTensors[
    {dlnW[-k], PrintAs -> "dlnW"},
    {GtDDD[-k, -i, -j], Symmetric[{-i, -j}], PrintAs -> "Gt"},
    {divbeta[], PrintAs -> "divbeta"},
    {lapW[], PrintAs -> "lapW"},
    {lapgamt[-i, -j], Symmetric[{-i, -j}], PrintAs -> "lapgamt"},
    {lapbeta[i], PrintAs -> "lapbeta"}
  ];

(* Each equation reads one derivative variable, dependent components included *)
SetEQN[dlnW[i_], W[] ^ -1 dW[i]];

SetEQN[GtDDD[k_, i_, j_], 1/2 (dgamt[i, j, k] + dgamt[j, k, i] - dgamt[k, i, j])];

SetEQN[divbeta[], dbeta[-k, k]];

SetEQN[lapW[], euclid[k, l] ddW[-k, -l]];

SetEQN[lapgamt[i_, j_], euclid[l, m] ddgamt[-l, -m, i, j]];

SetEQN[lapbeta[i_], euclid[k, l] ddbeta[-k, -l, i]];

SetEQN[dtW[], beta[k] dW[-k] - W[] divbeta[] + lapW[]];

SetEQN[dtbeta[i_], beta[k] dbeta[-k, i] + lapbeta[i]];

(* A derivative stored on a tile is read like any tile, with "In"; its two indices follow from its declaration *)
Generate["CarpetXGPU", "testTileDerivs.hxx", {
  "",
  Load[dtEvolVarlist, "Out"],
  Load[EvolVarlist, "In"],
  "",
  Load[dEvolVarlist, "In"],
  Load[ddEvolVarlist, "In"],
  "",
  "grid.loop_int_device<0, 0, 0>(",
  "  grid.nghostzones, [=] ARITH_DEVICE(const PointDesc &p) ARITH_INLINE {",
  "const int ijk = layout2.linear(p.i, p.j, p.k);",
  "const int ijk5 = layout5.linear(p.i, p.j, p.k);",
  "",
  Store[TempVarlist, "Temp"],
  "",
  Store[dtEvolVarlist, "Out"],
  "",
  "});"
}];
