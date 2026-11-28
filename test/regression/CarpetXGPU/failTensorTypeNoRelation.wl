(* ::Package:: *)

(* failTensorTypeNoRelation.wl *)
(* A run that must fail during generation: a tile load states TensorType "Vect" for variables that declare no DerivativeOf *)
(* The shape of SpacetimeX Z4cowGPU_set_rhs_GF3D5.wl:59-63 before its declarations state their relations *)

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

EvolVarlist =
  GridTensors[
    {W[], PrintAs -> "W"},
    {gamt[-i, -j], Symmetric[{-i, -j}], PrintAs -> "gamt"},
    {beta[i], PrintAs -> "beta"}
  ];

(*
    Derivatives stored on a tile, which do not declare what they are
    derivatives of. TensorType "Vect" used to lay dgamt and dbeta out with two
    indices; without DerivativeOf they would now get one, so the run stops
    where the load states it, after the header and the first initializations
    have been built.
*)

dEvolVarlist =
  TileTensors[
    {dW[-k], PrintAs -> "dW"},
    {dgamt[-k, -i, -j], Symmetric[{-i, -j}], PrintAs -> "dgamt"},
    {dbeta[-k, i], PrintAs -> "dbeta"}
  ];

SetOutputFile[FileNameJoin[{Directory[], "failTensorTypeNoRelation.hxx"}]];

SetMainPrint[
  pr[];
  PrintInitializations[{Mode -> "MainIn"}, EvolVarlist];
  pr[];
  PrintInitializations[{Mode -> "Derivs", DerivsOrder -> 1,
                        StorageType -> "Tile", TensorType -> "Vect"},
                       dEvolVarlist];
];

Import[FileNameJoin[{Environment["GENERATO"], "codes/CarpetXGPU.wl"}]];
