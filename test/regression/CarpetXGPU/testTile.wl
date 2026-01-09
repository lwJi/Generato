(* ::Package:: *)

(* testTile.wl *)
(* Integration test for TileTensors with Tile storage type *)

(* (c) Liwei Ji, 01/2026 *)

Needs["xAct`xCoba`", FileNameJoin[{Environment["GENERATO"], "src/Generato.wl"}]]

SetPVerbose[False];

SetPrintDate[False];

SetGridPointIndex[""];

SetTilePointIndex["[[tI]]"];

SetTempVariableType["vreal"];

DefManifold[M3, 3, IndexRange[a, z]];

DefChart[cart, M3, {1, 2, 3}, {X[], Y[], Z[]}, ChartColor -> Blue];

DefMetric[1, euclid[-i, -j], CD];

MetricInBasis[euclid, -cart, DiagonalMatrix[{1, 1, 1}]];

MetricInBasis[euclid, cart, DiagonalMatrix[{1, 1, 1}]];

(* Use TileTensors for tile-based storage *)
TileVarlist = TileTensors[{tileU[i], PrintAs -> "tile"}];

GridVarlist = GridTensors[{gridU[i], PrintAs -> "grid"}];

TempVarlist = TempTensors[{tempU[i], PrintAs -> "temp"}];

(* Simple equation: temp = grid, tile = temp *)
SetEQN[tempU[i_], gridU[i]];

SetEQN[tileU[i_], tempU[i]];

SetOutputFile[FileNameJoin[{Directory[], "testTile.hxx"}]];

SetMainPrint[
  pr[];
  PrintInitializations[{Mode -> "MainOut"}, TileVarlist];
  PrintInitializations[{Mode -> "MainIn", StorageType -> "Tile"}, GridVarlist];
  pr[];
  PrintEquations[{Mode -> "Temp"}, TempVarlist];
  pr[];
  PrintEquations[{Mode -> "Main"}, TileVarlist];
];

Import[FileNameJoin[{Environment["GENERATO"], "codes/CarpetXGPU.wl"}]];
