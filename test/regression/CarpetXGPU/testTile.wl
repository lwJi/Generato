(* ::Package:: *)

(* testTile.wl *)
(* Integration test for TileTensors: a tile-bound variable is loaded from its tile *)

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

(* Each list is loaded as what it is bound to: the tile from tl_, the grid function from gf_ *)
Generate["CarpetXGPU", "testTile.hxx", {
  "",
  Load[TileVarlist, "In"],
  Load[GridVarlist, "In"],
  "",
  Store[TempVarlist, "Temp"],
  "",
  Store[TileVarlist, "Out"]
}];
