(* ::Package:: *)

(* testDerivs.wl *)
(* Integration test for Mode->Derivs with finite difference stencils *)

(* (c) Liwei Ji, 01/2026 *)

Needs["xAct`xCoba`", FileNameJoin[{Environment["GENERATO"], "src/Generato.wl"}]]

$CurrentContext = SetPVerbose[$CurrentContext, False];

$CurrentContext = SetPrintDate[$CurrentContext, False];

$CurrentContext = SetGridPointIndex[$CurrentContext, ""];

$CurrentContext = SetTempVariableType[$CurrentContext, "vreal"];

DefManifold[M3, 3, IndexRange[a, z]];

DefChart[cart, M3, {1, 2, 3}, {X[], Y[], Z[]}, ChartColor -> Blue];

DefMetric[1, euclid[-i, -j], CD];

MetricInBasis[euclid, -cart, DiagonalMatrix[{1, 1, 1}]];

MetricInBasis[euclid, cart, DiagonalMatrix[{1, 1, 1}]];

(* Variables for derivative test *)
EvolVarlist = GridTensors[{phi[], PrintAs -> "phi"}];

DerivVarlist = GridTensors[{dphi[-i], PrintAs -> "dphi"}];

TempVarlist = TempTensors[{psi[], PrintAs -> "psi"}];

(* Simple equation using the derivative *)
SetEQN[psi[], euclid[i, j] dphi[-i] dphi[-j]];

$CurrentContext = SetOutputFile[$CurrentContext, FileNameJoin[{Directory[], "testDerivs.hxx"}]];

$MainPrint[] := (
  pr[];
  PrintInitializations[{Mode -> "MainIn"}, EvolVarlist];
  pr[];
  (* Test Mode->Derivs with DerivsOrder and DerivsAccuracy *)
  PrintInitializations[{Mode -> "Derivs", TensorType -> "Vect", DerivsOrder -> 1, DerivsAccuracy -> 4}, DerivVarlist];
  pr[];
  PrintEquations[{Mode -> "Temp"}, TempVarlist];
);

Import[FileNameJoin[{Environment["GENERATO"], "codes/CarpetXGPU.wl"}]];
