(* ::Package:: *)

(* testDerivs.wl *)
(* Integration test for the "Derivs" load: a derivative computed from the variable it declares *)

(* (c) Liwei Ji, 01/2026 *)

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

(* Variables for derivative test *)
EvolVarlist = GridTensors[{phi[], PrintAs -> "phi"}];

(* A derivative computed inline is a local, so it is bound as a temporary *)
DerivVarlist = TempTensors[{dphi[-i], DerivativeOf -> phi, PrintAs -> "dphi"}];

TempVarlist = TempTensors[{psi[], PrintAs -> "psi"}];

(* Simple equation using the derivative *)
SetEQN[psi[], euclid[i, j] dphi[-i] dphi[-j]];

Generate["CarpetXGPU", "testDerivs.hxx", {
  "",
  Load[EvolVarlist, "In"],
  "",
  (* dphi is computed from phi, which its declaration names *)
  Load[DerivVarlist, "Derivs"],
  "",
  Store[TempVarlist, "Temp"]
}];
