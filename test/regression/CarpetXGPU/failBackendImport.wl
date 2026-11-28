(* ::Package:: *)

(* failBackendImport.wl *)
(* A run that must fail at the Import of a backend file: a script that hands no program to Generate *)

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

SetEQN[rU[i_], 2 uU[i]];

(*
    The script is ported halfway: its held main content is gone, and it still
    ends as a script once did, with the Import of the backend file, which
    used to start the write. The file is one DefineBackend call and starts
    nothing, so without a stop here the script would run to its end, write
    nothing and report success. The message names the backend to hand to
    Generate.
*)

Import[FileNameJoin[{Environment["GENERATO"], "codes/CarpetXGPU.wl"}]];
