(* ::Package:: *)

(* test.wl *)

(* (c) Liwei Ji, 01/2024 *)

Needs["xAct`xCoba`", FileNameJoin[{Environment["GENERATO"], "src/Generato.wl"
    }]]

DefManifold[M3, 3, IndexRange[a, z]];

DefChart[cart, M3, {1, 2, 3}, {X[], Y[], Z[]}, ChartColor -> Blue];

(*DefChart[spnr, M3, {1, 2, 3}, {r[], th[], ph[]}, ChartColor -> Green];*)

DefMetric[1, euclid[-i, -j], CD];

MetricInBasis[euclid, -cart, DiagonalMatrix[{1, 1, 1}]];

MetricInBasis[euclid, cart, DiagonalMatrix[{1, 1, 1}]];

DefTensor[rU[i], M3, PrintAs->"r"];
DefTensor[mUD[i, -j], M3, PrintAs->"M"];
DefTensor[uU[i], M3, PrintAs->"u"];


(*
    r^i = M^i_j M^j_k u^k     if ADM_ConstraintNorm = Msqr
    r^i = M^i_j u^j           otherwise
*)

setEQN[rU[i_], "Msqr",
    mUD[i,-j]mUD[j,-k]uU[k]];

setEQN[rU[i_], "otherwise",
    mUD[i,-j]uU[j]];

Print[]

Print[]

Print[RHSOf[rUMsqr][k]]

Print[RHSOf[rUotherwise][k]]

Print[]

Print["More Infos:"]

Print["  charts:     ", ChartsOfManifold[M3]]

Print["  dimensions: ", DimOfManifold[M3]]
