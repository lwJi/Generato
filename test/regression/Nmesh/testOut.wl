(* ::Package:: *)

(* testOut.wl *)
(* Integration test for the "MainOut" initialization: a right-hand side is read from vlr *)
(* at the index of the variable it is the time derivative of. Reduced from GHG_rhs.wl *)

(* (c) Liwei Ji, 11/2026 *)

Needs["xAct`xCoba`", FileNameJoin[{Environment["GENERATO"], "src/Generato.wl"}]]

SetPVerbose[False];

SetPrintDate[False];

SetPrintHeaderMacro[False];

SetGridPointIndex["[[ijk]]"];

DefManifold[M4, 4, Complement[IndexRange[a, z], {g}]];

DefChart[cart, M4, {0, 1, 2, 3}, {T[], X[], Y[], Z[]}, ChartColor -> Blue];

dtEvolVarlist =
  GridTensors[
    {dtg[-a, -b], Symmetric[{-a, -b}]},
    {dtPi[-a, -b], Symmetric[{-a, -b}], PrintAs -> "dt\[CapitalPi]"},
    {dtPhi[-k, -a, -b], Symmetric[{-a, -b}], PrintAs -> "dt\[CapitalPhi]"}
  ];

EvolVarlist =
  GridTensors[
    {g[-a, -b], Symmetric[{-a, -b}]},
    {Pi$Upt[-a, -b], Symmetric[{-a, -b}], PrintAs -> "\[CapitalPi]"},
    {Phi[-k, -a, -b], Symmetric[{-a, -b}], PrintAs -> "\[CapitalPhi]"}
  ];

MoreInVarlist = GridTensors[{alpha[], PrintAs -> "\[Alpha]"}, {beta[i], PrintAs -> "\[Beta]"}];

DefConstantSymbol[gamma0, PrintAs -> "\!\(\*SubscriptBox[\(\[Gamma]\), \(0\)]\)"];

DefConstantSymbol[gamma1, PrintAs -> "\!\(\*SubscriptBox[\(\[Gamma]\), \(1\)]\)"];

DefConstantSymbol[gamma2, PrintAs -> "\!\(\*SubscriptBox[\(\[Gamma]\), \(2\)]\)"];

(* The terms of the GHG right-hand sides that need no temporary *)
SetEQN[dtg[a_, b_], -alpha[] Pi$Upt[a, b] - gamma1 beta[c] Phi[-c, a, b]];

SetEQN[dtPi[a_, b_], -gamma0 alpha[] Pi$Upt[a, b] - gamma1 gamma2 beta[c] Phi[-c, a, b]];

SetEQN[dtPhi[i_, a_, b_], -gamma2 alpha[] Phi[i, a, b]];

(* Write to files *)

SetOutputFile[FileNameJoin[{Directory[], "testOut.c"}]];

SetProject["GHG"];

SetMainPrint[
  Module[{project = GetProject[]},
    pr["#include \"nmesh.h\""];
    pr["#include \"" <> project <> ".h\""];
    pr[];
    pr["/* use globals from " <> project <> " */"];
    pr["extern t" <> project <> " " <> project <> "[1];"];
    pr[];
    pr[];
    pr["int testOut(tNode *node, tVarList *vlr, tVarList *vlu)"];
    pr["{"];
    pr["int ialpha = Ind(\"ADM_alpha\");"];
    pr["int ibetax = Ind(\"ADM_betax\");"];
    pr["int ijk;"];
    pr[];
    PrintInitializations[{Mode -> "MainOut"}, dtEvolVarlist];
    PrintInitializations[{Mode -> "MainIn"}, EvolVarlist];
    PrintInitializations[{Mode -> "MoreInOut"}, MoreInVarlist];
    pr[];
    pr["forpoints(node, ijk)"];
    pr["{"];
    pr["double gammas[3], gamma0, gamma1, gamma2;"];
    pr["GHG_gammas(node, ijk, gammas);"];
    pr["gamma0 = gammas[0];"];
    pr["gamma1 = gammas[1];"];
    pr["gamma2 = gammas[2];"];
    pr[];
    PrintEquations[{Mode -> "MainOut"}, dtEvolVarlist];
    pr["} /* end of points */"];
    pr[];
    pr["return 0;"];
    pr["} /* end of function */"];
  ]
];

Import[FileNameJoin[{Environment["GENERATO"], "codes/Nmesh.wl"}]];
