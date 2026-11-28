(* ::Package:: *)

(* testOut.wl *)
(* Integration test for the "Out" load: a right-hand side is read from vlr *)
(* at the index of the variable it is the time derivative of. Reduced from GHG_rhs.wl *)

(* (c) Liwei Ji, 11/2026 *)

Needs["xAct`xCoba`", FileNameJoin[{Environment["GENERATO"], "src/Generato.wl"}]]

SetPVerbose[False];

SetPrintDate[False];

SetPrintHeaderMacro[False];

SetGridPointIndex["[[ijk]]"];

DefManifold[M4, 4, Complement[IndexRange[a, z], {g}]];

DefChart[cart, M4, {0, 1, 2, 3}, {T[], X[], Y[], Z[]}, ChartColor -> Blue];

EvolVarlist =
  GridTensors[
    {g[-a, -b], Symmetric[{-a, -b}]},
    {Pi$Upt[-a, -b], Symmetric[{-a, -b}], PrintAs -> "\[CapitalPi]"},
    {Phi[-k, -a, -b], Symmetric[{-a, -b}], PrintAs -> "\[CapitalPhi]"}
  ];

(* Each right-hand side names the variable it is the time derivative of, which is declared above it *)
dtEvolVarlist =
  GridTensors[
    {dtg[-a, -b], Symmetric[{-a, -b}], TimeDerivativeOf -> g},
    {dtPi[-a, -b], Symmetric[{-a, -b}], TimeDerivativeOf -> Pi$Upt, PrintAs -> "dt\[CapitalPi]"},
    {dtPhi[-k, -a, -b], Symmetric[{-a, -b}], TimeDerivativeOf -> Phi, PrintAs -> "dt\[CapitalPhi]"}
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

SetProject["GHG"];

With[{project = GetProject[]},
  Generate["Nmesh", "testOut.c", {
    "#include \"nmesh.h\"",
    "#include \"" <> project <> ".h\"",
    "",
    "/* use globals from " <> project <> " */",
    "extern t" <> project <> " " <> project <> "[1];",
    "",
    "",
    "int testOut(tNode *node, tVarList *vlr, tVarList *vlu)",
    "{",
    "int ialpha = Ind(\"ADM_alpha\");",
    "int ibetax = Ind(\"ADM_betax\");",
    "int ijk;",
    "",
    Load[dtEvolVarlist, "Out"],
    Load[EvolVarlist, "In"],
    Load[MoreInVarlist, "MoreInOut"],
    "",
    "forpoints(node, ijk)",
    "{",
    "double gammas[3], gamma0, gamma1, gamma2;",
    "GHG_gammas(node, ijk, gammas);",
    "gamma0 = gammas[0];",
    "gamma1 = gammas[1];",
    "gamma2 = gammas[2];",
    "",
    Store[dtEvolVarlist, "Out"],
    "} /* end of points */",
    "",
    "return 0;",
    "} /* end of function */"
  }]
];
