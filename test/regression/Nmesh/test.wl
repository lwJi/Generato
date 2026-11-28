(* ::Package:: *)

(* test.wl *)

(* (c) Liwei Ji, 01/2024 *)

Needs["xAct`xCoba`", FileNameJoin[{Environment["GENERATO"], "src/Generato.wl"}]]

SetPVerbose[False];

SetPrintDate[False];

SetPrintHeaderMacro[False];

SetGridPointIndex["[[ijk]]"];

DefManifold[M3, 3, IndexRange[a, z]];

DefChart[cart, M3, {1, 2, 3}, {X[], Y[], Z[]}, ChartColor -> Blue];

(*DefChart[spnr, M3, {1, 2, 3}, {r[], th[], ph[]}, ChartColor -> Green];*)

DefMetric[1, euclid[-i, -j], CD];

MetricInBasis[euclid, -cart, DiagonalMatrix[{1, 1, 1}]];

MetricInBasis[euclid, cart, DiagonalMatrix[{1, 1, 1}]];

EvolVarlist = GridTensors[{uU[i], PrintAs -> "u"}];

(* The right-hand side names the variable it is the time derivative of, which is declared above it *)
dtEvolVarlist = GridTensors[{rU[i], TimeDerivativeOf -> uU, PrintAs -> "r"}];

MoreInVarlist = GridTensors[{MDD[-i, -j], Symmetric[{-i, -j}], PrintAs -> "M"}];

TempVarlist = TempTensors[{vU[i], PrintAs -> "v"}];

(*
    r^i = M^i_j M^j_k u^k     if ADM_ConstraintNorm = Msqr
    r^i = M^i_j u^j           otherwise
*)

SetEQN[vU[i_], euclid[i, k] MDD[-k, -j] uU[j]];

SetEQN[{SuffixName -> "Msqr"}, rU[i_], euclid[i, k] MDD[-k, -j] vU[j]];

SetEQN[{SuffixName -> "otherwise"}, rU[i_], vU[i]];

SetProject["C3GH"];

With[{project = GetProject[]},
  Generate["Nmesh", "test.c", {
    "#include \"nmesh.h\"",
    "#include \"" <> project <> ".h\"",
    "",
    "/* use globals from " <> project <> " */",
    "extern t" <> project <> " " <> project <> "[1];",
    "",
    "",
    "void test(tVarList *vlu, tVarList *vlr)",
    "{",
    "tMesh *mesh = u->mesh;",
    "",
    "int Msqr = GetvLax(Par(\"ADM_ConstraintNorm\"), \"Msqr \");",
    "",
    "formylnodes(mesh)",
    "{",
    "tNode *node = MyLnode;",
    "int ijk;",
    "",
    "forpoints(node, ijk)",
    "{",
    "int iMDD = Ind(\"ADM_gxx\");",
    "",
    Load[dtEvolVarlist, "Out"],
    Load[EvolVarlist, "In"],
    Load[MoreInVarlist, "MoreInOut"],
    "",
    Store[TempVarlist, "Temp"],
    "",
    "if(Msqr)",
    "{",
    Store[dtEvolVarlist, "Out", SuffixName -> "Msqr", ChartName -> cart],
    "}",
    "else",
    "{",
    Store[dtEvolVarlist, "Out", SuffixName -> "otherwise"],
    "}",
    "",
    "} /* end of points */",
    "} /* end of nodes */",
    "}"
  }]
];
