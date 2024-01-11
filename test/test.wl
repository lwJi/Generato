(* ::Package:: *)

(* test.wl *)

(* (c) Liwei Ji, 01/2024 *)

Needs["xAct`xCoba`", FileNameJoin[{Environment["GENERATO"], "src/Generato.wl"
    }]]

SetGridPointIndex["[[ijk]]"]

SetPVerbose[True];

DefManifold[M3, 3, IndexRange[a, z]];

DefChart[cart, M3, {1, 2, 3}, {X[], Y[], Z[]}, ChartColor -> Blue];

(*DefChart[spnr, M3, {1, 2, 3}, {r[], th[], ph[]}, ChartColor -> Green];*)

DefMetric[1, euclid[-i, -j], CD];

MetricInBasis[euclid, -cart, DiagonalMatrix[{1, 1, 1}]];

MetricInBasis[euclid, cart, DiagonalMatrix[{1, 1, 1}]];

dtEvolVarlist = {{rU[i]}};

EvolVarlist = {{uU[i]}};

MoreInVarlist = {{MDD[-i, -j], Symmetric[{-i, -j}]}};

TempVarlist = {{vU[i]}}

SetComponents[dtEvolVarlist];

SetComponents[EvolVarlist];

SetComponents[MoreInVarlist];

SetComponents[{WithoutGridPointIndex -> True}, TempVarlist];

(*
    r^i = M^i_j M^j_k u^k     if ADM_ConstraintNorm = Msqr
    r^i = M^i_j u^j           otherwise
*)

SetEQN[vU[i_], euclid[i, k] MDD[-k, -j] uU[j]];

SetEQN[{SuffixName -> "Msqr"}, rU[i_], euclid[i, k] MDD[-k, -j] vU[j]
    ];

SetEQN[{SuffixName -> "otherwise"}, rU[i_], "otherwise", vU[j]];

SetOutFile["test.c"];

$MainPrint[] :=
    Module[{projectname = "C3GH"},
        pr["#include \"nmesh.h\""];
        pr["#include \"" <> projectname <> ".h\""];
        pr[];
        pr["/* use globals from " <> projectname <> " */"];
        pr["extern t" <> projectname <> " " <> projectname <> "[1];"]
            ;
        pr[];
        pr[];
        pr["void test(tVarList *vlu, tVarList *vlr)"];
        pr["{"];
        pr["tMesh *mesh = u->mesh;"];
        pr[];
        pr["int Msqr = GetvLax(Par(\"ADM_ConstraintNorm\"), \"Msqr \");"
            ];
        pr[];
        pr["formylnodes(mesh)"];
        pr["{"];
        pr["tNode *node = MyLnode;"];
        pr["int ijk;"];
        pr[];
        pr["forpoints(node, ijk)"];
        pr["{"];
        pr["int iMDD = Ind(\"ADM_gxx\");"];
        pr[];
        PrintInitializations[{Mode -> "MainOut"}, dtEvolVarlist];
        PrintInitializations[{Mode -> "MainIn"}, EvolVarlist];
        PrintInitializations[{Mode -> "MoreInOut"}, MoreInVarlist];
        pr[];
        PrintEquations[{Mode -> "Temp"}, TempVarlist];
        pr[];
        pr["if(Msqr)"];
        pr["{"];
        PrintEquations[{Mode -> "Main", SuffixName -> "Msqr", ChartName
             -> cart}, dtEvolVarlist];
        pr["}"];
        pr["else"];
        pr["{"];
        PrintEquations[{Mode -> "Main", SuffixName -> "otherwise"}, dtEvolVarlist
            ];
        pr["}"];
        pr[];
        pr["} /* end of points */"];
        pr["} /* end of nodes */"];
        pr["}"];
    ];
