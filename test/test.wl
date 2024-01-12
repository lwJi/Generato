(* ::Package:: *)

(* test.wl *)

(* (c) Liwei Ji, 01/2024 *)

Needs["xAct`xCoba`", FileNameJoin[{Environment["GENERATO"], "src/Generato.wl"
    }]]

SetPVerbose[False];

SetPrintDate[False];

SetGridPointIndex["[[ijk]]"]

DefManifold[M3, 3, IndexRange[a, z]];

DefChart[cart, M3, {1, 2, 3}, {X[], Y[], Z[]}, ChartColor -> Blue];

(*DefChart[spnr, M3, {1, 2, 3}, {r[], th[], ph[]}, ChartColor -> Green];*)

DefMetric[1, euclid[-i, -j], CD];

MetricInBasis[euclid, -cart, DiagonalMatrix[{1, 1, 1}]];

MetricInBasis[euclid, cart, DiagonalMatrix[{1, 1, 1}]];

dtEvolVarlist = SetGridTensors[{rU[i]}];

EvolVarlist = SetGridTensors[{uU[i]}];

MoreInVarlist = SetGridTensors[{MDD[-i, -j], Symmetric[{-i, -j}]}];

TempVarlist = SetTempTensors[{vU[i]}];

(*
    r^i = M^i_j M^j_k u^k     if ADM_ConstraintNorm = Msqr
    r^i = M^i_j u^j           otherwise
*)

SetEQN[vU[i_], euclid[i, k] MDD[-k, -j] uU[j]];

SetEQN[{SuffixName -> "Msqr"}, rU[i_], euclid[i, k] MDD[-k, -j] vU[j]
    ];

SetEQN[{SuffixName -> "otherwise"}, rU[i_], vU[i]];

SetOutputFile[FileNameJoin[{Environment["GENERATO"], "test/test.c"}]];

SetProject["C3GH"];

$MainPrint[] :=
    Module[{project = GetProject[]},
        pr["#include \"nmesh.h\""];
        pr["#include \"" <> project <> ".h\""];
        pr[];
        pr["/* use globals from " <> project <> " */"];
        pr["extern t" <> project <> " " <> project <> "[1];"];
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

Import[FileNameJoin[{Environment["GENERATO"], "codes/Nmesh.wl"}]];
