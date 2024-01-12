(* ::Package:: *)

(* GHG_rhs.wl *)

(* (c) Liwei Ji, 01/2024 *)

Needs["xAct`xCoba`", FileNameJoin[{Environment["GENERATO"], "src/Generato.wl"
    }]]

SetPVerbose[False];

SetPrintDate[False];

SetGridPointIndex["[[ijk]]"];

DefManifold[M4, 4, Union[Complement[IndexRange[a, z], {g}], Table[ToExpression[
    "h" <> ToString[i]], {i, 1, 9}], Table[ToExpression["z" <> ToString[i
    ]], {i, 1, 9}]]];

DefChart[cart, M4, {0, 1, 2, 3}, {T[], X[], Y[], Z[]}, ChartColor -> 
    Blue];

dtEvolVarlist = SetGridTensors[
    {dtg[-a, -b], Symmetric[{-a, -b}]},
    {dtPi[-a, -b], Symmetric[{-a, -b}], PrintAs -> "dt\[CapitalPi]"},
    {dtPhi[-k, -a, -b], Symmetric[{-a, -b}], PrintAs -> "dt\[CapitalPhi]"}
];

EvolVarlist = SetGridTensors[
    {g[-a, -b], Symmetric[{-a, -b}]},
    {Pi$Upt[-a, -b], Symmetric[{-a, -b}], PrintAs -> "\[CapitalPi]"},
    {Phi[-k, -a, -b], Symmetric[{-a, -b}], PrintAs -> "\[CapitalPhi]"},
    {H[-a]}
];

MoreInVarlist = SetGridTensors[
    {Adg[-a, -b], Symmetric[{-a, -b}]},
    {AdPi[-a, -b], Symmetric[{-a, -b}], PrintAs -> "Ad\[CapitalPi]"},
    {AdPhi[-k, -a, -b], Symmetric[{-a, -b}], PrintAs -> "Ad\[CapitalPhi]"},
    {alpha[], PrintAs -> "\[Alpha]"},
    {beta[i], PrintAs -> "\[Beta]"},
    {srcSdH[-a, -b], Symmetric[{-a, -b}], PrintAs -> "\!\(\*SubscriptBox[\(\[PartialD]\), \((a\)]\)\!\(\*SubscriptBox[\(H\), \(\(b\)\()\)\)]\)"}
];

TempVarlist = SetTempTensors[
    {detinvh[], PrintAs -> "\[Gamma]"},
    {invh[i, j], Symmetric[{i, j}], PrintAs -> "\[Gamma]"},
    {nvec[a], PrintAs -> "n"},
    {ndua[-a], PrintAs -> "n"},
    {invg[a, b], Symmetric[{a, b}], PrintAs -> "g"},
    {dginFO[-c, -a, -b], Symmetric[{-a, -b}], PrintAs -> "dg"},
    {Gam[-c, -a, -b], Symmetric[{-a, -b}], PrintAs -> "\[CapitalGamma]"},
    {trGam[-a], PrintAs -> "\[CapitalGamma]"}
];

DefConstantSymbol[gamma0, PrintAs -> "\!\(\*SubscriptBox[\(\[Gamma]\), \(0\)]\)"];

DefConstantSymbol[gamma1, PrintAs -> "\!\(\*SubscriptBox[\(\[Gamma]\), \(1\)]\)"];

DefConstantSymbol[gamma2, PrintAs -> "\!\(\*SubscriptBox[\(\[Gamma]\), \(2\)]\)"];

DefConstantSymbol[interior, PrintAs -> "I"];

Module[{Mat, invMat},
    Mat = Table[g[{aa, -cart}, {bb, -cart}] // ToValues, {aa, 1, 3}, 
        {bb, 1, 3}];
    invMat = Inverse[Mat] /. {1 / Det[Mat] -> (detinvh[] // ToValues)
        };
    SetEQN[{CheckRHS -> False}, detinvh[], 1 / Det[Mat] // Simplify];
        
    SetEQNDelayed[
        invh[i_, j_]
        ,
        If[IndexType[i, UpIndexQ] && IndexType[j, UpIndexQ],
            If[i[[1]] > 0 && j[[1]] > 0,
                invMat[[i[[1]], j[[1]]]] // Simplify
            ]
            ,
            invh[i, j]
        ]
    ]
];

SetEQNDelayed[
    nvec[a_]
    ,
    If[IndexType[a, UpIndexQ],
        If[a[[1]] == 0,
            alpha[] ^ -1
            ,
            -alpha[] ^ -1 beta[a]
        ]
        ,
        nvec[a]
    ]
];

SetEQNDelayed[
    ndua[a_]
    ,
    If[IndexType[a, DownIndexQ],
        If[a[[1]] == 0,
            -alpha[]
            ,
            0.0
        ]
        ,
        nvec[a]
    ]
];

SetEQN[invg[a_, b_], invh[a, b] - nvec[a] nvec[b]];

SetEQNDelayed[
    dginFO[c_, a_, b_]
    ,
    If[IndexType[c, DownIndexQ] && IndexType[a, DownIndexQ] && IndexType[
        b, DownIndexQ],
        If[c[[1]] == 0,
            -alpha[] Pi$Upt[a, b] + beta[k] Phi[-k, a, b]
            ,
            Phi[c, a, b]
        ]
        ,
        dginFO[c, a, b]
    ]
];

SetEQN[Gam[c_, a_, b_], 1/2 (dginFO[a, b, c] + dginFO[b, c, a] - dginFO[
    c, a, b])];

SetEQN[trGam[c_], invg[a, b] Gam[c, -a, -b]];

SetEQN[dtg[a_, b_], -Adg[a, b] interior - alpha[] Pi$Upt[a, b] - gamma1
     beta[c] Phi[-c, a, b]];

SetEQN[dtPi[a_, b_], -AdPi[a, b] interior + 2 alpha[] invg[c, d] (invh[
    i, j] Phi[-i, -c, a] Phi[-j, -d, b] - Pi$Upt[-c, a] Pi$Upt[-d, b] - invg[
    e, f] Gam[a, -c, -e] Gam[b, -d, -f]) - 1/2 alpha[] nvec[c] nvec[d] Pi$Upt[
    -c, -d] Pi$Upt[a, b] - alpha[] nvec[c] Pi$Upt[-c, -i] invh[i, j] Phi[
    -j, a, b] + 2 alpha[] (invg[c, d] Gam[-c, a, b] H[-d]) + gamma0 alpha[
    ] ((H[a] + trGam[a]) ndua[b] + (H[b] + trGam[b]) ndua[a] - g[a, b] nvec[
    c] (H[-c] + trGam[-c])) - gamma1 gamma2 beta[i] Phi[-i, a, b] - srcSdH[
    a, b]];

SetEQN[dtPhi[i_, a_, b_], -AdPhi[i, a, b] interior + 1/2 alpha[] nvec[
    c] nvec[d] Phi[i, -c, -d] Pi$Upt[a, b] + alpha[] invh[j, k] nvec[c] Phi[
    i, -j, -c] Phi[-k, a, b] - gamma2 alpha[] Phi[i, a, b]];

(* Write to files *)

SetOutputFile[FileNameJoin[{Environment["GENERATO"], "test/integration/GHG_rhs.c"
    }]];

SetProject["GHG"];

$MainPrint[] :=
    Module[{project = GetProject[]},
        pr["#include \"nmesh.h\""];
        pr["#include \"" <> project <> ".h\""];
        pr[];
        pr["#define Power(x,y) (pow((double) (x),(double) (y)))"];
        pr["#define Log(x) log((double) (x))"];
        pr["#define pow2(x) ((x)*(x))"];
        pr["#define pow2inv(x) (1.0/((x)*(x)))"];
        pr["#define Cal(x,y,z) ((x)?(y):(z))"];
        pr["#define Sqrt(x) sqrt(x)"];
        pr["#define Abs(x) fabs(x)"];
        pr[];
        pr["/* use globals from " <> project <> " */"];
        pr["extern t" <> project <> " " <> project <> "[1];"];
        pr[];
        pr[];
        pr["int GHG_rhs(tNode *node, tVarList *vlr, tVarList *vlu)"];
            
        pr["{"];
        pr["tMesh *mesh = node->pat->mesh;"];
        pr["int ialpha = Ind(\"ADM_alpha\");"];
        pr["int ibetax = Ind(\"ADM_betax\");"];
        pr["int iAdgtt = Ind(\"GHG_Adgtt\");"];
        pr["int iAdPitt = Ind(\"GHG_AdPitt\");"];
        pr["int iAdPhixtt = Ind(\"GHG_AdPhixtt\");"];
        pr["int isrcSdHtt = Ind(\"GHG_srcSdHtt\");"];
        pr["int fc[6];"];
        pr["int ijk;"];
        pr[];
        PrintInitializations[{Mode -> "MainOut"}, dtEvolVarlist];
        PrintInitializations[{Mode -> "MainIn"}, EvolVarlist];
        PrintInitializations[{Mode -> "MoreInOut"}, MoreInVarlist];
        pr[];
        pr["TIMER_START;"];
        pr[];
        pr["/* compute */"];
        pr["forpoints(node, ijk)"];
        pr["{"];
        (* parameters *)
        pr["double interior = !(ind_on_nodeface(node, ijk, fc));"];
        pr["double gammas[3], gamma0, gamma1, gamma2;"];
        pr["GHG_gammas(node, ijk, gammas);"];
        pr["gamma0 = gammas[0];"];
        pr["gamma1 = gammas[1];"];
        pr["gamma2 = gammas[2];"];
        pr[];
        PrintEquations[{Mode -> "Temp"}, TempVarlist];
        pr[];
        PrintEquations[{Mode -> "Main"}, dtEvolVarlist];
        pr["} /* end of points */"];
        pr[];
        pr["TIMER_STOP;"];
        pr[];
        pr["return 0;"];
        pr["} /* end of function */"];
    ];

Import[FileNameJoin[{Environment["GENERATO"], "codes/Nmesh.wl"}]];

(*<< ../xActToC/Codes/Nmesh.wl*)
