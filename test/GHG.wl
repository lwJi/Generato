(* ::Package:: *)

(* GHG_rhs_set_gravity.wl *)

(* (c) Liwei Ji, 01/2024 *)

Needs["xAct`xCoba`", FileNameJoin[{Environment["GENERATO"], "src/Generato.wl"
    }]]

SetPVerbose[False];

SetPrintDate[False];

SetGridPointIndex["[[ijk]]"];

DefManifold[M4, 4, Union[Complement[IndexRange[a,z], {g}],
                           Table[ToExpression["h"<>ToString[i]], {i,1,9}],
                           Table[ToExpression["z"<>ToString[i]], {i,1,9}]]];

DefChart[cart, M4, {0, 1, 2, 3}, {T[], X[], Y[], Z[]}, ChartColor -> Blue];

dtEvolVarlist = SetGridTensors[
  {dtg[-a,-b], Symmetric[{-a,-b}]},
  {dtPi[-a,-b], Symmetric[{-a,-b}], PrintAs->"dt\[CapitalPi]"},
  {dtPhi[-k,-a,-b], Symmetric[{-a,-b}], PrintAs->"dt\[CapitalPhi]"}
];

EvolVarlist = SetGridTensors[
  {g[-a,-b], Symmetric[{-a,-b}]},
  {Pi$Upt[-a,-b], Symmetric[{-a,-b}], PrintAs->"\[CapitalPi]"},
  {Phi[-k,-a,-b], Symmetric[{-a,-b}], PrintAs->"\[CapitalPhi]"},
  {H[-a]}
];

MoreInVarlist = SetGridTensors[
  {Adg[-a,-b], Symmetric[{-a,-b}]},
  {AdPi[-a,-b], Symmetric[{-a,-b}], PrintAs->"Ad\[CapitalPi]"},
  {AdPhi[-k,-a,-b], Symmetric[{-a,-b}], PrintAs->"Ad\[CapitalPhi]"},
  {alpha[]},
  {beta[i]},
  {srcSdH[-a,-b],Symmetric[{-a,-b}]}
];

TempVarlist = SetTempTensors[
  {detinvh[]},
  {invh[i,j],Symmetric[{i,j}]},
  {nvec[a]},
  {ndua[-a]},
  {invg[a,b],Symmetric[{a,b}]},
  {dginFO[-c,-a,-b],Symmetric[{-a,-b}]},
  {Gam[-c,-a,-b],Symmetric[{-a,-b}]},
  {trGam[-a]}
];

DefConstantSymbol[gamma0];
DefConstantSymbol[gamma1];
DefConstantSymbol[gamma2];
DefConstantSymbol[interior];

Module[{Mat, invMat}
  Mat=Table[(g[{aa,-cart},{bb,-cart}]//ToValues), {aa,1,3}, {bb,1,3}];
  invMat=Inverse[Mat]/.{1/Det[Mat]->detinvh[]//ToValues}
  SetRHS[detinvh[], 1/Det[Mat]//Simplify];
  SetRHSDelayed[invh[i_,j_],
    If[IndexType[i,UpIndexQ]&&IndexType[j,UpIndexQ],
      If[i[[1]]>0&&j[[1]]>0, invMat[[i[[1]],j[[1]]]]//Simplify],
      invh[a,b]
    ]
  ]
];

SetEQNDelayed[nvec[a_],
  If[IndexType[a,UpIndexQ],
    If[a[[1]]==0,alpha[]^-1,-alpha[]^-1 beta[a]],
    nvec[a]
  ]
];
SetEQNDelayed[ndua[a_],
  If[IndexType[a,DownIndexQ],
    If[a[[1]]==0,-alpha[],0.0],
    nvec[a]
  ]
];

SetEQN[invg[a_,b_],invh[a,b]-nvec[a]nvec[b]];

SetEQNDelayed[dginFO[c_,a_,b_],
  If[IndexType[c,DownIndexQ]&&IndexType[a,DownIndexQ]&&
     IndexType[b,DownIndexQ],
    If[c[[1]]==0,
      -alpha[]Pi$Upt[a,b]+beta[k]Phi[-k,a,b],
      Phi[c,a,b]],
    dginFO[c,a,b]
  ]
];

SetEQN[Gam[c_,a_,b_],1/2 (dginFO[a,b,c]+dginFO[b,c,a]-dginFO[c,a,b])];
SetEQN[trGam[c_],invg[a,b]Gam[c,-a,-b]];

SetEQN[dtg[a_,b_],
  -Adg[a,b] interior
  -alpha[]Pi$Upt[a,b]-gamma1 beta[c]Phi[-c,a,b]];

SetEQN[dtPi[a_,b_],
  -AdPi[a,b]interior
  +2 alpha[]invg[c,d]
  (invh[i,j]Phi[-i,-c,a]Phi[-j,-d,b]-Pi$Upt[-c,a]Pi$Upt[-d,b]
    -invg[e,f]Gam[a,-c,-e]Gam[b,-d,-f])
  -1/2 alpha[]nvec[c]nvec[d]Pi$Upt[-c,-d]Pi$Upt[a,b]
  -alpha[]nvec[c]Pi$Upt[-c,-i]invh[i,j]Phi[-j,a,b]
  +2 alpha[](invg[c,d]Gam[-c,a,b]H[-d])
  +gamma0 alpha[]( (H[a]+trGam[a])ndua[b]+(H[b]+trGam[b])ndua[a]
                   -g[a,b]nvec[c](H[-c]+trGam[-c]) )
  -gamma1 gamma2 beta[i]Phi[-i,a,b]
  -srcSdH[a,b]];

SetEQN[dtPhi[i_,a_,b_],
  -AdPhi[i,a,b]interior
  +1/2 alpha[]nvec[c]nvec[d]Phi[i,-c,-d]Pi$Upt[a,b]
  +alpha[]invh[j,k]nvec[c]Phi[i,-j,-c]Phi[-k,a,b]
  -gamma2 alpha[]Phi[i,a,b]];


(* Write to files *)
SetOutputFile[FileNameJoin[{Environment["GENERATO"], "test/GHG.c"}]];

SetProject["GHG"];

$MainPart[] := Module[{},
  pr["#include \"nmesh.h\""];
  pr["#include \""<>GetprojectName[]<>".h\""];
  pr[];
  pr["#define Power(x,y) (pow((double) (x),(double) (y)))"];
  pr["#define Log(x) log((double) (x))"];
  pr["#define pow2(x) ((x)*(x))"];
  pr["#define pow2inv(x) (1.0/((x)*(x)))"];
  pr["#define Cal(x,y,z) ((x)?(y):(z))"];
  pr["#define Sqrt(x) sqrt(x)"];
  pr["#define Abs(x) fabs(x)"];
  pr[];
  pr["/* use globals from "<>GetprojectName[]<>" */"];
  pr["extern t"<>GetprojectName[]<>" "<>GetprojectName[]<>"[1];"];
  pr[];
  pr[];
  pr["int "<>$functionName<>"(tNode *node, tVarList *vlr, tVarList *vlu)"];
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

  PrintInitializations[{Mode->"MainOut"}, dtEvolVarlist];
  PrintInitializations[{Mode->"MainIn"}, EvolVarlist];
  PrintInitializations[{Mode->"MoreInOut"}, MoreInVarlist];

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

  PrintEquations[{Mode->"Temp"}, TempVarlist];
  pr[];
  PrintEquations[{Mode->"Main"}, dtEvolVarlist];

  pr["} /* end of points */"];
  pr[];
  pr["TIMER_STOP;"];
  pr[];

  pr["return 0;"];
  pr["} /* end of function */"];
  pr[];
  pr["/* "<>$functionName<>".c */"];
];

Import[FileNameJoin[{Environment["GENERATO"], "codes/Nmesh.wl"}]];
(*<< ../xActToC/Codes/Nmesh.wl*)
