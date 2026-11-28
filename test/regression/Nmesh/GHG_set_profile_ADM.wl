(* ::Package:: *)

(* GHG_set_profile_ADM.wl *)

(* (c) Liwei Ji, 04/2023 *)

Needs["xAct`xCoba`", FileNameJoin[{Environment["GENERATO"], "src/Generato.wl"}]]

SetPVerbose[False];

SetPrintDate[False];

SetPrintHeaderMacro[False];

SetGridPointIndex["[[ijk]]"];

DefManifold[M4, 4, Union[Complement[IndexRange[a, z], {g}], Table[ToExpression["h" <> ToString[i]], {i, 1, 9}], Table[ToExpression["z" <> ToString[i]], {i, 1, 9}]]];

DefChart[cart, M4, {0, 1, 2, 3}, {T[], X[], Y[], Z[]}, ChartColor -> Blue];

(* ==================== *)

(* variable declaration *)

(* ==================== *)

GHGVarlist = GridTensors[{g[-a, -b], Symmetric[{-a, -b}]}, {Pi$Upt[-a, -b], Symmetric[{-a, -b}], PrintAs -> "\[CapitalPi]"}, {Phi[-k, -a, -b], Symmetric[{-a, -b}], PrintAs -> "\[CapitalPhi]"}, {H[-a]}];

MoreOutputVarlist = GridTensors[{the[-a], PrintAs -> "\[Theta]"}];

ADMVarlist = GridTensors[{alpha[], PrintAs -> "\[Alpha]"}, {beta[i], PrintAs -> "\[Beta]"}, {gamma[-i, -j], Symmetric[{-i, -j}], PrintAs -> "\[Gamma]"}, {exK[-i, -j], Symmetric[{-i, -j}], PrintAs -> "K"}];

dADMVarlist = GridTensors[{dalpha[-k], PrintAs -> "d\[Alpha]"}, {dbeta[i, -k], PrintAs -> "d\[Beta]"}, {dgamma[-i, -j, -k], Symmetric[{-i, -j}], PrintAs -> "d\[Gamma]"}];

EinVarlist = GridTensors[{dtg[-a, -b], Symmetric[{-a, -b}]}];

dHVarlist = GridTensors[{dH[-k, -a]}];

TempVarlist = TempTensors[{detgamma[], PrintAs -> "\[Gamma]"}, {invgamma[i, j], Symmetric[{i, j}], PrintAs -> "\[Gamma]"}, {trK[], PrintAs -> "K"}, {nvec[c], PrintAs -> "n"}];

GaugeVarlist = TempTensors[{dtalpha[], PrintAs -> "dt\[Alpha]"}, {dtbeta[i], PrintAs -> "dt\[Beta]"}];

GaugePunctureVarlist = TempTensors[{tr3Gam[i], PrintAs -> "\[CapitalGamma]3"}];

GaugeGHVarlist = TempTensors[{HofGH[-a], PrintAs -> "H"}];

GaugeDWVarlist = TempTensors[{betaD[-a], PrintAs -> "\[Beta]"}, {logSgOa[], PrintAs -> "log(Sqrt[\[Gamma]]/\[Alpha])"}, {muL[]}, {muS[]}];

dgVarlist = TempTensors[{dmetricg[-c, -a, -b], Symmetric[{-a, -b}], PrintAs -> "dg"}];

MoreTempVarlist = TempTensors[{metricgUU[a, b], Symmetric[{a, b}], PrintAs -> "g"}, {GamDDD[-c, -a, -b], Symmetric[{-a, -b}], PrintAs -> "\[CapitalGamma]"}, {trGam[-c], PrintAs -> "\[CapitalGamma]"}];

DefConstantSymbol[parnu, PrintAs -> "\[Nu]"];

DefConstantSymbol[pareta, PrintAs -> "\[\Eta]"]

DefConstantSymbol[parmu0, PrintAs -> "\[Mu]"];

(* =================== *)

(* equation definition *)

(* =================== *)

Module[{mat, invmat},
  mat = Table[gamma[{aa, -cart}, {bb, -cart}] // ToValues, {aa, 1, 3}, {bb, 1, 3}];
  invmat = Inverse[mat] /. {Det[mat] -> (detgamma[] // ToValues)};
  SetEQNDelayed[detgamma[], Det[mat] // Simplify];
  SetEQNDelayed[invgamma[i_, j_], invmat[[i[[1]], j[[1]]]] // Simplify]
];

SetEQN[trK[], invgamma[i, j] exK[-i, -j]];

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

(* Gauge freeze *)

SetEQN[{SuffixName -> "Freeze"}, dtalpha[], 0];

SetEQN[{SuffixName -> "Freeze"}, dtbeta[i_], 0];

(* Gauge puncture *)

SetEQN[tr3Gam[i_], detgamma[] ^ (1/3) (invgamma[i, k] invgamma[j, l] - 1/3 invgamma[i, j] invgamma[k, l]) dgamma[-k, -l, -j]];

SetEQN[{SuffixName -> "Puncture"}, dtalpha[], beta[k] dalpha[-k] - 2 alpha[] trK[]];

SetEQN[{SuffixName -> "Puncture"}, dtbeta[i_], beta[k] dbeta[i, -k] + parnu tr3Gam[i] - pareta beta[i]];

(* Gauge GH *)

SetEQN[{SuffixName -> "GH"}, dtalpha[], beta[k] dalpha[-k] - alpha[] (HofGH[{0, -GetDefaultChart[]}] - beta[k] HofGH[-k] + alpha[] trK[])];

SetEQN[{SuffixName -> "GH"}, dtbeta[i_], beta[k] dbeta[i, -k] + alpha[] invgamma[i, j] (alpha[] (HofGH[-j] + invgamma[k, l] (dgamma[-l, -j, -k] + dgamma[-j, -k, -l] - dgamma[-k, -l, -j]) / 2) - dalpha[-j])];

(* Gauge GH DW *)

DefTensor[nvecD[-a], GetDefaultManifold[]];

Do[
  ComponentValue[
    nvecD[{aa, -GetDefaultChart[]}]
    ,
    If[aa == 0,
      -alpha[] // ToValues
      ,
      0
    ]
  ]
  ,
  {aa, 0, 3}
];

SetEQNDelayed[
  betaD[a_]
  ,
  If[IndexType[a, DownIndexQ],
    If[a[[1]] == 0,
      gamma[-k, -l] beta[k] beta[l]
      ,
      gamma[a, -l] beta[l]
    ]
    ,
    betaD[a]
  ]
];

SetEQN[logSgOa[], Log[Sqrt[detgamma[]] / alpha[]]];

SetEQN[muL[], parmu0 logSgOa[] ^ 2];

SetEQN[muS[], muL[]];

SetEQN[{SuffixName -> "DW"}, HofGH[a_], muL[] logSgOa[] nvecD[a] - muS[] betaD[a] / alpha[]];

(* Gauge GH fromEin *)

SetEQNDelayed[
  {SuffixName -> "Ein"}
  ,
  dmetricg[c_, a_, b_]
  ,
  If[IndexType[c, DownIndexQ] && IndexType[a, DownIndexQ] && IndexType[b, DownIndexQ],
    If[c[[1]] == 0,
      If[a[[1]] == 0 && b[[1]] == 0,
        dtg[{0, -GetDefaultChart[]}, {0, -GetDefaultChart[]}]
        ,
        If[a[[1]] == 0,
          dtg[{0, -GetDefaultChart[]}, b]
          ,
          If[b[[1]] == 0,
            dtg[{0, -GetDefaultChart[]}, a]
            ,
            dtg[a, b]
          ]
        ]
      ]
      ,
      dmetricg[c, a, b]
    ]
    ,
    dmetricg[c, a, b]
  ]
];

(* dg *)

SetEQNDelayed[
  dmetricg[c_, a_, b_]
  ,
  If[IndexType[c, DownIndexQ] && IndexType[a, DownIndexQ] && IndexType[b, DownIndexQ],
    If[c[[1]] == 0,
      If[a[[1]] == 0 && b[[1]] == 0,
        -2 alpha[] dtalpha[] + 2 gamma[-k, -l] beta[k] dtbeta[l] + beta[i] beta[j] dmetricg[{0, -GetDefaultChart[]}, -i, -j]
        ,
        If[a[[1]] == 0,
          beta[l] dmetricg[{0, -GetDefaultChart[]}, b, -l] + gamma[b, -l] dtbeta[l]
          ,
          If[b[[1]] == 0,
            beta[l] dmetricg[{0, GetDefaultChart[]}, a, -l] + gamma[a, -l] dtbeta[l]
            ,
            -2 alpha[] exK[a, b] + (dmetricg[a, {0, -GetDefaultChart[]}, b] + dmetricg[b, {0, -GetDefaultChart[]}, a]) - beta[k] ((dmetricg[a, -k, b] + dmetricg[b, -k, a]) - dmetricg[-k, a, b])
          ]
        ]
      ]
      ,
      If[a[[1]] == 0 && b[[1]] == 0,
        -2 alpha[] dalpha[c] + 2 gamma[-k, -l] beta[k] dbeta[l, c] + beta[i] beta[j] dgamma[-i, -j, c]
        ,
        If[a[[1]] == 0,
          beta[l] dgamma[b, -l, c] + gamma[b, -l] dbeta[l, c]
          ,
          If[b[[1]] == 0,
            beta[l] dgamma[a, -l, c] + gamma[a, -l] dbeta[l, c]
            ,
            dgamma[a, b, c]
          ]
        ]
      ]
    ]
    ,
    dmetricg[c, a, b]
  ]
];

(* Gam *)

SetEQN[metricgUU[a_, b_], invgamma[a, b] - nvec[a] nvec[b]];

SetEQN[GamDDD[c_, a_, b_], (dmetricg[a, b, c] + dmetricg[b, c, a] - dmetricg[c, a, b]) / 2];

SetEQN[trGam[c_], metricgUU[a, b] GamDDD[c, -a, -b]];

(* EvolVarlist *)

SetEQNDelayed[
  g[a_, b_]
  ,
  If[IndexType[a, DownIndexQ] && IndexType[b, DownIndexQ],
    If[a[[1]] == 0 && b[[1]] == 0,
      -alpha[] ^ 2 + gamma[-k, -l] beta[k] beta[l]
      ,
      If[a[[1]] == 0,
        gamma[b, -k] beta[k]
        ,
        If[b[[1]] == 0,
          gamma[a, -k] beta[k]
          ,
          gamma[a, b]
        ]
      ]
    ]
    ,
    g[a, b]
  ]
];

SetEQN[Pi$Upt[a_, b_], -nvec[c] dmetricg[-c, a, b]];

SetEQN[Phi[k_, a_, b_], dmetricg[k, a, b]];

SetEQN[H[a_], -trGam[a]];

(* theta *)

SetEQN[the[a_], -beta[k] dH[-k, a]];

(* ============== *)

(* Write to files *)

(* ============== *)

SetProject["GHG"];

(* d_c g_{ab}, one component: the program below writes them in an order of its own *)

dg[cc_, aa_, bb_, opts___] := Store[dmetricg[{cc, -cart}, {aa, -cart}, {bb, -cart}], "Out", opts];

With[{project = GetProject[]},
  Generate["Nmesh", "GHG_set_profile_ADM.c", {
    "#include \"nmesh.h\"",
    "#include \"" <> project <> ".h\"",
    "",
    "#define Power(x,y) (pow((double) (x),(double) (y)))",
    "#define Log(x) log((double) (x))",
    "#define pow2(x) ((x)*(x))",
    "#define pow2inv(x) (1.0/((x)*(x)))",
    "#define Cal(x,y,z) ((x)?(y):(z))",
    "#define Sqrt(x) sqrt(x)",
    "#define Abs(x) fabs(x)",
    "",
    "/* use globals from " <> project <> " */",
    "extern t" <> project <> " " <> project <> "[1];",
    "",
    "",
    "int GHG_set_profile_ADM(tVarList *vlu)",
    "{",
    "tMesh *mesh = vlu->mesh;",
    "int ialpha = Ind(\"ADM_alpha\");",
    "int ibetax = Ind(\"ADM_betax\");",
    "int igammaxx = Ind(\"ADM_gxx\");",
    "int iexKxx = Ind(\"ADM_Kxx\");",
    "int idalphax = Ind(\"ADM_dalphax\");",
    "int idbetaxx = Ind(\"ADM_dbetaxx\");",
    "int idgammaxxx = Ind(\"ADM_dgxxx\");",
    "int idHxt = Ind(\"GHG_dHxt\");",
    "",
    "formylnodes(mesh)",
    "{",
    "tNode *node = MyLnode;",
    "int ijk;",
    "",
    "/* derivatives */",
    "cart_partials_dU_di(node,  ialpha, idalphax);",
    "cart_partials_dUi_dj(node, ibetax, idbetaxx);",
    "cart_partials_dSij_dk(node, igammaxx, idgammaxxx);",
    "",
    (* load the variables *)
    Load[GHGVarlist, "In"],
    Load[MoreOutputVarlist, "In"],
    Load[ADMVarlist, "MoreInOut"],
    Load[dADMVarlist, "MoreInOut"],
    Load[dHVarlist, "MoreInOut"],
    Load[GaugeVarlist, "Temp"],
    Load[dgVarlist, "Temp"],
    "",
    (* from Ein case *)
    "int idtgtt;",
    "if(GHG->gauge_H_dH_from_Ein) {",
    "  idtgtt = Ind(\"Ein_dtgtt\");",
    "} else {",
    "  idtgtt = Ind(\"GHG_gtt\"); /* set to dummy values */",
    "}",
    Load[EinVarlist, "MoreInOut"],
    "",
    (* parameters *)
    "double parnu = Getd(GHG->nu);",
    "double pareta = Getd(GHG->eta2);",
    "double parmu0 = Getd(GHG->mu0);",
    "",
    "TIMER_START;",
    "",
    "/* compute */",
    "forpoints(node, ijk)",
    "{",
    "",
    (* declare temporary vars *)
    Store[TempVarlist, "Temp"],
    "",
    (* set d_k g_{ab} *)
    Table[dg[cc, aa, bb], {cc, 3, 1, -1}, {aa, 3, 0, -1}, {bb, 3, aa, -1}],
    (* set d_t g_{ab} *)
    "if(GHG->gauge_H_dH_from_Ein) {",
    "",
    Table[dg[0, aa, bb, SuffixName -> "Ein"], {aa, 3, 0, -1}, {bb, 3, aa, -1}],
    "",
    "} else {",
    "",
    "if (GHG->gauge_freeze) {",
    "",
    Store[GaugeVarlist, "Out", SuffixName -> "Freeze"],
    "} else if (GHG->gauge_puncture) {",
    "",
    Store[GaugePunctureVarlist, "Temp"],
    Store[GaugeVarlist, "Out", SuffixName -> "Puncture"],
    "} else if (GHG->gauge_DW) {",
    "",
    Store[GaugeDWVarlist, "Temp"],
    Store[GaugeGHVarlist, "Temp", SuffixName -> "DW"],
    Store[GaugeVarlist, "Out", SuffixName -> "GH"],
    "} else { errorexit(\"unknown ID GHG_gauge!\"); }",
    "",
    Table[dg[0, aa, bb], {aa, 3, 0, -1}, {bb, 3, aa, -1}],
    "",
    "}", (* end of if(GHG->gauge_H_dH_from_Ein) *)
    "",
    (* set g^{ab}, Gam_{cab}, Gam_{c} *)
    Store[MoreTempVarlist, "Temp"],
    "",
    (* set GHG vars *)
    Store[GHGVarlist, "Out"],
    "",
    "} /* end of points */",
    "",
    "/* derivatives */",
    "cart_partials_diUa(node, Vind(GHG->vlu,GHG->i_Ht), idHxt);",
    "",
    "forpoints(node, ijk)",
    "{",
    (* set more GHG vars *)
    Store[MoreOutputVarlist, "Out"],
    "} /* end of points */",
    "",
    "TIMER_STOP;",
    "",
    "} /* end of nodes */",
    "",
    "return 0;",
    "} /* end of function */"
  }]
];
