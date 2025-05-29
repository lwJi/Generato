(* ::Package:: *)

(* Generato`Basic`, set up basics for Generato *)

(* (c) Liwei Ji, 01/2024 *)

BeginPackage["Generato`Basic`", "xAct`xTensor`", "xAct`xCoba`"];

Print["------------------------------------------------------------"];

Print["Package Generato`Basic`, {2024, 1, 11}"];

Print["------------------------------------------------------------"];

GetCheckInputEquations::usage = "GetCheckInputEquations[] return the Boolean variable specifying if we are checking input equations.";

SetCheckInputEquations::usage = "SetCheckInputEquations[True] update the Boolean variable specifying if we are checking input equations.";

SetPVerbose::usage = "SetPVerbose[True] update the Boolean variable specifying if print more messages.";

GetPrintDate::usage = "GetPrintDate[] return the Boolean variable specifying if print date in file.";

SetPrintDate::usage = "SetPrintDate[True] update the Boolean variable specifying if print date in file.";

GetPrintHeaderMacro::usage = "GetPrintHeaderMacro[] return the Boolean variable specifying if print date in file.";

SetPrintHeaderMacro::usage = "SetPrintHeaderMacro[True] update the Boolean variable specifying if print date in file.";

GetGridPointIndex::usage = "GetGridPointIndex[] return the grid index name.";

SetGridPointIndex::usage = "SetGridPointIndex[girdindex] reset the grid index name.";

GetTilePointIndex::usage = "GetTilePointIndex[] return the tile index name.";

SetTilePointIndex::usage = "SetTilePointIndex[girdindex] reset the tile index name.";

GetSuffixUnprotected::usage = "GetSuffixUnprotected[] returns the suffix added to vars which would conflict with system default otherwise.";

SetSuffixUnprotected::usage = "SetSuffixUnprotected[suffix] reset the suffix added to vars which would conflict with system default otherwise.";

GetOutputFile::usage = "GetOutputFile[] return the variable storing the output file name.";

SetOutputFile::usage = "SetOutputFile[name] update the variable storing the output file name.";

GetProject::usage = "GetProject[] return the project name we are generating file for.";

SetProject::usage = "SetProject[name] update the project name we are generating file for.";

GetDefaultChart::usage = "GetDefaultChart[] return the default chart we are using.";

GetDim::usage = "GetDim[] return the dimension of the manifold.";

IsDefined::usage = "IsDefined[term] return True/False if term is defined or not.";

IndexType::usage = "IndexType[compindex, indextype] return True if second compindex is indextype.";

IndexType3D::usage = "IndexType3D[compindex, indextype] return True if second compindex is indextype and it's a 3D index.";

RHSOf::usage = "RHSOf[var, suffix] return the expression of 'var$RHS' or 'varsuffix$RHS' (if suffix is not empty).";

SetEQN::usage = "SetEQN[{CheckRHS->..., Suffix->...}, var, varrhs] set 'var$RHS/varsuffix$RHS' equal to 'varrhs', considering if CheckRHS.";

CheckRHS::usage = "CheckRHS is an option for SetEQN specifying if check there are undefined terms in varrhs.";

SetEQNDelayed::usage = "SetEQNDelayed[{Suffix->...}, var, varrhs] set 'var$RHS/varsuffix$RHS' equal to varrhs for if cases.";

PrintVerbose::usage = "PrintVerbose[var] print var only if GetPVerbose[] is True.";

Begin["`Private`"];

(* Data *)

$CheckInputEquations = False;

$PVerbose = False;

$PrintDate = True;

$PrintHeaderMacro = True;

$GridPointIndex = "";

$TilePointIndex = "";

$SuffixUnprotected = "$Upt";

$OutputFile = "output.c";

$Project = "TEST";

(* Function *)

GetCheckInputEquations[] :=
  Return[$CheckInputEquations];

Protect[GetCheckInputEquations];

SetCheckInputEquations[checkinput_] :=
  Module[{},
    $CheckInputEquations = checkinput
  ];

Protect[SetCheckInputEquations];

GetPVerbose[] :=
  Return[$PVerbose];

SetPVerbose[verbose_] :=
  Module[{},
    $PVerbose = verbose
  ];

Protect[SetPVerbose];

GetPrintDate[] :=
  Return[$PrintDate];

Protect[GetPrintDate];

SetPrintDate[print_] :=
  Module[{},
    $PrintDate = print
  ];

Protect[SetPrintDate];

GetPrintHeaderMacro[] :=
  Return[$PrintHeaderMacro];

Protect[GetPrintHeaderMacro];

SetPrintHeaderMacro[print_] :=
  Module[{},
    $PrintHeaderMacro = print
  ];

Protect[SetPrintHeaderMacro];

GetGridPointIndex[] :=
  Return[$GridPointIndex];

Protect[GetGridPointIndex];

SetGridPointIndex[gridindex_] :=
  Module[{},
    $GridPointIndex = gridindex
  ];

Protect[SetGridPointIndex];

GetTilePointIndex[] :=
  Return[$TilePointIndex];

Protect[GetTilePointIndex];

SetTilePointIndex[tileindex_] :=
  Module[{},
    $TilePointIndex = tileindex
  ];

Protect[SetTilePointIndex];

GetSuffixUnprotected[] :=
  Return[$SuffixUnprotected];

Protect[GetSuffixUnprotected];

SetSuffixUnprotected[suffix_] :=
  Module[{},
    $SuffixUnprotected = suffix
  ];

Protect[SetSuffixUnprotected];

GetOutputFile[] :=
  Return[$OutputFile];

Protect[GetOutputFile];

SetOutputFile[filename_] :=
  Module[{},
    $OutputFile = filename
  ];

Protect[SetOutputFile];

GetProject[] :=
  Return[$Project];

Protect[GetProject];

SetProject[name_] :=
  Module[{},
    $Project = name
  ];

Protect[SetProject];

GetDefaultChart[] :=
  Return[ChartsOfManifold[$Manifolds[[1]]][[1]]];

GetDim[] :=
  Return[DimOfManifold[$Manifolds[[1]]]];

IsDefined[term_] :=
  Module[{head = Head[term]},
    Which[
      NumberQ[term],
        Return[True]
      ,
      IsExprComb[head],
        Module[{subterms, undefinedsubterm = 0},
          subterms = Apply[List, term];
          Do[
            If[!IsDefined[subterms[[isub]]],
              undefinedsubterm = isub;
              Break[]
            ]
            ,
            {isub, 1, Length[subterms]}
          ];
          If[undefinedsubterm > 0,
            Message[IsDefined::ETerm, undefinedsubterm, term];
            Return[False]
            ,
            Return[True]
          ]
        ]
      ,
      head === Symbol,
        Return[ConstantSymbolQ[term]]
      ,
      Head[head] === Symbol,
        Return[xTensorQ[head]]
      ,
      True,
        Throw @ Message[IsDefined::EType, term]
    ]
  ];

IsDefined::ETerm = "The `1`-th subterm in '`2`' is not defined!";

IsDefined::EType = "The Expression type of '`1`' is not detectable!";

Protect[IsDefined];

IsExprComb[head_] :=
  Return[head === Power || head === Plus || head === Times || head === Log];

IndexType[compindexlist_?ListQ, indextype_] :=
  indextype[compindexlist[[2]]];

Protect[IndexType];

IndexType3D[compindexlist_?ListQ, indextype_] :=
  indextype[compindexlist[[2]]] && (compindexlist[[1]] > 0);

Protect[IndexType3D];

RHSOf[var__] :=
  Module[{arglist = List[var]},
    Switch[Length[arglist],
      1,
        ToExpression[ToString[var] <> "$RHS"]
      ,
      2,
        ToExpression[ToString[arglist[[1]]] <> ToString[arglist[[2]]] <> "$RHS"]
      ,
      _,
        Throw @ Message[RHSOf::Eargs, Length[arglist]]
    ]
  ];

RHSOf::Eargs = "`1` arguments unsupported yet!";

Protect[RHSOf];

(* Replace -x with x for free indexes in eqs, to support patterns like {1, -cart} *)

AdjustEQNIndexes[var_, varrhs_] :=
  Module[{var0, varrhs0, idxname},
    varrhs0 = varrhs;
    (* replace -x_ with x_ for free indexes in varrhs *)
    Do[
      If[Head[var[[idx]]] === Times && var[[idx]][[1]] === -1, (* arguments start with '-' *)
        (* get x from x_ from -x_ *)
        idxname = var[[idx]][[2]][[1]];
        (* replace -x with x in varrhs *)
        varrhs0 = varrhs0 /. {-idxname -> idxname}
      ]
      ,
      {idx, 1, Length[var]}
    ];
    (* replace -x_ with x_ in var *)
    var0 = var /. var[[0]][indices__] :> var[[0]] @@ Map[Replace[#, -x_ :> x]&, {indices}];
    Return[{var0, varrhs0}]
  ];

Protect[AdjustEQNIndexes];

SetEQNdetail[checkrhs_, suffix_, var_, varrhs_] :=
  ReleaseHold @
    Module[{replacetimes = 0, suffix0},
      suffix0 =
        If[suffix === Null,
          ""
          ,
          ToString[suffix]
        ];
      If[IsExprComb[Head[var]],
        Throw @ Message[SetEQNdetail::Evar, var]
      ];
      If[checkrhs && !IsDefined[varrhs],
        Throw @ Message[SetEQNdetail::Evarrhs, varrhs]
      ];
      Hold[IndexSet[var, varrhs]] /. {var[[0]] :> RHSOf[ToString[var[[0]]] <> suffix0] /; replacetimes++ == 0}
    ];

SetEQNdetail::Evarrhs = "There are undefined terms in the RHS '`1`'!"

SetEQNdetail::Evar = "Var '`1`' is used with IndexSet before, please use a different name!"

Protect[SetEQNdetail];

(*
  Assign var$RHS to varrhs, allowing index patterns such as {1, -cart}.
  - Internally uses 'IndexSet'.
  - Ensures all variables used in varrhs are defined beforehand.
  - Verifies that the symbol used in var has not been used previously.
*)

Options[SetEQN] = {CheckRHS -> True, SuffixName -> Null};

SetEQN[OptionsPattern[], var_, varrhs_] :=
  Module[{checkrhs, suffix, var0, varrhs0},
    {checkrhs, suffix} = OptionValue[{CheckRHS, SuffixName}];
    {var0, varrhs0} = AdjustEQNIndexes[var, varrhs];
    SetEQNdetail[checkrhs, suffix, var0, varrhs0]
  ];

Protect[SetEQN];

SetAttributes[SetEQNDelayed, {HoldAll, SequenceHold}];

Options[SetEQNDelayed] = {SuffixName -> Null};

SetEQNDelayed[OptionsPattern[], var_, varrhs_] :=
  ReleaseHold @
    Module[{suffix, replacetimes = 0},
      {suffix} = OptionValue[{SuffixName}];
      suffix =
        If[suffix === Null,
          ""
          ,
          ToString[suffix]
        ];
      If[IsExprComb[Head[var]],
        Throw @ Message[SetEQNDelayed::Evar, var]
      ];
      Hold[IndexSetDelayed[var, varrhs]] /. {var[[0]] :> RHSOf[ToString[var[[0]]] <> suffix] /; replacetimes++ == 0}
    ];

SetEQNDelayed::Evar = "Var '`1`' is used with IndexSet before, please use a different name!"

Protect[SetEQNDelayed];

PrintVerbose[var__:""] :=
  Module[{},
    If[GetPVerbose[],
      Print[var]
    ]
  ];

Protect[PrintVerbose];

End[];

EndPackage[];
