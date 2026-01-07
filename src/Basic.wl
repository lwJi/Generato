(* ::Package:: *)

(* Generato`Basic`, set up basics for Generato *)

(* (c) Liwei Ji, 01/2024 *)

(* Suppress xAct loading banners in quiet mode *)
If[Environment["QUIET"] === "1",
  Block[{Print}, BeginPackage["Generato`Basic`", {"xAct`xTensor`", "xAct`xCoba`"}]],
  BeginPackage["Generato`Basic`", {"xAct`xTensor`", "xAct`xCoba`"}]
];

If[Environment["QUIET"] =!= "1",
  System`Print["------------------------------------------------------------"];
  System`Print["Package Generato`Basic`, {2024, 1, 11}"];
  System`Print["------------------------------------------------------------"];
];

GetCheckInputEquations::usage = "GetCheckInputEquations[] returns True if input equation checking is enabled.";

SetCheckInputEquations::usage = "SetCheckInputEquations[bool] enables or disables checking of input equations.";

SetPVerbose::usage = "SetPVerbose[bool] enables or disables verbose printing of messages.";

GetPrintDate::usage = "GetPrintDate[] returns True if the date is printed in output files.";

SetPrintDate::usage = "SetPrintDate[bool] enables or disables printing the date in output files.";

GetPrintHeaderMacro::usage = "GetPrintHeaderMacro[] returns True if header guard macros are printed in output files.";

SetPrintHeaderMacro::usage = "SetPrintHeaderMacro[bool] enables or disables printing header guard macros in output files.";

GetGridPointIndex::usage = "GetGridPointIndex[] returns the grid point index string appended to variable names.";

SetGridPointIndex::usage = "SetGridPointIndex[index] sets the grid point index string appended to variable names.";

GetTilePointIndex::usage = "GetTilePointIndex[] returns the tile point index string appended to variable names.";

SetTilePointIndex::usage = "SetTilePointIndex[index] sets the tile point index string appended to variable names.";

GetSuffixUnprotected::usage = "GetSuffixUnprotected[] returns the suffix added to variable names that would conflict with system symbols.";

SetSuffixUnprotected::usage = "SetSuffixUnprotected[suffix] sets the suffix added to variable names that would conflict with system symbols.";

GetOutputFile::usage = "GetOutputFile[] returns the output file name.";

SetOutputFile::usage = "SetOutputFile[name] sets the output file name.";

GetProject::usage = "GetProject[] returns the project name used in code generation.";

SetProject::usage = "SetProject[name] sets the project name used in code generation.";

GetDefaultChart::usage = "GetDefaultChart[] returns the default coordinate chart of the first defined manifold.";

GetDim::usage = "GetDim[] returns the dimension of the first defined manifold.";

IsDefined::usage = "IsDefined[term] returns True if term is a defined tensor or constant, False otherwise.";

IndexType::usage = "IndexType[compindexlist, indextype] returns True if the second component index satisfies indextype.";

IndexType3D::usage = "IndexType3D[compindexlist, indextype] returns True if the second component index satisfies indextype and is a 3D index.";

RHSOf::usage = "RHSOf[var] returns the symbol var$RHS.\nRHSOf[var, suffix] returns the symbol varsuffix$RHS.";

SetEQN::usage = "SetEQN[var, varrhs] assigns varrhs to var$RHS using IndexSet.\nSetEQN[{CheckRHS->bool, SuffixName->suffix}, var, varrhs] assigns with options.";

CheckRHS::usage = "CheckRHS is an option for SetEQN that specifies whether to verify all terms in the right-hand side are defined.";

SetEQNDelayed::usage = "SetEQNDelayed[var, varrhs] assigns varrhs to var$RHS using IndexSetDelayed.\nSetEQNDelayed[{SuffixName->suffix}, var, varrhs] assigns with a suffix.";

PrintVerbose::usage = "PrintVerbose[expr] prints expr if verbose mode is enabled.";

Begin["`Private`"];

(* Data *)

$CheckInputEquations = False;

$PVerbose = False;

(* Quiet mode - suppress all output when QUIET=1 environment variable is set *)
$QuietMode = (Environment["QUIET"] === "1");

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
      CovDQ[Head[head]],
        Return[True]
      ,
      True,
        Throw @ Message[IsDefined::EType, term]
    ]
  ];

IsDefined::ETerm = "The `1`-th subterm in '`2`' is not defined!";

IsDefined::EType = "The Expression type of '`1`' is not detectable!";

Protect[IsDefined];

IsExprComb[head_] :=
  MemberQ[{Power, Plus, Times, Log, Max, Min, Sin, Cos, Tan, Csc, Sec, Cot}, head];

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

(* Detailed implementation of SetEQN and SetEQNDelayed *)

SetEQNdetail[checkrhs_, suffix_, var_, varrhs_] :=
  Module[{suffix0, replacetimes = 0},
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
    ReleaseHold[Hold[IndexSet[var, varrhs]] /. {var[[0]] :> RHSOf[ToString[var[[0]]] <> suffix0] /; replacetimes++ == 0}]
  ];

SetEQNdetail::Evarrhs = "There are undefined terms in the RHS '`1`'!";

SetEQNdetail::Evar = "Var '`1`' is used with IndexSet before, please use a different name!";

Protect[SetEQNdetail];

SetAttributes[SetEQNDelayeddetail, {HoldAll, SequenceHold}];

SetEQNDelayeddetail[suffix_, var_, varrhs_] :=
  Module[{suffix0, replacetimes = 0},
    suffix0 =
      If[suffix === Null,
        ""
        ,
        ToString[suffix]
      ];
    If[IsExprComb[Head[var]],
      Throw @ Message[SetEQNDelayeddetail::Evar, var]
    ];
    ReleaseHold[Hold[IndexSetDelayed[var, varrhs]] /. {var[[0]] :> RHSOf[ToString[var[[0]]] <> suffix0] /; replacetimes++ == 0}]
  ];

SetEQNDelayeddetail::Evar = "Var '`1`' is used with IndexSet before, please use a different name!";

Protect[SetEQNDelayeddetail];

(*
  Assign var$RHS to varrhs, allowing index patterns such as {1, -cart}.
  - Internally uses 'IndexSetDelayed'.
  - Ensures all variables used in varrhs are defined beforehand.
  - Verifies that the symbol used in var has not been used previously.
*)

Options[SetEQN] = {CheckRHS -> True, SuffixName -> Null};

SetEQN[OptionsPattern[], var_, varrhs_] :=
  Module[{checkrhs, suffix},
    {checkrhs, suffix} = OptionValue[{CheckRHS, SuffixName}];
    (* If[allowlowerfreeindex, {var0, varrhs0} = AdjustEQNIndexes[var, varrhs]]; *)
    SetEQNdetail[checkrhs, suffix, var, varrhs]
  ];

Protect[SetEQN];

(*
  Delayed version of SetEQN.
  - Does not inspect 'varrhs', as it may contain unconventional or unevaluated expressions.
  - HoldAll:      prevents evaluation of arguments for symbolic manipulation.
  - SequenceHold: preserves argument structure by preventing sequence flattening.
*)

SetAttributes[SetEQNDelayed, {HoldAll, SequenceHold}];

Options[SetEQNDelayed] = {SuffixName -> Null};

SetEQNDelayed[OptionsPattern[], var_, varrhs_] :=
  Module[{suffix},
    {suffix} = OptionValue[{SuffixName}];
    SetEQNDelayeddetail[suffix, var, varrhs]
  ];

Protect[SetEQNDelayed];

PrintVerbose[var__:""] :=
  Module[{},
    If[GetPVerbose[],
      System`Print[var]
    ]
  ];

Protect[PrintVerbose];

End[];

EndPackage[];
