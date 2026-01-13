(* ::Package:: *)

(* Generato`Basic`, set up basics for Generato *)

(* (c) Liwei Ji, 01/2024 *)

(* Suppress xAct loading banners in quiet mode *)

If[Environment["QUIET"] === "1",
  Block[{Print},
    BeginPackage["Generato`Basic`", {"xAct`xTensor`", "xAct`xCoba`", "Generato`Context`"}]
  ]
  ,
  BeginPackage["Generato`Basic`", {"xAct`xTensor`", "xAct`xCoba`", "Generato`Context`"}]
];

If[Environment["QUIET"] =!= "1",
  System`Print["------------------------------------------------------------"];
  System`Print["Package Generato`Basic`, {2024, 1, 11}"];
  System`Print["------------------------------------------------------------"];
];

GetCheckInputEquations::usage = "GetCheckInputEquations[] returns True if input equation checking is enabled.";

SetCheckInputEquations::usage = "SetCheckInputEquations[bool] enables or disables checking of input equations.";

SetPVerbose::usage = "SetPVerbose[bool] enables or disables verbose printing of messages.";

GetPVerbose::usage = "GetPVerbose[] returns True if verbose printing of messages is enabled.";

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

GetDefaultManifold::usage = "GetDefaultManifold[] returns the first defined manifold. Returns $Failed if no manifolds have been defined.";

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

(* Function *)

(* Context-aware getter *)
GetCheckInputEquations[ctx_Association] := GetCtx[ctx, "CheckInputEquations"];

Protect[GetCheckInputEquations];

(* Context-aware setter - returns new context *)
SetCheckInputEquations[ctx_Association, checkinput_] :=
  SetCtx[ctx, "CheckInputEquations", checkinput];

Protect[SetCheckInputEquations];

(* Context-aware getter *)
GetPVerbose[ctx_Association] := GetCtx[ctx, "PVerbose"];

Protect[GetPVerbose];

(* Context-aware setter - returns new context *)
SetPVerbose[ctx_Association, verbose_] :=
  SetCtx[ctx, "PVerbose", verbose];

Protect[SetPVerbose];

(* Context-aware getter *)
GetPrintDate[ctx_Association] := GetCtx[ctx, "PrintDate"];

Protect[GetPrintDate];

(* Context-aware setter - returns new context *)
SetPrintDate[ctx_Association, print_] :=
  SetCtx[ctx, "PrintDate", print];

Protect[SetPrintDate];

(* Context-aware getter *)
GetPrintHeaderMacro[ctx_Association] := GetCtx[ctx, "PrintHeaderMacro"];

Protect[GetPrintHeaderMacro];

(* Context-aware setter - returns new context *)
SetPrintHeaderMacro[ctx_Association, print_] :=
  SetCtx[ctx, "PrintHeaderMacro", print];

Protect[SetPrintHeaderMacro];

(* Context-aware getter *)
GetGridPointIndex[ctx_Association] := GetCtx[ctx, "GridPointIndex"];

Protect[GetGridPointIndex];

(* Context-aware setter - returns new context *)
SetGridPointIndex[ctx_Association, gridindex_] :=
  SetCtx[ctx, "GridPointIndex", gridindex];

Protect[SetGridPointIndex];

(* Context-aware getter *)
GetTilePointIndex[ctx_Association] := GetCtx[ctx, "TilePointIndex"];

Protect[GetTilePointIndex];

(* Context-aware setter - returns new context *)
SetTilePointIndex[ctx_Association, tileindex_] :=
  SetCtx[ctx, "TilePointIndex", tileindex];

Protect[SetTilePointIndex];

(* Context-aware getter *)
GetSuffixUnprotected[ctx_Association] := GetCtx[ctx, "SuffixUnprotected"];

Protect[GetSuffixUnprotected];

(* Context-aware setter - returns new context *)
SetSuffixUnprotected[ctx_Association, suffix_] :=
  SetCtx[ctx, "SuffixUnprotected", suffix];

Protect[SetSuffixUnprotected];

(* Context-aware getter *)
GetOutputFile[ctx_Association] := GetCtx[ctx, "OutputFile"];

Protect[GetOutputFile];

(* Context-aware setter - returns new context *)
SetOutputFile[ctx_Association, filename_] :=
  SetCtx[ctx, "OutputFile", filename];

Protect[SetOutputFile];

(* Context-aware getter *)
GetProject[ctx_Association] := GetCtx[ctx, "Project"];

Protect[GetProject];

(* Context-aware setter - returns new context *)
SetProject[ctx_Association, name_] :=
  SetCtx[ctx, "Project", name];

Protect[SetProject];

GetDefaultManifold[] :=
  Module[{},
    If[Length[$Manifolds] === 0,
      Message[GetDefaultManifold::NoManifolds];
      Return[$Failed]
      ,
      Return[$Manifolds[[1]]]
    ]
  ];

GetDefaultManifold::NoManifolds = "No manifolds have been defined. Use DefManifold to define a manifold.";

Protect[GetDefaultManifold];

GetDefaultChart[] :=
  Module[{manifold = GetDefaultManifold[]},
    If[manifold === $Failed,
      Return[$Failed]
      ,
      Return[ChartsOfManifold[manifold][[1]]]
    ]
  ];

Protect[GetDefaultChart];

GetDim[] :=
  Module[{manifold = GetDefaultManifold[]},
    If[manifold === $Failed,
      Return[$Failed]
      ,
      Return[DimOfManifold[manifold]]
    ]
  ];

Protect[GetDim];

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

SetEQNDetail[checkrhs_, suffix_, var_, varrhs_] :=
  Module[{suffix0, replacetimes = 0},
    suffix0 =
      If[suffix === Null,
        ""
        ,
        ToString[suffix]
      ];
    If[IsExprComb[Head[var]],
      Throw @ Message[SetEQNDetail::Evar, var]
    ];
    If[checkrhs && !IsDefined[varrhs],
      Throw @ Message[SetEQNDetail::Evarrhs, varrhs]
    ];
    ReleaseHold[Hold[IndexSet[var, varrhs]] /. {var[[0]] :> RHSOf[ToString[var[[0]]] <> suffix0] /; replacetimes++ == 0}]
  ];

SetEQNDetail::Evarrhs = "There are undefined terms in the RHS '`1`'!";

SetEQNDetail::Evar = "Var '`1`' is used with IndexSet before, please use a different name!";

Protect[SetEQNDetail];

SetAttributes[SetEQNDelayedDetail, {HoldAll, SequenceHold}];

SetEQNDelayedDetail[suffix_, var_, varrhs_] :=
  Module[{suffix0, replacetimes = 0},
    suffix0 =
      If[suffix === Null,
        ""
        ,
        ToString[suffix]
      ];
    If[IsExprComb[Head[var]],
      Throw @ Message[SetEQNDelayedDetail::Evar, var]
    ];
    ReleaseHold[Hold[IndexSetDelayed[var, varrhs]] /. {var[[0]] :> RHSOf[ToString[var[[0]]] <> suffix0] /; replacetimes++ == 0}]
  ];

SetEQNDelayedDetail::Evar = "Var '`1`' is used with IndexSet before, please use a different name!";

Protect[SetEQNDelayedDetail];

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
    SetEQNDetail[checkrhs, suffix, var, varrhs]
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
    SetEQNDelayedDetail[suffix, var, varrhs]
  ];

Protect[SetEQNDelayed];

PrintVerbose[var__:""] :=
  Module[{},
    If[GetPVerbose[$CurrentContext],
      System`Print[var]
    ]
  ];

Protect[PrintVerbose];

End[];

EndPackage[];
