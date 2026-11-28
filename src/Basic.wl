(* ::Package:: *)

(* Generato`Basic`, set up basics for Generato *)

(* (c) Liwei Ji, 01/2024 *)

(* Suppress xAct loading banners in quiet mode *)

If[Environment["QUIET"] === "1",
  Block[{Print},
    BeginPackage["Generato`Basic`", {"xAct`xTensor`", "xAct`xCoba`", "Generato`Failure`", "Generato`Context`"}]
  ]
  ,
  BeginPackage["Generato`Basic`", {"xAct`xTensor`", "xAct`xCoba`", "Generato`Failure`", "Generato`Context`"}]
];

If[Environment["QUIET"] =!= "1",
  System`Print["------------------------------------------------------------"];
  System`Print["Package Generato`Basic`, {2024, 1, 11}"];
  System`Print["------------------------------------------------------------"];
];

GetDefaultManifold::usage = "GetDefaultManifold[] returns the first defined manifold. Stops the run if no manifolds have been defined.";

GetDefaultChart::usage = "GetDefaultChart[] returns the default coordinate chart of the first defined manifold.";

GetDim::usage = "GetDim[] returns the dimension of the first defined manifold.";

IsDefined::usage = "IsDefined[term] returns True if term is a defined tensor or constant, False otherwise.";

IndexType::usage = "IndexType[compindexlist, indextype] returns True if the second component index satisfies indextype.";

IndexType3D::usage = "IndexType3D[compindexlist, indextype] returns True if the second component index satisfies indextype and is a 3D index.";

RHSOf::usage = "RHSOf[var] returns the symbol var$RHS.\nRHSOf[var, suffix] returns the symbol varsuffix$RHS.";

SetEQN::usage = "SetEQN[var, varrhs] assigns varrhs to var$RHS using IndexSet.\nSetEQN[{CheckRHS->bool, SuffixName->suffix}, var, varrhs] assigns with options.";

CheckRHS::usage = "CheckRHS is an option for SetEQN that specifies whether to verify all terms in the right-hand side are defined.";

SetEQNDelayed::usage = "SetEQNDelayed[var, varrhs] assigns varrhs to var$RHS using IndexSetDelayed.\nSetEQNDelayed[{SuffixName->suffix}, var, varrhs] assigns with a suffix.";

ComputeRHSValue::usage = "ComputeRHSValue[coordinate, compname, suffix, extrareplacerules] computes the RHS value of the equation set for a component under the suffix name suffix, \"\" for an equation set without one.";

GetGFIndexName::usage = "GetGFIndexName[index] converts integer index to GF index name (p1, m1, c0, etc.).";

GetGFIndexNameMix2nd::usage = "GetGFIndexNameMix2nd[index1, index2] creates mixed 2nd derivative GF index name.";

GetInterfaceName::usage = "GetInterfaceName[compname] returns the interface name for a component.";

PrintVerbose::usage = "PrintVerbose[expr] prints expr if verbose mode is enabled.";

Begin["`Private`"];

GetDefaultManifold[] :=
  Module[{},
    If[Length[$Manifolds] === 0,
      Stop[GetDefaultManifold::NoManifolds]
      ,
      Return[$Manifolds[[1]]]
    ]
  ];

GetDefaultManifold::NoManifolds = "No manifolds have been defined. Use DefManifold to define a manifold.";

Protect[GetDefaultManifold];

GetDefaultChart[] :=
  ChartsOfManifold[GetDefaultManifold[]][[1]];

Protect[GetDefaultChart];

GetDim[] :=
  DimOfManifold[GetDefaultManifold[]];

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
            (* False is the answer, not a failure of generation: SetEQN reads it and stops with its own message *)
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
        Stop[IsDefined::EType, term]
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
        Stop[RHSOf::Eargs, Length[arglist]]
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
      Stop[SetEQNDetail::Evar, var]
    ];
    If[checkrhs && !IsDefined[varrhs],
      Stop[SetEQNDetail::Evarrhs, varrhs]
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
      Stop[SetEQNDelayedDetail::Evar, var]
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

(*
  ComputeRHSValue - Computes the RHS value for an equation
  The suffix name selects which of the equations set for the variable is
  evaluated; it is an argument, so nothing here reads a mode.
*)
ComputeRHSValue[coordinate_, compname_, suffix_String, extrareplacerules_] :=
  Module[{rhssToValue},
    rhssToValue =
      (compname /. {compname[[0]] -> RHSOf[compname[[0]], suffix]}) //
      DummyToBasis[coordinate] // TraceBasisDummy // ToValues;
    If[GetSimplifyEquation[],
      rhssToValue = rhssToValue // Simplify
    ];
    If[Length[extrareplacerules] > 0,
      rhssToValue = (rhssToValue // ToValues) /. extrareplacerules
    ];
    rhssToValue
  ];

Protect[ComputeRHSValue];

(*
  The three functions below name a grid function at a stencil point and a
  component at an interface. They were shared by backends that are gone:
  nothing in src/ or codes/ calls them, and no test exercises them.
*)

(* GetGFIndexName - Converts integer index to GF index name *)
GetGFIndexName[index_?IntegerQ] :=
  Module[{gfindex},
    gfindex =
      Which[
        index > 0, "p" <> ToString[index],
        index < 0, "m" <> ToString[Abs[index]],
        True, "c0"
      ];
    ToExpression[gfindex]
  ];

Protect[GetGFIndexName];

(* GetGFIndexNameMix2nd - Creates mixed 2nd derivative GF index name *)
GetGFIndexNameMix2nd[index1_?IntegerQ, index2_?IntegerQ] :=
  Module[{gfindex},
    gfindex = ToString[GetGFIndexName[index1]]
           <> ToString[GetGFIndexName[index2]];
    ToExpression[gfindex]
  ];

Protect[GetGFIndexNameMix2nd];

(* GetInterfaceName - Returns the interface name for a component *)
GetInterfaceName[compname_] :=
  Module[{intfname = ToString[compname[[0]]], colist = {"t", "x", "y", "z"}, coindex},
    Do[
      coindex = compname[[icomp]][[1]];
      intfname = intfname <> colist[[coindex + 1]]
      ,
      {icomp, 1, Length[compname]}
    ];
    intfname = ToString[CForm[ToExpression[intfname <> GetGridPointIndex[]]]];
    Return[intfname];
  ];

Protect[GetInterfaceName];

PrintVerbose[var__:""] :=
  Module[{},
    If[GetPVerbose[],
      System`Print[var]
    ]
  ];

Protect[PrintVerbose];

End[];

EndPackage[];
