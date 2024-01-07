(* ::Package:: *)

(* Generato`Basic`, set up basics for Generato *)

(* (c) Liwei Ji, 01/2024 *)

BeginPackage["Generato`Basic`", "xAct`xTensor`", "xAct`xCoba`"];

GetGridPointIndex::usage = "GetGridPointIndex[] return the grid index name.";

SetGridPointIndex::usage = "SetGridPointIndex[girdindex] reset the grid index name.";

IsDefined::usage = "IsDefined[term] return True/False if term is defined or not."

RHSOf::usage = "RHSOf[var, suffix] return the expression of 'var$RHS' or 'varsuffix$RHS' (if suffix is not empty).";

SetEQN::usage = "SetEQN[{DelaySet->..., CheckRHS->...}, var, suffix, varrhs] set 'var$RHS/varsuffix$RHS' equal to 'varrhs', considering if DelaySet, CheckRHS.";

DelaySet::usage = "DelaySet is an option for SetEQN specifying if use IndexSetDelayed.";

CheckRHS::usage = "CheckRHS is an option for SetEQN specifying if check there are undefined terms in varrhs.";

SetEQNDelayed::usage = "SetEQNDelayed[var, suffix, varrhs] returns SetEQN[ {DelaySet->True}, var, suffix, varrhs].";

Begin["`Private`"];

(* Data *)

$GridPointIndex = "";

GetGridPointIndex[] :=
    Return[$GridPointIndex];

Protect[GetGridPointIndex];

SetGridPointIndex[gridindex_] :=
    Module[{},
        $GridPointIndex = gridindex
    ];

Protect[SetGridPointIndex];

(* Function *)

IsDefined[term_] :=
    Module[{head = Head[term]},
        Which[
            NumberQ[term],
                Return[True]
            ,
            head === Power || head === Plus || head === Times || head
                 === Log,
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
                        Message[IsDefined::ETerm, undefinedsubterm, term
                            ];
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

RHSOf[var__] :=
    Module[{argList = List[var]},
        Switch[Length[argList],
            1,
                ToExpression[ToString[var] <> "$RHS"]
            ,
            2,
                ToExpression[ToString[argList[[1]]] <> ToString[argList
                    [[2]]] <> "$RHS"]
            ,
            _,
                Throw @ Message[RHSOf::Eargs, Length[argList]]
        ]
    ];

RHSOf::Eargs = "`1` arguments unsupported yet!";

Protect[RHSOf];

Options[SetEQN] = {DelaySet -> False, CheckRHS -> True};

SetEQN[OptionsPattern[], var_, suffix_String:"", varrhs_] :=
    ReleaseHold @
        Module[{delayset, checkrhs, replaceTimes = 0},
            {delayset, checkrhs} = OptionValue[{DelaySet, CheckRHS}];
                
            (* check if there is undefined term *)
            If[checkrhs && !IsDefined[varrhs],
                Throw @ Message[SetEQN::Evarrhs, varrhs]
            ];
            (* set var$RHS or varsuffix$RHS to varrhs *)
            If[delayset,
                Hold[IndexSetDelayed[var, varrhs]] /. {var[[0]] :> RHSOf[
                    ToString[var[[0]]] <> suffix] /; replaceTimes++ == 0}
                ,
                Hold[IndexSet[var, varrhs]] /. {var[[0]] :> RHSOf[ToString[
                    var[[0]]] <> suffix] /; replaceTimes++ == 0}
            ]
        ];

SetEQN::Evarrhs = "There are undefined terms in the RHS '`1`'!"

Protect[SetEQN];

SetEQNDelayed[var_, suffix_String:"", varrhs_] :=
    Module[{},
        SetEQN[{DelaySet -> True, CheckRHS -> False}, var, suffix, varrhs
            ]
    ];

Protect[SetEQNDelayed];

End[];

EndPackage[];
