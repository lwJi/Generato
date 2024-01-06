(* ::Package:: *)

(* Generato`Basic`, set up basics for Generato *)

(* (c) Liwei Ji, 01/2024 *)

BeginPackage["Generato`Basic`", "xAct`xTensor`", "xAct`xCoba`"];

isDefined::usage = "isDefined[term] return True/False if term is defined or not."

RHSOf::usage = "RHSOf[var, suffix] return the expression of 'var$RHS' or 'varsuffix$RHS' (if suffix is not empty).";

setEQN::usage = "setEQN[{delaySet->..., checkRHS->...}, var, suffix, varrhs] set 'var$RHS/varsuffix$RHS' equal to 'varrhs', considering if delaySet, checkRHS.";

delaySet::usage = "delaySet is an option for setEQN specifying if use IndexSetDelayed.";

checkRHS::usage = "checkRHS is an option for setEQN specifying if check there are undefined terms in varrhs.";

setEQNDelayed::usage = "setEQNDelayed[var, suffix, varrhs] returns setEQN[ {delaySet->True}, var, suffix, varrhs].";

Begin["`Private`"];

(* Data *)

(* Function *)

isDefined[term_] :=
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
                        If[!isDefined[subterms[[isub]]],
                            undefinedsubterm = isub;
                            Break[]
                        ]
                        ,
                        {isub, 1, Length[subterms]}
                    ];
                    If[undefinedsubterm > 0,
                        Message[isDefined::ETerm, undefinedsubterm, term
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
                Throw @ Message[isDefined::EType, term]
        ]
    ];

isDefined::ETerm = "The `1`-th subterm in '`2`' is not defined!";

isDefined::EType = "The Expression type of '`1`' is not detectable!";

Protect[isDefined];

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

Options[setEQN] = {delaySet -> False, checkRHS -> True};

setEQN[OptionsPattern[], var_, suffix_String:"", varrhs_] :=
    ReleaseHold @
        Module[{delayset, checkrhs, replaceTimes = 0},
            {delayset, checkrhs} = OptionValue[{delaySet, checkRHS}];
                
            (* check if there is undefined term *)
            If[checkrhs && !isDefined[varrhs],
                Throw @ Message[setEQN::Evarrhs, varrhs]
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

setEQN::Evarrhs = "There are undefined terms in the RHS '`1`'!"

Protect[setEQN];

setEQNDelayed[var_, suffix_String:"", varrhs_] :=
    Module[{},
        setEQN[{delaySet -> True, checkRHS -> False}, var, suffix, varrhs
            ]
    ];

Protect[setEQNDelayed];

End[];

EndPackage[];
