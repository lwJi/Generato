(* ::Package:: *)

(* Generato`Basic`, set up basics for Generato *)

(* (c) Liwei Ji, 01/2024 *)

BeginPackage["Generato`Basic`", {"xAct`xCoba`"}];

RHSOf::usage = "RHSOf[var, suffix] return the expression of 'var$RHS' or 'varsuffix$RHS' (if suffix is not empty).";

setRHS::usage = "setRHS[lhs, suffix, rhs, {delaySet->..., checkRHS->...}] set 'rhs' to 'lhs$RHS' or 'lhssuffix$RHS', considering if delaySet.";

delaySet::usage = "delaySet is an option for setRHS specifying if use IndexSetDelayed or IndexSet.";

setRHSDelayed::usage = "setRHSDelayed[lhs, suffix, rhs] returns setRHS[lhs, suffix, rhs, {delaySet->True}].";

Begin["`Private`"];

(* Data *)

(* Function *)

RHSOf[var__] :=
    Module[{argList = List[var]},
        Switch[Length[argList],
            1,
                ToExpression[ToString[var] <> "$RHS"]
            ,
            2,
                ToExpression[ToString[argList[[1]]] <> ToString[argList[[2]]] <>
                     "$RHS"]
            ,
            _,
                Throw @ Message[RHSOf::Eargs, Length[argList]]
        ]
    ];

RHSOf::Eargs = "`1` arguments unsupported yet!";

Protect[RHSOf];

Options[setRHS] =
    {delaySet -> False, checkRHS -> True};

setRHS[lhs_, suffix_String:"", rhs_, OptionsPattern[]] :=
    ReleaseHold @
        Module[{delayset, checkrhs, replaceTimes = 0},
            {delayset, checkrhs} = OptionValue[{delaySet, checkRHS}];
                
            (* check if there is undefined term *)
            If[checkrhs && !isDefined[rhs],
                Throw @ Message[setRHS::Erhs, rhs]
            ];
            (* define rhs *)
            If[delayset,
                Hold[IndexSetDelayed[lhs, rhs]] /. {lhs[[0]] :> RHSOf[
                    lhs[[0]], suffix] /; replaceTimes++ == 0}
                ,
                Hold[IndexSet[lhs, rhs]] /. {lhs[[0]] :> RHSOf[lhs[[0
                    ]], suffix] /; replaceTimes++ == 0}
            ]
        ];

setRHS::Erhs = "There are undefined terms in the RHS '`1`'!"

Protect[setRHS];

setRHSDelayed[lhs_, suffix_String:"", rhs_] :=
    Module[{},
        setRHS[{delaySet -> True, checkRHS -> False}, lhs, suffix, rhs
            ]
    ];

Protect[setRHSDelayed];

End[];

EndPackage[];
