(* ::Package:: *)

(* Generato`Basic`, set up basics for Generato *)

(* (c) Liwei Ji, 01/2024 *)

BeginPackage["Generato`Basic`", {"xAct`xCoba`"}];

RHSOf::usage = "RHSOf[var, suffix] return the expression of 'var$RHS' or 'varsuffix$RHS' (if suffix is not empty).";

setEQN::usage = "setEQN[{delaySet->..., checkRHS->...}, var, suffix, varrhs] set 'var$RHS/varsuffix$RHS' equal to 'varrhs', considering if delaySet, checkRHS.";

delaySet::usage = "delaySet is an option for setEQN specifying if use IndexSetDelayed.";

checkRHS::usage = "checkRHS is an option for setEQN specifying if check there are undefined terms in varrhs.";

setEQNDelayed::usage = "setEQNDelayed[var, suffix, varrhs] returns setEQN[ {delaySet->True}, var, suffix, varrhs].";

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
