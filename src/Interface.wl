(* ::Package:: *)

(* (c) Liwei Ji, 01/2024 *)

BeginPackage["Generato`Interface`"];

Needs["Generato`Varlist`"];

SetComponents::usage = "SetComponents[varlist, mode] set components of varlist based on mode [\"independent\"(default), \"using vl_index\", \"temporary\"].";

Begin["`Private`"];

Options[SetComponents] :=
    {ChartName -> GetdefaultChart[], IndependentIndexForEachVar -> True,
         Mode -> "Main"};

SetComponents[OptionsPattern[], varlist_?ListQ] :=
    Module[{chartname, indepidx, mode},
        {chartname, indepidx, mode} = OptionValue[{ChartName, IndependentIndexForEachVar,
             Mode}];
        SetParseModeAllToFalse[];
        SetParseMode[SetComp -> True];
        If[indepidx,
            SetParseMode[SetCompIndep -> True]
        ];
        If[StringMatchQ[mode, "Temp"],
            SetParseMode[SetCompTemp -> True]
        ];
        ParseVarlist[varlist, chartname}];
    ];

Protect[SetComponents];

Options[PrintEquations] :=
    {ChartName -> GetdefaultChart[], SuffixName -> Null, Mode -> "Main"
        };

PrintEquations[OptionsPattern[], varlist_?ListQ] :=
    Module[{chartname, suffixname, mode},
        {chartname, suffixname, mode} = OptionValue[{ChartName, SuffixName,
             Mode}];
        If[suffixname != Null,
            SetSuffixName[suffixname]
        ];
        SetParseModeAllToFalse[];
        SetParseMode[PrintComp -> True];
        SetParseMode[PrintCompEQN -> True];
        Which[
            StringMatchQ[mode, "NewVar"],
                SetParseMode[PrintCompEQNNewVar -> True];
                StringMatchQ[mode, "Main"]
            ,
            SetParseMode[PrintCompEQNMain -> True];
                StringMatchQ[mode, "AddToMain"],
                SetParseMode[PrintCompEQNAddToMain -> True];
                True
            ,
            Throw @ Message[PrintEquations::EMode, mode]
        ];
        ManipulateVarlist[varlist, chartname];
    ];

PrintEquations::EMode = "PrintEquations mode '`1`' unsupported yet!";

Protect[PrintEquations];

End[];

EndPackage[];
