(* ::Package:: *)

(* (c) Liwei Ji, 01/2024 *)

BeginPackage["Generato`Interface`"];

Needs["Generato`Varlist`"];

SetComponents::usage = "SetComponents[varlist, mode] set components of varlist based on mode [\"independent\"(default), \"using vl_index\", \"temporary\"].";

Begin["`Private`"];

Options[SetComponents] :=
    {ChartName -> GetDefaultChart[], IndependentIndexForEachVar -> True,
         WithoutGridPointIndex -> False};

SetComponents[OptionsPattern[], varlist_?ListQ] :=
    Module[{chartname, indepidx, nogpidx},
        {chartname, indepidx, nogpidx} = OptionValue[{ChartName, IndependentIndexForEachVar,
             WithoutGridPointIndex}];
        SetParseModeAllToFalse[];
        SetParseMode[SetComp -> True];
        If[indepidx,
            SetParseMode[SetCompIndep -> True]
        ];
        If[nogpidx,
            SetParseMode[SetCompNoGPIndex -> True]
        ];
        ParseVarlist[varlist, chartname}];
    ];

Protect[SetComponents];

Options[PrintEquations] :=
    {ChartName -> GetDefaultChart[], SuffixName -> Null, Mode -> "Main"
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
                SetParseMode[PrintCompEQNNewVar -> True]
            ,
            StringMatchQ[mode, "Main"],
                SetParseMode[PrintCompEQNMain -> True]
            ,
            StringMatchQ[mode, "AddToMain"],
                SetParseMode[PrintCompEQNAddToMain -> True]
            ,
            True,
                Throw @ Message[PrintEquations::EMode, mode]
        ];
        ManipulateVarlist[varlist, chartname];
    ];

PrintEquations::EMode = "PrintEquations mode '`1`' unsupported yet!";

Protect[PrintEquations];

Options[PrintInitializations] :=
    {ChartName -> GetDefaultChart[], SuffixName -> Null, Mode -> "Main"
        };

PrintInitializations[OptionsPattern[], varlist_?ListQ] :=
    Module[{chartname, suffixname, mode},
        {chartname, suffixname, mode} = OptionValue[{ChartName, SuffixName,
             Mode}];
        If[suffixname != Null,
            SetSuffixName[suffixname]
        ];
        SetParseModeAllToFalse[];
        SetParseMode[PrintComp -> True];
        SetParseMode[PrintCompInit -> True];
        Which[
            StringMatchQ[mode, "MainOut"],
                SetParseMode[PrintCompInitMainOut -> True]
            ,
            StringMatchQ[mode, "MainIn"],
                SetParseMode[PrintCompInitMainIn -> True]
            ,
            StringMatchQ[mode, "MoreInOut"],
                SetParseMode[PrintCompInitMoreInOut -> True]
            ,
            StringMatchQ[mode, "Temp"],
                SetParseMode[PrintCompInitTemp -> True]
            ,
            True,
                Throw @ Message[PrintInitializations::EMode, mode]
        ];
        ManipulateVarlist[varlist, chartname];
        ParseVarlist[varlist, chartname];
    ];

PrintInitializations::EMode = "PrintInitializations mode '`1`' unsupported yet!";

Protect[PrintInitializations];

End[];

EndPackage[];
