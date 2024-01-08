(* ::Package:: *)

(* (c) Liwei Ji, 01/2024 *)

BeginPackage["Generato`Interface`"];

Needs["Generato`Varlist`"];

SetComponents::usage = "SetComponents[varlist, mode] set components of varlist based on mode [\"independent\"(default), \"using vl_index\", \"temporary\"].";

Begin["`Private`"];

Options[SetComponents] :=
    {ChartName -> GetdefaultChart[], SuffixName -> "", IndependentIndexForEachVar
         -> True, TemporaryVars -> False};

SetComponents[varlist_?ListQ, OptionsPattern[]] :=
    Module[{chartname, suffixname},
        {chartname, suffixname, indepidx, temp} = OptionValue[{ChartName,
             SuffixName, IndependentIndexForEachVar, TemporaryVars}];
        SetParseModeAllToFalse[];
        SetParseMode[SetComp -> True];
        If[indepidx,
            SetParseMode[SetCompIndep -> True]
        ];
        If[temp,
            SetParseMode[SetCompTemp -> True]
        ];
        ParseVarlist[varlist, chartname, suffixname}];
    ];

Protect[SetComponents];

End[];

EndPackage[];
