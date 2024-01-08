(* ::Package:: *)

(* (c) Liwei Ji, 01/2024 *)

BeginPackage["Generato`Interface`"];

Needs["Generato`Varlist`"];

SetComponents::usage = "SetComponents[varlist, mode] set components of varlist based on mode [\"independent\"(default), \"using vl_index\", \"temporary\"].";

Begin["`Private`"];

Options[SetComponents] :=
    {ChartName -> GetdefaultChart[], SuffixName -> ""};

SetComponents[varlist_?ListQ, OptionsPattern[]] :=
    Module[{chartname, suffixname},
        {chartname, suffixname} = OptionValue[{ChartName, SuffixName}
            ];
        SetParseMode[SetComp -> True];
        ParseVarlist[varlist, chartname, suffixname}];
    ];

Protect[SetComponents];

End[];

EndPackage[];
