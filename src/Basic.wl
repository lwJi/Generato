(* ::Package:: *)

(* Generato`Basic`, set up basics for Generato *)

(* (c) Liwei Ji, 01/2024 *)

BeginPackage["Generato`Basic`", {"xAct`xCoba`"}];

getDim::usage = "getDim[] returns the dim of the default manifold we are on.";

setDim::usage = "setDim[dim] update the dim of the default manifold to be generated.";

getDefaultChart::usage = "getDefaultChart[] returns the default chart name of the mainfold.";

setDefaultChart::usage = "setDefaultChart[chart] update the default chart name of the mainfold.";

Begin["`Private`"];

(* Data *)

$Dim = 0;

$DefaultChart = Null;

(* Function *)

getDim[] :=
    Return[$Dim];

Protect[getDim];

setDim[dim_] :=
    Module[{},
        $Dim = dim
    ];

Protect[setDim];

getDefaultChart[] :=
    Return[$DefaultChart];

Protect[getDefaultChart];

setDefaultChart[chart_] :=
    Module[{},
        $DefaultChart = chart
    ];

Protect[setDefaultChart];

End[];

EndPackage[];
