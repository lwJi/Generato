(* ::Package:: *)

(* Generato`Basic`, set up basics for Generato *)

(* (c) Liwei Ji, 01/2024 *)

BeginPackage["Generato`Basic`", {"xAct`xCoba`"}];

getDim::usage = "getDim[] returns the dim of the default manifold we are on.";

setDim::usage = "setDim[dim] update the dim of the default manifold to be generated.";

Begin["`Private`"];

(* Data *)

$Dim = 0;

(* Function *)

getDim[] :=
    Return[$Dim];

setDim[dim_] :=
    Module[{},
        $Dim = dim
    ];

Protect[getDim]

Protect[setDim]

End[];

EndPackage[];
