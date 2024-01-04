(* ::Package:: *)

(* Generato`Basic`, set up basics for Generato *)

(* (c) Liwei Ji, 01/2024 *)

BeginPackage["Generato`Basic`", {"xAct`xCoba`"}];

getDim::usage = "getDim[] returns the dimension of the default manifold.";

Begin["`Private`"];

(* Data *)

$dim = 0;

(* Function *)

getDim[] :=
    Return[$dim];

Protect[getDim];

setDim[dim_] :=
    Module[{},
        $dim = dim
    ];

End[];

EndPackage[];
