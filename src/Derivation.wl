(* ::Package:: *)

(* (c) Liwei Ji, 01/2024 *)

BeginPackage["Generato`Derivation`"];

System`Print["------------------------------------------------------------"];

System`Print["Package Generato`Derivation`, {2024, 1, 18}"];

System`Print["------------------------------------------------------------"];

TestEQN::usage = "TestEQN[equal, terms] print SUCCEED/FAILED if equal/not";

Begin["`Private`"];

TestEQN[equal_?BooleanQ, terms_ : ""] :=
  Module[{},
    If[equal,
      System`Print["  Testing " <> terms <> " SUCCEED!"]
      ,
      System`Print["  Testing " <> terms <> " FAILED!"];
      Abort[]
    ]
  ];

Protect[TestEQN];

End[];

EndPackage[];
