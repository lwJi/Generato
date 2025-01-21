(* ::Package:: *)

(* (c) Liwei Ji, 01/2024 *)

BeginPackage["Generato`Derivation`"];

Print["------------------------------------------------------------"];

Print["Package Generato`Derivation`, {2024, 1, 18}"];

Print["------------------------------------------------------------"];

TestEQN::usage = "TestEQN[equal, terms] print SUCCEED/FAILED if equal/not";

Begin["`Private`"];

TestEQN[equal_?BooleanQ, terms_ : String ""] :=
  Module[{},
    If[equal,
      Print["  Testing " <> terms <> " SUCCEED!"]
      ,
      Print["  Testing " <> terms <> " FAILED!"];
      Abort[]
    ]
  ];

Protect[TestEQN];

End[];

EndPackage[];