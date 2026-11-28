(* ::Package:: *)

(* (c) Liwei Ji, 01/2024 *)

BeginPackage["Generato`Derivation`", {"Generato`Failure`"}];

If[Environment["QUIET"] =!= "1",
  System`Print["------------------------------------------------------------"];
  System`Print["Package Generato`Derivation`, {2024, 1, 18}"];
  System`Print["------------------------------------------------------------"];
];

TestEQN::usage = "TestEQN[bool, label] prints 'SUCCEED' if bool is True, otherwise stops the run with the message TestEQN::EFailed.";

Begin["`Private`"];

TestEQN[equal_?BooleanQ, terms_:""] :=
  Module[{},
    If[equal,
      System`Print["  Testing " <> terms <> " SUCCEED!"]
      ,
      Stop[TestEQN::EFailed, terms]
    ]
  ];

TestEQN::EFailed = "Testing `1` FAILED!";

Protect[TestEQN];

End[];

EndPackage[];
