(* ::Package:: *)

(* BasicTests.wl *)
(* Unit tests for Generato`Basic module *)

If[Environment["QUIET"] =!= "1", Print["Loading BasicTests.wl..."]];

(* Load Generato *)
Needs["xAct`xCoba`", FileNameJoin[{Environment["GENERATO"], "src/Generato.wl"}]];

(* Suppress verbose output during tests *)
$CurrentContext = SetPVerbose[$CurrentContext, False];
$CurrentContext = SetPrintDate[$CurrentContext, False];

(* ========================================= *)
(* Test: Global Configuration Functions *)
(* ========================================= *)

AppendTo[$AllTests,
  VerificationTest[
    $CurrentContext = SetPVerbose[$CurrentContext, True];
    (* PVerbose should be settable without error *)
    True,
    True,
    TestID -> "SetPVerbose-NoError"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    $CurrentContext = SetPrintDate[$CurrentContext, False];
    GetPrintDate[$CurrentContext],
    False,
    TestID -> "GetSetPrintDate-ReturnsFalse"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    $CurrentContext = SetGridPointIndex[$CurrentContext, "[[ijk]]"];
    GetGridPointIndex[$CurrentContext],
    "[[ijk]]",
    TestID -> "GetSetGridPointIndex-ReturnsValue"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    $CurrentContext = SetTilePointIndex[$CurrentContext, "(ti,tj,tk)"];
    GetTilePointIndex[$CurrentContext],
    "(ti,tj,tk)",
    TestID -> "GetSetTilePointIndex-ReturnsValue"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    $CurrentContext = SetOutputFile[$CurrentContext, "/tmp/test_output.c"];
    GetOutputFile[$CurrentContext],
    "/tmp/test_output.c",
    TestID -> "GetSetOutputFile-ReturnsPath"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    $CurrentContext = SetProject[$CurrentContext, "TestProject"];
    GetProject[$CurrentContext],
    "TestProject",
    TestID -> "GetSetProject-ReturnsValue"
  ]
];

(* ========================================= *)
(* Test: SetEQN and RHSOf *)
(* ========================================= *)

(* Setup a simple manifold for equation tests *)
If[!MemberQ[$Manifolds, TestM3],
  DefManifold[TestM3, 3, IndexRange[a, z]];
  DefChart[testCart, TestM3, {1, 2, 3}, {TX[], TY[], TZ[]}, ChartColor -> Blue];
  DefMetric[1, testEuclid[-i, -j], testCD];
];

(* Define test tensors *)
testVarlist = GridTensors[{testVec[i], PrintAs -> "tv"}];

AppendTo[$AllTests,
  VerificationTest[
    (* SetEQN should set an equation without error *)
    SetEQN[testVec[i_], 2 testVec[i]];
    True,
    True,
    TestID -> "SetEQN-Basic-NoError"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* RHSOf should return a symbol referencing the tensor's RHS *)
    rhs = RHSOf[testVec];
    Head[rhs] === Symbol && StringContainsQ[ToString[rhs], "RHS"],
    True,
    TestID -> "RHSOf-ReturnsValue"
  ]
];

(* ========================================= *)
(* Test: Suffix handling *)
(* ========================================= *)

AppendTo[$AllTests,
  VerificationTest[
    SetEQN[{SuffixName -> "testSuffix"}, testVec[i_], 3 testVec[i]];
    True,
    True,
    TestID -> "SetEQN-WithSuffix-NoError"
  ]
];

(* ========================================= *)
(* Test: GetCheckInputEquations / SetCheckInputEquations *)
(* ========================================= *)

AppendTo[$AllTests,
  VerificationTest[
    $CurrentContext = SetCheckInputEquations[$CurrentContext, True];
    GetCheckInputEquations[$CurrentContext],
    True,
    TestID -> "GetSetCheckInputEquations-True"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    $CurrentContext = SetCheckInputEquations[$CurrentContext, False];
    GetCheckInputEquations[$CurrentContext],
    False,
    TestID -> "GetSetCheckInputEquations-False"
  ]
];

(* ========================================= *)
(* Test: GetPrintHeaderMacro / SetPrintHeaderMacro *)
(* ========================================= *)

AppendTo[$AllTests,
  VerificationTest[
    $CurrentContext = SetPrintHeaderMacro[$CurrentContext, True];
    GetPrintHeaderMacro[$CurrentContext],
    True,
    TestID -> "GetSetPrintHeaderMacro-True"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    $CurrentContext = SetPrintHeaderMacro[$CurrentContext, False];
    GetPrintHeaderMacro[$CurrentContext],
    False,
    TestID -> "GetSetPrintHeaderMacro-False"
  ]
];

(* ========================================= *)
(* Test: GetSuffixUnprotected / SetSuffixUnprotected *)
(* ========================================= *)

AppendTo[$AllTests,
  VerificationTest[
    $CurrentContext = SetSuffixUnprotected[$CurrentContext, "_test"];
    GetSuffixUnprotected[$CurrentContext],
    "_test",
    TestID -> "GetSetSuffixUnprotected-String"
  ]
];

(* ========================================= *)
(* Test: GetDefaultManifold *)
(* ========================================= *)

AppendTo[$AllTests,
  VerificationTest[
    (* GetDefaultManifold should return a symbol when manifolds exist *)
    Module[{result = GetDefaultManifold[]},
      Head[result] === Symbol && MemberQ[$Manifolds, result]
    ],
    True,
    TestID -> "GetDefaultManifold-ReturnsFirstManifold"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* GetDefaultManifold should return the first manifold in $Manifolds *)
    GetDefaultManifold[] === $Manifolds[[1]],
    True,
    TestID -> "GetDefaultManifold-ReturnsFirstInList"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* GetDefaultManifold should return $Failed when no manifolds exist *)
    Module[{savedManifolds = $Manifolds, result},
      Block[{$Manifolds = {}},
        result = Quiet[GetDefaultManifold[]];
        result === $Failed
      ]
    ],
    True,
    TestID -> "GetDefaultManifold-EmptyManifolds-ReturnsFailed"
  ]
];

(* ========================================= *)
(* Test: GetDefaultChart *)
(* ========================================= *)

AppendTo[$AllTests,
  VerificationTest[
    (* GetDefaultChart should return a symbol *)
    Head[GetDefaultChart[]] === Symbol || GetDefaultChart[] === Null,
    True,
    TestID -> "GetDefaultChart-ReturnsSymbolOrNull"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* GetDefaultChart should return $Failed when no manifolds exist *)
    Module[{savedManifolds = $Manifolds, result},
      Block[{$Manifolds = {}},
        result = Quiet[GetDefaultChart[]];
        result === $Failed
      ]
    ],
    True,
    TestID -> "GetDefaultChart-EmptyManifolds-ReturnsFailed"
  ]
];

(* ========================================= *)
(* Test: GetDim *)
(* ========================================= *)

AppendTo[$AllTests,
  VerificationTest[
    (* GetDim should return a valid dimension (1-10) *)
    Module[{dim = GetDim[]},
      IntegerQ[dim] && dim >= 1 && dim <= 10
    ],
    True,
    TestID -> "GetDim-ReturnsValidDimension"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* GetDim should return $Failed when no manifolds exist *)
    Module[{savedManifolds = $Manifolds, result},
      Block[{$Manifolds = {}},
        result = Quiet[GetDim[]];
        result === $Failed
      ]
    ],
    True,
    TestID -> "GetDim-EmptyManifolds-ReturnsFailed"
  ]
];

(* Reset to clean state - all modified variables *)
$CurrentContext = SetPVerbose[$CurrentContext, False];
$CurrentContext = SetPrintDate[$CurrentContext, False];
$CurrentContext = SetGridPointIndex[$CurrentContext, ""];
$CurrentContext = SetTilePointIndex[$CurrentContext, ""];
$CurrentContext = SetOutputFile[$CurrentContext, ""];
$CurrentContext = SetProject[$CurrentContext, ""];
$CurrentContext = SetCheckInputEquations[$CurrentContext, True];
$CurrentContext = SetPrintHeaderMacro[$CurrentContext, True];
$CurrentContext = SetSuffixUnprotected[$CurrentContext, ""];

If[Environment["QUIET"] =!= "1", Print["BasicTests.wl completed."]];
