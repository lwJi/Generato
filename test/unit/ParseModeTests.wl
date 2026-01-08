(* ::Package:: *)

(* ParseModeTests.wl *)
(* Unit tests for Generato`ParseMode module *)

If[Environment["QUIET"] =!= "1", Print["Loading ParseModeTests.wl..."]];

(* Load Generato *)
Needs["xAct`xCoba`", FileNameJoin[{Environment["GENERATO"], "src/Generato.wl"}]];

SetPVerbose[False];
SetPrintDate[False];

(* ========================================= *)
(* Test: ParseMode (Level 1) *)
(* ========================================= *)

AppendTo[$AllTests,
  VerificationTest[
    CleanParseMode[];
    SetParseMode[<|SetComp -> True|>];
    GetParseMode[SetComp],
    True,
    TestID -> "ParseMode-SetComp-True"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    CleanParseMode[];
    SetParseMode[<|PrintComp -> True|>];
    GetParseMode[PrintComp],
    True,
    TestID -> "ParseMode-PrintComp-True"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    CleanParseMode[];
    GetParseMode[NonExistentKey],
    False,
    TestID -> "ParseMode-NonExistentKey-ReturnsFalse"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    CleanParseMode[];
    SetParseMode[<|SetComp -> True, PrintComp -> True|>];
    SetParseModeAllToFalse[];
    {GetParseMode[SetComp], GetParseMode[PrintComp]},
    {False, False},
    TestID -> "ParseMode-SetAllToFalse"
  ]
];

(* ========================================= *)
(* Test: ParseSetCompMode (Level 2) *)
(* ========================================= *)

AppendTo[$AllTests,
  VerificationTest[
    CleanParseSetCompMode[];
    SetParseSetCompMode[<|IndependentVarlistIndex -> True|>];
    GetParseSetCompMode[IndependentVarlistIndex],
    True,
    TestID -> "ParseSetCompMode-IndependentVarlistIndex-True"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    CleanParseSetCompMode[];
    SetParseSetCompMode[<|WithoutGridPointIndex -> True|>];
    GetParseSetCompMode[WithoutGridPointIndex],
    True,
    TestID -> "ParseSetCompMode-WithoutGridPointIndex-True"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    CleanParseSetCompMode[];
    SetParseSetCompMode[<|UseTilePointIndex -> True|>];
    GetParseSetCompMode[UseTilePointIndex],
    True,
    TestID -> "ParseSetCompMode-UseTilePointIndex-True"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    CleanParseSetCompMode[];
    GetParseSetCompMode[NonExistentKey],
    False,
    TestID -> "ParseSetCompMode-NonExistentKey-ReturnsFalse"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    CleanParseSetCompMode[];
    SetParseSetCompMode[<|IndependentVarlistIndex -> True, WithoutGridPointIndex -> True, UseTilePointIndex -> True|>];
    SetParseSetCompModeAllToFalse[];
    {GetParseSetCompMode[IndependentVarlistIndex], GetParseSetCompMode[WithoutGridPointIndex], GetParseSetCompMode[UseTilePointIndex]},
    {False, False, False},
    TestID -> "ParseSetCompMode-SetAllToFalse"
  ]
];

(* ========================================= *)
(* Test: ParsePrintCompMode (Level 3) *)
(* ========================================= *)

AppendTo[$AllTests,
  VerificationTest[
    CleanParsePrintCompMode[];
    SetParsePrintCompMode[<|Initializations -> True|>];
    GetParsePrintCompMode[Initializations],
    True,
    TestID -> "ParsePrintCompMode-Initializations-True"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    CleanParsePrintCompMode[];
    SetParsePrintCompMode[<|Equations -> True|>];
    GetParsePrintCompMode[Equations],
    True,
    TestID -> "ParsePrintCompMode-Equations-True"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    CleanParsePrintCompMode[];
    GetParsePrintCompMode[NonExistentKey],
    False,
    TestID -> "ParsePrintCompMode-NonExistentKey-ReturnsFalse"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    CleanParsePrintCompMode[];
    SetParsePrintCompMode[<|Initializations -> True, Equations -> True|>];
    SetParsePrintCompModeAllToFalse[];
    {GetParsePrintCompMode[Initializations], GetParsePrintCompMode[Equations]},
    {False, False},
    TestID -> "ParsePrintCompMode-SetAllToFalse"
  ]
];

(* ========================================= *)
(* Test: ParsePrintCompInitMode (Level 4) *)
(* ========================================= *)

AppendTo[$AllTests,
  VerificationTest[
    CleanParsePrintCompInitMode[];
    SetParsePrintCompInitMode[<|"TestKey" -> "TestValue"|>];
    GetParsePrintCompInitMode["TestKey"],
    "TestValue",
    TestID -> "ParsePrintCompInitMode-SetGet"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    CleanParsePrintCompInitMode[];
    GetParsePrintCompInitMode["NonExistentKey"],
    False,
    TestID -> "ParsePrintCompInitMode-NonExistentKey-ReturnsFalse"
  ]
];

(* ========================================= *)
(* Test: ParsePrintCompEQNMode (Level 5) *)
(* ========================================= *)

AppendTo[$AllTests,
  VerificationTest[
    CleanParsePrintCompEQNMode[];
    SetParsePrintCompEQNMode[<|"TestKey" -> "TestValue"|>];
    GetParsePrintCompEQNMode["TestKey"],
    "TestValue",
    TestID -> "ParsePrintCompEQNMode-SetGet"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    CleanParsePrintCompEQNMode[];
    GetParsePrintCompEQNMode["NonExistentKey"],
    False,
    TestID -> "ParsePrintCompEQNMode-NonExistentKey-ReturnsFalse"
  ]
];

(* ========================================= *)
(* Test: ParsePrintCompInitTensorType (Level 6) *)
(* ========================================= *)

AppendTo[$AllTests,
  VerificationTest[
    CleanParsePrintCompInitTensorType[];
    SetParsePrintCompInitTensorType[<|"TestKey" -> "TestValue"|>];
    GetParsePrintCompInitTensorType["TestKey"],
    "TestValue",
    TestID -> "ParsePrintCompInitTensorType-SetGet"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    CleanParsePrintCompInitTensorType[];
    GetParsePrintCompInitTensorType["NonExistentKey"],
    False,
    TestID -> "ParsePrintCompInitTensorType-NonExistentKey-ReturnsFalse"
  ]
];

(* ========================================= *)
(* Test: ParsePrintCompInitStorageType (Level 7) *)
(* ========================================= *)

AppendTo[$AllTests,
  VerificationTest[
    CleanParsePrintCompInitStorageType[];
    SetParsePrintCompInitStorageType[<|"TestKey" -> "TestValue"|>];
    GetParsePrintCompInitStorageType["TestKey"],
    "TestValue",
    TestID -> "ParsePrintCompInitStorageType-SetGet"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    CleanParsePrintCompInitStorageType[];
    GetParsePrintCompInitStorageType["NonExistentKey"],
    False,
    TestID -> "ParsePrintCompInitStorageType-NonExistentKey-ReturnsFalse"
  ]
];

(* ========================================= *)
(* Cleanup *)
(* ========================================= *)

CleanParseMode[];
CleanParseSetCompMode[];
CleanParsePrintCompMode[];
CleanParsePrintCompInitMode[];
CleanParsePrintCompEQNMode[];
CleanParsePrintCompInitTensorType[];
CleanParsePrintCompInitStorageType[];

If[Environment["QUIET"] =!= "1", Print["ParseModeTests.wl completed."]];
