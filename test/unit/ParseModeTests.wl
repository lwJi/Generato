(* ::Package:: *)

(* ParseModeTests.wl *)
(* Unit tests for Generato`ParseMode module *)

Print["Loading ParseModeTests.wl..."];

(* Load Generato *)
Needs["xAct`xCoba`", FileNameJoin[{Environment["GENERATO"], "src/Generato.wl"}]];

SetPVerbose[False];
SetPrintDate[False];

(* ========================================= *)
(* Test: ParseMode (Level 1) *)
(* ========================================= *)

VerificationTest[
  CleanParseMode[];
  SetParseMode[<|SetComp -> True|>];
  GetParseMode[SetComp],
  True,
  TestID -> "ParseMode-SetComp-True"
];

VerificationTest[
  CleanParseMode[];
  SetParseMode[<|PrintComp -> True|>];
  GetParseMode[PrintComp],
  True,
  TestID -> "ParseMode-PrintComp-True"
];

VerificationTest[
  CleanParseMode[];
  GetParseMode[NonExistentKey],
  False,
  TestID -> "ParseMode-NonExistentKey-ReturnsFalse"
];

VerificationTest[
  CleanParseMode[];
  SetParseMode[<|SetComp -> True, PrintComp -> True|>];
  SetParseModeAllToFalse[];
  {GetParseMode[SetComp], GetParseMode[PrintComp]},
  {False, False},
  TestID -> "ParseMode-SetAllToFalse"
];

(* ========================================= *)
(* Test: ParseSetCompMode (Level 2) *)
(* ========================================= *)

VerificationTest[
  CleanParseSetCompMode[];
  SetParseSetCompMode[<|IndependentVarlistIndex -> True|>];
  GetParseSetCompMode[IndependentVarlistIndex],
  True,
  TestID -> "ParseSetCompMode-IndependentVarlistIndex-True"
];

VerificationTest[
  CleanParseSetCompMode[];
  SetParseSetCompMode[<|WithoutGridPointIndex -> True|>];
  GetParseSetCompMode[WithoutGridPointIndex],
  True,
  TestID -> "ParseSetCompMode-WithoutGridPointIndex-True"
];

VerificationTest[
  CleanParseSetCompMode[];
  SetParseSetCompMode[<|UseTilePointIndex -> True|>];
  GetParseSetCompMode[UseTilePointIndex],
  True,
  TestID -> "ParseSetCompMode-UseTilePointIndex-True"
];

VerificationTest[
  CleanParseSetCompMode[];
  GetParseSetCompMode[NonExistentKey],
  False,
  TestID -> "ParseSetCompMode-NonExistentKey-ReturnsFalse"
];

VerificationTest[
  CleanParseSetCompMode[];
  SetParseSetCompMode[<|IndependentVarlistIndex -> True, WithoutGridPointIndex -> True, UseTilePointIndex -> True|>];
  SetParseSetCompModeAllToFalse[];
  {GetParseSetCompMode[IndependentVarlistIndex], GetParseSetCompMode[WithoutGridPointIndex], GetParseSetCompMode[UseTilePointIndex]},
  {False, False, False},
  TestID -> "ParseSetCompMode-SetAllToFalse"
];

(* ========================================= *)
(* Test: ParsePrintCompMode (Level 3) *)
(* ========================================= *)

VerificationTest[
  CleanParsePrintCompMode[];
  SetParsePrintCompMode[<|Initializations -> True|>];
  GetParsePrintCompMode[Initializations],
  True,
  TestID -> "ParsePrintCompMode-Initializations-True"
];

VerificationTest[
  CleanParsePrintCompMode[];
  SetParsePrintCompMode[<|Equations -> True|>];
  GetParsePrintCompMode[Equations],
  True,
  TestID -> "ParsePrintCompMode-Equations-True"
];

VerificationTest[
  CleanParsePrintCompMode[];
  GetParsePrintCompMode[NonExistentKey],
  False,
  TestID -> "ParsePrintCompMode-NonExistentKey-ReturnsFalse"
];

VerificationTest[
  CleanParsePrintCompMode[];
  SetParsePrintCompMode[<|Initializations -> True, Equations -> True|>];
  SetParsePrintCompModeAllToFalse[];
  {GetParsePrintCompMode[Initializations], GetParsePrintCompMode[Equations]},
  {False, False},
  TestID -> "ParsePrintCompMode-SetAllToFalse"
];

(* ========================================= *)
(* Test: ParsePrintCompInitMode (Level 4) *)
(* ========================================= *)

VerificationTest[
  CleanParsePrintCompInitMode[];
  SetParsePrintCompInitMode[<|"TestKey" -> "TestValue"|>];
  GetParsePrintCompInitMode["TestKey"],
  "TestValue",
  TestID -> "ParsePrintCompInitMode-SetGet"
];

VerificationTest[
  CleanParsePrintCompInitMode[];
  GetParsePrintCompInitMode["NonExistentKey"],
  False,
  TestID -> "ParsePrintCompInitMode-NonExistentKey-ReturnsFalse"
];

(* ========================================= *)
(* Test: ParsePrintCompEQNMode (Level 5) *)
(* ========================================= *)

VerificationTest[
  CleanParsePrintCompEQNMode[];
  SetParsePrintCompEQNMode[<|"TestKey" -> "TestValue"|>];
  GetParsePrintCompEQNMode["TestKey"],
  "TestValue",
  TestID -> "ParsePrintCompEQNMode-SetGet"
];

VerificationTest[
  CleanParsePrintCompEQNMode[];
  GetParsePrintCompEQNMode["NonExistentKey"],
  False,
  TestID -> "ParsePrintCompEQNMode-NonExistentKey-ReturnsFalse"
];

(* ========================================= *)
(* Test: ParsePrintCompInitTensorType (Level 6) *)
(* ========================================= *)

VerificationTest[
  CleanParsePrintCompInitTensorType[];
  SetParsePrintCompInitTensorType[<|"TestKey" -> "TestValue"|>];
  GetParsePrintCompInitTensorType["TestKey"],
  "TestValue",
  TestID -> "ParsePrintCompInitTensorType-SetGet"
];

VerificationTest[
  CleanParsePrintCompInitTensorType[];
  GetParsePrintCompInitTensorType["NonExistentKey"],
  False,
  TestID -> "ParsePrintCompInitTensorType-NonExistentKey-ReturnsFalse"
];

(* ========================================= *)
(* Test: ParsePrintCompInitStorageType (Level 7) *)
(* ========================================= *)

VerificationTest[
  CleanParsePrintCompInitStorageType[];
  SetParsePrintCompInitStorageType[<|"TestKey" -> "TestValue"|>];
  GetParsePrintCompInitStorageType["TestKey"],
  "TestValue",
  TestID -> "ParsePrintCompInitStorageType-SetGet"
];

VerificationTest[
  CleanParsePrintCompInitStorageType[];
  GetParsePrintCompInitStorageType["NonExistentKey"],
  False,
  TestID -> "ParsePrintCompInitStorageType-NonExistentKey-ReturnsFalse"
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

Print["ParseModeTests.wl completed."];
