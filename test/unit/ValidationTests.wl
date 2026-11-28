(* ::Package:: *)

(* ValidationTests.wl *)
(* Validation Tests for Basic.wl and Component.wl setters *)

If[Environment["QUIET"] =!= "1", Print["Loading ValidationTests.wl..."]];

(* Load Generato *)
Needs["xAct`xCoba`", FileNameJoin[{Environment["GENERATO"], "src/Generato.wl"}]];

SetPVerbose[False];
SetPrintDate[False];

(* Test helper *)
ResetContext[] := ($CurrentContext = $ContextDefaults);

(* ===== Basic.wl Setter Validation Tests ===== *)

(* Boolean setters should reject non-boolean values *)
AppendTo[$AllTests,
  VerificationTest[
    ResetContext[];
    Catch[
      SetCheckInputEquations["invalid"];
      "NoThrow",
      GeneratoFailure
    ],
    $Failed,  (* Stop issues the message, then throws $Failed to the tag *)
    {SetCheckInputEquations::EInvalidValue},
    TestID -> "SetCheckInputEquations rejects string"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    ResetContext[];
    SetCheckInputEquations[True];
    GetCheckInputEquations[],
    True,
    TestID -> "SetCheckInputEquations accepts True"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    ResetContext[];
    Catch[
      SetPVerbose[1];
      "NoThrow",
      GeneratoFailure
    ],
    $Failed,
    {SetPVerbose::EInvalidValue},
    TestID -> "SetPVerbose rejects integer"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    ResetContext[];
    SetPVerbose[False];
    GetPVerbose[],
    False,
    TestID -> "SetPVerbose accepts False"
  ]
];

(* String setters should reject non-string values *)
AppendTo[$AllTests,
  VerificationTest[
    ResetContext[];
    Catch[
      SetGridPointIndex[123];
      "NoThrow",
      GeneratoFailure
    ],
    $Failed,
    {SetGridPointIndex::EInvalidValue},
    TestID -> "SetGridPointIndex rejects integer"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    ResetContext[];
    SetGridPointIndex["[ijk]"];
    GetGridPointIndex[],
    "[ijk]",
    TestID -> "SetGridPointIndex accepts string"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    ResetContext[];
    Catch[
      SetOutputFile[True];
      "NoThrow",
      GeneratoFailure
    ],
    $Failed,
    {SetOutputFile::EInvalidValue},
    TestID -> "SetOutputFile rejects boolean"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    ResetContext[];
    SetOutputFile["myfile.cxx"];
    GetOutputFile[],
    "myfile.cxx",
    TestID -> "SetOutputFile accepts string"
  ]
];

(* ===== Component.wl Setter Validation Tests ===== *)

AppendTo[$AllTests,
  VerificationTest[
    ResetContext[];
    Catch[
      SetSimplifyEquation["yes"];
      "NoThrow",
      GeneratoFailure
    ],
    $Failed,
    {SetSimplifyEquation::EInvalidValue},
    TestID -> "SetSimplifyEquation rejects string"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    ResetContext[];
    SetSimplifyEquation[False];
    GetSimplifyEquation[],
    False,
    TestID -> "SetSimplifyEquation accepts False"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    ResetContext[];
    Catch[
      SetTempVariableType[123];
      "NoThrow",
      GeneratoFailure
    ],
    $Failed,
    {SetTempVariableType::EInvalidValue},
    TestID -> "SetTempVariableType rejects integer"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    ResetContext[];
    SetTempVariableType["float"];
    GetTempVariableType[],
    "float",
    TestID -> "SetTempVariableType accepts string"
  ]
];

(* ===== Cleanup ===== *)

ResetContext[];

If[Environment["QUIET"] =!= "1", Print["ValidationTests.wl completed."]];
