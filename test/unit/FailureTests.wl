(* ::Package:: *)

(* FailureTests.wl *)
(* Unit tests for Generato`Failure`, and for the error sites that used to return a value or abort *)

If[Environment["QUIET"] =!= "1", Print["Loading FailureTests.wl..."]];

(* Load Generato which includes Failure *)
If[!MemberQ[$Packages, "Generato`Failure`"],
  Needs["xAct`xCoba`", FileNameJoin[{Environment["GENERATO"], "src/Generato.wl"}]]
];

SetPVerbose[False];
SetPrintDate[False];

(* Helper to reset context to defaults *)
ResetContext[] := ($CurrentContext = $ContextDefaults);

failureTest::EWithText = "Stopped with `1` and `2`.";

(* ========================================= *)
(* Test: Stop *)
(* ========================================= *)

AppendTo[$AllTests,
  VerificationTest[
    (* Stop issues the message and throws $Failed to the tag; what follows does not run *)
    failureReached = False;
    {
      Catch[
        Stop[failureTest::EWithText, 1, "two"];
        failureReached = True;
        "NoThrow",
        GeneratoFailure
      ],
      failureReached
    },
    {$Failed, False},
    {failureTest::EWithText},
    TestID -> "Stop-MessageThenTaggedThrow"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* a message name that has text evaluates to it, so a name left unevaluated here has none *)
    Select[
      Hold[
        SetPVerbose::EInvalidValue,
        WithMode::EInvalidValue,
        GetDefaultManifold::NoManifolds,
        IsDefined::EType,
        RHSOf::Eargs,
        Generato`Basic`Private`SetEQNDetail::Evar,
        Generato`Basic`Private`SetEQNDetail::Evarrhs,
        Generato`Basic`Private`SetEQNDelayedDetail::Evar,
        ParseVar::EVar,
        ParseVar::EKey,
        ParseVar::EShape,
        ParseVar::ETensorType,
        ParseVar::ESymmetryType,
        DefineTensor::EExists,
        DefineTensor::EBase,
        DefineTensor::EDerivativeOf,
        DefineTensor::ETimeDerivativeOf,
        RecordBinding::EState,
        GridTensors::EBound,
        TileTensors::EBound,
        TempTensors::EBound,
        VariableRecord::EUnbound,
        ComponentRecord::EComponent,
        RequireChart::EChart,
        ResolveBackend::EUnknown,
        ResolveBackend::EFile,
        ResolveBackend::ECycle,
        Render::EKey,
        Render::ELines,
        Render::EStatement,
        CurrentBackend::ENoWrite,
        pr::ENoWrite,
        WriteToFile::EThrow,
        WriteToFile::EBackend,
        BaseComponent::EComponent,
        RequireRelation::EMissing,
        GridTensors::EDeclared,
        TileTensors::EDeclared,
        TempTensors::EDeclared,
        PrintEquations::EMode,
        PrintEquations::ESuffixName,
        PrintComponentEquation::EMode,
        PrintInitializations::EMode,
        PrintInitializations::ETensorType,
        Generate::EArguments,
        Generate::ENode,
        Generate::EVar,
        Generate::EUnbound,
        Generate::EComponent,
        Generate::EBase,
        Generate::EMode,
        Generate::EStorage,
        Generate::ERelation,
        Generate::EEquation,
        Generate::EProblems,
        Generate::ENoRule,
        TestEQN::EFailed
      ],
      Function[name, !StringQ[name], HoldFirst]
    ],
    Hold[],
    TestID -> "Stop-EveryMessageRaisedInSrcHasText"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* a Catch without the tag does not catch it, so a script's own Catch cannot hide a failure *)
    Catch[
      Catch[Stop[failureTest::EWithText, 1, 2]; "NoThrow"];
      "InnerCatchReturned",
      GeneratoFailure
    ],
    $Failed,
    {failureTest::EWithText},
    TestID -> "Stop-PassesAnUntaggedCatch"
  ]
];

(* ========================================= *)
(* Test: sites that used to return a value or abort *)
(* ========================================= *)

AppendTo[$AllTests,
  VerificationTest[
    (* a failed check used to print and abort *)
    Catch[
      TestEQN[False, "the identity"];
      "NoThrow",
      GeneratoFailure
    ],
    $Failed,
    {TestEQN::EFailed},
    TestID -> "TestEQN-False-Stops"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* WithMode restores its keys when the body stops *)
    ResetContext[];
    {
      Catch[
        WithMode[{"SuffixName" -> "inside"}, Stop[failureTest::EWithText, 1, 2]],
        GeneratoFailure
      ],
      GetSuffixName[]
    },
    {$Failed, ""},
    {failureTest::EWithText},
    TestID -> "WithMode-RestoresWhenBodyStops"
  ]
];

(* ========================================= *)
(* Cleanup *)
(* ========================================= *)

ResetContext[];

If[Environment["QUIET"] =!= "1", Print["FailureTests.wl completed."]];
