(* ::Package:: *)

(* Driver.wl, what the Generato wrapper runs: wolframscript -script Driver.wl script.wl *)

(* (c) Liwei Ji, 11/2026 *)

(*
  The driver does the two things a script cannot do for itself.
  - It owns the output path before the run. A file an earlier run generated
    is removed before the script starts, and WriteToFile in Emit.wl writes
    the new one only after its last line is built. So a run that fails leaves
    no file, and no failure path has to delete one.
  - It sets the exit status. wolframscript exits 0 when a script ends in an
    uncaught throw or is aborted, so the driver catches every such ending
    and exits 1. Nothing else in src/ ends the kernel.
  The script must find the kernel as it would without the driver. Every
  symbol here is in the context Generato`Driver`, which is not put on the
  context path, and no other file of src/ is loaded here: the script loads
  them itself.
*)

Begin["Generato`Driver`"];

(* A generated file is named after its script and has one of these extensions. *)
$OutputExtensions = {".hxx", ".cxx", ".c", ".h"};

(* The second line of a generated file starts with this text: the header WriteToFile prints in Emit.wl. *)
$GeneratedMark = "/* Produced with Generato";

GeneratedFileQ[file_String] :=
  FileType[file] === File &&
  MatchQ[Quiet[ReadList[file, "String", 2]], {_String, _String?(StringStartsQ[$GeneratedMark])}];

(*
  ClearStaleOutput - removes what an earlier run of the script generated.
  The name is a convention: a script that names its output differently still
  gets a correct file, only its stale copy stays after a failure. A file
  without the mark is hand-written and is left alone.
*)
ClearStaleOutput[script_String] :=
  Scan[
    If[GeneratedFileQ[#], DeleteFile[#]] &,
    FileNameJoin[{Directory[], FileBaseName[script] <> #}] & /@ $OutputExtensions
  ];

(* A thrown value may be a whole expression; the report shows where it starts. *)
ShortText[expr_] :=
  Module[{text = ToString[expr, InputForm]},
    If[StringLength[text] > 200, StringTake[text, 200] <> " ...", text]
  ];

(*
  ReportEnding - says how the run ended, when no message has said it yet.
  A run that Stop ended has printed its message already.
*)
ReportEnding[script_, returned[ranToEnd]] := Null;

ReportEnding[script_, returned[thrown[_, Generato`Failure`GeneratoFailure]]] := Null;

ReportEnding[script_, returned[thrown[value_, tag_]]] :=
  Print["Generato: ", script, " stopped at an uncaught Throw[", ShortText[value], ", ", ShortText[tag], "]."];

ReportEnding[script_, returned[aborted]] :=
  Print["Generato: ", script, " was aborted."];

ReportEnding[script_, returned[unreadable]] :=
  Print["Generato: ", script, " stopped at a syntax error."];

ReportEnding[script_, value_] :=
  Print["Generato: ", script, " stopped at an uncaught Throw[", ShortText[value], "]."];

(*
  RunScript - runs the script and returns True if it ran to its end.
  Every other ending is caught here:
  - a throw with a tag, which is how Stop ends a run, by the inner Catch;
  - a throw without a tag, which is how an xAct error ends one, by the outer
    Catch, since a Catch that names a tag pattern lets it pass;
  - an abort, by CheckAbort;
  - a syntax error, after which Get returns as if the file had ended there,
    by Check.
*)
RunScript[script_String] :=
  Module[{ending},
    ClearStaleOutput[script];
    ending =
      Catch[
        returned[
          Catch[
            CheckAbort[
              Check[
                Get[ExpandFileName[script]];
                ranToEnd
                ,
                unreadable
                ,
                {Syntax::sntx, Syntax::sntxb, Syntax::sntxf, Syntax::sntxi, Syntax::sntue}
              ]
              ,
              aborted
            ]
            ,
            _
            ,
            thrown
          ]
        ]
      ];
    ReportEnding[script, ending];
    ending === returned[ranToEnd]
  ];

End[];

If[Length[$ScriptCommandLine] =!= 2,
  Print["Generato: Driver.wl takes one script: wolframscript -script Driver.wl script.wl"];
  Exit[1]
];

If[!Generato`Driver`RunScript[$ScriptCommandLine[[2]]],
  Exit[1]
];
