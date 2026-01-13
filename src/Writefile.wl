(* ::Package:: *)

(* Generato`Writefile`, write content to a file *)

(* (c) Liwei Ji, 01/2025 *)

BeginPackage["Generato`Writefile`"];

Needs["Generato`Context`"];

Needs["Generato`Basic`"];

Needs["Generato`Component`"];

If[Environment["QUIET"] =!= "1",
  System`Print["------------------------------------------------------------"];
  System`Print["Package Generato`Writefile`, {2025, 1, 23}"];
  System`Print["------------------------------------------------------------"];
];

SetMainPrint::usage = "SetMainPrint[content] sets the content to be written as the main body of the output file.\nSetMainPrint[ctx, content] returns new context with content stored.";

GetMainPrint::usage = "GetMainPrint[] returns and evaluates the content set by SetMainPrint.\nGetMainPrint[ctx] returns the content from context.";

WriteToFile::usage = "WriteToFile[filename] writes the main content to the specified file with header and footer.\nWriteToFile[ctx, filename] writes using settings from context.";

ReplaceGFIndexName::usage = "ReplaceGFIndexName[filename, rule] applies string replacement rule to the contents of filename.";

Begin["`Private`"];

(* Function *)

(* Context-aware getter *)
GetMainPrint[ctx_Association] := GetCtx[ctx, "MainPrint"];

Protect[GetMainPrint];

(* SetMainPrint needs special handling:
   - Context version (2 args): evaluate ctx, hold content

   Use HoldAll and manually evaluate the ctx argument for 2-arg version *)

SetAttributes[SetMainPrint, HoldAll];

(* Context-aware setter - evaluate ctx explicitly, keep content held *)
SetMainPrint[ctx_, content_] /; AssociationQ[ctx] :=
  With[{evalCtx = ctx},
    SetCtx[evalCtx, "MainPrint", Hold[content]]
  ];

Protect[SetMainPrint];

(* Context-aware WriteToFile - uses settings from context *)
WriteToFile[ctx_Association, outputfile_String] :=
  Module[{filepointer, mainPrint, tempVarType, printDate, printHeaderMacro},
    (* Get settings from context *)
    mainPrint = GetMainPrint[ctx];
    tempVarType = GetTempVariableType[ctx];
    printDate = GetPrintDate[ctx];
    printHeaderMacro = GetPrintHeaderMacro[ctx];

    If[Environment["QUIET"] =!= "1",
      System`Print["Writing to \"", outputfile, "\"...\n"]
    ];
    If[FileExistsQ[outputfile],
      If[Environment["QUIET"] =!= "1",
        System`Print["\"", outputfile, "\" already exists, replacing it ...\n"]
      ];
      DeleteFile[outputfile]
    ];
    (* define pr *)
    filepointer = OpenAppend[outputfile];
    Block[{Global`pr},
      Global`pr[x_:""] :=
        Module[{},
          If[x == tempVarType,
            WriteString[filepointer, x]
            ,
            WriteLine[filepointer, x]
          ]
        ];
      (* print first few lines *)
      Global`pr["/* " <> FileNameTake[outputfile] <> " */"];
      Global`pr[
        "/* Produced with Generato" <>
          If[printDate,
            " on " <> DateString[{"Month", "/", "Day", "/", "Year"}]
            ,
            ""
          ] <> " */"
      ];
      Global`pr[];
      If[printHeaderMacro,
        Global`pr["#ifndef " <> StringReplace[ToUpperCase[FileNameTake[outputfile]], "." -> "_"]];
        Global`pr["#define " <> StringReplace[ToUpperCase[FileNameTake[outputfile]], "." -> "_"]];
        Global`pr[]
      ];
      (* Evaluate the main print content *)
      (* Priority: context-stored Hold, then global $MainPrint[] *)
      Which[
        Head[mainPrint] === Hold,
          ReleaseHold[mainPrint],
        mainPrint =!= Null,
          mainPrint,
        Length[DownValues[Global`$MainPrint]] > 0,
          Global`$MainPrint[],
        True,
          Null  (* No main print defined *)
      ];
      Global`pr[];
      If[printHeaderMacro,
        Global`pr["#endif // #ifndef " <> StringReplace[ToUpperCase[FileNameTake[outputfile]], "." -> "_"]];
        Global`pr[]
      ];
      Global`pr["/* " <> FileNameTake[outputfile] <> " */"];
    ];
    If[Environment["QUIET"] =!= "1",
      System`Print["Done generating \"", outputfile, "\"\n"]
    ];
    Close[filepointer]
  ];

Protect[WriteToFile];

ReplaceGFIndexName[outputfile_, replacerule_] :=
  Module[{content},
    content = Import[outputfile, "Text"];
    Export[outputfile, StringReplace[content, replacerule] <> "\n", "Text"];
  ];

Protect[ReplaceGFIndexName];

End[];

EndPackage[];
