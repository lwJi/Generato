(* ::Package:: *)

(* Generato`Writefile`, write content to a file *)

(* (c) Liwei Ji, 01/2025 *)

BeginPackage["Generato`Writefile`"];

Needs["Generato`Basic`"];

Needs["Generato`Component`"];

Print["------------------------------------------------------------"];

Print["Package Generato`Writefile`, {2025, 1, 23}"];

Print["------------------------------------------------------------"];

SetMainPrint::usage = "SetMainPrint[content] update the variable storing function printing more main content.";

GetMainPrint::usage = "GetMainPrint[content] return the variable storing function printing more main content.";

WriteToFile::usage = "WriteToFile[] writing contents to a file.";

ReplaceGFIndexName::usage = "ReplaceGFIndexName[] writing contents to a file.";

Begin["`Private`"];

(* Data *)

$MainPrint = Null;

(* Function *)

GetMainPrint[] :=
  Return[$MainPrint];

SetAttributes[SetMainPrint, HoldAll];
SetMainPrint[content_] := ($MainPrint := content)

Protect[SetMainPrint];

WriteToFile[outputfile_] :=
  Module[{},
    Print["Writing to \"", outputfile, "\"...\n"];
    If[FileExistsQ[outputfile],
      Print["\"", outputfile, "\" already exist, replacing it ...\n"];
      DeleteFile[outputfile]
    ];

    (* define pr *)
    filepointer = OpenAppend[outputfile];
    Global`pr[x_:""] :=
      Module[{},
        If[x == GetTempVariableType[],
          WriteString[filepointer, x]
          ,
          WriteLine[filepointer, x]
        ]
      ];

    (* print first few lines *)
    Global`pr["/* " <> FileNameTake[outputfile] <> " */"];
    Global`pr[
      "/* Produced with Generato" <>
        If[GetPrintDate[],
          " on " <> DateString[{"Month", "/", "Day", "/", "Year"}]
          ,
          ""
        ] <> " */"
    ];
    Global`pr[];

    If[GetPrintHeaderMacro[],
      Global`pr["#ifndef " <> StringReplace[ToUpperCase[FileNameTake[outputfile]], "." -> "_"]];
      Global`pr["#define " <> StringReplace[ToUpperCase[FileNameTake[outputfile]], "." -> "_"]];
      Global`pr[]
    ];

    GetMainPrint[];
    Global`pr[];

    If[GetPrintHeaderMacro[],
      Global`pr["#endif // #ifndef " <> StringReplace[ToUpperCase[FileNameTake[outputfile]], "." -> "_"]];
      Global`pr[]
    ];

    Global`pr["/* " <> FileNameTake[outputfile] <> " */"];
    Print["Done generating \"", outputfile, "\"\n"];
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
