(* ::Package:: *)

(* Generato`Writefile`, write content to a file *)

(* (c) Liwei Ji, 01/2025 *)

BeginPackage["Generato`Writefile`"];

Needs["Generato`Basic`"];

Needs["Generato`Component`"];

If[Environment["QUIET"] =!= "1",
  System`Print["------------------------------------------------------------"];
  System`Print["Package Generato`Writefile`, {2025, 1, 23}"];
  System`Print["------------------------------------------------------------"];
];

SetMainPrint::usage = "SetMainPrint[content] sets the content to be written as the main body of the output file.";

GetMainPrint::usage = "GetMainPrint[] returns and evaluates the content set by SetMainPrint.";

WriteToFile::usage = "WriteToFile[filename] writes the main content to the specified file with header and footer.";

ReplaceGFIndexName::usage = "ReplaceGFIndexName[filename, rule] applies string replacement rule to the contents of filename.";

Begin["`Private`"];

(* Data *)

$MainPrint = Null;

(* Function *)

GetMainPrint[] :=
  Return[$MainPrint];

SetAttributes[SetMainPrint, HoldAll];

SetMainPrint[content_] :=
  Module[{},
    $MainPrint := content
  ];

Protect[SetMainPrint];

WriteToFile[outputfile_] :=
  Module[{},
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
