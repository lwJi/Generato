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

WriteToFile::usage = "WriteToFile[filename] writes the main content to the specified file with header and footer.";

ReplaceGFIndexName::usage = "ReplaceGFIndexName[filename, rule] applies string replacement rule to the contents of filename.";

Begin["`Private`"];

(* WriteToFile - uses global getters *)
WriteToFile[outputfile_String] :=
  Module[{filepointer, headerGuard},
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
    headerGuard =
      StringReplace[
        ToUpperCase[FileNameTake[outputfile]],
        Except[LetterCharacter | DigitCharacter] -> "_"
      ];
    Block[{Global`pr},
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
        Global`pr["#ifndef " <> headerGuard];
        Global`pr["#define " <> headerGuard];
        Global`pr[]
      ];
      (* GetMainPrint[] evaluates the held main print content *)
      GetMainPrint[];
      Global`pr[];
      If[GetPrintHeaderMacro[],
        Global`pr["#endif // #ifndef " <> headerGuard];
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
