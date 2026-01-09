Needs["CodeFormatter`"]

(* Format a single file with standard settings *)
FormatFile[fname_String] := Module[{file, formattedContent},
  file = File[fname];
  formattedContent = CodeFormat[file, "IndentationString" -> "  ", LineWidth -> 4096];
  Export[file, formattedContent, "String"]
];

(* Check if a file argument was provided *)
If[Length[$ScriptCommandLine] >= 1,
  (* Single file mode *)
  Module[{fname = First[$ScriptCommandLine]},
    If[!FileExistsQ[fname],
      System`Print["Error: File '" <> fname <> "' does not exist."];
      Exit[1]
    ];
    If[!StringMatchQ[fname, "*.wl"],
      System`Print["Error: File '" <> fname <> "' is not a .wl file."];
      Exit[1]
    ];
    System`Print["Formatting '" <> fname <> "' ..."];
    FormatFile[fname];
    System`Print["Done"];
  ],
  (* Batch mode: format all .wl files in predefined directories *)
  Module[{dirs = {"src", "test/Nmesh", "test/CarpetX"}, files},
    Do[
      System`Print["Formatting '.wl' files in " <> dir <> " ..."];
      files = FileNames["*.wl", dir];
      Do[FormatFile[fname], {fname, files}],
    {dir, dirs}];
    System`Print["Done"];
  ]
];
