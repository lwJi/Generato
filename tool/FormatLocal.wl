Needs["CodeFormatter`"]

Module[{dirs = {"./"}},
  Do[
    Print["Formatting '.wl' files in "<>dir<>" ..."];
    files = FileNames["*.wl", dir];
    Do[
      file = File[fname];
      formattedContent = CodeFormat[Import[file, "String"], "IndentationString" -> "  ", LineWidth -> 8192];
      Export[file, formattedContent, "String"],
    {fname, files}],
  {dir, dirs}]
];

Print["Done"];
