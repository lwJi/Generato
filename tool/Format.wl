Needs["CodeFormatter`"]

Module[{dirs = {"src", "test/Nmesh", "test/CarpetX"}},
  Do[
    Print["Formatting '.wl' files in "<>dir<>" ..."];
    files = FileNames["*.wl", dir];
    Do[
      file = File[fname];
      formattedContent = CodeFormat[file, "IndentationString" -> "  ", LineWidth -> 4096];
      Export[file, formattedContent, "String"],
    {fname, files}],
  {dir, dirs}]
];

Print["Done"];
