Needs["CodeFormatter`"]

Module[{dirs = {"src", "test/Nmesh", "test/CarpetX"}},
  Do[
    System`Print["Formatting '.wl' files in "<>dir<>" ..."];
    files = FileNames["*.wl", dir];
    Do[
      file = File[fname];
      formattedContent = CodeFormat[file, "IndentationString" -> "  ", LineWidth -> 4096];
      Export[file, formattedContent, "String"],
    {fname, files}],
  {dir, dirs}]
];

System`Print["Done"];
