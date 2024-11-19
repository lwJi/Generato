Needs["CodeFormatter`"]

Module[{dirs = {"./"}},
    Do[
        Print["Formatting '.wl' files in "<>dirs[[d]]<>" ..."];
        files = FileNames["*rhs.wl", dirs[[d]]];
        Do[
            file = File[files[[i]]];
            Export[file,
              CodeFormat[file, "IndentationString"->"  ", LineWidth->8000],
              "String"],
        {i, 1, Length[files]}],
    {d, 1, Length[dirs]}]
];

Print["Done"];
