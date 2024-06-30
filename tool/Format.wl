Needs["CodeFormatter`"]

Module[{dirs = {"codes", "src", "test/Nmesh", "test/CarpetX"}},
    Do[
        Print["Formatting '.wl' files in "<>dirs[[d]]<>" ..."];
        files = FileNames["*.wl", dirs[[d]]];
        Do[
            file = File[files[[i]]];
            Export[file, CodeFormat[file, "IndentationString"->"  ", LineWidth->80], "String"],
        {i, 1, Length[files]}],
    {d, 1, Length[dirs]}]
];

Print["Done"];
