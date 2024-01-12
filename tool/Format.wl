Needs["CodeFormatter`"]

Module[{dirs = {"codes", "src", "test/unit", "test/integration"}},
    Do[
        Print["Formatting '.wl' files in "<>dirs[[d]]<>" ..."];
        files = FileNames["*.wl", dirs[[d]]];
        Do[
            file = File[files[[i]]];
            Export[file, CodeFormat[file], "String"],
        {i, 1, Length[files]}],
    {d, 1, Length[dirs]}]
];

Print["Done"];
