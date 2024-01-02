Needs["CodeFormatter`"]

Print["Formatting '.wl' files in src ..."];

files = FileNames["*.wl", "src"];
Do[
    file = File[files[[i]]];
    Export[file, CodeFormat[file], "String"],
{i, 1, Length[files]}];

Print["Done"];
