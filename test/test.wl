(* ::Package:: *)

(* test.wl *)

(* (c) Liwei Ji, 01/2024 *)

Module[{
    fname =
        If[$InputFileName == "",
            NotebookFileName[]
            ,
            $InputFileName
        ]
},
    Needs["xAct`xCoba`", FileNameJoin[{ParentDirectory[DirectoryName[
        fname]], "src/Generato.wl"}]]
];

DefManifold[M3, 3, {a, b, c, d, e, f}];

DefChart[cart, M3, {1, 2, 3}, {x[], y[], z[]}, ChartColor -> Blue];

DefChart[spnr, M3, {1, 2, 3}, {r[], th[], ph[]}, ChartColor -> Green];

ChartsOfManifold[M3]
