(* ::Package:: *)

(* Generato`Basic`, set up basics for Generato *)

(* (c) Liwei Ji, 01/2024 *)

BeginPackage["Generato`Basic`", "xAct`xTensor`", "xAct`xCoba`"];

Print["------------------------------------------------------------"];

Print["Package Generato`Basic`, {2024, 1, 11}"];

Print["------------------------------------------------------------"];

SetPVerbose::usage = "SetPVerbose[True] update the Boolean variable specifying if print more messages.";

GetPrintDate::usage = "GetPrintDate[] return the Boolean variable specifying if print date in file.";

SetPrintDate::usage = "SetPrintDate[True] update the Boolean variable specifying if print date in file.";

GetGridPointIndex::usage = "GetGridPointIndex[] return the grid index name.";

SetGridPointIndex::usage = "SetGridPointIndex[girdindex] reset the grid index name.";

GetSuffixUnprotected::usage = "GetSuffixUnprotected[] returns the suffix added to vars which would conflict with system default otherwise.";

SetSuffixUnprotected::usage = "SetSuffixUnprotected[suffix] reset the suffix added to vars which would conflict with system default otherwise.";

GetOutputFile::usage = "GetOutputFile[] return the variable storing the output file name.";

SetOutputFile::usage = "SetOutputFile[name] update the variable storing the output file name.";

GetProject::usage = "GetProject[] return the project name we are generating file for.";

SetProject::usage = "SetProject[name] update the project name we are generating file for.";

GetDefaultChart::usage = "GetDefaultChart[] return the default chart we are using.";

GetDim::usage = "GetDim[] return the dimension of the manifold.";

IsDefined::usage = "IsDefined[term] return True/False if term is defined or not.";

RHSOf::usage = "RHSOf[var, suffix] return the expression of 'var$RHS' or 'varsuffix$RHS' (if suffix is not empty).";

SetEQN::usage = "SetEQN[{DelaySet->..., CheckRHS->..., Suffix->...}, var, varrhs] set 'var$RHS/varsuffix$RHS' equal to 'varrhs', considering if DelaySet, CheckRHS.";

DelaySet::usage = "DelaySet is an option for SetEQN specifying if use IndexSetDelayed.";

CheckRHS::usage = "CheckRHS is an option for SetEQN specifying if check there are undefined terms in varrhs.";

SetEQNDelayed::usage = "SetEQNDelayed[{CheckRHS->..., Suffix->...}, var, varrhs] returns SetEQN[ {DelaySet->True, ...}, var, varrhs].";

PrintVerbose::usage = "PrintVerbose[var] print var only if GetPVerbose[] is True.";

Begin["`Private`"];

(* Data *)

$PVerbose = False;

$PrintDate = True;

$GridPointIndex = "";

$SuffixUnprotected = "$Upt";

$OutputFile = "output.c";

$Project = "TEST";

$SimplifyEquation = True;

(* Function *)

GetPVerbose[] :=
    Return[$PVerbose];

SetPVerbose[verbose_] :=
    Module[{},
        $PVerbose = verbose
    ];

Protect[SetPVerbose];

GetPrintDate[] :=
    Return[$PrintDate];

Protect[GetPrintDate];

SetPrintDate[print_] :=
    Module[{},
        $PrintDate = print
    ];

Protect[SetPrintDate];

GetGridPointIndex[] :=
    Return[$GridPointIndex];

Protect[GetGridPointIndex];

SetGridPointIndex[gridindex_] :=
    Module[{},
        $GridPointIndex = gridindex
    ];

Protect[SetGridPointIndex];

GetSuffixUnprotected[] :=
    Return[$SuffixUnprotected];

Protect[GetSuffixUnprotected];

SetSuffixUnprotected[suffix_] :=
    Module[{},
        $SuffixUnprotected = suffix
    ];

Protect[SetSuffixUnprotected];

GetOutputFile[] :=
    Return[$OutputFile];

Protect[GetOutputFile];

SetOutputFile[filename_] :=
    Module[{},
        $OutputFile = filename
    ];

Protect[SetOutputFile];

GetProject[] :=
    Return[$Project];

Protect[GetProject];

SetProject[name_] :=
    Module[{},
        $Project = name
    ];

Protect[SetProject];

GetDefaultChart[] :=
    Return[ChartsOfManifold[$Manifolds[[1]]][[1]]];

GetDim[] :=
    Return[DimOfManifold[$Manifolds[[1]]]];

IsDefined[term_] :=
    Module[{head = Head[term]},
        Which[
            NumberQ[term],
                Return[True]
            ,
            head === Power || head === Plus || head === Times || head
                 === Log,
                Module[{subterms, undefinedsubterm = 0},
                    subterms = Apply[List, term];
                    Do[
                        If[!IsDefined[subterms[[isub]]],
                            undefinedsubterm = isub;
                            Break[]
                        ]
                        ,
                        {isub, 1, Length[subterms]}
                    ];
                    If[undefinedsubterm > 0,
                        Message[IsDefined::ETerm, undefinedsubterm, term
                            ];
                        Return[False]
                        ,
                        Return[True]
                    ]
                ]
            ,
            head === Symbol,
                Return[ConstantSymbolQ[term]]
            ,
            Head[head] === Symbol,
                Return[xTensorQ[head]]
            ,
            True,
                Throw @ Message[IsDefined::EType, term]
        ]
    ];

IsDefined::ETerm = "The `1`-th subterm in '`2`' is not defined!";

IsDefined::EType = "The Expression type of '`1`' is not detectable!";

Protect[IsDefined];

RHSOf[var__] :=
    Module[{argList = List[var]},
        Switch[Length[argList],
            1,
                ToExpression[ToString[var] <> "$RHS"]
            ,
            2,
                ToExpression[ToString[argList[[1]]] <> ToString[argList
                    [[2]]] <> "$RHS"]
            ,
            _,
                Throw @ Message[RHSOf::Eargs, Length[argList]]
        ]
    ];

RHSOf::Eargs = "`1` arguments unsupported yet!";

Protect[RHSOf];

Options[SetEQN] = {DelaySet -> False, CheckRHS -> True, SuffixName ->
     Null};

SetEQN[OptionsPattern[], var_, varrhs_] :=
    ReleaseHold @
        Module[{delayset, checkrhs, suffix, replaceTimes = 0},
            {delayset, checkrhs, suffix} = OptionValue[{DelaySet, CheckRHS,
                 SuffixName}];
            suffix =
                If[suffix === Null,
                    ""
                    ,
                    ToString[suffix]
                ];
            (* check if there is undefined term *)
            If[checkrhs && !IsDefined[varrhs],
                Throw @ Message[SetEQN::Evarrhs, varrhs]
            ];
            (* set var$RHS or varsuffix$RHS to varrhs *)
            If[delayset,
                Hold[IndexSetDelayed[var, varrhs]] /. {var[[0]] :> RHSOf[
                    ToString[var[[0]]] <> suffix] /; replaceTimes++ == 0}
                ,
                Hold[IndexSet[var, varrhs]] /. {var[[0]] :> RHSOf[ToString[
                    var[[0]]] <> suffix] /; replaceTimes++ == 0}
            ]
        ];

SetEQN::Evarrhs = "There are undefined terms in the RHS '`1`'!"

Protect[SetEQN];

Options[SetEQNDelayed] = {CheckRHS -> True, SuffixName -> Null};

SetEQNDelayed[OptionsPattern[], var_, varrhs_] :=
    Module[{checkrhs, suffix},
        {checkrhs, suffix} = OptionValue[{CheckRHS, SuffixName}];
        SetEQN[{DelaySet -> True, CheckRHS -> checkrhs, SuffixName ->
             suffix}, var, varrhs]
    ];

Protect[SetEQNDelayed];

PrintVerbose[var__:""] :=
    Module[{},
        If[GetPVerbose[],
            Print[var]
        ]
    ];

Protect[PrintVerbose];

End[];

EndPackage[];
