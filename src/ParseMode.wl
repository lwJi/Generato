(* ::Package:: *)

(* (c) Liwei Ji, 07/2024 *)

BeginPackage["Generato`ParseMode`", {"Generato`Context`"}];

If[Environment["QUIET"] =!= "1",
  System`Print["------------------------------------------------------------"];
  System`Print["Package Generato`ParseMode`, {2024, 7, 06}"];
  System`Print["------------------------------------------------------------"];
];

(* Core Functions *)
WithMode::usage = "WithMode[settings, body] sets modes, evaluates body, then restores previous mode state. Settings use flat keys (e.g., \"Phase\" -> \"SetComp\").";

Begin["`Private`"];

(* ========================================= *)
(* Core Functions *)
(* ========================================= *)

(* Scoped mode settings with automatic restore *)
(* settings is a list of "FlatKey" -> value rules *)
SetAttributes[WithMode, HoldRest];

WithMode[settings_List, body_] :=
  Module[{flatKeys, savedValues},
    (* Extract flat keys from settings *)
    flatKeys = First /@ settings;
    (* Validate all settings before applying any *)
    Scan[
      Function[setting,
        If[!ValidateContextModeValue[First[setting], Last[setting]],
          Throw @ Message[WithMode::EInvalidValue, First[setting], Last[setting]]
        ]
      ],
      settings
    ];
    (* Save current values for keys we're about to modify *)
    savedValues = $CurrentContext[#] & /@ flatKeys;
    (* Apply new settings *)
    Scan[
      Function[setting,
        $CurrentContext = Append[$CurrentContext, First[setting] -> Last[setting]]
      ],
      settings
    ];
    (* Restore after body evaluates *)
    WithCleanup[
      body,
      Do[
        $CurrentContext = Append[$CurrentContext, flatKeys[[i]] -> savedValues[[i]]],
        {i, Length[flatKeys]}
      ]
    ]
  ];

Protect[WithMode];

WithMode::EInvalidValue = "Invalid value for key \"`1`\": `2`";

End[];

EndPackage[];
