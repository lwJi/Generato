(* ::Package:: *)

(* (c) Liwei Ji, 07/2024 *)

BeginPackage["Generato`ParseMode`"];

Print["------------------------------------------------------------"];

Print["Package Generato`ParseMode`, {2024, 7, 06}"];

Print["------------------------------------------------------------"];

GetParseMode::usage = "GetParseMode[key] returns the mode correspond to the key";

SetParseMode::usage = "SetParseMode[key] add/update the mode correspond to the key";

SetParseModeAllToFalse::usage = "SetParseModeAllToFalse[] set all the modes to false";

SetComp::usage = "ParseMode option.";

Protect[SetComp];

SetCompIndep::usage = "ParseMode option.";

SetCompNoGPIndex::usage = "ParseMode option.";

PrintComp::usage = "ParseMode option.";

Protect[PrintComp];

PrintCompInit::usage = "ParseMode option.";

PrintCompInitMainOut::usage = "ParseMode option.";

PrintCompInitMainIn::usage = "ParseMode option.";

PrintCompInitMoreInOut::usage = "ParseMode option.";

PrintCompInitTemp::usage = "ParseMode option.";

PrintCompInitGF3D2::usage = "ParseMode option.";

PrintCompInitGF3D5::usage = "ParseMode option.";

PrintCompInitVecGF3D2::usage = "ParseMode option.";

PrintCompInitVecGF3D5::usage = "ParseMode option.";

PrintCompInitSmatGF3D2::usage = "ParseMode option.";

PrintCompInitSmatGF3D5::usage = "ParseMode option.";

PrintCompEQN::usage = "ParseMode option.";

PrintCompEQNNewVar::usage = "ParseMode option.";

PrintCompEQNMain::usage = "ParseMode option.";

PrintCompEQNMainCarpetX::usage = "ParseMode option.";

PrintCompEQNAddToMain::usage = "ParseMode option.";

Begin["`Private`"];

(* Data *)

$ParseModeAssociation = <||>;

(* Function *)

GetParseMode[key_] :=
  Return[$ParseModeAssociation[key]];

Protect[GetParseMode];

SetParseMode[assoc_] :=
  Module[{},
    AppendTo[$ParseModeAssociation, assoc]
  ];

Protect[SetParseMode];

SetParseModeAllToFalse[] :=
  Module[{},
    AppendTo[$ParseModeAssociation, <|SetComp -> False, PrintComp -> False,
       SetCompIndep -> False, SetCompNoGPIndex -> False, PrintCompInit -> False,
       PrintCompInitMainOut -> False, PrintCompInitMainIn -> False, PrintCompInitMoreInOut
       -> False, PrintCompInitTemp -> False, PrintCompInitGF3D2 -> False, PrintCompInitGF3D5
       -> False, PrintCompInitVecGF3D2 -> False, PrintCompInitVecGF3D5 -> False,
       PrintCompInitSmatGF3D2 -> False, PrintCompInitSmatGF3D5 -> False, PrintCompEQN
       -> False, PrintCompEQNNewVar -> False, PrintCompEQNMain -> False, PrintCompEQNMainCarpetX
       -> False, PrintCompEQNAddToMain -> False|>]
  ];

Protect[SetParseModeAllToFalse];

End[];

EndPackage[];
