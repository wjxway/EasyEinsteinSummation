(* ::Package:: *)

(* ::Section:: *)
(*Easy Einstein Summation*)


(* ::Subsection:: *)
(*Preprocessing*)


BeginPackage["EinsteinSummation`"];


(* ::Subsubsection:: *)
(*$TensorSymb*)


(* ::Input::Initialization:: *)
Unprotect[$tensorSymb];
ClearAll@$tensorSymb;
$tensorSymb::usage="Wrapper function for tensor objects.";
$tensorSymb[s_Association]/;(s["Dimensions"]==0):=s["Value"];
$tensorSymb[s_Association][query_]:=s[query];
Protect[$tensorSymb];


(* ::Subsubsection:: *)
(*Parse String*)


(* ::Subsubsubsection:: *)
(*Determine symbols' order*)


(* ::Input::Initialization:: *)
SymbolOrder[str_String,position___]:=Module[{cl=Characters[StringReplace[StringDelete[str,"\\("|"\\)"],{"\\\\ \\\\ "->"\[Wolf]","\[NonBreakingSpace]\[NonBreakingSpace]"->"\[Wolf]"," "->"","\[NonBreakingSpace]"->""}]]},{MapIndexed[If[#=!="\[Wolf]",#2[[1]]->{#,position},Nothing]&,DeleteCases[cl,"("|")"|"["|"]"]],Thread[(#-Range[0,Length@#-1])->Thread@{cl[[#]],position}]&@Position[cl,"("|")"|"["|"]"][[;;,1]]}]


(* ::Subsubsubsection:: *)
(*Construct tensor Association*)


(* ::Input::Initialization:: *)
ProcTensor::usage="\!\(\*
StyleBox[\"ProcTensor\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"[\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"{\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"symb_\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\",\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"sub_\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\",\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"super_\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"}\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"]\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"process\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"the\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"input\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"string\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"and\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"convert\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"it\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"to\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"properties\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"of\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"a\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"tensor\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\".\",\nFontWeight->\"Plain\"]\)";
ProcTensor::index="Improper index found when processing matrix `1`.";


(* ::Input::Initialization:: *)
ProcTensor[{symb_,sub_,super_}]:=Module[{all={SymbolOrder[sub,-1],SymbolOrder[super,1]},index},
index=SortBy[Catenate@all[[;;,1]],First];
If[index[[;;,1]]=!=Range@Length@index,Message[ProcTensor::index,ToString@symb];Abort[]];
<|"Symbol"->symb,"IndexName"->index[[;;,2,1]],"Dimensions"->Length@index,"IndexPosition"->index[[;;,2,2]],"SymmetryMarker"-><|-1->#1,1->#2|>&@@(Map[{#[[1]],#[[2,1]]}&,all[[;;,2]],{2}])|>]


(* ::Subsubsubsection:: *)
(*String patterns for parsing tensors*)


(* ::Input::Initialization:: *)
SetAttributes[TensorStringReplaceRule,HoldAll];


(* ::Input::Initialization:: *)
TensorStringReplaceRule[operation_]:=
Module[{pattlist={"\\*SubsuperscriptBox[\\("~~Shortest[symb__]~~"\\), \\("~~Shortest[sub__]~~"\\), \\("~~Shortest[super__]~~"\\)]",
"\\*SubscriptBox[\\("~~Shortest[symb__]~~"\\), \\("~~Shortest[sub__]~~"\\)]",
"\\*SuperscriptBox[\\("~~Shortest[symb__]~~"\\), \\("~~Shortest[super__]~~"\\)]"
}},
{"\\!\\("~~pattlist[[1]]~~"\\)":>operation[{symb,sub,super}],
pattlist[[1]]:>operation[{symb,sub,super}],
"\\!\\("~~pattlist[[2]]~~"\\)":>operation[{symb,sub,""}],
pattlist[[2]]:>operation[{symb,sub,""}],
"\\!\\("~~pattlist[[3]]~~"\\)":>operation[{symb,"",super}],
pattlist[[3]]:>operation[{symb,"",super}]
}]


(* ::Subsubsubsection:: *)
(*Parse tensor string*)


(* ::Text:: *)
(*You can write like \!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(\([\)\(\[Mu]\)\)]*)
(*\*SubscriptBox[\(A\), \(\(\[Nu]\)\(]\)\)]\), but you should never write like \!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(\([\)\(\[Mu]\)\)]\(( *)
(*\*SubscriptBox[\(A\), \(\(\[Nu]\)\(]\)\)] + *)
(*\*SubscriptBox[\(B\), \(\(\[Nu]\)\(]\)\)])\)\) or \!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(\([\)\(\[Mu]\)\)]\(( *)
(*\*SubscriptBox[\(A\), \(\[Nu]\)] + *)
(*\*SubscriptBox[\(B\), \(\[Nu]\)])\)\) Subscript[C, \[Lambda]]] because this might cause confusion and can be detrimental to performance.*)
(*Please manually expand these expressions to \!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(\([\)\(\[Mu]\)\)]*)
(*\*SubscriptBox[\(A\), \(\(\[Nu]\)\(]\)\)]\)+\!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(\([\)\(\[Mu]\)\)]*)
(*\*SubscriptBox[\(B\), \(\(\[Nu]\)\(]\)\)]\) and \!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(\([\)\(\[Mu]\)\)]*)
(*\*SubscriptBox[\(A\), \(\[Nu]\)]\) Subscript[C, \[Lambda]]]+\!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(\([\)\(\[Mu]\)\)]*)
(*\*SubscriptBox[\(B\), \(\[Nu]\)]\) Subscript[C, \[Lambda]]] or evaluate Subscript[A, \[Nu]]+Subscript[B, \[Nu]] first using my code and then do \!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(\([\)\(\[Mu]\)\)]*)
(*\*SubscriptBox[\(Q\), \(\(\[Nu]\)\(]\)\)]\) or \!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(\([\)\(\[Mu]\)\)]*)
(*\*SubscriptBox[\(Q\), \(\[Nu]\)]\) Subscript[C, \[Lambda]]].*)
(*\!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(\([\)\(\[Mu]\)\)]*)
(*\*SuperscriptBox[\(T\), \(\(\[Nu]\)\(]\)\)]\) is not allowed as well, and \!\( *)
(*\*SubsuperscriptBox[\(T\), \(\(\ \ \)\((\[Sigma]\)\), \((\[Rho]\)] *)
(*\*SubsuperscriptBox[\(T\), \(\(\ \ \)\(\[Nu]\)\()\)\), \(\(\[Mu]\)\()\)\)]\) will make \[Rho],\[Mu] symmetric and \[Sigma],\[Nu] symmetric.*)
(**)
(*Please always wrap a parentheses around Subscript[\[PartialD], \[Mu]] or Subscript[\[Del], \[Mu]] for safety, because Mathematica assumes that \!\( *)
(*\*SubscriptBox[\(\[Del]\), \(\[Mu]\)]\(+*)
(*\*SubscriptBox[\(A\), \(\[Nu]\)]\)\) means \!\( *)
(*\*SubscriptBox[\(\[Del]\), \(\[Mu]\)]\((\(+*)
(*\*SubscriptBox[\(A\), \(\[Nu]\)]\))\)\) when processing Boxes...*)


(* ::Input::Initialization:: *)
ParseTensor::usage="ParseTensor[str_String] generates the standard Mathematica representation of a Tensor.";


(* ::Input::Initialization:: *)
ParseTensor[str_String]:=StringCases[ToString@InputForm@Identity@str,TensorStringReplaceRule[ProcTensor]]


(* ::Subsubsubsection:: *)
(*Parse tensor expression*)


(* ::Input::Initialization:: *)
ParseTensorExpression::usage="\!\(\*
StyleBox[\"GetTensor\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"[\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"str_String\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"]\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"converts\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"an\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"expression\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"written\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"in\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"Einstein\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"summation\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"rules\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"to\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"a\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"Mathematica\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"expression\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"for\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"further\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"processing\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\".\",\nFontWeight->\"Plain\"]\)";


(* ::Input::Initialization:: *)
ParseTensorExpression[str_String]:=Module[{us="$tensorSymb",i=1,temp,symbs},Internal`InheritedBlock[{Times,CircleDot},
Unprotect@Times;
ClearAttributes[Times,Orderless];

temp=Reap[ToExpression@ToExpression@StringReplace[StringReplace[ToString@InputForm@Identity@str,TensorStringReplaceRule[(Sow[#];us<>"["<>ToString[i++]<>"]")&]],"\\!\\(TraditionalForm\\`"~~Shortest[cont__]~~"\\)":>cont]/.Times->CircleDot];

symbs=ProcTensor/@Flatten[temp[[2]],1];
((temp[[1]]/.$tensorSymb[i_]:>$tensorSymb[symbs[[i]]])/.Times->CircleDot)/.CircleDot[s_]:>s
]]


(* ::Subsection:: *)
(*Global Variables*)


(* ::Text:: *)
(*My gradient which consider everything other than $vars as variables.*)


(* ::Input::Initialization:: *)
myGrad[symb:(Except[_Symbol]),vars_.]:=Dt[symb,#,Constants->Join[DeleteCases[$vars,#],$constants]]&/@$vars
myGrad[symb_]:=myGrad[symb,$vars]


(* ::Text:: *)
(*Tensor dataset (actually a multi-layer association would be better for performance, but that's just ugly...).*)
(**)
(*data should be in the form of:*)
(*<|"Symbol"->"g","Dimension"->2,"IndexPosition"->{-1,-1},"Value"->$gd|>*)


(* ::Input::Initialization:: *)
$TensorDefinitions=Dataset[{}];


(* ::Input::Initialization:: *)
AddTensorToDataset::usage="AddTensorToDataset[ten_Association] \!\(\*
StyleBox[\"adds\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"tensor\",\nFontWeight->\"Plain\"]\) ten \!\(\*
StyleBox[\"to\",\nFontWeight->\"Plain\"]\) $TensorDefinitions\!\(\*
StyleBox[\".\",\nFontWeight->\"Plain\"]\)";
AddTensorToDataset::keys="The given tensor is not in the standard form. It should contain three keys: \"Symbol\",\"IndexPosition\", and \"Value\".";


(* ::Input::Initialization:: *)
AddTensorToDataset[ten_Association]:=If[!ContainsAll[Keys[ten],{"IndexPosition","Symbol","Value"}],Message[AddTensorToDataset::keys],
$TensorDefinitions=$TensorDefinitions[DeleteCases[_?(#Symbol===ten["Symbol"]&&#IndexPosition===ten["IndexPosition"]&)]];AppendTo[$TensorDefinitions,Insert[ten[[{"Symbol","IndexPosition","Value"}]],"Dimensions"->Length@ten["IndexPosition"],2]];]


(* ::Text:: *)
(*gd is Subscript[g, \[Mu]\[Nu]], gu is g^\[Mu]\[Nu].*)


(* ::Input::Initialization:: *)
SetVars::usage="\!\(\*
StyleBox[\"SetVars\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"[\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"var_List\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"]\",\nFontWeight->\"Bold\"]\) \!\(\*
StyleBox[\"setup\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"the\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"parameters\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"used\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"to\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"describe\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"the\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"space\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\".\",\nFontWeight->\"Plain\"]\)";
$constants::usage="$constants\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"setup\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"all\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"varibales\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"which\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"are\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"considered\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"constant\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"with\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"respect\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"to\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"space\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"parameters\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\".\",\nFontWeight->\"Plain\"]\)";
SetMetric::usage="\!\(\*
StyleBox[\"SetMetric\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"[\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"g_List\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"]\",\nFontWeight->\"Bold\"]\) \!\(\*
StyleBox[\"setup\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"the\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"metric\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"tensor\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[FormBox[SubscriptBox[\"g\", \"\[Mu]\[Nu]\"],
TraditionalForm],\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\".\",\nFontWeight->\"Plain\"]\)";
SetMetric::dim="\!\(\*
StyleBox[\"Dimension\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"of\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[FormBox[SubscriptBox[\"g\", \"\[Mu]\[Nu]\"],
TraditionalForm],\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"is\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"invalid\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"!\",\nFontWeight->\"Bold\"]\)";


(* ::Input::Initialization:: *)
$constants={};


(* ::Input::Initialization:: *)
SetVars[vars_]:=($vars=vars)
SetMetric[g_]:=(
If[(Length[#]!=2||#[[1]]!=#[[2]])&@TensorDimensions[g],Message[SetMetric::dim]];
$gd=g;
$gu=Inverse[$gd];
$\[CapitalGamma]=TensorContract[Inverse[$gd]\[TensorProduct](-TensorTranspose[myGrad[$gd],{1,3,2}]+TensorTranspose[myGrad[$gd],{2,1,3}]+TensorTranspose[myGrad[$gd],{2,3,1}]),{{2,4}}]/2;
$dim=Length@$gd;

AddTensorToDataset[<|"Symbol"->"g","IndexPosition"->{-1,-1},"Value"->$gd|>];
AddTensorToDataset[<|"Symbol"->"g","IndexPosition"->{1,1},"Value"->$gu|>];
AddTensorToDataset[<|"Symbol"->"\[CapitalGamma]","IndexPosition"->{1,-1,-1},"Value"->$\[CapitalGamma]|>]
)


(* ::Input::Initialization:: *)
SetVars[{t,x,y,z}];
SetMetric[SparseArray@DiagonalMatrix[{-1,1,1,1}]];


(* ::Subsection:: *)
(*Evaluation*)


(* ::Subsubsection:: *)
(*Move Tensor Index*)


(* ::Input::Initialization:: *)
MatrixMult[mat1_,mat2_,{ind1_,ind2_}]:=TensorTranspose[mat1,Cycles[{{ind1,Length@TensorDimensions@mat1}}]].TensorTranspose[mat2,Cycles[{{ind2,1}}]]


(* ::Input::Initialization:: *)
AdjustIndex::usage="AdjustIndex[mat_,{{pos1,rd1},{pos2,rd2},...}]\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"raise\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"Tensor\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"mat\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"'\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"s\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"index\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"at\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"position\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"pos1\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"if\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"rd1\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"=\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"1\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\",\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"and\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"reduce\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"if\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"rd1\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"=\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"-\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"1\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\",\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"same\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"to\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"pos2\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\",\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"...\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\".\",\nFontWeight->\"Plain\"]\)";


(* ::Input::Initialization:: *)
AdjustIndex[mat_,pos_List]:=Fold[TensorTranspose[If[#2[[2]]==1,$gu,$gd].TensorTranspose[#,Cycles[{{#2[[1]],1}}]],Cycles[{{#2[[1]],1}}]]&,mat,Select[pos,#[[2]]==1||#[[2]]==-1&]]


(* ::Input::Initialization:: *)
ConvertTensorIndex::usage="\!\(\*
StyleBox[\"ConvertMatrixIndex\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"[\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"{\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"symb_\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\",\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"sub_\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\",\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"super_\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"}\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"]\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"convert\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"the\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"component\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"of\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"tensor\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"using\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[SuperscriptBox[\"g\", \"\[Mu]\[Nu]\"],\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"and\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[SubscriptBox[\"g\", \"\[Mu]\[Nu]\"],\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"according\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"to\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"definitions\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"stored\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\".\",\nFontWeight->\"Plain\"]\)";
ConvertTensorIndex::nstored="Cannot find definition of `1` in definition dataset.";
ConvertTensorIndex::invindex="Contraction of `1` at index position `2` is invalid. No index is allowed to appear for more than 2 times.";
ConvertTensorIndex::invindexpos="Contraction of `1` at index position `2` is invalid. Indexes awaiting summation should be at superscript and subscript respectively.";
ConvertTensorIndex::invparent="Mismatched parentheses in `1`";
ConvertTensorIndex::dtindex="Derivative symbol `1` has 0 or more than 2 indexes, please manually expand them.";


(* ::Text:: *)
(*Move indexes up and down.*)


(* ::Input::Initialization:: *)
ConvertTensorIndex[s_Association]:=Module[{stored=Normal[$TensorDefinitions[Select[#Symbol==s["Symbol"]&&#Dimensions==s["Dimensions"]&]][First@*MinimalBy[Abs[#IndexPosition-s["IndexPosition"]]&]]],temps=s,temp,diff},
If[Head[stored]=!=Association,Message[ConvertTensorIndex::nstored,s["Symbol"]];Abort[]];

(*Raise and reduce index*)
diff=(stored["IndexPosition"]-s["IndexPosition"])/2;
temp=stored["Value"];
temp=AdjustIndex[temp,Thread[{Range@Length@#,#}]&@diff];
AppendTo[temps,"Value"->temp];
AddTensorToDataset[temps];
ConvertTensorIndex[temps]
]


(* ::Text:: *)
(*Summation first, symmetrize later. If you want to change the order, please use metric explicitly to calculate sum, e.g. \!\( *)
(*\*SubsuperscriptBox[\(g\), \(\(\ \ \)\(\[Lambda]\)\), \(\[Rho]\)] *)
(*\*SubsuperscriptBox[\(\[CapitalGamma]\), \(\(\ \ \)\((\[Alpha]\[Rho]\[Gamma])\)\), \(\[Lambda]\)]\).*)


(* ::Input::Initialization:: *)
ConvertTensorIndex[s_Association]/;KeyExistsQ[s,"Value"]:=
Module[{temps=s,odims=Length@s["IndexPosition"],contractpos,parentpos,stack,parents={},i,j,tempfunc},

(*Summation Rules*)
contractpos=Select[GatherBy[Thread[{#,Range@Length@#}],First],Length@#>1&][[;;,;;,2]]&@s["IndexName"];
If[Length@#!=2,Message[ConvertTensorIndex::invindex,s["Symbol"],#];Abort[],If[(Times@@(s["IndexPosition"][[#]]))!=-1,Message[ConvertTensorIndex::invindexpos,s["Symbol"],#];Abort[]]]&/@contractpos;
temps["Value"]=TensorContract[temps["Value"],contractpos];
temps["Dimensions"]-=2 Length@contractpos;
(temps[#]=Delete[temps[#],List/@Flatten@contractpos])&/@{"IndexName","IndexPosition"};
If[temps["Dimensions"]==0,Return[<|"Symbol"->temps["Symbol"],"IndexName"->{},"Dimensions"->0,"IndexPosition"->{},"SymmetryMarker"->{},"Value"->temps["Value"]|>]];

(*Symmetrize*)
parentpos=Accumulate@ReplacePart[ConstantArray[1,odims+1],(#+1)->0&/@Flatten@Sort[contractpos]];
temps["SymmetryMarker"]=Map[{parentpos[[#[[1]]]],#[[2]]}&,temps["SymmetryMarker"],{2}];
(*Use Stack to process parentheses*)

tempfunc=(temps["Value"]=SparseArray@Symmetrize[temps["Value"],#[Select[Position[temps["IndexPosition"],i][[;;,1]],stack[[-1,1]]<=#<j[[1]]&]]];stack=Most@stack)&;

Do[
stack={};
Do[
If[Length@stack>0,
Switch[{stack[[-1,2]],j[[2]]},
{"(",")"},tempfunc[Symmetric],
{"[","]"},tempfunc[Antisymmetric],
{"(","]"}|{"[",")"},Message[ConvertTensorIndex::invparent,s["Symbol"]];Abort[],
_,AppendTo[stack,j]
],AppendTo[stack,j]
],
{j,temps["SymmetryMarker"][i]}];
temps["SymmetryMarker"][i]=stack,
{i,{1,-1}}];

temps
]


(* ::Text:: *)
(*Process Derivatives.*)


(* ::Input::Initialization:: *)
ConvertTensorIndex[s_Association]/;(s["Symbol"]==="\[PartialD]"||s["Symbol"]=="\[Del]"):=If[s["Dimensions"]=!=1,Message[ConvertTensorIndex::dtindex,s];Abort[],s]


(* ::Subsection:: *)
(*Process Addition, Multiplication and Derivatives*)


(* ::Input::Initialization:: *)
RandomT$:=ToString@Unique["T$"]


(* ::Text:: *)
(*Only * + -  can operate on Einstein tensors.*)


(* ::Input::Initialization:: *)
EvaluateEisteinSummation::usage="\!\(\*
StyleBox[\"EvaluateEisteinSummation\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"[\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"expr_\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"]\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"Evaluate\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"the\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"tensor\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"expression\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"inside\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"using\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"Einstein\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"summation\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"rules\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\".\",\nFontWeight->\"Plain\"]\)";
EvaluateEisteinSummation::indexmismatch="Cannot add up expression `1` whose components has different free indexes and/or free indexes' positions.";
EvaluateEisteinSummation::parentinsum="Free parentheses appeared in summation `1`, which is not allowed.\nIf you insists, please expand the terms and retry.";


(* ::Subsubsection:: *)
(*Irrelevant expressions*)


(* ::Input::Initialization:: *)
EvaluateEisteinSummation[expr:Except[_String]]/;FreeQ[expr,$tensorSymb]:=expr


(* ::Subsubsection:: *)
(*Simple tensor*)


(* ::Input::Initialization:: *)
EvaluateEisteinSummation[expr_$tensorSymb]:=$tensorSymb@ConvertTensorIndex[expr[[1]]]


(* ::Subsubsection:: *)
(*Addition*)


(* ::Subsubsubsection:: *)
(*Calculate free indexes' combination*)


(* ::Input::Initialization:: *)
FreeIndexes::usage="\!\(\*
StyleBox[\"IndexCombination\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"[\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"expr_\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"]\",\nFontWeight->\"Bold\"]\) \!\(\*
StyleBox[\"determines\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"free\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"indexes\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"and\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"their\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"position\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"(\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"up\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"/\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"down\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\")\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"of\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"the\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"result\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\".\",\nFontWeight->\"Plain\"]\)";
FreeIndexes::invindex="Contraction of `1` is invalid. No index is allowed to appear for more than 2 times and indexes awaiting summation should be at superscript and subscript respectively.";


(* ::Input::Initialization:: *)
(*Others*)
FreeIndexes[expr_]/;FreeQ[expr,$tensorSymb]:={}
(*Tensor*)
FreeIndexes[expr_$tensorSymb]:=Thread[{expr["IndexName"],expr["IndexPosition"]}]
(*Summation*)
FreeIndexes[expr_Plus]:=FreeIndexes[expr[[1]]];
(*Multiplication and Application*)
FreeIndexes[expr_CircleDot]:=Module[{comb=GatherBy[Catenate[FreeIndexes/@(List@@expr)],First]},
If[Count[comb,x_/;(Length[x]>2||(Length[x]==2&&Times@@x[[;;,2]]!=-1))]!=0,Message[FreeIndexes::invindex,expr];Abort[],Select[comb,Length[#]==1&][[;;,1]]]
]


(* ::Subsubsubsection:: *)
(*Calculate free parentheses*)


(* ::Input::Initialization:: *)
FreeParentheses::usage="\!\(\*
StyleBox[\"FreeParentheses\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"[\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"expr_\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"]\",\nFontWeight->\"Bold\"]\) \!\(\*
StyleBox[\"determines\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"free\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"parentheses\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"'\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"order\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"in\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"an\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"expression\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\".\",\nFontWeight->\"Plain\"]\)";
FreeParentheses::invparent="Mismatched parentheses in `1`";


(* ::Input::Initialization:: *)
FreeParentheses[expr_CircleDot]:=Module[{stack={}},
Do[
If[Length@stack>0,
Switch[{stack[[-1]],j},
{"(",")"}|{"[","]"},stack=Most@stack,
{"(","]"}|{"[",")"},Message[FreeParentheses::invparent,expr];Abort[],
_,AppendTo[stack,j]
],AppendTo[stack,j]
],
{j,#}];
stack]&/@Merge[FreeParentheses/@(List@@expr),Catenate]

FreeParentheses[expr_$tensorSymb]:=expr[[1,"SymmetryMarker",;;,;;,2]]
FreeParentheses[expr___]:=<|-1->{},1->{}|>


(* ::Subsubsubsection:: *)
(*Main*)


(* ::Input::Initialization:: *)
EvaluateEisteinSummation[expr_Plus]/;(!FreeQ[expr,$tensorSymb]):=
Module[{cont=EvaluateEisteinSummation/@(List@@expr),cat,targetorder},

(*Check Validity*)
(*Same index and index's height, no need for same position*)
If[!(SameQ@@(Sort@*FreeIndexes/@cont)),Message[EvaluateEisteinSummation::indexmismatch,expr];Abort[]];
(*No free parentheses*)
If[!(And@@(({{},{}}===Values@FreeParentheses@#)&/@cont)),Message[EvaluateEisteinSummation::parentinsum,expr];Abort[]];

(*Categorize and Sum up*)
cat=OperatorApplied[Flatten][1]/@Reap[
Do[Switch[i,
_CircleDot,Sow[i,1],
$tensorSymb[s_Association/;(s["Symbol"]==="\[PartialD]"||s["Symbol"]=="\[Del]")],Sow[i,1],
_$tensorSymb,Sow[i,2],
_,Sow[i,1]
],{i,cont}],
{1,2}][[2]];

Total[cat[[1]]]+Switch[Length@cat[[2]],
0,0,
1,cat[[2,1]],
_,targetorder=Ordering@cat[[2,1,1,"IndexName"]];
ReplacePart[cat[[2,1]],{{1,Key["Symbol"]}->RandomT$,{1,Key["Value"]}->Total[TensorTranspose[#["Value"],targetorder[[Ordering@Ordering@#["IndexName"]]]]&/@cat[[2,;;,1]]]}]
]
]


(* ::Subsubsection:: *)
(*Multiplication*)


(* ::Subsubsubsection:: *)
(*Tensor operation*)


(* ::Text:: *)
(*Something that do TensorC=TensorA TensorB where TensorB is a value tensor and return a value tensor Tensor C.*)


(* ::Input::Initialization:: *)
TensorOperate::usage="\!\(\*
StyleBox[\"TensorOperate\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"[\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"TensorA\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\",\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"TensorB\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"]\",\nFontWeight->\"Plain\"]\) do \!\(\*
StyleBox[\"TensorC\",\nFontWeight->\"Bold\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"=\",\nFontWeight->\"Bold\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"TensorA\",\nFontWeight->\"Bold\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Bold\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"TensorB\",\nFontWeight->\"Bold\",\nFontSlant->\"Italic\"]\) where \!\(\*
StyleBox[\"TensorB\",\nFontWeight->\"Bold\",\nFontSlant->\"Italic\"]\) is a value tensor and return a value tensor \!\(\*
StyleBox[\"Tensor\",\nFontWeight->\"Bold\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Bold\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"C\",\nFontWeight->\"Bold\",\nFontSlant->\"Italic\"]\).";


(* ::Text:: *)
(*Tensor.Tensor*)


(* ::Input::Initialization:: *)
TensorOperate[TenA_$tensorSymb,TenB_$tensorSymb]:=Module[{s=Merge[{TenA,TenB}[[;;,1]],Identity],dimA=TenA["Dimensions"]},$tensorSymb@ConvertTensorIndex[<|"Symbol"->RandomT$,"IndexName"->Catenate@s["IndexName"],"Dimensions"->Total@s["Dimensions"],"IndexPosition"->Catenate@s["IndexPosition"],"SymmetryMarker"->(Join[#[[1]],{#[[1]]+dimA,#[[2]]}&/@#[[2]]]&/@Merge[s["SymmetryMarker"],Identity]),"Value"->(TensorProduct@@s["Value"])|>]]


(* ::Text:: *)
(*\[PartialD] Tensor*)


(* ::Input::Initialization:: *)
TensorOperate[TenA_$tensorSymb,TenB_$tensorSymb]/;(TenA["Symbol"]==="\[PartialD]"):=Module[{s=Merge[{TenA,TenB}[[;;,1]],Identity],tempval},
(*value for \!\(
\*SubscriptBox[\(\[PartialD]\), \(\[Mu]\)]T\)*)
tempval=TensorTranspose[myGrad[TenB["Value"]],Cycles[{Range@Total@s["Dimensions"]}]];

(*contract/symmetrize etc.*)
$tensorSymb@ConvertTensorIndex[<|"Symbol"->RandomT$,"IndexName"->Catenate@s["IndexName"],"Dimensions"->Total@s["Dimensions"],"IndexPosition"->Catenate@s["IndexPosition"],"SymmetryMarker"->(Join[#[[1]],{#[[1]]+1,#[[2]]}&/@#[[2]]]&/@Merge[s["SymmetryMarker"],Identity]),"Value"->If[TenA[[1,"IndexPosition",1]]==1,$gu.tempval,tempval]|>]]


(* ::Text:: *)
(*\[Del] Tensor*)


(* ::Input::Initialization:: *)
TensorOperate[TenA_$tensorSymb,TenB_$tensorSymb]/;(TenA["Symbol"]==="\[Del]"):=
Module[{s=Merge[{TenA,TenB}[[;;,1]],Identity],tempindex1=ToString@Unique["Var$"],tempindex2=ToString@Unique["Var$"],tempsymark=<|-1->{},1->{}|>,indname,partialterm,gammaterm,res1},

indname=Catenate@s["IndexName"];

(*\[PartialD]T*)
partialterm=TensorOperate[$tensorSymb[<|"Symbol"->"\[PartialD]","IndexName"->{tempindex1},"Dimensions"->1,"IndexPosition"->{-1},"SymmetryMarker"->tempsymark|>],ReplacePart[TenB,{1,"SymmetryMarker"}->tempsymark]];

(*\[PlusMinus]\[CapitalGamma]T*)
gammaterm=With[{indpos=TenB[[1,"IndexPosition",#]]},
TensorOperate[$tensorSymb[<|"Symbol"->"\[CapitalGamma]","IndexName"->If[indpos==1,{TenB[[1,"IndexName",#]],tempindex1,tempindex2},{tempindex2,tempindex1,TenB[[1,"IndexName",#]]}],"Dimensions"->3,"IndexPosition"->{1,-1,-1},"SymmetryMarker"->tempsymark,"Value"->(indpos*$\[CapitalGamma])|>],ReplacePart[TenB,{{1,"SymmetryMarker"}->tempsymark,{1,"IndexName",#}->tempindex2}]]
]&/@Range[TenB["Dimensions"]];

(*Summation & change index order*)
res1=TensorTranspose[#["Value"],Ordering[ReplacePart[indname,1->tempindex1]][[Ordering@Ordering@#["IndexName"]]]]&@EvaluateEisteinSummation[Total@Prepend[gammaterm,partialterm]][[1]];

(*contract/symmetrize etc.*)
$tensorSymb@ConvertTensorIndex[<|"Symbol"->RandomT$,"IndexName"->indname,"Dimensions"->Total@s["Dimensions"],"IndexPosition"->Catenate@s["IndexPosition"],"SymmetryMarker"->(Join[#[[1]],{#[[1]]+1,#[[2]]}&/@#[[2]]]&/@Merge[s["SymmetryMarker"],Identity]),"Value"->If[TenA[[1,"IndexPosition",1]]==1,$gu.res1,res1]|>]
]


(* ::Text:: *)
(*\[PartialD] or \[Del] Scalar*)


(* ::Input::Initialization:: *)
TensorOperate[TenA_$tensorSymb,TenB_]/;(TenA["Symbol"]==="\[PartialD]"||TenA["Symbol"]=="\[Del]"):=ReplacePart[TenA,{{1,Key["Symbol"]}->RandomT$,{1,Key["Value"]}->myGrad[TenB,$vars]}]


(* ::Text:: *)
(*Tensor.Scalar or Scalar.Tensor*)


(* ::Input::Initialization:: *)
TensorOperate[TenA_$tensorSymb,TenB_]:=ReplacePart[TenA,{{1,Key["Symbol"]}->RandomT$,{1,Key["Value"]}->(TenA["Value"]*TenB)}]
TensorOperate[TenA_,TenB_$tensorSymb]:=ReplacePart[TenB,{{1,Key["Symbol"]}->RandomT$,{1,Key["Value"]}->(TenA*TenB["Value"])}]


(* ::Text:: *)
(*Scalar.Scalar*)


(* ::Input::Initialization:: *)
TensorOperate[TenA_,TenB_]:=(Echo[{TenA,TenB}];TenA*TenB)


(* ::Subsubsubsection:: *)
(*Main*)


(* ::Input::Initialization:: *)
EvaluateEisteinSummation[expr_CircleDot]/;(!FreeQ[expr,$tensorSymb]):=
Module[{terms,mosterms,lasterms,termres},
(*Check Validity*)
FreeIndexes@expr;
FreeParentheses@expr;

(*Divide expression into terms which will calculate to a value matrix*)
terms=MapAt[Most,Split[Append[EvaluateEisteinSummation/@(List@@expr),$tensorSymb[<|"Symbol"->"\[PartialD]"|>]],!(FreeQ[#1,$tensorSymb[s_Association/;(s["Symbol"]==="\[PartialD]"||s["Symbol"]=="\[Del]")]])&],-1];
If[Length@terms==1,Return[1]];
(*those which can be calculated*)
mosterms=Most@terms;
(*those left at the tail of the expression, which cannot be calculated*)
lasterms=Last@terms;

(*Calculate each term's result, which must be a value tensor*)
termres=
If[Length[#]==1,#[[1]],
Module[{expanded=Distribute[CircleDot@@#]},
If[Head@expanded===Plus,
EvaluateEisteinSummation[
$tensorSymb@Fold[TensorOperate[#2,#1]&,
Reverse@Internal`InheritedBlock[{CircleDot},SetAttributes[CircleDot,Flat];#]
]&/@expanded],
Fold[TensorOperate[#2,#1]&,
Reverse@Internal`InheritedBlock[{CircleDot},SetAttributes[CircleDot,Flat];expanded]]
]]
]&/@mosterms;

(*merge each term's result. Note that all of them are value tensors*)
If[Length@lasterms==0,#,CircleDot@@Prepend[lasterms,#]]&@Fold[TensorOperate[#2,#1]&,Reverse@termres]
]


(* ::Section:: *)
(*Complete Evaluation From String*)


(* ::Input::Initialization:: *)
EvaluateEisteinSummation[s_String]/;True:=EvaluateEisteinSummation@ParseTensorExpression@s


EndPackage[];
