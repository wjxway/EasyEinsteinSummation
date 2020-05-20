(* ::Package:: *)

(* ::Section:: *)
(*Usage for Parsing Functions*)


(* ::Code::Initialization::Plain:: *)
$tensorSymb::usage="Wrapper function for tensor objects.";


(* ::Input::Initialization::Plain:: *)
ConvertTeXExpression::usage="Convert TeX expression to parse-able form.";


(* ::Input::Initialization::Plain:: *)
ParseTensor::usage="ParseTensor[str_String] generates the standard Mathematica representation of a Tensor.";


(* ::Input::Initialization::Plain:: *)
ParseTensorExpression::usage="ParseTensorExpression[str_String] converts an expression written in Einstein summation rules to a Mathematica expression for further processing.";


(* ::Code::Initialization::Plain:: *)
$TensorDefinitions::usage="$TensorDefinitions is a variable that stored all defined variables.";


(* ::Input::Initialization::Plain:: *)
AddTensorToDataset::usage="AddTensorToDataset[ten_Association] adds tensor ten to $TensorDefinitions.";


(* ::Input::Initialization::Plain:: *)
SetVars::usage="SetVars[var_List] setup the parameters used to describe the space.";
$constants::usage="$constants setup all varibales which are considered constant with respect to space parameters.";
SetMetric::usage="SetMetric[g_List] setup the metric tensor \*SubscriptBox[\"g\", \"\[Mu]\[Nu]\"]";


(* ::Input::Initialization::Plain:: *)
AdjustIndex::usage="AdjustIndex[mat_,{{pos1,rd1},{pos2,rd2},...}] raise Tensor mat's index at position pos1 if rd1=1, and reduce if rd1=-1, same to pos2,... .";


(* ::Code::Initialization::Plain:: *)
ConvertTensorIndex::usage="ConvertMatrixIndex[{symb_,sub_,super_}] convert the component of tensor using \*SubscriptBox[\"g\", \"\[Mu]\[Nu]\"] and \*SuperscriptBox[\"g\", \"\[Mu]\[Nu]\"] according to definitions stored.";


(* ::Code::Initialization::Plain:: *)
FreeIndexes::usage="IndexCombination[expr_] determines free indexes and their position(up/down) of the result.";


(* ::Code::Initialization::Plain:: *)
FreeParentheses::usage="FreeParentheses[expr_] determines free parentheses' order in an expression.";


(* ::Code::Initialization::Plain:: *)
TensorOperate::usage="TensorOperate[TensorA,TensorB] do TensorC = TensorA TensorB where TensorB is a value tensor and return a value tensor Tensor C.";


(* ::Code::Initialization::Plain:: *)
EvaluateEisteinSummation::usage="EvaluateEisteinSummation[expr_] Evaluate the tensor expression inside using Einstein summation rules.";
