(* ::Package:: *)

(* ::Section:: *)
(*Messages in Evaluation Phase*)


AddTensorToDataset::keys="The given tensor is not in the standard form. It should contain three keys: \"Symbol\",\"IndexPosition\", and \"Value\".";
AddTensorToDataset::nmatchdim="The given tensor's dimension(`1`) and the dimension of space(`2`) do not match.";
AddTensorToDataset::nmatchind="The given tensor's depth(`1`) and the number of indexes(`2`) do not match, it might be possible that you didn't input a square array.";


SetMetric::dim="Dimension of \*SubscriptBox[\"g\", \"\[Mu]\[Nu]\"] is invalid!";


ConvertTensorIndex::nstored="Cannot find definition of `1` in definition dataset.";
ConvertTensorIndex::invindex="Contraction of `1` at index position `2` is invalid. No index is allowed to appear for more than 2 times.";
ConvertTensorIndex::invindexpos="Contraction of `1` at index position `2` is invalid. Indexes awaiting summation should be at superscript and subscript respectively.";
ConvertTensorIndex::invparent="Mismatched parentheses in `1`";
ConvertTensorIndex::dtindex="Derivative symbol `1` has 0 or more than 2 indexes, please manually expand them.";


FreeIndexes::invindex="Contraction of `1` is invalid. No index is allowed to appear for more than 2 times and indexes awaiting summation should be at superscript and subscript respectively.";


FreeParentheses::invparent="Mismatched parentheses in `1`";


EvaluateEisteinSummation::indexmismatch="Cannot add up expression `1` whose components has different free indexes and/or free indexes' positions.";
EvaluateEisteinSummation::parentinsum="Free parentheses appeared in summation `1`, which is not allowed.\nIf you insists, please expand the terms and retry.";
