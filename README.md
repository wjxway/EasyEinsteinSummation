# Easy Einstein Summation

This is a Mathematica package for doing tensor operations using Einstein summation rules, which is especially useful if you are working with special relativity or elasticity.

## Functionality

This package:

1. **Does basic tensor operations including:**
   - Tensor product
   - Tensor contract
   - Symmetrize and anti-symmetrize
   - Derivative & parallel transport derivative in curved space.
2. **Has complete support for Einstein summation rules.**
3. **Supports calculation curved space of arbitrary dimension.**

## Features

This package has the following exceptional features which significantly reduces your effort when computing Einstein summation.

1. **WRITE AND COMPUTE!!!** You can painlessly input the expression using virtually anything:
   - Mathematica built-in non-linear type system or Math Assistant.
   - Windows' built-in Math Input Panel. (which allows you to **HANDWRITE** the expression!!!)
   - Word Equation or other sources which use MathML (e.g. MathType).
   - ![](https://latex.codecogs.com/svg.latex?TeX) strings (Sometimes buggy due to internal errors of Mathematica).
2. **Use Operators everywhere!** You can use ![](https://latex.codecogs.com/svg.latex?%5Cpartial) and ![](https://latex.codecogs.com/svg.latex?%5Cnabla) as operators! You may also get a result in the form of a operator!
3. **Free Symmetrize!** Support a very free usage of symmetrize symbol () and anti-symmetrize symbol []. e.g. you can write ![](https://latex.codecogs.com/svg.latex?%5Cnabla%5E%7B%5B%5Cmu%7D%20A%5E%7B%5Cnu%5D%7D), or even ![](https://latex.codecogs.com/svg.latex?A_%7B%28%5Cmu%7D%20%5Cnabla_%5Cnu%20B_%7B%5Csigma%29%7D%5E%7B%5C%20%5C%20%5Cnu%7D), which do summation on ![](https://latex.codecogs.com/svg.latex?%5Cnu) first then symmetrize ![](https://latex.codecogs.com/svg.latex?%5Cmu%2C%20%5Csigma).
4. **Support symbolic tensors!!!** You can work on subjects even if you don't know the exact value or even the exact dimension, thanks to Mathematica's latest feature!!!



## Prerequisites

To use this package, **Mathematica 11.3** or later are required. (There is a slight possibility that Version 10 can work, if you have tried, please notify me and I'll update~) [*Wolfram Engine*](https://www.wolfram.com/engine/) is also a viable option, which is freely available to developers.



## Downloads and Installation

The simplest way to configure your package is executing the following code:

```mathematica
PacletInstall["https://github.com/wjxway/EasyEinsteinSummation/releases/download/0.2.0/EinsteinSummation-0.2.0.paclet"]
```

Or you can download the lastest release of EinsteinSummation package on the [Releases page](https://github.com/wjxway/EasyEinsteinSummation/releases), and install with:

```mathematica
PacletInstall["directory"]
```

To load and test the package, execute

```mathematica
<<EinsteinSummation`
SetVars[{x,y}];
$constants={a,b};
SetMetric[DiagonalMatrix@{a,b}]
AddTensorToDataset[<|"Symbol"->"A","IndexPosition"->{-1},"Value"->{p[x,y],q[x,y]}|>]
EvaluateEisteinSummation["\*SuperscriptBox[\(g\), \(pq\)]\*SubscriptBox[\(\[Del]\), \([p\)]\*SubscriptBox[\(\[Del]\), \(\(q\)\(]\)\)]\*SuperscriptBox[\(A\), \(r\)]"]["Value"]//Normal
```

If you get {0,0} then the package is (probably) working right!



## Basic Usage

As a demonstration of basic workflow, here we will try to figure out the Killing vector field in a 2-D spherical surface.



**Step 1.** First load the package:

```mathematica
<<EinsteinSummation`
```

**Step 2.** Setup parameters of space and the corresponding metric. 

By default, variables are {t,x,y,z} and metric is diag{-1,1,1,1}, which corresponds to 4-D Minkovski spacetime.

Here, we are working on a spherical surface ![](https://latex.codecogs.com/svg.latex?ds%5E2%3Dd%5Ctheta%5E2%2Bsin%5E2%5Ctheta%5C%2Cd%5Cphi%5E2), so ![](https://latex.codecogs.com/svg.latex?g_%7B%5Cmu%5Cnu%7D%3Ddiag%5C%7B1%2Csin%5E2%5Ctheta%5C%7D). To optimize your reading, we will use t and p as parameters.

```mathematica
SetVars[{t, p}];
SetMetric[DiagonalMatrix@{1, Sin[t]^2}];
```

**Step 3.** Then we need to define a (1,0) style vector ![](https://latex.codecogs.com/svg.latex?K%5E%5Cmu) as Killing vector.

Here we encountered our first vector(tensor). To input it, you have two choices:

1. Manually input, here you will need to specify three properties: the name of your tensor, "Symbol", the position of its indexes, "IndexPosition" which takes 1 for upper index and -1 for lower index, and the value of your tensor, "Value":

   ```mathematica
   AddTensorToDataset[<|"Symbol" -> "K", "IndexPosition" -> {1}, "Value" -> {kt[t, p], kp[t, p]}|>];
   ```

2. Or you might do the other way, You can:

   1. Use Mathematica's built-in Basic Math Assistant (which can be found under Palettes menu) and type in ![](https://latex.codecogs.com/svg.latex?K%5E%5Cmu) directly, and write like.

   ```mathematica
   AddTensorToDataset["\!\(\*SuperscriptBox[\(K\), \(\[Mu]\)]\)", {kt[t, p], kp[t, p]}];
   ```
   BTW, Don't be frightened by the long expression, this is just how Mathematica format things, not how you should input them.

   2. Use LaTeX To Type in the equation.

   ```mathematica
   AddTensorToDataset["K^\\mu", {kt[t, p], kp[t, p]}];
   ```

   3. Use Word/MathType To Type in equation (A bit complicated, For details check this [wiki page](https://github.com/wjxway/EasyEinsteinSummation/wiki))

**Step 4.** Type in the equation same as the step above and evaluate!!!

   ```mathematica
   res=EvaluateEisteinSummation["\!\(\*SubscriptBox[\(\[Del]\), \((\[Mu]\)]\)\!\(\*SubscriptBox[\(K\), \(\(\[Nu]\)\()\)\)]\)"]
   ```

   Unfortunately, you cannot use TeX Strings here due to some internal bugs of Mathematica.

**Step 5.** Analyze the result.

You will obtain a ``tensorSymb`` object which shows you all sorts of information about the resulting tensor. From ``res["IndexName"]`` you know that the free indexes are ![](https://latex.codecogs.com/svg.latex?%5Cmu%2C%5C%3B%5Cnu) and there're no symmetry marker left. and from ``res["IndexPosition"]`` you know that the result is a (0,2) type tensor. an finally, from ``res["Value"]`` you know the computation's result, which is in the form of ``SparseArray`` in this case.

You can do further analysis like ``TensorSymmetry@res["Value"]`` which tells you that the resulting tensor is symmetric.

If you want to do something more later, you can also store it like:

```mathematica
AddTensorToDataset["Q", res];
```

so next time when you want to use this you only need to write Q in your string.



For more details, please check the [wiki page](https://github.com/wjxway/EasyEinsteinSummation/wiki) or the Mathematica documentation built in the package (Not ready yet~).
