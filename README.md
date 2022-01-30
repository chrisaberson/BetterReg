# BetterReg
Better statistics for regression (OLS and logistic)

This is current the workspace for a package that includes tools for getting better statistics out of regression-based models. Currently, there is a single function that calculates partial and semipartial correlations (and their squared values) in multiple regression models with up to 10 predictors. The squared semipartial correlation, although somewhat rarely used, provides an index of the unique variance explained by a predictor. This is the equivalent to running a model with all but one variable and then addressing R-square change following addition of the final variable. 

Planned functions include: Likelihood ratio chi-square and R-square analogs (a.k.a. psuedo R-square) for logistic models. 
