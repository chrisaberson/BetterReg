# BetterReg
Better statistics for regression (OLS and logistic)

This is current the workspace for a package that includes tools for getting better statistics out of regression-based models. Currently, there is a single function that calculates partial and semipartial correlations (and their squared values) in multiple regression models with up to 10 predictors. The squared semipartial correlation, although somewhat rarely used, provides an index of the unique variance explained by a predictor. This is the equivalent to running a model with all but one variable and then addressing R-square change following addition of the final variable. (Note: presently only semipartials are available). 

Planned functions include: Likelihood ratio chi-square and R-square analogs (a.k.a. psuedo R-square) for logistic models. 

Presently the only function available is called *parts*. To use the function, first download and then open it. Select all text (you can simply click control-A ... or whatever you Mac people do) and run everything. Once you've done this you will see in your environment (upper right hand corner of the R Studio interface) a function called parts. parts takes two items for input. The first is your regression model object (i.e., whatever you called your analysis). The second is the number of predictors. For example, if you produced your model with the following code, then the model would be Model.2 and the number of predictors six. 

Model.2<-lm(formula = AgreeAA ~ diverse + fairness + merit + NeedAA + denied + Future, data = aadata)

With the parts function simply issue the following code (after having activated the function). 

parts(model = Model.2, preds = 6) 

That't it. Boom, all the stats. 

A video demonstrating this function exists here: https://youtu.be/ZpUPMSOlZVQ
