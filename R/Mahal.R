#'Compute Mahalanobis Distance for Multiple Regression
#'
#'@param model name of model
#'@param pred number of predictors
#'@param values number of Mahal values to print (highest values). Default is 10
#'
#'@examples
#'\donttest{mymodel<-lm(y~x1+x2+x3+x4, testreg)}
#'\donttest{Mahal(model=mymodel, pred=5, values = 10)}
#'
#'@return Mahalanobis Distance to detect MV outliers
#'@export
#'
#'

Mahal<-function(model=NULL, pred=NULL, values=5){
  hat<-hatvalues(model)
  n<-model$df.residual + pred + 1
  mah<-((n-1)*((hat)))-1
  tail(sort(mah),values)
}
