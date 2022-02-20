#'Compute tolerance for Multiple Regression
#'@param model name of model
#'@examples
#'\donttest{mymodel<-lm(y~x1+x2+x3+x4+x5, data=testreg)}
#'\donttest{tolerance(model=mymodel)}
#'@return Tolerance for MR
#'@export
#'

tolerance<-function(model=NULL){
1/car::vif(model)}
