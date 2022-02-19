#'Psuedo R-square Values for Binomial Logistic Regression
#'
#'@param model name of model
#'
#'@examples
#'mymodel<-glm(y~x1+x2+x3+x4, testlog,family = binomial())
#'psuedo(model=mymodel)
#'
#'@return Psuedo R-square Values for Logistic Regression
#'@export
#'
#'

psuedo<-function(model=NULL)
  {
nulldev<-mymodel$null.deviance
moddev<-mymodel$deviance
n<-mymodel$df.null+1
R2_L<-round(1-(moddev/nulldev),3)
R2_M <-round(1-exp(-(nulldev-moddev)/n),3)
R2_N <-round(R2_M/(1-exp(-(nulldev/n))),3)
message("Likelihood Ratio R-squared (McFadden, Recommended) = ",R2_L)
message("Cox-Snell R-squared) = ",R2_M)
message("Nagelkerk R-squared  = ",R2_N)
}



