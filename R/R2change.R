#'R-square change for Hierarchical Multiple Regression
#'@param model1 first regression model
#'@param model2 second regression model
#'@examples


R2change<-function(model1=NULL, model2=NULL){
comp<-stats::anova(model1,model2)
df1<-comp$Df[2]
df2<-mymodel2$df.residual
m1<-summary(model1)
m2<-summary(model2)
r2.1<-m1$r.squared
r2.2<-m2$r.squared
R2change<-round((r2.2-r2.1),3)
F<-round((comp$F[2]),3)
p<-comp$`Pr(>F)`[2]
message("R-square change = ", R2change)
message("F(", df1,",", df2,") = ", F, ", p = ", p)
}
