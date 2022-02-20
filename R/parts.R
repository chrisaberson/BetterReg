#'Compute squared semi partial correlatoins for Multiple Regression
#'
#'@param model name of model
#'@param pred number of predictors
#'
#'@examples
#'\donttest{mymodel<-lm(y~x1+x2+x3+x4+x5, data=testreg)}
#'\donttest{parts(model=mymodel, pred=5)}
#'
#'@return Squared semipartial correlations for MRC with up to 10 predictors
#'@export
#'
#'

parts<-function(model=NULL, pred=NULL){

  values<-summary(model)
  R2<-values$r.squared[1]
  dfr<-values$df[2]

if (pred=="2"){
t1<-values$coefficients[2,3]
t2<-values$coefficients[3,3]

#Calculates the sr2 values using the formula earlier in the handout
sr1<-round(((t1^2)/dfr)*(1-R2),3)
sr2<-round(((t2^2)/dfr)*(1-R2),3)

#Calculates semi partial corr
semi1<-round(sqrt(sr1),3)
semi2<-round(sqrt(sr2),3)

message("Predictor 1: semi partial = ", semi1, "; squared semipartial = ",sr1)
message("Predictor 2: semi partial = ", semi2, "; squared semipartial = ",sr2)
}

if (pred=="3"){
  t1<-values$coefficients[2,3]
  t2<-values$coefficients[3,3]
  t3<-values$coefficients[4,3]

  #Calculates the sr2 values using the formula earlier in the handout
  sr1<-round(((t1^2)/dfr)*(1-R2),3)
  sr2<-round(((t2^2)/dfr)*(1-R2),3)
  sr3<-round(((t3^2)/dfr)*(1-R2),3)

  #Calculates semi partial corr
  semi1<-round(sqrt(sr1),3)
  semi2<-round(sqrt(sr2),3)
  semi3<-round(sqrt(sr3),3)

  message("Predictor 1: semi partial = ", semi1, "; squared semipartial = ",sr1)
  message("Predictor 2: semi partial = ", semi2, "; squared semipartial = ",sr2)
  message("Predictor 3: semi partial = ", semi3, "; squared semipartial = ",sr3)
}
if (pred=="4"){

  t1<-values$coefficients[2,3]
  t2<-values$coefficients[3,3]
  t3<-values$coefficients[4,3]
  t4<-values$coefficients[5,3]

  #Calculates the sr2 values using the formula earlier in the handout
  sr1<-round(((t1^2)/dfr)*(1-R2),3)
  sr2<-round(((t2^2)/dfr)*(1-R2),3)
  sr3<-round(((t3^2)/dfr)*(1-R2),3)
  sr4<-round(((t4^2)/dfr)*(1-R2),3)

  #Calculates semi partial corr
  semi1<-round(sqrt(sr1),3)
  semi2<-round(sqrt(sr2),3)
  semi3<-round(sqrt(sr3),3)
  semi4<-round(sqrt(sr4),3)

  message("Predictor 1: semi partial = ", semi1, "; squared semipartial = ",sr1)
  message("Predictor 2: semi partial = ", semi2, "; squared semipartial = ",sr2)
  message("Predictor 3: semi partial = ", semi3, "; squared semipartial = ",sr3)
  message("Predictor 4: semi partial = ", semi4, "; squared semipartial = ",sr4)


}
if (pred=="5"){

  t1<-values$coefficients[2,3]
  t2<-values$coefficients[3,3]
  t3<-values$coefficients[4,3]
  t4<-values$coefficients[5,3]
  t5<-values$coefficients[6,3]

  #Calculates the sr2 values using the formula earlier in the handout
  sr1<-round(((t1^2)/dfr)*(1-R2),3)
  sr2<-round(((t2^2)/dfr)*(1-R2),3)
  sr3<-round(((t3^2)/dfr)*(1-R2),3)
  sr4<-round(((t4^2)/dfr)*(1-R2),3)
  sr5<-round(((t5^2)/dfr)*(1-R2),3)

  #Calculates semi partial corr
  semi1<-round(sqrt(sr1),3)
  semi2<-round(sqrt(sr2),3)
  semi3<-round(sqrt(sr3),3)
  semi4<-round(sqrt(sr4),3)
  semi5<-round(sqrt(sr5),3)

  message("Predictor 1: semi partial = ", semi1, "; squared semipartial = ",sr1)
  message("Predictor 2: semi partial = ", semi2, "; squared semipartial = ",sr2)
  message("Predictor 3: semi partial = ", semi3, "; squared semipartial = ",sr3)
  message("Predictor 4: semi partial = ", semi4, "; squared semipartial = ",sr4)
  message("Predictor 5: semi partial = ", semi5, "; squared semipartial = ",sr5)

}
if (pred=="6"){

  t1<-values$coefficients[2,3]
  t2<-values$coefficients[3,3]
  t3<-values$coefficients[4,3]
  t4<-values$coefficients[5,3]
  t5<-values$coefficients[6,3]
  t6<-values$coefficients[7,3]


  #Calculates the sr2 values using the formula earlier in the handout
  sr1<-round(((t1^2)/dfr)*(1-R2),3)
  sr2<-round(((t2^2)/dfr)*(1-R2),3)
  sr3<-round(((t3^2)/dfr)*(1-R2),3)
  sr4<-round(((t4^2)/dfr)*(1-R2),3)
  sr5<-round(((t5^2)/dfr)*(1-R2),3)
  sr6<-round(((t6^2)/dfr)*(1-R2),3)

  #Calculates semi partial corr
  semi1<-round(sqrt(sr1),3)
  semi2<-round(sqrt(sr2),3)
  semi3<-round(sqrt(sr3),3)
  semi4<-round(sqrt(sr4),3)
  semi5<-round(sqrt(sr5),3)
  semi6<-round(sqrt(sr6),3)

  message("Predictor 1: semi partial = ", semi1, "; squared semipartial = ",sr1)
  message("Predictor 2: semi partial = ", semi2, "; squared semipartial = ",sr2)
  message("Predictor 3: semi partial = ", semi3, "; squared semipartial = ",sr3)
  message("Predictor 4: semi partial = ", semi4, "; squared semipartial = ",sr4)
  message("Predictor 5: semi partial = ", semi5, "; squared semipartial = ",sr5)
  message("Predictor 6: semi partial = ", semi6, "; squared semipartial = ",sr6)}

  if (pred=="7"){

    t1<-values$coefficients[2,3]
    t2<-values$coefficients[3,3]
    t3<-values$coefficients[4,3]
    t4<-values$coefficients[5,3]
    t5<-values$coefficients[6,3]
    t6<-values$coefficients[7,3]
    t7<-values$coefficients[8,3]


    #Calculates the sr2 values using the formula earlier in the handout
    sr1<-round(((t1^2)/dfr)*(1-R2),3)
    sr2<-round(((t2^2)/dfr)*(1-R2),3)
    sr3<-round(((t3^2)/dfr)*(1-R2),3)
    sr4<-round(((t4^2)/dfr)*(1-R2),3)
    sr5<-round(((t5^2)/dfr)*(1-R2),3)
    sr6<-round(((t6^2)/dfr)*(1-R2),3)
    sr7<-round(((t7^2)/dfr)*(1-R2),3)

    #Calculates semi partial corr
    semi1<-round(sqrt(sr1),3)
    semi2<-round(sqrt(sr2),3)
    semi3<-round(sqrt(sr3),3)
    semi4<-round(sqrt(sr4),3)
    semi5<-round(sqrt(sr5),3)
    semi6<-round(sqrt(sr6),3)
    semi7<-round(sqrt(sr7),3)

    message("Predictor 1: semi partial = ", semi1, "; squared semipartial = ",sr1)
    message("Predictor 2: semi partial = ", semi2, "; squared semipartial = ",sr2)
    message("Predictor 3: semi partial = ", semi3, "; squared semipartial = ",sr3)
    message("Predictor 4: semi partial = ", semi4, "; squared semipartial = ",sr4)
    message("Predictor 5: semi partial = ", semi5, "; squared semipartial = ",sr5)
    message("Predictor 6: semi partial = ", semi6, "; squared semipartial = ",sr6)
    message("Predictor 7: semi partial = ", semi7, "; squared semipartial = ",sr7)}

  if (pred=="8"){

    t1<-values$coefficients[2,3]
    t2<-values$coefficients[3,3]
    t3<-values$coefficients[4,3]
    t4<-values$coefficients[5,3]
    t5<-values$coefficients[6,3]
    t6<-values$coefficients[7,3]
    t7<-values$coefficients[8,3]
    t8<-values$coefficients[9,3]


    #Calculates the sr2 values using the formula earlier in the handout
    sr1<-round(((t1^2)/dfr)*(1-R2),3)
    sr2<-round(((t2^2)/dfr)*(1-R2),3)
    sr3<-round(((t3^2)/dfr)*(1-R2),3)
    sr4<-round(((t4^2)/dfr)*(1-R2),3)
    sr5<-round(((t5^2)/dfr)*(1-R2),3)
    sr6<-round(((t6^2)/dfr)*(1-R2),3)
    sr7<-round(((t7^2)/dfr)*(1-R2),3)
    sr8<-round(((t8^2)/dfr)*(1-R2),3)

    #Calculates semi partial corr
    semi1<-round(sqrt(sr1),3)
    semi2<-round(sqrt(sr2),3)
    semi3<-round(sqrt(sr3),3)
    semi4<-round(sqrt(sr4),3)
    semi5<-round(sqrt(sr5),3)
    semi6<-round(sqrt(sr6),3)
    semi7<-round(sqrt(sr7),3)
    semi8<-round(sqrt(sr8),3)

    message("Predictor 1: semi partial = ", semi1, "; squared semipartial = ",sr1)
    message("Predictor 2: semi partial = ", semi2, "; squared semipartial = ",sr2)
    message("Predictor 3: semi partial = ", semi3, "; squared semipartial = ",sr3)
    message("Predictor 4: semi partial = ", semi4, "; squared semipartial = ",sr4)
    message("Predictor 5: semi partial = ", semi5, "; squared semipartial = ",sr5)
    message("Predictor 6: semi partial = ", semi6, "; squared semipartial = ",sr6)
    message("Predictor 7: semi partial = ", semi7, "; squared semipartial = ",sr7)
    message("Predictor 8: semi partial = ", semi8, "; squared semipartial = ",sr8)}


  if (pred=="9"){

    t1<-values$coefficients[2,3]
    t2<-values$coefficients[3,3]
    t3<-values$coefficients[4,3]
    t4<-values$coefficients[5,3]
    t5<-values$coefficients[6,3]
    t6<-values$coefficients[7,3]
    t7<-values$coefficients[8,3]
    t8<-values$coefficients[9,3]
    t9<-values$coefficients[10,3]


    #Calculates the sr2 values using the formula earlier in the handout
    sr1<-round(((t1^2)/dfr)*(1-R2),3)
    sr2<-round(((t2^2)/dfr)*(1-R2),3)
    sr3<-round(((t3^2)/dfr)*(1-R2),3)
    sr4<-round(((t4^2)/dfr)*(1-R2),3)
    sr5<-round(((t5^2)/dfr)*(1-R2),3)
    sr6<-round(((t6^2)/dfr)*(1-R2),3)
    sr7<-round(((t7^2)/dfr)*(1-R2),3)
    sr8<-round(((t8^2)/dfr)*(1-R2),3)
    sr9<-round(((t9^2)/dfr)*(1-R2),3)

    #Calculates semi partial corr
    semi1<-round(sqrt(sr1),3)
    semi2<-round(sqrt(sr2),3)
    semi3<-round(sqrt(sr3),3)
    semi4<-round(sqrt(sr4),3)
    semi5<-round(sqrt(sr5),3)
    semi6<-round(sqrt(sr6),3)
    semi7<-round(sqrt(sr7),3)
    semi8<-round(sqrt(sr8),3)
    semi9<-round(sqrt(sr9),3)

    message("Predictor 1: semi partial = ", semi1, "; squared semipartial = ",sr1)
    message("Predictor 2: semi partial = ", semi2, "; squared semipartial = ",sr2)
    message("Predictor 3: semi partial = ", semi3, "; squared semipartial = ",sr3)
    message("Predictor 4: semi partial = ", semi4, "; squared semipartial = ",sr4)
    message("Predictor 5: semi partial = ", semi5, "; squared semipartial = ",sr5)
    message("Predictor 6: semi partial = ", semi6, "; squared semipartial = ",sr6)
    message("Predictor 7: semi partial = ", semi7, "; squared semipartial = ",sr7)
    message("Predictor 8: semi partial = ", semi8, "; squared semipartial = ",sr8)
    message("Predictor 9: semi partial = ", semi9, "; squared semipartial = ",sr9)}

  if (pred=="10"){

    t1<-values$coefficients[2,3]
    t2<-values$coefficients[3,3]
    t3<-values$coefficients[4,3]
    t4<-values$coefficients[5,3]
    t5<-values$coefficients[6,3]
    t6<-values$coefficients[7,3]
    t7<-values$coefficients[8,3]
    t8<-values$coefficients[9,3]
    t9<-values$coefficients[10,3]
    t10<-values$coefficients[11,3]

    #Calculates the sr2 values using the formula earlier in the handout
    sr1<-round(((t1^2)/dfr)*(1-R2),3)
    sr2<-round(((t2^2)/dfr)*(1-R2),3)
    sr3<-round(((t3^2)/dfr)*(1-R2),3)
    sr4<-round(((t4^2)/dfr)*(1-R2),3)
    sr5<-round(((t5^2)/dfr)*(1-R2),3)
    sr6<-round(((t6^2)/dfr)*(1-R2),3)
    sr7<-round(((t7^2)/dfr)*(1-R2),3)
    sr8<-round(((t8^2)/dfr)*(1-R2),3)
    sr9<-round(((t9^2)/dfr)*(1-R2),3)
    sr10<-round(((t10^2)/dfr)*(1-R2),3)

    #Calculates semi partial corr
    semi1<-round(sqrt(sr1),3)
    semi2<-round(sqrt(sr2),3)
    semi3<-round(sqrt(sr3),3)
    semi4<-round(sqrt(sr4),3)
    semi5<-round(sqrt(sr5),3)
    semi6<-round(sqrt(sr6),3)
    semi7<-round(sqrt(sr7),3)
    semi8<-round(sqrt(sr8),3)
    semi9<-round(sqrt(sr9),3)
    semi10<-round(sqrt(sr10),3)

    message("Predictor 1: semi partial = ", semi1, "; squared semipartial = ",sr1)
    message("Predictor 2: semi partial = ", semi2, "; squared semipartial = ",sr2)
    message("Predictor 3: semi partial = ", semi3, "; squared semipartial = ",sr3)
    message("Predictor 4: semi partial = ", semi4, "; squared semipartial = ",sr4)
    message("Predictor 5: semi partial = ", semi5, "; squared semipartial = ",sr5)
    message("Predictor 6: semi partial = ", semi6, "; squared semipartial = ",sr6)
    message("Predictor 7: semi partial = ", semi7, "; squared semipartial = ",sr7)
    message("Predictor 8: semi partial = ", semi8, "; squared semipartial = ",sr8)
    message("Predictor 9: semi partial = ", semi9, "; squared semipartial = ",sr9)
    message("Predictor 10: semi partial = ", semi10, "; squared semipartial = ",sr10)}

}

