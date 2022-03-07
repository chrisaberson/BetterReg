#'Comparing Independent Coefficients in Multiple Regression
#'@param model1 Summary of first model (see example for how to summarize)
#'@param model2 Summary of second model (see example for how to summarize)
#'@param comps Type of comparison. "abs" - absolute value of coefficient
#'(recommended). "raw" raw values of coefficient
#'@examples
#'y_1<-rnorm(200); x1_1<-rnorm(200); x2_1<-rnorm(200)
#'y_2<-rnorm(200); x1_2<-rnorm(200);x2_2<-rnorm(200)
#'df1<-as.data.frame(cbind(y_1, x1_1,x2_1))
#'df2<-as.data.frame(cbind(y_2, x1_2,x2_2))
#'model1_2<-summary(lm(y_1~x1_1+x2_1, data=df1))
#'model2_2<-summary(lm(y_2~x1_2+x2_2, data=df2))
#'indbcomp(model1 = model1_2, model2 = model2_2, comps="abs")
#'@return Comparing Independent Coefficients in Multiple Regression
#'@export
#'



indbcomp<-function(model1=NULL, model2=NULL, comps="abs")
{

  pred1<-model1$df[1]-1
  pred2<-model2$df[1]-1

  try(if(pred1!=pred2) stop("Models must be identical"))

  if ((pred1 =="2") && (comps=="abs")){

  b1_1<-(model1$coefficients)[2,1]
  b2_1<-(model1$coefficients)[3,1]
  b1_2<-(model2$coefficients)[2,1]
  b2_2<-(model2$coefficients)[3,1]
  seb1_1<-(model1$coefficients)[2,2]
  seb2_1<-(model1$coefficients)[3,2]
  seb1_2<-(model2$coefficients)[2,2]
  seb2_2<-(model2$coefficients)[3,2]
  sebb1<-((seb1_1^2)+(seb2_1^2))^.5
  sebb2<-((seb1_2^2)+(seb2_2^2))^.5
  t1<-abs(abs(b1_1)-abs(b1_2))/sebb1
  t2<-abs(abs(b2_1)-abs(b2_2))/sebb2
  df1<-model1$df[2]+model2$df[2]
  df2<-model2$df[2]
  p1<-2*round(1-(pt(q=abs(t1), df=df1, lower.tail=TRUE)),3)
  p2<-2*round(1-(pt(q=abs(t2), df=df2, lower.tail=TRUE)),3)
  t1<-round((t1),3)
  t2<-round((t2),3)
  message("Predictor 1: "," t = ", t1,", p = ", p1)
  message("Predictor 2: "," t = ", t2,", p = ", p2)
  }


  if ((pred1 =="3") && (comps=="abs")){

    b1_1<-(model1$coefficients)[2,1]
    b2_1<-(model1$coefficients)[3,1]
    b3_1<-(model1$coefficients)[4,1]
    b1_2<-(model2$coefficients)[2,1]
    b2_2<-(model2$coefficients)[3,1]
    b3_2<-(model2$coefficients)[4,1]
    seb1_1<-(model1$coefficients)[2,2]
    seb2_1<-(model1$coefficients)[3,2]
    seb3_1<-(model1$coefficients)[4,2]
    seb1_2<-(model2$coefficients)[2,2]
    seb2_2<-(model2$coefficients)[3,2]
    seb3_2<-(model2$coefficients)[4,2]
    sebb1<-((seb1_1^2)+(seb1_2^2))^.5
    sebb2<-((seb2_1^2)+(seb2_2^2))^.5
    sebb3<-((seb3_1^2)+(seb3_2^2))^.5
    t1<-abs(abs(b1_1)-abs(b1_2))/sebb1
    t2<-abs(abs(b2_1)-abs(b2_2))/sebb2
    t3<-abs(abs(b3_1)-abs(b3_2))/sebb3
    df1<-model1$df[2]+model2$df[2]
    p1<-2*round(1-(pt(q=abs(t1), df=df1, lower.tail=TRUE)),3)
    p2<-2*round(1-(pt(q=abs(t2), df=df1, lower.tail=TRUE)),3)
    p3<-2*round(1-(pt(q=abs(t3), df=df1, lower.tail=TRUE)),3)
    t1<-round((t1),3)
    t2<-round((t2),3)
    t3<-round((t3),3)
    message("Predictor 1: "," t = ", t1,", p = ", p1)
    message("Predictor 2: "," t = ", t2,", p = ", p2)
    message("Predictor 3: "," t = ", t3,", p = ", p3)
  }


  if ((pred1 =="4") && (comps=="abs")){

    b1_1<-(model1$coefficients)[2,1]
    b2_1<-(model1$coefficients)[3,1]
    b3_1<-(model1$coefficients)[4,1]
    b4_1<-(model1$coefficients)[5,1]
    b1_2<-(model2$coefficients)[2,1]
    b2_2<-(model2$coefficients)[3,1]
    b3_2<-(model2$coefficients)[4,1]
    b4_2<-(model2$coefficients)[5,1]
    seb1_1<-(model1$coefficients)[2,2]
    seb2_1<-(model1$coefficients)[3,2]
    seb3_1<-(model1$coefficients)[4,2]
    seb4_1<-(model1$coefficients)[5,2]
    seb1_2<-(model2$coefficients)[2,2]
    seb2_2<-(model2$coefficients)[3,2]
    seb3_2<-(model2$coefficients)[4,2]
    seb4_2<-(model2$coefficients)[5,2]
    sebb1<-((seb1_1^2)+(seb1_2^2))^.5
    sebb2<-((seb2_1^2)+(seb2_2^2))^.5
    sebb3<-((seb3_1^2)+(seb3_2^2))^.5
    sebb4<-((seb4_1^2)+(seb4_2^2))^.5
    t1<-abs(abs(b1_1)-abs(b1_2))/sebb1
    t2<-abs(abs(b2_1)-abs(b2_2))/sebb2
    t3<-abs(abs(b3_1)-abs(b3_2))/sebb3
    t4<-abs(abs(b4_1)-abs(b4_2))/sebb4
    df1<-model1$df[2]+model2$df[2]
    p1<-2*round(1-(pt(q=abs(t1), df=df1, lower.tail=TRUE)),3)
    p2<-2*round(1-(pt(q=abs(t2), df=df1, lower.tail=TRUE)),3)
    p3<-2*round(1-(pt(q=abs(t3), df=df1, lower.tail=TRUE)),3)
    p4<-2*round(1-(pt(q=abs(t4), df=df1, lower.tail=TRUE)),3)
    t1<-round((t1),3)
    t2<-round((t2),3)
    t3<-round((t3),3)
    t4<-round((t4),3)
    message("Predictor 1: "," t = ", t1,", p = ", p1)
    message("Predictor 2: "," t = ", t2,", p = ", p2)
    message("Predictor 3: "," t = ", t3,", p = ", p3)
    message("Predictor 4: "," t = ", t4,", p = ", p4)
  }
  if ((pred1 =="5") && (comps=="abs")){

    b1_1<-(model1$coefficients)[2,1]
    b2_1<-(model1$coefficients)[3,1]
    b3_1<-(model1$coefficients)[4,1]
    b4_1<-(model1$coefficients)[5,1]
    b5_1<-(model1$coefficients)[6,1]
    b1_2<-(model2$coefficients)[2,1]
    b2_2<-(model2$coefficients)[3,1]
    b3_2<-(model2$coefficients)[4,1]
    b4_2<-(model2$coefficients)[5,1]
    b5_2<-(model2$coefficients)[6,1]
    seb1_1<-(model1$coefficients)[2,2]
    seb2_1<-(model1$coefficients)[3,2]
    seb3_1<-(model1$coefficients)[4,2]
    seb4_1<-(model1$coefficients)[5,2]
    seb5_1<-(model1$coefficients)[6,2]
    seb1_2<-(model2$coefficients)[2,2]
    seb2_2<-(model2$coefficients)[3,2]
    seb3_2<-(model2$coefficients)[4,2]
    seb4_2<-(model2$coefficients)[5,2]
    seb5_2<-(model2$coefficients)[6,2]
    sebb1<-((seb1_1^2)+(seb1_2^2))^.5
    sebb2<-((seb2_1^2)+(seb2_2^2))^.5
    sebb3<-((seb3_1^2)+(seb3_2^2))^.5
    sebb4<-((seb4_1^2)+(seb4_2^2))^.5
    sebb5<-((seb5_1^2)+(seb5_2^2))^.5
    t1<-abs(abs(b1_1)-abs(b1_2))/sebb1
    t2<-abs(abs(b2_1)-abs(b2_2))/sebb2
    t3<-abs(abs(b3_1)-abs(b3_2))/sebb3
    t4<-abs(abs(b4_1)-abs(b4_2))/sebb4
    t5<-abs(abs(b5_1)-abs(b5_2))/sebb5
    df1<-model1$df[2]+model2$df[2]
    p1<-2*round(1-(pt(q=abs(t1), df=df1, lower.tail=TRUE)),3)
    p2<-2*round(1-(pt(q=abs(t2), df=df1, lower.tail=TRUE)),3)
    p3<-2*round(1-(pt(q=abs(t3), df=df1, lower.tail=TRUE)),3)
    p4<-2*round(1-(pt(q=abs(t4), df=df1, lower.tail=TRUE)),3)
    p5<-2*round(1-(pt(q=abs(t5), df=df1, lower.tail=TRUE)),3)
    t1<-round((t1),3)
    t2<-round((t2),3)
    t3<-round((t3),3)
    t4<-round((t4),3)
    t5<-round((t5),3)
    message("Predictor 1: "," t = ", t1,", p = ", p1)
    message("Predictor 2: "," t = ", t2,", p = ", p2)
    message("Predictor 3: "," t = ", t3,", p = ", p3)
    message("Predictor 4: "," t = ", t4,", p = ", p4)
    message("Predictor 5: "," t = ", t5,", p = ", p5)
  }
  if ((pred1 =="6") && (comps=="abs")){

    b1_1<-(model1$coefficients)[2,1]
    b2_1<-(model1$coefficients)[3,1]
    b3_1<-(model1$coefficients)[4,1]
    b4_1<-(model1$coefficients)[5,1]
    b5_1<-(model1$coefficients)[6,1]
    b6_1<-(model1$coefficients)[7,1]
    b1_2<-(model2$coefficients)[2,1]
    b2_2<-(model2$coefficients)[3,1]
    b3_2<-(model2$coefficients)[4,1]
    b4_2<-(model2$coefficients)[5,1]
    b5_2<-(model2$coefficients)[6,1]
    b6_2<-(model2$coefficients)[7,1]
    seb1_1<-(model1$coefficients)[2,2]
    seb2_1<-(model1$coefficients)[3,2]
    seb3_1<-(model1$coefficients)[4,2]
    seb4_1<-(model1$coefficients)[5,2]
    seb5_1<-(model1$coefficients)[6,2]
    seb6_1<-(model1$coefficients)[7,2]
    seb1_2<-(model2$coefficients)[2,2]
    seb2_2<-(model2$coefficients)[3,2]
    seb3_2<-(model2$coefficients)[4,2]
    seb4_2<-(model2$coefficients)[5,2]
    seb5_2<-(model2$coefficients)[6,2]
    seb6_2<-(model2$coefficients)[7,2]
    sebb1<-((seb1_1^2)+(seb1_2^2))^.5
    sebb2<-((seb2_1^2)+(seb2_2^2))^.5
    sebb3<-((seb3_1^2)+(seb3_2^2))^.5
    sebb4<-((seb4_1^2)+(seb4_2^2))^.5
    sebb5<-((seb5_1^2)+(seb5_2^2))^.5
    sebb6<-((seb6_1^2)+(seb6_2^2))^.5
    t1<-abs(abs(b1_1)-abs(b1_2))/sebb1
    t2<-abs(abs(b2_1)-abs(b2_2))/sebb2
    t3<-abs(abs(b3_1)-abs(b3_2))/sebb3
    t4<-abs(abs(b4_1)-abs(b4_2))/sebb4
    t5<-abs(abs(b5_1)-abs(b5_2))/sebb5
    t6<-abs(abs(b6_1)-abs(b6_2))/sebb6
    df1<-model1$df[2]+model2$df[2]
    p1<-2*round(1-(pt(q=abs(t1), df=df1, lower.tail=TRUE)),3)
    p2<-2*round(1-(pt(q=abs(t2), df=df1, lower.tail=TRUE)),3)
    p3<-2*round(1-(pt(q=abs(t3), df=df1, lower.tail=TRUE)),3)
    p4<-2*round(1-(pt(q=abs(t4), df=df1, lower.tail=TRUE)),3)
    p5<-2*round(1-(pt(q=abs(t5), df=df1, lower.tail=TRUE)),3)
    p6<-2*round(1-(pt(q=abs(t6), df=df1, lower.tail=TRUE)),3)
    t1<-round((t1),3)
    t2<-round((t2),3)
    t3<-round((t3),3)
    t4<-round((t4),3)
    t5<-round((t5),3)
    t6<-round((t6),3)
    message("Predictor 1: "," t = ", t1,", p = ", p1)
    message("Predictor 2: "," t = ", t2,", p = ", p2)
    message("Predictor 3: "," t = ", t3,", p = ", p3)
    message("Predictor 4: "," t = ", t4,", p = ", p4)
    message("Predictor 5: "," t = ", t5,", p = ", p5)
    message("Predictor 6: "," t = ", t6,", p = ", p6)
  }
  if ((pred1 =="7") && (comps=="abs")){

    b1_1<-(model1$coefficients)[2,1]
    b2_1<-(model1$coefficients)[3,1]
    b3_1<-(model1$coefficients)[4,1]
    b4_1<-(model1$coefficients)[5,1]
    b5_1<-(model1$coefficients)[6,1]
    b6_1<-(model1$coefficients)[7,1]
    b7_1<-(model1$coefficients)[8,1]
    b1_2<-(model2$coefficients)[2,1]
    b2_2<-(model2$coefficients)[3,1]
    b3_2<-(model2$coefficients)[4,1]
    b4_2<-(model2$coefficients)[5,1]
    b5_2<-(model2$coefficients)[6,1]
    b6_2<-(model2$coefficients)[7,1]
    b7_2<-(model2$coefficients)[8,1]
    seb1_1<-(model1$coefficients)[2,2]
    seb2_1<-(model1$coefficients)[3,2]
    seb3_1<-(model1$coefficients)[4,2]
    seb4_1<-(model1$coefficients)[5,2]
    seb5_1<-(model1$coefficients)[6,2]
    seb6_1<-(model1$coefficients)[7,2]
    seb7_1<-(model1$coefficients)[8,2]
    seb1_2<-(model2$coefficients)[2,2]
    seb2_2<-(model2$coefficients)[3,2]
    seb3_2<-(model2$coefficients)[4,2]
    seb4_2<-(model2$coefficients)[5,2]
    seb5_2<-(model2$coefficients)[6,2]
    seb6_2<-(model2$coefficients)[7,2]
    seb7_2<-(model2$coefficients)[8,2]
    sebb1<-((seb1_1^2)+(seb1_2^2))^.5
    sebb2<-((seb2_1^2)+(seb2_2^2))^.5
    sebb3<-((seb3_1^2)+(seb3_2^2))^.5
    sebb4<-((seb4_1^2)+(seb4_2^2))^.5
    sebb5<-((seb5_1^2)+(seb5_2^2))^.5
    sebb6<-((seb6_1^2)+(seb6_2^2))^.5
    sebb7<-((seb7_1^2)+(seb7_2^2))^.5
    t1<-abs(abs(b1_1)-abs(b1_2))/sebb1
    t2<-abs(abs(b2_1)-abs(b2_2))/sebb2
    t3<-abs(abs(b3_1)-abs(b3_2))/sebb3
    t4<-abs(abs(b4_1)-abs(b4_2))/sebb4
    t5<-abs(abs(b5_1)-abs(b5_2))/sebb5
    t6<-abs(abs(b6_1)-abs(b6_2))/sebb6
    t7<-abs(abs(b7_1)-abs(b7_2))/sebb7
    df1<-model1$df[2]+model2$df[2]
    p1<-2*round(1-(pt(q=abs(t1), df=df1, lower.tail=TRUE)),3)
    p2<-2*round(1-(pt(q=abs(t2), df=df1, lower.tail=TRUE)),3)
    p3<-2*round(1-(pt(q=abs(t3), df=df1, lower.tail=TRUE)),3)
    p4<-2*round(1-(pt(q=abs(t4), df=df1, lower.tail=TRUE)),3)
    p5<-2*round(1-(pt(q=abs(t5), df=df1, lower.tail=TRUE)),3)
    p6<-2*round(1-(pt(q=abs(t6), df=df1, lower.tail=TRUE)),3)
    p7<-2*round(1-(pt(q=abs(t7), df=df1, lower.tail=TRUE)),3)
    t1<-round((t1),3)
    t2<-round((t2),3)
    t3<-round((t3),3)
    t4<-round((t4),3)
    t5<-round((t5),3)
    t6<-round((t6),3)
    t7<-round((t7),3)
    message("Predictor 1: "," t = ", t1,", p = ", p1)
    message("Predictor 2: "," t = ", t2,", p = ", p2)
    message("Predictor 3: "," t = ", t3,", p = ", p3)
    message("Predictor 4: "," t = ", t4,", p = ", p4)
    message("Predictor 5: "," t = ", t5,", p = ", p5)
    message("Predictor 6: "," t = ", t6,", p = ", p6)
    message("Predictor 7: "," t = ", t7,", p = ", p7)

  }
  if ((pred1 =="8") && (comps=="abs")){

    b1_1<-(model1$coefficients)[2,1]
    b2_1<-(model1$coefficients)[3,1]
    b3_1<-(model1$coefficients)[4,1]
    b4_1<-(model1$coefficients)[5,1]
    b5_1<-(model1$coefficients)[6,1]
    b6_1<-(model1$coefficients)[7,1]
    b7_1<-(model1$coefficients)[8,1]
    b8_1<-(model1$coefficients)[9,1]
    b1_2<-(model2$coefficients)[2,1]
    b2_2<-(model2$coefficients)[3,1]
    b3_2<-(model2$coefficients)[4,1]
    b4_2<-(model2$coefficients)[5,1]
    b5_2<-(model2$coefficients)[6,1]
    b6_2<-(model2$coefficients)[7,1]
    b7_2<-(model2$coefficients)[8,1]
    b8_2<-(model2$coefficients)[9,1]
    seb1_1<-(model1$coefficients)[2,2]
    seb2_1<-(model1$coefficients)[3,2]
    seb3_1<-(model1$coefficients)[4,2]
    seb4_1<-(model1$coefficients)[5,2]
    seb5_1<-(model1$coefficients)[6,2]
    seb6_1<-(model1$coefficients)[7,2]
    seb7_1<-(model1$coefficients)[8,2]
    seb8_1<-(model1$coefficients)[9,2]
    seb1_2<-(model2$coefficients)[2,2]
    seb2_2<-(model2$coefficients)[3,2]
    seb3_2<-(model2$coefficients)[4,2]
    seb4_2<-(model2$coefficients)[5,2]
    seb5_2<-(model2$coefficients)[6,2]
    seb6_2<-(model2$coefficients)[7,2]
    seb7_2<-(model2$coefficients)[8,2]
    seb8_2<-(model2$coefficients)[9,2]
    sebb1<-((seb1_1^2)+(seb1_2^2))^.5
    sebb2<-((seb2_1^2)+(seb2_2^82))^.5
    sebb3<-((seb3_1^2)+(seb3_2^2))^.5
    sebb4<-((seb4_1^2)+(seb4_2^2))^.5
    sebb5<-((seb5_1^2)+(seb5_2^2))^.5
    sebb6<-((seb6_1^2)+(seb6_2^2))^.5
    sebb7<-((seb7_1^2)+(seb7_2^2))^.5
    sebb8<-((seb8_1^2)+(seb8_2^2))^.5
    t1<-abs(abs(b1_1)-abs(b1_2))/sebb1
    t2<-abs(abs(b2_1)-abs(b2_2))/sebb2
    t3<-abs(abs(b3_1)-abs(b3_2))/sebb3
    t4<-abs(abs(b4_1)-abs(b4_2))/sebb4
    t5<-abs(abs(b5_1)-abs(b5_2))/sebb5
    t6<-abs(abs(b6_1)-abs(b6_2))/sebb6
    t7<-abs(abs(b7_1)-abs(b7_2))/sebb7
    t8<-abs(abs(b8_1)-abs(b8_2))/sebb8
    df1<-model1$df[2]+model2$df[2]
    p1<-2*round(1-(pt(q=abs(t1), df=df1, lower.tail=TRUE)),3)
    p2<-2*round(1-(pt(q=abs(t2), df=df1, lower.tail=TRUE)),3)
    p3<-2*round(1-(pt(q=abs(t3), df=df1, lower.tail=TRUE)),3)
    p4<-2*round(1-(pt(q=abs(t4), df=df1, lower.tail=TRUE)),3)
    p5<-2*round(1-(pt(q=abs(t5), df=df1, lower.tail=TRUE)),3)
    p6<-2*round(1-(pt(q=abs(t6), df=df1, lower.tail=TRUE)),3)
    p7<-2*round(1-(pt(q=abs(t7), df=df1, lower.tail=TRUE)),3)
    p8<-2*round(1-(pt(q=abs(t8), df=df1, lower.tail=TRUE)),3)
    t1<-round((t1),3)
    t2<-round((t2),3)
    t3<-round((t3),3)
    t4<-round((t4),3)
    t5<-round((t5),3)
    t6<-round((t6),3)
    t7<-round((t7),3)
    t8<-round((t8),3)
    message("Predictor 1: "," t = ", t1,", p = ", p1)
    message("Predictor 2: "," t = ", t2,", p = ", p2)
    message("Predictor 3: "," t = ", t3,", p = ", p3)
    message("Predictor 4: "," t = ", t4,", p = ", p4)
    message("Predictor 5: "," t = ", t5,", p = ", p5)
    message("Predictor 6: "," t = ", t6,", p = ", p6)
    message("Predictor 7: "," t = ", t7,", p = ", p7)
    message("Predictor 8: "," t = ", t8,", p = ", p8)
  }
  if ((pred1 =="9") && (comps=="abs")){

    b1_1<-(model1$coefficients)[2,1]
    b2_1<-(model1$coefficients)[3,1]
    b3_1<-(model1$coefficients)[4,1]
    b4_1<-(model1$coefficients)[5,1]
    b5_1<-(model1$coefficients)[6,1]
    b6_1<-(model1$coefficients)[7,1]
    b7_1<-(model1$coefficients)[8,1]
    b8_1<-(model1$coefficients)[9,1]
    b9_1<-(model1$coefficients)[10,1]
    b1_2<-(model2$coefficients)[2,1]
    b2_2<-(model2$coefficients)[3,1]
    b3_2<-(model2$coefficients)[4,1]
    b4_2<-(model2$coefficients)[5,1]
    b5_2<-(model2$coefficients)[6,1]
    b6_2<-(model2$coefficients)[7,1]
    b7_2<-(model2$coefficients)[8,1]
    b8_2<-(model2$coefficients)[9,1]
    b9_2<-(model2$coefficients)[10,1]
    seb1_1<-(model1$coefficients)[2,2]
    seb2_1<-(model1$coefficients)[3,2]
    seb3_1<-(model1$coefficients)[4,2]
    seb4_1<-(model1$coefficients)[5,2]
    seb5_1<-(model1$coefficients)[6,2]
    seb6_1<-(model1$coefficients)[7,2]
    seb7_1<-(model1$coefficients)[8,2]
    seb8_1<-(model1$coefficients)[9,2]
    seb9_1<-(model1$coefficients)[10,2]
    seb1_2<-(model2$coefficients)[2,2]
    seb2_2<-(model2$coefficients)[3,2]
    seb3_2<-(model2$coefficients)[4,2]
    seb4_2<-(model2$coefficients)[5,2]
    seb5_2<-(model2$coefficients)[6,2]
    seb6_2<-(model2$coefficients)[7,2]
    seb7_2<-(model2$coefficients)[8,2]
    seb8_2<-(model2$coefficients)[9,2]
    seb9_2<-(model2$coefficients)[10,2]
    sebb1<-((seb1_1^2)+(seb1_2^2))^.5
    sebb2<-((seb2_1^2)+(seb2_2^2))^.5
    sebb3<-((seb3_1^2)+(seb3_2^2))^.5
    sebb4<-((seb4_1^2)+(seb4_2^2))^.5
    sebb5<-((seb5_1^2)+(seb5_2^2))^.5
    sebb6<-((seb6_1^2)+(seb6_2^2))^.5
    sebb7<-((seb7_1^2)+(seb7_2^2))^.5
    sebb8<-((seb8_1^2)+(seb8_2^2))^.5
    sebb9<-((seb9_1^2)+(seb9_2^2))^.5
    t1<-abs(abs(b1_1)-abs(b1_2))/sebb1
    t2<-abs(abs(b2_1)-abs(b2_2))/sebb2
    t3<-abs(abs(b3_1)-abs(b3_2))/sebb3
    t4<-abs(abs(b4_1)-abs(b4_2))/sebb4
    t5<-abs(abs(b5_1)-abs(b5_2))/sebb5
    t6<-abs(abs(b6_1)-abs(b6_2))/sebb6
    t7<-abs(abs(b7_1)-abs(b7_2))/sebb7
    t8<-abs(abs(b8_1)-abs(b8_2))/sebb8
    t9<-abs(abs(b9_1)-abs(b9_2))/sebb9
    df1<-model1$df[2]+model2$df[2]
    p1<-2*round(1-(pt(q=abs(t1), df=df1, lower.tail=TRUE)),3)
    p2<-2*round(1-(pt(q=abs(t2), df=df1, lower.tail=TRUE)),3)
    p3<-2*round(1-(pt(q=abs(t3), df=df1, lower.tail=TRUE)),3)
    p4<-2*round(1-(pt(q=abs(t4), df=df1, lower.tail=TRUE)),3)
    p5<-2*round(1-(pt(q=abs(t5), df=df1, lower.tail=TRUE)),3)
    p6<-2*round(1-(pt(q=abs(t6), df=df1, lower.tail=TRUE)),3)
    p7<-2*round(1-(pt(q=abs(t7), df=df1, lower.tail=TRUE)),3)
    p8<-2*round(1-(pt(q=abs(t8), df=df1, lower.tail=TRUE)),3)
    p9<-2*round(1-(pt(q=abs(t9), df=df1, lower.tail=TRUE)),3)
    t1<-round((t1),3)
    t2<-round((t2),3)
    t3<-round((t3),3)
    t4<-round((t4),3)
    t5<-round((t5),3)
    t6<-round((t6),3)
    t7<-round((t7),3)
    t8<-round((t8),3)
    t9<-round((t9),3)
    message("Predictor 1: "," t = ", t1,", p = ", p1)
    message("Predictor 2: "," t = ", t2,", p = ", p2)
    message("Predictor 3: "," t = ", t3,", p = ", p3)
    message("Predictor 4: "," t = ", t4,", p = ", p4)
    message("Predictor 5: "," t = ", t5,", p = ", p5)
    message("Predictor 6: "," t = ", t6,", p = ", p6)
    message("Predictor 7: "," t = ", t7,", p = ", p7)
    message("Predictor 8: "," t = ", t8,", p = ", p8)
    message("Predictor 9: "," t = ", t9,", p = ", p9)
  }
  if ((pred1 =="10") && (comps=="abs")){

    b1_1<-(model1$coefficients)[2,1]
    b2_1<-(model1$coefficients)[3,1]
    b3_1<-(model1$coefficients)[4,1]
    b4_1<-(model1$coefficients)[5,1]
    b5_1<-(model1$coefficients)[6,1]
    b6_1<-(model1$coefficients)[7,1]
    b7_1<-(model1$coefficients)[8,1]
    b8_1<-(model1$coefficients)[9,1]
    b9_1<-(model1$coefficients)[10,1]
    b10_1<-(model1$coefficients)[11,1]
    b1_2<-(model2$coefficients)[2,1]
    b2_2<-(model2$coefficients)[3,1]
    b3_2<-(model2$coefficients)[4,1]
    b4_2<-(model2$coefficients)[5,1]
    b5_2<-(model2$coefficients)[6,1]
    b6_2<-(model2$coefficients)[7,1]
    b7_2<-(model2$coefficients)[8,1]
    b8_2<-(model2$coefficients)[9,1]
    b9_2<-(model2$coefficients)[10,1]
    b10_2<-(model2$coefficients)[11,1]
    seb1_1<-(model1$coefficients)[2,2]
    seb2_1<-(model1$coefficients)[3,2]
    seb3_1<-(model1$coefficients)[4,2]
    seb4_1<-(model1$coefficients)[5,2]
    seb5_1<-(model1$coefficients)[6,2]
    seb6_1<-(model1$coefficients)[7,2]
    seb7_1<-(model1$coefficients)[8,2]
    seb8_1<-(model1$coefficients)[9,2]
    seb9_1<-(model1$coefficients)[10,2]
    seb10_1<-(model1$coefficients)[11,2]
    seb1_2<-(model2$coefficients)[2,2]
    seb2_2<-(model2$coefficients)[3,2]
    seb3_2<-(model2$coefficients)[4,2]
    seb4_2<-(model2$coefficients)[5,2]
    seb5_2<-(model2$coefficients)[6,2]
    seb6_2<-(model2$coefficients)[7,2]
    seb7_2<-(model2$coefficients)[8,2]
    seb8_2<-(model2$coefficients)[9,2]
    seb9_2<-(model2$coefficients)[10,2]
    seb10_2<-(model2$coefficients)[11,2]
    sebb1<-((seb1_1^2)+(seb1_2^2))^.5
    sebb2<-((seb2_1^2)+(seb2_2^2))^.5
    sebb3<-((seb3_1^2)+(seb3_2^2))^.5
    sebb4<-((seb4_1^2)+(seb4_2^2))^.5
    sebb5<-((seb5_1^2)+(seb5_2^2))^.5
    sebb6<-((seb6_1^2)+(seb6_2^2))^.5
    sebb7<-((seb7_1^2)+(seb7_2^2))^.5
    sebb8<-((seb8_1^2)+(seb8_2^2))^.5
    sebb9<-((seb9_1^2)+(seb9_2^2))^.5
    sebb10<-((seb10_1^2)+(seb10_2^2))^.5
    t1<-abs(abs(b1_1)-abs(b1_2))/sebb1
    t2<-abs(abs(b2_1)-abs(b2_2))/sebb2
    t3<-abs(abs(b3_1)-abs(b3_2))/sebb3
    t4<-abs(abs(b4_1)-abs(b4_2))/sebb4
    t5<-abs(abs(b5_1)-abs(b5_2))/sebb5
    t6<-abs(abs(b6_1)-abs(b6_2))/sebb6
    t7<-abs(abs(b7_1)-abs(b7_2))/sebb7
    t8<-abs(abs(b8_1)-abs(b8_2))/sebb8
    t9<-abs(abs(b9_1)-abs(b9_2))/sebb9
    t10<-abs(abs(b10_1)-abs(b10_2))/sebb10
    df1<-model1$df[2]+model2$df[2]
    p1<-2*round(1-(pt(q=abs(t1), df=df1, lower.tail=TRUE)),3)
    p2<-2*round(1-(pt(q=abs(t2), df=df1, lower.tail=TRUE)),3)
    p3<-2*round(1-(pt(q=abs(t3), df=df1, lower.tail=TRUE)),3)
    p4<-2*round(1-(pt(q=abs(t4), df=df1, lower.tail=TRUE)),3)
    p5<-2*round(1-(pt(q=abs(t5), df=df1, lower.tail=TRUE)),3)
    p6<-2*round(1-(pt(q=abs(t6), df=df1, lower.tail=TRUE)),3)
    p7<-2*round(1-(pt(q=abs(t7), df=df1, lower.tail=TRUE)),3)
    p8<-2*round(1-(pt(q=abs(t8), df=df1, lower.tail=TRUE)),3)
    p9<-2*round(1-(pt(q=abs(t9), df=df1, lower.tail=TRUE)),3)
    p10<-2*round(1-(pt(q=abs(t10), df=df1, lower.tail=TRUE)),3)
    t1<-round((t1),3)
    t2<-round((t2),3)
    t3<-round((t3),3)
    t4<-round((t4),3)
    t5<-round((t5),3)
    t6<-round((t6),3)
    t7<-round((t7),3)
    t8<-round((t8),3)
    t9<-round((t9),3)
    t10<-round((t10),3)
    message("Predictor 1: "," t = ", t1,", p = ", p1)
    message("Predictor 2: "," t = ", t2,", p = ", p2)
    message("Predictor 3: "," t = ", t3,", p = ", p3)
    message("Predictor 4: "," t = ", t4,", p = ", p4)
    message("Predictor 5: "," t = ", t5,", p = ", p5)
    message("Predictor 6: "," t = ", t6,", p = ", p6)
    message("Predictor 7: "," t = ", t7,", p = ", p7)
    message("Predictor 8: "," t = ", t8,", p = ", p8)
    message("Predictor 9: "," t = ", t9,", p = ", p9)
    message("Predictor 10: "," t = ", t10,", p = ", p10)
  }
  if ((pred1 =="2") && (comps=="raw")){

    b1_1<-(model1$coefficients)[2,1]
    b2_1<-(model1$coefficients)[3,1]
    b1_2<-(model2$coefficients)[2,1]
    b2_2<-(model2$coefficients)[3,1]
    seb1_1<-(model1$coefficients)[2,2]
    seb2_1<-(model1$coefficients)[3,2]
    seb1_2<-(model2$coefficients)[2,2]
    seb2_2<-(model2$coefficients)[3,2]
    sebb1<-((seb1_1^2)+(seb1_2^2))^.5
    sebb2<-((seb2_1^2)+(seb2_2^2))^.5
    t1<-((b1_1-b1_2))/sebb1
    t2<-((b2_1-b2_2))/sebb2
    df1<-model1$df[2]+model2$df[2]
    df2<-model2$df[2]
    p1<-2*round(1-(pt(q=abs(t1), df=df1, lower.tail=TRUE)),3)
    p2<-2*round(1-(pt(q=abs(t2), df=df2, lower.tail=TRUE)),3)
    t1<-round((t1),3)
    t2<-round((t2),3)
    message("Predictor 1: "," t = ", t1,", p = ", p1)
    message("Predictor 2: "," t = ", t2,", p = ", p2)
  }


  if ((pred1 =="3") && (comps=="raw")){

    b1_1<-(model1$coefficients)[2,1]
    b2_1<-(model1$coefficients)[3,1]
    b3_1<-(model1$coefficients)[4,1]
    b1_2<-(model2$coefficients)[2,1]
    b2_2<-(model2$coefficients)[3,1]
    b3_2<-(model2$coefficients)[4,1]
    seb1_1<-(model1$coefficients)[2,2]
    seb2_1<-(model1$coefficients)[3,2]
    seb3_1<-(model1$coefficients)[4,2]
    seb1_2<-(model2$coefficients)[2,2]
    seb2_2<-(model2$coefficients)[3,2]
    seb3_2<-(model2$coefficients)[4,2]
    sebb1<-((seb1_1^2)+(seb1_2^2))^.5
    sebb2<-((seb2_1^2)+(seb2_2^2))^.5
    sebb3<-((seb3_1^2)+(seb3_2^2))^.5
    t1<-((b1_1-b1_2))/sebb1
    t2<-((b2_1-b2_2))/sebb2
    t3<-((b3_1-b3_2))/sebb3
    df1<-model1$df[2]+model2$df[2]
    p1<-2*round(1-(pt(q=abs(t1), df=df1, lower.tail=TRUE)),3)
    p2<-2*round(1-(pt(q=abs(t2), df=df1, lower.tail=TRUE)),3)
    p3<-2*round(1-(pt(q=abs(t3), df=df1, lower.tail=TRUE)),3)
    t1<-round((t1),3)
    t2<-round((t2),3)
    t3<-round((t3),3)
    message("Predictor 1: "," t = ", t1,", p = ", p1)
    message("Predictor 2: "," t = ", t2,", p = ", p2)
    message("Predictor 3: "," t = ", t3,", p = ", p3)
  }


  if ((pred1 =="4") && (comps=="raw")){

    b1_1<-(model1$coefficients)[2,1]
    b2_1<-(model1$coefficients)[3,1]
    b3_1<-(model1$coefficients)[4,1]
    b4_1<-(model1$coefficients)[5,1]
    b1_2<-(model2$coefficients)[2,1]
    b2_2<-(model2$coefficients)[3,1]
    b3_2<-(model2$coefficients)[4,1]
    b4_2<-(model2$coefficients)[5,1]
    seb1_1<-(model1$coefficients)[2,2]
    seb2_1<-(model1$coefficients)[3,2]
    seb3_1<-(model1$coefficients)[4,2]
    seb4_1<-(model1$coefficients)[5,2]
    seb1_2<-(model2$coefficients)[2,2]
    seb2_2<-(model2$coefficients)[3,2]
    seb3_2<-(model2$coefficients)[4,2]
    seb4_2<-(model2$coefficients)[5,2]
    sebb1<-((seb1_1^2)+(seb1_2^2))^.5
    sebb2<-((seb2_1^2)+(seb2_2^2))^.5
    sebb3<-((seb3_1^2)+(seb3_2^2))^.5
    sebb4<-((seb4_1^2)+(seb4_2^2))^.5
    t1<-((b1_1-b1_2))/sebb1
    t2<-((b2_1-b2_2))/sebb2
    t3<-((b3_1-b3_2))/sebb3
    t4<-((b4_1-b4_2))/sebb4
    df1<-model1$df[2]+model2$df[2]
    p1<-2*round(1-(pt(q=abs(t1), df=df1, lower.tail=TRUE)),3)
    p2<-2*round(1-(pt(q=abs(t2), df=df1, lower.tail=TRUE)),3)
    p3<-2*round(1-(pt(q=abs(t3), df=df1, lower.tail=TRUE)),3)
    p4<-2*round(1-(pt(q=abs(t4), df=df1, lower.tail=TRUE)),3)
    t1<-round((t1),3)
    t2<-round((t2),3)
    t3<-round((t3),3)
    t4<-round((t4),3)
    message("Predictor 1: "," t = ", t1,", p = ", p1)
    message("Predictor 2: "," t = ", t2,", p = ", p2)
    message("Predictor 3: "," t = ", t3,", p = ", p3)
    message("Predictor 4: "," t = ", t4,", p = ", p4)
  }
  if ((pred1 =="5") && (comps=="raw")){

    b1_1<-(model1$coefficients)[2,1]
    b2_1<-(model1$coefficients)[3,1]
    b3_1<-(model1$coefficients)[4,1]
    b4_1<-(model1$coefficients)[5,1]
    b5_1<-(model1$coefficients)[6,1]
    b1_2<-(model2$coefficients)[2,1]
    b2_2<-(model2$coefficients)[3,1]
    b3_2<-(model2$coefficients)[4,1]
    b4_2<-(model2$coefficients)[5,1]
    b5_2<-(model2$coefficients)[6,1]
    seb1_1<-(model1$coefficients)[2,2]
    seb2_1<-(model1$coefficients)[3,2]
    seb3_1<-(model1$coefficients)[4,2]
    seb4_1<-(model1$coefficients)[5,2]
    seb5_1<-(model1$coefficients)[6,2]
    seb1_2<-(model2$coefficients)[2,2]
    seb2_2<-(model2$coefficients)[3,2]
    seb3_2<-(model2$coefficients)[4,2]
    seb4_2<-(model2$coefficients)[5,2]
    seb5_2<-(model2$coefficients)[6,2]
    sebb1<-((seb1_1^2)+(seb1_2^2))^.5
    sebb2<-((seb2_1^2)+(seb2_2^2))^.5
    sebb3<-((seb3_1^2)+(seb3_2^2))^.5
    sebb4<-((seb4_1^2)+(seb4_2^2))^.5
    sebb5<-((seb5_1^2)+(seb5_2^2))^.5
    t1<-((b1_1-b1_2))/sebb1
    t2<-((b2_1-b2_2))/sebb2
    t3<-((b3_1-b3_2))/sebb3
    t4<-((b4_1-b4_2))/sebb4
    t5<-((b5_1-b5_2))/sebb5
    df1<-model1$df[2]+model2$df[2]
    p1<-2*round(1-(pt(q=abs(t1), df=df1, lower.tail=TRUE)),3)
    p2<-2*round(1-(pt(q=abs(t2), df=df1, lower.tail=TRUE)),3)
    p3<-2*round(1-(pt(q=abs(t3), df=df1, lower.tail=TRUE)),3)
    p4<-2*round(1-(pt(q=abs(t4), df=df1, lower.tail=TRUE)),3)
    p5<-2*round(1-(pt(q=abs(t5), df=df1, lower.tail=TRUE)),3)
    t1<-round((t1),3)
    t2<-round((t2),3)
    t3<-round((t3),3)
    t4<-round((t4),3)
    t5<-round((t5),3)
    message("Predictor 1: "," t = ", t1,", p = ", p1)
    message("Predictor 2: "," t = ", t2,", p = ", p2)
    message("Predictor 3: "," t = ", t3,", p = ", p3)
    message("Predictor 4: "," t = ", t4,", p = ", p4)
    message("Predictor 5: "," t = ", t5,", p = ", p5)
  }
  if ((pred1 =="6") && (comps=="raw")){

    b1_1<-(model1$coefficients)[2,1]
    b2_1<-(model1$coefficients)[3,1]
    b3_1<-(model1$coefficients)[4,1]
    b4_1<-(model1$coefficients)[5,1]
    b5_1<-(model1$coefficients)[6,1]
    b6_1<-(model1$coefficients)[7,1]
    b1_2<-(model2$coefficients)[2,1]
    b2_2<-(model2$coefficients)[3,1]
    b3_2<-(model2$coefficients)[4,1]
    b4_2<-(model2$coefficients)[5,1]
    b5_2<-(model2$coefficients)[6,1]
    b6_2<-(model2$coefficients)[7,1]
    seb1_1<-(model1$coefficients)[2,2]
    seb2_1<-(model1$coefficients)[3,2]
    seb3_1<-(model1$coefficients)[4,2]
    seb4_1<-(model1$coefficients)[5,2]
    seb5_1<-(model1$coefficients)[6,2]
    seb6_1<-(model1$coefficients)[7,2]
    seb1_2<-(model2$coefficients)[2,2]
    seb2_2<-(model2$coefficients)[3,2]
    seb3_2<-(model2$coefficients)[4,2]
    seb4_2<-(model2$coefficients)[5,2]
    seb5_2<-(model2$coefficients)[6,2]
    seb6_2<-(model2$coefficients)[7,2]
    sebb1<-((seb1_1^2)+(seb1_2^2))^.5
    sebb2<-((seb2_1^2)+(seb2_2^2))^.5
    sebb3<-((seb3_1^2)+(seb3_2^2))^.5
    sebb4<-((seb4_1^2)+(seb4_2^2))^.5
    sebb5<-((seb5_1^2)+(seb5_2^2))^.5
    sebb6<-((seb6_1^2)+(seb6_2^2))^.5
    t1<-((b1_1-b1_2))/sebb1
    t2<-((b2_1-b2_2))/sebb2
    t3<-((b3_1-b3_2))/sebb3
    t4<-((b4_1-b4_2))/sebb4
    t5<-((b5_1-b5_2))/sebb5
    t6<-((b6_1-b6_2))/sebb6
    df1<-model1$df[2]+model2$df[2]
    p1<-2*round(1-(pt(q=abs(t1), df=df1, lower.tail=TRUE)),3)
    p2<-2*round(1-(pt(q=abs(t2), df=df1, lower.tail=TRUE)),3)
    p3<-2*round(1-(pt(q=abs(t3), df=df1, lower.tail=TRUE)),3)
    p4<-2*round(1-(pt(q=abs(t4), df=df1, lower.tail=TRUE)),3)
    p5<-2*round(1-(pt(q=abs(t5), df=df1, lower.tail=TRUE)),3)
    p6<-2*round(1-(pt(q=abs(t6), df=df1, lower.tail=TRUE)),3)
    t1<-round((t1),3)
    t2<-round((t2),3)
    t3<-round((t3),3)
    t4<-round((t4),3)
    t5<-round((t5),3)
    t6<-round((t6),3)
    message("Predictor 1: "," t = ", t1,", p = ", p1)
    message("Predictor 2: "," t = ", t2,", p = ", p2)
    message("Predictor 3: "," t = ", t3,", p = ", p3)
    message("Predictor 4: "," t = ", t4,", p = ", p4)
    message("Predictor 5: "," t = ", t5,", p = ", p5)
    message("Predictor 6: "," t = ", t6,", p = ", p6)
  }
  if ((pred1 =="7") && (comps=="raw")){

    b1_1<-(model1$coefficients)[2,1]
    b2_1<-(model1$coefficients)[3,1]
    b3_1<-(model1$coefficients)[4,1]
    b4_1<-(model1$coefficients)[5,1]
    b5_1<-(model1$coefficients)[6,1]
    b6_1<-(model1$coefficients)[7,1]
    b7_1<-(model1$coefficients)[8,1]
    b1_2<-(model2$coefficients)[2,1]
    b2_2<-(model2$coefficients)[3,1]
    b3_2<-(model2$coefficients)[4,1]
    b4_2<-(model2$coefficients)[5,1]
    b5_2<-(model2$coefficients)[6,1]
    b6_2<-(model2$coefficients)[7,1]
    b7_2<-(model2$coefficients)[8,1]
    seb1_1<-(model1$coefficients)[2,2]
    seb2_1<-(model1$coefficients)[3,2]
    seb3_1<-(model1$coefficients)[4,2]
    seb4_1<-(model1$coefficients)[5,2]
    seb5_1<-(model1$coefficients)[6,2]
    seb6_1<-(model1$coefficients)[7,2]
    seb7_1<-(model1$coefficients)[8,2]
    seb1_2<-(model2$coefficients)[2,2]
    seb2_2<-(model2$coefficients)[3,2]
    seb3_2<-(model2$coefficients)[4,2]
    seb4_2<-(model2$coefficients)[5,2]
    seb5_2<-(model2$coefficients)[6,2]
    seb6_2<-(model2$coefficients)[7,2]
    seb7_2<-(model2$coefficients)[8,2]
    sebb1<-((seb1_1^2)+(seb1_2^2))^.5
    sebb2<-((seb2_1^2)+(seb2_2^2))^.5
    sebb3<-((seb3_1^2)+(seb3_2^2))^.5
    sebb4<-((seb4_1^2)+(seb4_2^2))^.5
    sebb5<-((seb5_1^2)+(seb5_2^2))^.5
    sebb6<-((seb6_1^2)+(seb6_2^2))^.5
    sebb7<-((seb7_1^2)+(seb7_2^2))^.5
    t1<-((b1_1-b1_2))/sebb1
    t2<-((b2_1-b2_2))/sebb2
    t3<-((b3_1-b3_2))/sebb3
    t4<-((b4_1-b4_2))/sebb4
    t5<-((b5_1-b5_2))/sebb5
    t6<-((b6_1-b6_2))/sebb6
    t7<-((b7_1-b7_2))/sebb7
    df1<-model1$df[2]+model2$df[2]
    p1<-2*round(1-(pt(q=abs(t1), df=df1, lower.tail=TRUE)),3)
    p2<-2*round(1-(pt(q=abs(t2), df=df1, lower.tail=TRUE)),3)
    p3<-2*round(1-(pt(q=abs(t3), df=df1, lower.tail=TRUE)),3)
    p4<-2*round(1-(pt(q=abs(t4), df=df1, lower.tail=TRUE)),3)
    p5<-2*round(1-(pt(q=abs(t5), df=df1, lower.tail=TRUE)),3)
    p6<-2*round(1-(pt(q=abs(t6), df=df1, lower.tail=TRUE)),3)
    p7<-2*round(1-(pt(q=abs(t7), df=df1, lower.tail=TRUE)),3)
    t1<-round((t1),3)
    t2<-round((t2),3)
    t3<-round((t3),3)
    t4<-round((t4),3)
    t5<-round((t5),3)
    t6<-round((t6),3)
    t7<-round((t7),3)
    message("Predictor 1: "," t = ", t1,", p = ", p1)
    message("Predictor 2: "," t = ", t2,", p = ", p2)
    message("Predictor 3: "," t = ", t3,", p = ", p3)
    message("Predictor 4: "," t = ", t4,", p = ", p4)
    message("Predictor 5: "," t = ", t5,", p = ", p5)
    message("Predictor 6: "," t = ", t6,", p = ", p6)
    message("Predictor 7: "," t = ", t7,", p = ", p7)

  }
  if ((pred1 =="8") && (comps=="raw")){

    b1_1<-(model1$coefficients)[2,1]
    b2_1<-(model1$coefficients)[3,1]
    b3_1<-(model1$coefficients)[4,1]
    b4_1<-(model1$coefficients)[5,1]
    b5_1<-(model1$coefficients)[6,1]
    b6_1<-(model1$coefficients)[7,1]
    b7_1<-(model1$coefficients)[8,1]
    b8_1<-(model1$coefficients)[9,1]
    b1_2<-(model2$coefficients)[2,1]
    b2_2<-(model2$coefficients)[3,1]
    b3_2<-(model2$coefficients)[4,1]
    b4_2<-(model2$coefficients)[5,1]
    b5_2<-(model2$coefficients)[6,1]
    b6_2<-(model2$coefficients)[7,1]
    b7_2<-(model2$coefficients)[8,1]
    b8_2<-(model2$coefficients)[9,1]
    seb1_1<-(model1$coefficients)[2,2]
    seb2_1<-(model1$coefficients)[3,2]
    seb3_1<-(model1$coefficients)[4,2]
    seb4_1<-(model1$coefficients)[5,2]
    seb5_1<-(model1$coefficients)[6,2]
    seb6_1<-(model1$coefficients)[7,2]
    seb7_1<-(model1$coefficients)[8,2]
    seb8_1<-(model1$coefficients)[9,2]
    seb1_2<-(model2$coefficients)[2,2]
    seb2_2<-(model2$coefficients)[3,2]
    seb3_2<-(model2$coefficients)[4,2]
    seb4_2<-(model2$coefficients)[5,2]
    seb5_2<-(model2$coefficients)[6,2]
    seb6_2<-(model2$coefficients)[7,2]
    seb7_2<-(model2$coefficients)[8,2]
    seb8_2<-(model2$coefficients)[9,2]
    sebb1<-((seb1_1^2)+(seb1_2^2))^.5
    sebb2<-((seb2_1^2)+(seb2_2^2))^.5
    sebb3<-((seb3_1^2)+(seb3_2^2))^.5
    sebb4<-((seb4_1^2)+(seb4_2^2))^.5
    sebb5<-((seb5_1^2)+(seb5_2^2))^.5
    sebb6<-((seb6_1^2)+(seb6_2^2))^.5
    sebb7<-((seb7_1^2)+(seb7_2^2))^.5
    sebb8<-((seb8_1^2)+(seb8_2^2))^.5
    t1<-((b1_1-b1_2))/sebb1
    t2<-((b2_1-b2_2))/sebb2
    t3<-((b3_1-b3_2))/sebb3
    t4<-((b4_1-b4_2))/sebb4
    t5<-((b5_1-b5_2))/sebb5
    t6<-((b6_1-b6_2))/sebb6
    t7<-((b7_1-b7_2))/sebb7
    t8<-((b8_1-b8_2))/sebb8
    df1<-model1$df[2]+model2$df[2]
    p1<-2*round(1-(pt(q=abs(t1), df=df1, lower.tail=TRUE)),3)
    p2<-2*round(1-(pt(q=abs(t2), df=df1, lower.tail=TRUE)),3)
    p3<-2*round(1-(pt(q=abs(t3), df=df1, lower.tail=TRUE)),3)
    p4<-2*round(1-(pt(q=abs(t4), df=df1, lower.tail=TRUE)),3)
    p5<-2*round(1-(pt(q=abs(t5), df=df1, lower.tail=TRUE)),3)
    p6<-2*round(1-(pt(q=abs(t6), df=df1, lower.tail=TRUE)),3)
    p7<-2*round(1-(pt(q=abs(t7), df=df1, lower.tail=TRUE)),3)
    p8<-2*round(1-(pt(q=abs(t8), df=df1, lower.tail=TRUE)),3)
    t1<-round((t1),3)
    t2<-round((t2),3)
    t3<-round((t3),3)
    t4<-round((t4),3)
    t5<-round((t5),3)
    t6<-round((t6),3)
    t7<-round((t7),3)
    t8<-round((t8),3)
    message("Predictor 1: "," t = ", t1,", p = ", p1)
    message("Predictor 2: "," t = ", t2,", p = ", p2)
    message("Predictor 3: "," t = ", t3,", p = ", p3)
    message("Predictor 4: "," t = ", t4,", p = ", p4)
    message("Predictor 5: "," t = ", t5,", p = ", p5)
    message("Predictor 6: "," t = ", t6,", p = ", p6)
    message("Predictor 7: "," t = ", t7,", p = ", p7)
    message("Predictor 8: "," t = ", t8,", p = ", p8)
  }
  if ((pred1 =="9") && (comps=="raw")){

    b1_1<-(model1$coefficients)[2,1]
    b2_1<-(model1$coefficients)[3,1]
    b3_1<-(model1$coefficients)[4,1]
    b4_1<-(model1$coefficients)[5,1]
    b5_1<-(model1$coefficients)[6,1]
    b6_1<-(model1$coefficients)[7,1]
    b7_1<-(model1$coefficients)[8,1]
    b8_1<-(model1$coefficients)[9,1]
    b9_1<-(model1$coefficients)[10,1]
    b1_2<-(model2$coefficients)[2,1]
    b2_2<-(model2$coefficients)[3,1]
    b3_2<-(model2$coefficients)[4,1]
    b4_2<-(model2$coefficients)[5,1]
    b5_2<-(model2$coefficients)[6,1]
    b6_2<-(model2$coefficients)[7,1]
    b7_2<-(model2$coefficients)[8,1]
    b8_2<-(model2$coefficients)[9,1]
    b9_2<-(model2$coefficients)[10,1]
    seb1_1<-(model1$coefficients)[2,2]
    seb2_1<-(model1$coefficients)[3,2]
    seb3_1<-(model1$coefficients)[4,2]
    seb4_1<-(model1$coefficients)[5,2]
    seb5_1<-(model1$coefficients)[6,2]
    seb6_1<-(model1$coefficients)[7,2]
    seb7_1<-(model1$coefficients)[8,2]
    seb8_1<-(model1$coefficients)[9,2]
    seb9_1<-(model1$coefficients)[10,2]
    seb1_2<-(model2$coefficients)[2,2]
    seb2_2<-(model2$coefficients)[3,2]
    seb3_2<-(model2$coefficients)[4,2]
    seb4_2<-(model2$coefficients)[5,2]
    seb5_2<-(model2$coefficients)[6,2]
    seb6_2<-(model2$coefficients)[7,2]
    seb7_2<-(model2$coefficients)[8,2]
    seb8_2<-(model2$coefficients)[9,2]
    seb9_2<-(model2$coefficients)[10,2]
    sebb1<-((seb1_1^2)+(seb1_2^2))^.5
    sebb2<-((seb2_1^2)+(seb2_2^2))^.5
    sebb3<-((seb3_1^2)+(seb3_2^2))^.5
    sebb4<-((seb4_1^2)+(seb4_2^2))^.5
    sebb5<-((seb5_1^2)+(seb5_2^2))^.5
    sebb6<-((seb6_1^2)+(seb6_2^2))^.5
    sebb7<-((seb7_1^2)+(seb7_2^2))^.5
    sebb8<-((seb8_1^2)+(seb8_2^2))^.5
    sebb9<-((seb9_1^2)+(seb9_2^2))^.5
    t1<-((b1_1-b1_2))/sebb1
    t2<-((b2_1-b2_2))/sebb2
    t3<-((b3_1-b3_2))/sebb3
    t4<-((b4_1-b4_2))/sebb4
    t5<-((b5_1-b5_2))/sebb5
    t6<-((b6_1-b6_2))/sebb6
    t7<-((b7_1-b7_2))/sebb7
    t8<-((b8_1-b8_2))/sebb8
    t9<-((b9_1-b9_2))/sebb9
    df1<-model1$df[2]+model2$df[2]
    p1<-2*round(1-(pt(q=abs(t1), df=df1, lower.tail=TRUE)),3)
    p2<-2*round(1-(pt(q=abs(t2), df=df1, lower.tail=TRUE)),3)
    p3<-2*round(1-(pt(q=abs(t3), df=df1, lower.tail=TRUE)),3)
    p4<-2*round(1-(pt(q=abs(t4), df=df1, lower.tail=TRUE)),3)
    p5<-2*round(1-(pt(q=abs(t5), df=df1, lower.tail=TRUE)),3)
    p6<-2*round(1-(pt(q=abs(t6), df=df1, lower.tail=TRUE)),3)
    p7<-2*round(1-(pt(q=abs(t7), df=df1, lower.tail=TRUE)),3)
    p8<-2*round(1-(pt(q=abs(t8), df=df1, lower.tail=TRUE)),3)
    p9<-2*round(1-(pt(q=abs(t9), df=df1, lower.tail=TRUE)),3)
    t1<-round((t1),3)
    t2<-round((t2),3)
    t3<-round((t3),3)
    t4<-round((t4),3)
    t5<-round((t5),3)
    t6<-round((t6),3)
    t7<-round((t7),3)
    t8<-round((t8),3)
    t9<-round((t9),3)
    message("Predictor 1: "," t = ", t1,", p = ", p1)
    message("Predictor 2: "," t = ", t2,", p = ", p2)
    message("Predictor 3: "," t = ", t3,", p = ", p3)
    message("Predictor 4: "," t = ", t4,", p = ", p4)
    message("Predictor 5: "," t = ", t5,", p = ", p5)
    message("Predictor 6: "," t = ", t6,", p = ", p6)
    message("Predictor 7: "," t = ", t7,", p = ", p7)
    message("Predictor 8: "," t = ", t8,", p = ", p8)
    message("Predictor 9: "," t = ", t9,", p = ", p9)
  }
  if ((pred1 =="10") && (comps=="raw")){

    b1_1<-(model1$coefficients)[2,1]
    b2_1<-(model1$coefficients)[3,1]
    b3_1<-(model1$coefficients)[4,1]
    b4_1<-(model1$coefficients)[5,1]
    b5_1<-(model1$coefficients)[6,1]
    b6_1<-(model1$coefficients)[7,1]
    b7_1<-(model1$coefficients)[8,1]
    b8_1<-(model1$coefficients)[9,1]
    b9_1<-(model1$coefficients)[10,1]
    b10_1<-(model1$coefficients)[11,1]
    b1_2<-(model2$coefficients)[2,1]
    b2_2<-(model2$coefficients)[3,1]
    b3_2<-(model2$coefficients)[4,1]
    b4_2<-(model2$coefficients)[5,1]
    b5_2<-(model2$coefficients)[6,1]
    b6_2<-(model2$coefficients)[7,1]
    b7_2<-(model2$coefficients)[8,1]
    b8_2<-(model2$coefficients)[9,1]
    b9_2<-(model2$coefficients)[10,1]
    b10_2<-(model2$coefficients)[11,1]
    seb1_1<-(model1$coefficients)[2,2]
    seb2_1<-(model1$coefficients)[3,2]
    seb3_1<-(model1$coefficients)[4,2]
    seb4_1<-(model1$coefficients)[5,2]
    seb5_1<-(model1$coefficients)[6,2]
    seb6_1<-(model1$coefficients)[7,2]
    seb7_1<-(model1$coefficients)[8,2]
    seb8_1<-(model1$coefficients)[9,2]
    seb9_1<-(model1$coefficients)[10,2]
    seb10_1<-(model1$coefficients)[11,2]
    seb1_2<-(model2$coefficients)[2,2]
    seb2_2<-(model2$coefficients)[3,2]
    seb3_2<-(model2$coefficients)[4,2]
    seb4_2<-(model2$coefficients)[5,2]
    seb5_2<-(model2$coefficients)[6,2]
    seb6_2<-(model2$coefficients)[7,2]
    seb7_2<-(model2$coefficients)[8,2]
    seb8_2<-(model2$coefficients)[9,2]
    seb9_2<-(model2$coefficients)[10,2]
    seb10_2<-(model2$coefficients)[11,2]
    sebb1<-((seb1_1^2)+(seb1_2^2))^.5
    sebb2<-((seb2_1^2)+(seb2_2^2))^.5
    sebb3<-((seb3_1^2)+(seb3_2^2))^.5
    sebb4<-((seb4_1^2)+(seb4_2^2))^.5
    sebb5<-((seb5_1^2)+(seb5_2^2))^.5
    sebb6<-((seb6_1^2)+(seb6_2^2))^.5
    sebb7<-((seb7_1^2)+(seb7_2^2))^.5
    sebb8<-((seb8_1^2)+(seb8_2^2))^.5
    sebb9<-((seb9_1^2)+(seb9_2^2))^.5
    sebb10<-((seb10_1^2)+(seb10_2^2))^.5
    t1<-((b1_1-b1_2))/sebb1
    t2<-((b2_1-b2_2))/sebb2
    t3<-((b3_1-b3_2))/sebb3
    t4<-((b4_1-b4_2))/sebb4
    t5<-((b5_1-b5_2))/sebb5
    t6<-((b6_1-b6_2))/sebb6
    t7<-((b7_1-b7_2))/sebb7
    t8<-((b8_1-b8_2))/sebb8
    t9<-((b9_1-b9_2))/sebb9
    t10<-((b10_1-b10_2))/sebb10
    df1<-model1$df[2]+model2$df[2]
    p1<-2*round(1-(pt(q=abs(t1), df=df1, lower.tail=TRUE)),3)
    p2<-2*round(1-(pt(q=abs(t2), df=df1, lower.tail=TRUE)),3)
    p3<-2*round(1-(pt(q=abs(t3), df=df1, lower.tail=TRUE)),3)
    p4<-2*round(1-(pt(q=abs(t4), df=df1, lower.tail=TRUE)),3)
    p5<-2*round(1-(pt(q=abs(t5), df=df1, lower.tail=TRUE)),3)
    p6<-2*round(1-(pt(q=abs(t6), df=df1, lower.tail=TRUE)),3)
    p7<-2*round(1-(pt(q=abs(t7), df=df1, lower.tail=TRUE)),3)
    p8<-2*round(1-(pt(q=abs(t8), df=df1, lower.tail=TRUE)),3)
    p9<-2*round(1-(pt(q=abs(t9), df=df1, lower.tail=TRUE)),3)
    p10<-2*round(1-(pt(q=abs(t10), df=df1, lower.tail=TRUE)),3)
    t1<-round((t1),3)
    t2<-round((t2),3)
    t3<-round((t3),3)
    t4<-round((t4),3)
    t5<-round((t5),3)
    t6<-round((t6),3)
    t7<-round((t7),3)
    t8<-round((t8),3)
    t9<-round((t9),3)
    t10<-round((t10),3)
    message("Predictor 1: "," t = ", t1,", p = ", p1)
    message("Predictor 2: "," t = ", t2,", p = ", p2)
    message("Predictor 3: "," t = ", t3,", p = ", p3)
    message("Predictor 4: "," t = ", t4,", p = ", p4)
    message("Predictor 5: "," t = ", t5,", p = ", p5)
    message("Predictor 6: "," t = ", t6,", p = ", p6)
    message("Predictor 7: "," t = ", t7,", p = ", p7)
    message("Predictor 8: "," t = ", t8,", p = ", p8)
    message("Predictor 9: "," t = ", t9,", p = ", p9)
    message("Predictor 10: "," t = ", t10,", p = ", p10)
  }
  }


