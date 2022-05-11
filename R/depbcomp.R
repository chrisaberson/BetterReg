#'Power for Comparing Dependent Coefficients in Multiple Regression with Two or Three Predictors
#'Requires correlations between all variables as sample size. Means, sds, and alpha are option. Also computes Power(All)
#'@param data name of your datafile, loaded
#'@param  y dependent variable name
#'@param  x1 first predictor variable name
#'@param  x2 second predictor variable name
#'@param  x3 third predictor variable name
#'@param  x4 fourth predictor variable name
#'@param  x5 fifth predictor variable name
#'@param data name of data file
#'@param numpred number of predictors
#'@param comps Type of comparison, "abs" for absolute values or "raw" for raw coefficients
#'@examples
#'depbcomp(data=testreg,y="y",x1="x1",x2="x2",x3="x3",x4="x4",x5="x5", numpred=5,comps="abs")
#'@return Comparing Dependent Coefficients in Multiple Regression
#'@export
#'

depbcomp<-function(data=NULL,y=NULL, x1=NULL, x2=NULL,x3=NULL,x4=NULL,x5=NULL,numpred=NULL, comps="abs")
{
  if (numpred ==2 && comps=="abs")
  {
  xx<-dplyr::select(data,y,x1,x2)
  xx<-as.data.frame(xx)
  mod<-lm(xx[,1]~xx[,2]+xx[,3])
  values<-summary(mod)
  b1<-(values$coefficients)[2,1] #grabs b from each analysis
  b2<-(values$coefficients)[3,1]
  seb1<-(values$coefficients)[2,2]
  seb2<-(values$coefficients)[3,2]
  df<- values$df[2]
  varx1<-var(xx[,2])
  varx2<-var(xx[,3])
  r12<-abs(cor(xx[,2],xx[,3]))
  mat<-cbind(c(1,r12),c(r12,1))
  inv<-solve(mat)*mat
  pij<-inv[1,2] #inv of cor between pred1 and 2
  pii<-inv[1,1] #inv of cov, v1
  pjj<-inv[2,2] #inv of cov, v2
  den1<-seb1^2+seb2^2
  den2<-2*seb1*seb2
  den3<-pij/(pii+pjj)
  den<-(den1-(den2*den3))^.5
  t12<-abs((abs(b1)-(abs(b2)))) / den
  p12<-2*(1-(pt(q=abs(t12), df=df, lower.tail=TRUE)))
  t12<-round((t12),3)
  message("Pred 1 vs. Pred 2 "," : t = ", t12,", p = ", p12)

    }
  if (numpred ==3 && comps=="abs")
  {
    xx<-dplyr::select(data,y,x1,x2,x3)
    xx<-as.data.frame(xx)
    mod<-lm(xx[,1]~xx[,2]+xx[,3]+xx[,4])
    values<-summary(mod)
    b1<-(values$coefficients)[2,1] #grabs b from each analysis
    b2<-(values$coefficients)[3,1]
    b3<-(values$coefficients)[4,1]
    seb1<-(values$coefficients)[2,2]
    seb2<-(values$coefficients)[3,2]
    seb3<-(values$coefficients)[4,2]
    df<- values$df[2]

    r12<-abs(cor(xx[,2],xx[,3]))
    r13<-abs(cor(xx[,2],xx[,4]))
    r23<-abs(cor(xx[,3],xx[,4]))
    mat<-cbind(c(1,r12,r13),c(r12,1,r23), c(r13,r23,1))
    inv<-solve(mat)*mat
    # 1 vs 2
    pij1<-inv[1,2] #inv of cor between pred of interest 1 vs. 2
    pii1<-inv[1,1] #inv of cov, v1
    pjj1<-inv[2,2] #inv of cov, v2
    den1a<-seb1^2+seb2^2
    den2a<-2*seb1*seb2
    den3a<-pij1/(pii1+pjj1)
    den<-(den1a-(den2a*den3a))^.5
    t12<-abs((abs(b1)-(abs(b2)))) / den
    p12<-2*(1-(pt(q=abs(t12), df=df, lower.tail=TRUE)))
    t12<-round((t12),3)
    #1 vs 3
    pij2<-inv[1,3] #inv of cor between pred of interest 1 vs. 2
    pii2<-inv[1,1] #inv of cov, v1
    pjj2<-inv[3,3] #inv of cov, v2
    den1b<-seb1^2+seb3^2
    den2b<-2*seb1*seb3
    den3b<-pij2/(pii2+pjj2)
    denb<-(den1b-(den2b*den3b))^.5
    t13<-abs(abs(b1)-abs(b3)) / denb
    p13<-2*(1-(pt(q=abs(t13), df=df, lower.tail=TRUE)))
    t13<-round((t13),3)
    #2 vs 3
    pij3<-inv[2,3] #inv of cor between pred of interest 1 vs. 2
    pii3<-inv[2,2] #inv of cov, v1
    pjj3<-inv[3,3] #inv of cov, v2
    den1c<-seb2^2+seb3^2
    den2c<-2*seb2*seb3
    den3c<-pij3/(pii3+pjj3)
    denc<-(den1c-(den2c*den3c))^.5
    t23<-abs(abs(b2)-abs(b3)) / denc
    p23<-2*(1-(pt(q=abs(t23), df=df, lower.tail=TRUE)))
    t23<-round((t23),3)
    message("Pred 1 vs. Pred 2 "," : t = ", t12,", p = ", p12)
    message("Pred 1 vs. Pred 3 "," : t = ", t13,", p = ", p13)
    message("Pred 2 vs. Pred 3 "," : t = ", t23,", p = ", p23)
  }
  if (numpred ==4 && comps=="abs")
  {
    xx<-dplyr::select(data,y,x1,x2,x3,x4)
    xx<-as.data.frame(xx)
    mod<-lm(xx[,1]~xx[,2]+xx[,3]+xx[,4]+xx[,5])
    values<-summary(mod)
    b1<-(values$coefficients)[2,1] #grabs b from each analysis
    b2<-(values$coefficients)[3,1]
    b3<-(values$coefficients)[4,1]
    b4<-(values$coefficients)[5,1]
    seb1<-(values$coefficients)[2,2]
    seb2<-(values$coefficients)[3,2]
    seb3<-(values$coefficients)[4,2]
    seb4<-(values$coefficients)[5,2]
    df<- values$df[2]

    r12<-abs(cor(xx[,2],xx[,3]))
    r13<-abs(cor(xx[,2],xx[,4]))
    r14<-abs(cor(xx[,2],xx[,5]))
    r23<-abs(cor(xx[,3],xx[,4]))
    r24<-abs(cor(xx[,3],xx[,5]))
    r34<-abs(cor(xx[,4],xx[,5]))
    mat<-cbind(c(1,r12,r13,r14),c(r12,1,r23,r24), c(r13,r23,1,r34),c(r14,r24,r34,1))
    inv<-solve(mat)*mat
    # 1 vs 2
    pij1<-inv[1,2] #inv of cor between pred of interest 1 vs. 2
    pii1<-inv[1,1] #inv of cov, v1
    pjj1<-inv[2,2] #inv of cov, v2
    den1a<-seb1^2+seb2^2
    den2a<-2*seb1*seb2
    den3a<-pij1/(pii1+pjj1)
    den<-(den1a-(den2a*den3a))^.5
    t12<-abs((abs(b1)-(abs(b2)))) / den
    p12<-2*(1-(pt(q=abs(t12), df=df, lower.tail=TRUE)))
    t12<-round((t12),3)
    #1 vs 3
    pij2<-inv[1,3] #inv of cor between pred of interest 1 vs. 2
    pii2<-inv[1,1] #inv of cov, v1
    pjj2<-inv[3,3] #inv of cov, v2
    den1b<-seb1^2+seb3^2
    den2b<-2*seb1*seb3
    den3b<-pij2/(pii2+pjj2)
    denb<-(den1b-(den2b*den3b))^.5
    t13<-abs(abs(b1)-abs(b3)) / denb
    p13<-2*(1-(pt(q=abs(t13), df=df, lower.tail=TRUE)))
    t13<-round((t13),3)
    #1 vs 4
    pij4<-inv[1,4] #inv of cor between pred of interest 1 vs. 2
    pii4<-inv[1,1] #inv of cov, v1
    pjj4<-inv[4,4] #inv of cov, v2
    den1d<-seb1^2+seb4^2
    den2d<-2*seb1*seb4
    den3d<-pij4/(pii4+pjj4)
    dend<-(den1d-(den2d*den3d))^.5
    t14<-abs(abs(b1)-abs(b4)) / dend
    p14<-2*(1-(pt(q=abs(t14), df=df, lower.tail=TRUE)))
    t14<-round((t14),3)
      #2 vs 3
    pij3<-inv[2,3] #inv of cor between pred of interest 1 vs. 2
    pii3<-inv[2,2] #inv of cov, v1
    pjj3<-inv[3,3] #inv of cov, v2
    den1c<-seb2^2+seb3^2
    den2c<-2*seb2*seb3
    den3c<-pij3/(pii3+pjj3)
    denc<-(den1c-(den2c*den3c))^.5
    t23<-abs(abs(b2)-abs(b3)) / denc
    p23<-2*(1-(pt(q=abs(t23), df=df, lower.tail=TRUE)))
    t23<-round((t23),3)

    #2 vs 4
    pij5<-inv[2,4] #inv of cor between pred of interest 1 vs. 2
    pii5<-inv[2,2] #inv of cov, v1
    pjj5<-inv[4,4] #inv of cov, v2
    den1e<-seb2^2+seb4^2
    den2e<-2*seb2*seb4
    den3e<-pij5/(pii5+pjj5)
    denc<-(den1e-(den2e*den3e))^.5
    t24<-abs(abs(b2)-abs(b4)) / denc
    p24<-2*(1-(pt(q=abs(t24), df=df, lower.tail=TRUE)))
    t24<-round((t24),3)

    #3 vs 4
    pij6<-inv[3,4] #inv of cor between pred of interest 1 vs. 2
    pii6<-inv[3,3] #inv of cov, v1
    pjj6<-inv[4,4] #inv of cov, v2
    den1f<-seb3^2+seb4^2
    den2f<-2*seb3*seb4
    den3f<-pij6/(pii6+pjj6)
    denc<-(den1f-(den2f*den3f))^.5
    t34<-abs(abs(b3)-abs(b4)) / denc
    p34<-2*(1-(pt(q=abs(t34), df=df, lower.tail=TRUE)))
    t34<-round((t34),3)
    message("Pred 1 vs. Pred 2 "," : t = ", t12,", p = ", p12)
    message("Pred 1 vs. Pred 3 "," : t = ", t13,", p = ", p13)
    message("Pred 1 vs. Pred 4 "," : t = ", t14,", p = ", p14)
    message("Pred 2 vs. Pred 3 "," : t = ", t23,", p = ", p23)
    message("Pred 2 vs. Pred 4 "," : t = ", t24,", p = ", p24)
    message("Pred 3 vs. Pred 4 "," : t = ", t34,", p = ", p34)
  }

  if (numpred ==5 && comps=="abs")
  {
    xx<-dplyr::select(data,y,x1,x2,x3,x4,x5)
    xx<-as.data.frame(xx)
    mod<-lm(xx[,1]~xx[,2]+xx[,3]+xx[,4]+xx[,5]+xx[,6])
    values<-summary(mod)
    b1<-(values$coefficients)[2,1] #grabs b from each analysis
    b2<-(values$coefficients)[3,1]
    b3<-(values$coefficients)[4,1]
    b4<-(values$coefficients)[5,1]
    b5<-(values$coefficients)[6,1]
    seb1<-(values$coefficients)[2,2]
    seb2<-(values$coefficients)[3,2]
    seb3<-(values$coefficients)[4,2]
    seb4<-(values$coefficients)[5,2]
    seb5<-(values$coefficients)[6,2]
    df<- values$df[2]

    r12<-abs(cor(xx[,2],xx[,3]))
    r13<-abs(cor(xx[,2],xx[,4]))
    r14<-abs(cor(xx[,2],xx[,5]))
    r15<-abs(cor(xx[,2],xx[,6]))
    r23<-abs(cor(xx[,3],xx[,4]))
    r24<-abs(cor(xx[,3],xx[,5]))
    r25<-abs(cor(xx[,3],xx[,6]))
    r34<-abs(cor(xx[,4],xx[,5]))
    r35<-abs(cor(xx[,4],xx[,5]))
    r45<-abs(cor(xx[,5],xx[,6]))
    mat<-cbind(c(1,r12,r13,r14,r15),c(r12,1,r23,r24,r25), c(r13,r23,1,r34,r35),
               c(r14,r24,r34,1,r45),c(r15,r25,r35,r45,1))
    inv<-solve(mat)*mat
    # 1 vs 2
    pij1<-inv[1,2] #inv of cor between pred of interest 1 vs. 2
    pii1<-inv[1,1] #inv of cov, v1
    pjj1<-inv[2,2] #inv of cov, v2
    den1a<-seb1^2+seb2^2
    den2a<-2*seb1*seb2
    den3a<-pij1/(pii1+pjj1)
    den<-(den1a-(den2a*den3a))^.5
    t12<-abs((abs(b1)-(abs(b2)))) / den
    p12<-2*(1-(pt(q=abs(t12), df=df, lower.tail=TRUE)))
    t12<-round((t12),3)
    #1 vs 3
    pij2<-inv[1,3] #inv of cor between pred of interest 1 vs. 2
    pii2<-inv[1,1] #inv of cov, v1
    pjj2<-inv[3,3] #inv of cov, v2
    den1b<-seb1^2+seb3^2
    den2b<-2*seb1*seb3
    den3b<-pij2/(pii2+pjj2)
    denb<-(den1b-(den2b*den3b))^.5
    t13<-abs(abs(b1)-abs(b3)) / denb
    p13<-2*(1-(pt(q=abs(t13), df=df, lower.tail=TRUE)))
    t13<-round((t13),3)
    #1 vs 4
    pij4<-inv[1,4] #inv of cor between pred of interest 1 vs. 2
    pii4<-inv[1,1] #inv of cov, v1
    pjj4<-inv[4,4] #inv of cov, v2
    den1d<-seb1^2+seb4^2
    den2d<-2*seb1*seb4
    den3d<-pij4/(pii4+pjj4)
    dend<-(den1d-(den2d*den3d))^.5
    t14<-abs(abs(b1)-abs(b4)) / dend
    p14<-2*(1-(pt(q=abs(t14), df=df, lower.tail=TRUE)))
    t14<-round((t14),3)

    #1 vs 5
    pij6<-inv[1,5] #inv of cor between pred of interest 1 vs. 2
    pii6<-inv[1,1] #inv of cov, v1
    pjj6<-inv[5,5] #inv of cov, v2
    den1f<-seb1^2+seb5^2
    den2f<-2*seb1*seb5
    den3f<-pij6/(pii4+pjj6)
    denf<-(den1f-(den2f*den3f))^.5
    t15<-abs(abs(b1)-abs(b5)) / dend
    p15<-2*(1-(pt(q=abs(t15), df=df, lower.tail=TRUE)))
    t15<-round((t15),3)
    #2 vs 3
    pij3<-inv[2,3] #inv of cor between pred of interest 1 vs. 2
    pii3<-inv[2,2] #inv of cov, v1
    pjj3<-inv[3,3] #inv of cov, v2
    den1c<-seb2^2+seb3^2
    den2c<-2*seb2*seb3
    den3c<-pij3/(pii3+pjj3)
    denc<-(den1c-(den2c*den3c))^.5
    t23<-abs(abs(b2)-abs(b3)) / denc
    p23<-2*(1-(pt(q=abs(t23), df=df, lower.tail=TRUE)))
    t23<-round((t23),3)
      #2 vs 4
    pij5<-inv[2,4] #inv of cor between pred of interest 1 vs. 2
    pii5<-inv[2,2] #inv of cov, v1
    pjj5<-inv[4,4] #inv of cov, v2
    den1e<-seb2^2+seb4^2
    den2e<-2*seb2*seb4
    den3e<-pij5/(pii5+pjj5)
    denc<-(den1e-(den2e*den3e))^.5
    t24<-abs(abs(b2)-abs(b4)) / denc
    p24<-2*(1-(pt(q=abs(t24), df=df, lower.tail=TRUE)))
    t24<-round((t24),3)

    #2 vs 5
    pij7<-inv[2,5] #inv of cor between pred of interest 1 vs. 2
    pii7<-inv[2,2] #inv of cov, v1
    pjj7<-inv[5,5] #inv of cov, v2
    den1g<-seb2^2+seb5^2
    den2g<-2*seb2*seb5
    den3g<-pij7/(pii7+pjj7)
    deng<-(den1g-(den2g*den3g))^.5
    t25<-abs(abs(b2)-abs(b5)) / deng
    p25<-2*(1-(pt(q=abs(t25), df=df, lower.tail=TRUE)))
    t25<-round((t25),3)

    #3 vs 4
    pij8<-inv[3,4] #inv of cor between pred of interest 1 vs. 2
    pii8<-inv[3,3] #inv of cov, v1
    pjj8<-inv[4,4] #inv of cov, v2
    den1h<-seb3^2+seb4^2
    den2h<-2*seb3*seb4
    den3h<-pij8/(pii8+pjj8)
    denc<-(den1h-(den2h*den3h))^.5
    t34<-abs(abs(b3)-abs(b4)) / denc
    p34<-2*(1-(pt(q=abs(t34), df=df, lower.tail=TRUE)))
    t34<-round((t34),3)

    #3 vs 5
    pij9<-inv[3,5] #inv of cor between pred of interest 1 vs. 2
    pii9<-inv[3,3] #inv of cov, v1
    pjj9<-inv[5,5] #inv of cov, v2
    den1i<-seb3^2+seb5^2
    den2i<-2*seb3*seb5
    den3i<-pij9/(pii9+pjj9)
    deni<-(den1i-(den2i*den3i))^.5
    t35<-abs(abs(b3)-abs(b5)) / denc
    p35<-2*(1-(pt(q=abs(t35), df=df, lower.tail=TRUE)))
    t35<-round((t35),3)

    #4 vs 5
    pij10<-inv[4,5] #inv of cor between pred of interest 1 vs. 2
    pii10<-inv[4,4] #inv of cov, v1
    pjj10<-inv[5,5] #inv of cov, v2
    den1j<-seb4^2+seb5^2
    den2j<-2*seb4*seb5
    den3j<-pij10/(pii10+pjj10)
    denj<-(den1h-(den2h*den3h))^.5
    t45<-abs(abs(b4)-abs(b5)) / denc
    p45<-2*(1-(pt(q=abs(t45), df=df, lower.tail=TRUE)))
    t45<-round((t45),3)


    message("Pred 1 vs. Pred 2 "," : t = ", t12,", p = ", p12)
    message("Pred 1 vs. Pred 3 "," : t = ", t13,", p = ", p13)
    message("Pred 1 vs. Pred 4 "," : t = ", t14,", p = ", p14)
    message("Pred 1 vs. Pred 5 "," : t = ", t15,", p = ", p15)
    message("Pred 2 vs. Pred 3 "," : t = ", t23,", p = ", p23)
    message("Pred 2 vs. Pred 4 "," : t = ", t24,", p = ", p24)
    message("Pred 2 vs. Pred 5 "," : t = ", t25,", p = ", p25)
    message("Pred 3 vs. Pred 4 "," : t = ", t34,", p = ", p34)
    message("Pred 3 vs. Pred 5 "," : t = ", t35,", p = ", p35)
    message("Pred 4 vs. Pred 5 "," : t = ", t45,", p = ", p45)
  }
  if (numpred ==2 && comps=="raw")
  {
    xx<-dplyr::select(data,y,x1,x2)
    xx<-as.data.frame(xx)
    mod<-lm(xx[,1]~xx[,2]+xx[,3])
    values<-summary(mod)
    b1<-(values$coefficients)[2,1] #grabs b from each analysis
    b2<-(values$coefficients)[3,1]
    seb1<-(values$coefficients)[2,2]
    seb2<-(values$coefficients)[3,2]
    df<- values$df[2]
    varx1<-var(xx[,2])
    varx2<-var(xx[,3])
    r12<-cor(xx[,2],xx[,3])
    mat<-cbind(c(1,r12),c(r12,1))
    inv<-solve(mat)*mat
    pij<-inv[1,2] #inv of cor between pred1 and 2
    pii<-inv[1,1] #inv of cov, v1
    pjj<-inv[2,2] #inv of cov, v2
    den1<-seb1^2+seb2^2
    den2<-2*seb1*seb2
    den3<-pij/(pii+pjj)
    den<-(den1-(den2*den3))^.5
    t12<-(b1-b2)/ den
    p12<-2*(1-(pt(q=abs(t12), df=df, lower.tail=TRUE)))
    t12<-round((t12),3)
    message("Pred 1 vs. Pred 2 "," : t = ", t12,", p = ", p12)

  }
  if (numpred ==3 && comps=="raw")
  {
    xx<-dplyr::select(data,y,x1,x2,x3)
    xx<-as.data.frame(xx)
    mod<-lm(xx[,1]~xx[,2]+xx[,3]+xx[,4])
    values<-summary(mod)
    b1<-(values$coefficients)[2,1] #grabs b from each analysis
    b2<-(values$coefficients)[3,1]
    b3<-(values$coefficients)[4,1]
    seb1<-(values$coefficients)[2,2]
    seb2<-(values$coefficients)[3,2]
    seb3<-(values$coefficients)[4,2]
    df<- values$df[2]

    r12<-cor(xx[,2],xx[,3])
    r13<-cor(xx[,2],xx[,4])
    r23<-cor(xx[,3],xx[,4])
    mat<-cbind(c(1,r12,r13),c(r12,1,r23), c(r13,r23,1))
    inv<-solve(mat)*mat
    # 1 vs 2
    pij1<-inv[1,2] #inv of cor between pred of interest 1 vs. 2
    pii1<-inv[1,1] #inv of cov, v1
    pjj1<-inv[2,2] #inv of cov, v2
    den1a<-seb1^2+seb2^2
    den2a<-2*seb1*seb2
    den3a<-pij1/(pii1+pjj1)
    den<-(den1a-(den2a*den3a))^.5
    t12<-(b1-b2) / den
    p12<-2*(1-(pt(q=abs(t12), df=df, lower.tail=TRUE)))
    t12<-round((t12),3)
    #1 vs 3
    pij2<-inv[1,3] #inv of cor between pred of interest 1 vs. 2
    pii2<-inv[1,1] #inv of cov, v1
    pjj2<-inv[3,3] #inv of cov, v2
    den1b<-seb1^2+seb3^2
    den2b<-2*seb1*seb3
    den3b<-pij2/(pii2+pjj2)
    denb<-(den1b-(den2b*den3b))^.5
    t13<-(b1-b3) / denb
    p13<-2*(1-(pt(q=abs(t13), df=df, lower.tail=TRUE)))
    t13<-round((t13),3)
    #2 vs 3
    pij3<-inv[2,3] #inv of cor between pred of interest 1 vs. 2
    pii3<-inv[2,2] #inv of cov, v1
    pjj3<-inv[3,3] #inv of cov, v2
    den1c<-seb2^2+seb3^2
    den2c<-2*seb2*seb3
    den3c<-pij3/(pii3+pjj3)
    denc<-(den1c-(den2c*den3c))^.5
    t23<-abs(abs(b2)-abs(b3)) / denc
    p23<-2*(1-(pt(q=abs(t23), df=df, lower.tail=TRUE)))
    t23<-round((t23),3)
    message("Pred 1 vs. Pred 2 "," : t = ", t12,", p = ", p12)
    message("Pred 1 vs. Pred 3 "," : t = ", t13,", p = ", p13)
    message("Pred 2 vs. Pred 3 "," : t = ", t23,", p = ", p23)
  }
  if (numpred ==4 && comps=="raw")
  {
    xx<-dplyr::select(data,y,x1,x2,x3,x4)
    xx<-as.data.frame(xx)
    mod<-lm(xx[,1]~xx[,2]+xx[,3]+xx[,4]+xx[,5])
    values<-summary(mod)
    b1<-(values$coefficients)[2,1] #grabs b from each analysis
    b2<-(values$coefficients)[3,1]
    b3<-(values$coefficients)[4,1]
    b4<-(values$coefficients)[5,1]
    seb1<-(values$coefficients)[2,2]
    seb2<-(values$coefficients)[3,2]
    seb3<-(values$coefficients)[4,2]
    seb4<-(values$coefficients)[5,2]
    df<- values$df[2]

    r12<-cor(xx[,2],xx[,3])
    r13<-cor(xx[,2],xx[,4])
    r14<-cor(xx[,2],xx[,5])
    r23<-cor(xx[,3],xx[,4])
    r24<-cor(xx[,3],xx[,5])
    r34<-cor(xx[,4],xx[,5])
    mat<-cbind(c(1,r12,r13,r14),c(r12,1,r23,r24), c(r13,r23,1,r34),c(r14,r24,r34,1))
    inv<-solve(mat)*mat
    # 1 vs 2
    pij1<-inv[1,2] #inv of cor between pred of interest 1 vs. 2
    pii1<-inv[1,1] #inv of cov, v1
    pjj1<-inv[2,2] #inv of cov, v2
    den1a<-seb1^2+seb2^2
    den2a<-2*seb1*seb2
    den3a<-pij1/(pii1+pjj1)
    den<-(den1a-(den2a*den3a))^.5
    t12<-(b1-b2) / den
    p12<-2*(1-(pt(q=abs(t12), df=df, lower.tail=TRUE)))
    t12<-round((t12),3)
    #1 vs 3
    pij2<-inv[1,3] #inv of cor between pred of interest 1 vs. 2
    pii2<-inv[1,1] #inv of cov, v1
    pjj2<-inv[3,3] #inv of cov, v2
    den1b<-seb1^2+seb3^2
    den2b<-2*seb1*seb3
    den3b<-pij2/(pii2+pjj2)
    denb<-(den1b-(den2b*den3b))^.5
    t13<-(b1-b3) / denb
    p13<-2*(1-(pt(q=abs(t13), df=df, lower.tail=TRUE)))
    t13<-round((t13),3)
    #1 vs 4
    pij4<-inv[1,4] #inv of cor between pred of interest 1 vs. 2
    pii4<-inv[1,1] #inv of cov, v1
    pjj4<-inv[4,4] #inv of cov, v2
    den1d<-seb1^2+seb4^2
    den2d<-2*seb1*seb4
    den3d<-pij4/(pii4+pjj4)
    dend<-(den1d-(den2d*den3d))^.5
    t14<-(b1-b4) / dend
    p14<-2*(1-(pt(q=abs(t14), df=df, lower.tail=TRUE)))
    t14<-round((t14),3)
    #2 vs 3
    pij3<-inv[2,3] #inv of cor between pred of interest 1 vs. 2
    pii3<-inv[2,2] #inv of cov, v1
    pjj3<-inv[3,3] #inv of cov, v2
    den1c<-seb2^2+seb3^2
    den2c<-2*seb2*seb3
    den3c<-pij3/(pii3+pjj3)
    denc<-(den1c-(den2c*den3c))^.5
    t23<-abs(abs(b2)-abs(b3)) / denc
    p23<-2*(1-(pt(q=abs(t23), df=df, lower.tail=TRUE)))
    t23<-round((t23),3)

    #2 vs 4
    pij5<-inv[2,4] #inv of cor between pred of interest 1 vs. 2
    pii5<-inv[2,2] #inv of cov, v1
    pjj5<-inv[4,4] #inv of cov, v2
    den1e<-seb2^2+seb4^2
    den2e<-2*seb2*seb4
    den3e<-pij5/(pii5+pjj5)
    denc<-(den1e-(den2e*den3e))^.5
    t24<-(b2-b4) / denc
    p24<-2*(1-(pt(q=abs(t24), df=df, lower.tail=TRUE)))
    t24<-round((t24),3)

    #3 vs 4
    pij6<-inv[3,4] #inv of cor between pred of interest 1 vs. 2
    pii6<-inv[3,3] #inv of cov, v1
    pjj6<-inv[4,4] #inv of cov, v2
    den1f<-seb3^2+seb4^2
    den2f<-2*seb3*seb4
    den3f<-pij6/(pii6+pjj6)
    denc<-(den1f-(den2f*den3f))^.5
    t34<-(b3-b4) / denc
    p34<-2*(1-(pt(q=abs(t34), df=df, lower.tail=TRUE)))
    t34<-round((t34),3)
    message("Pred 1 vs. Pred 2 "," : t = ", t12,", p = ", p12)
    message("Pred 1 vs. Pred 3 "," : t = ", t13,", p = ", p13)
    message("Pred 1 vs. Pred 4 "," : t = ", t14,", p = ", p14)
    message("Pred 2 vs. Pred 3 "," : t = ", t23,", p = ", p23)
    message("Pred 2 vs. Pred 4 "," : t = ", t24,", p = ", p24)
    message("Pred 3 vs. Pred 4 "," : t = ", t34,", p = ", p34)
  }

  if (numpred ==5 && comps=="raw")
  {
    xx<-dplyr::select(data,y,x1,x2,x3,x4,x5)
    xx<-as.data.frame(xx)
    mod<-lm(xx[,1]~xx[,2]+xx[,3]+xx[,4]+xx[,5]+xx[,6])
    values<-summary(mod)
    b1<-(values$coefficients)[2,1] #grabs b from each analysis
    b2<-(values$coefficients)[3,1]
    b3<-(values$coefficients)[4,1]
    b4<-(values$coefficients)[5,1]
    b5<-(values$coefficients)[6,1]
    seb1<-(values$coefficients)[2,2]
    seb2<-(values$coefficients)[3,2]
    seb3<-(values$coefficients)[4,2]
    seb4<-(values$coefficients)[5,2]
    seb5<-(values$coefficients)[6,2]
    df<- values$df[2]

    r12<-cor(xx[,2],xx[,3])
    r13<-cor(xx[,2],xx[,4])
    r14<-cor(xx[,2],xx[,5])
    r15<-cor(xx[,2],xx[,6])
    r23<-cor(xx[,3],xx[,4])
    r24<-cor(xx[,3],xx[,5])
    r25<-cor(xx[,3],xx[,6])
    r34<-cor(xx[,4],xx[,5])
    r35<-cor(xx[,4],xx[,5])
    r45<-cor(xx[,5],xx[,6])
    mat<-cbind(c(1,r12,r13,r14,r15),c(r12,1,r23,r24,r25), c(r13,r23,1,r34,r35),
               c(r14,r24,r34,1,r45),c(r15,r25,r35,r45,1))
    inv<-solve(mat)*mat
    # 1 vs 2
    pij1<-inv[1,2] #inv of cor between pred of interest 1 vs. 2
    pii1<-inv[1,1] #inv of cov, v1
    pjj1<-inv[2,2] #inv of cov, v2
    den1a<-seb1^2+seb2^2
    den2a<-2*seb1*seb2
    den3a<-pij1/(pii1+pjj1)
    den<-(den1a-(den2a*den3a))^.5
    t12<-(b1-b2) / den
    p12<-2*(1-(pt(q=abs(t12), df=df, lower.tail=TRUE)))
    t12<-round((t12),3)
    #1 vs 3
    pij2<-inv[1,3] #inv of cor between pred of interest 1 vs. 2
    pii2<-inv[1,1] #inv of cov, v1
    pjj2<-inv[3,3] #inv of cov, v2
    den1b<-seb1^2+seb3^2
    den2b<-2*seb1*seb3
    den3b<-pij2/(pii2+pjj2)
    denb<-(den1b-(den2b*den3b))^.5
    t13<-(b1-b3) / denb
    p13<-2*(1-(pt(q=abs(t13), df=df, lower.tail=TRUE)))
    t13<-round((t13),3)
    #1 vs 4
    pij4<-inv[1,4] #inv of cor between pred of interest 1 vs. 2
    pii4<-inv[1,1] #inv of cov, v1
    pjj4<-inv[4,4] #inv of cov, v2
    den1d<-seb1^2+seb4^2
    den2d<-2*seb1*seb4
    den3d<-pij4/(pii4+pjj4)
    dend<-(den1d-(den2d*den3d))^.5
    t14<-(b1-b4) / dend
    p14<-2*(1-(pt(q=abs(t14), df=df, lower.tail=TRUE)))
    t14<-round((t14),3)

    #1 vs 5
    pij6<-inv[1,5] #inv of cor between pred of interest 1 vs. 2
    pii6<-inv[1,1] #inv of cov, v1
    pjj6<-inv[5,5] #inv of cov, v2
    den1f<-seb1^2+seb5^2
    den2f<-2*seb1*seb5
    den3f<-pij6/(pii4+pjj6)
    denf<-(den1f-(den2f*den3f))^.5
    t15<-(b1-b5) / dend
    p15<-2*(1-(pt(q=abs(t15), df=df, lower.tail=TRUE)))
    t15<-round((t15),3)
    #2 vs 3
    pij3<-inv[2,3] #inv of cor between pred of interest 1 vs. 2
    pii3<-inv[2,2] #inv of cov, v1
    pjj3<-inv[3,3] #inv of cov, v2
    den1c<-seb2^2+seb3^2
    den2c<-2*seb2*seb3
    den3c<-pij3/(pii3+pjj3)
    denc<-(den1c-(den2c*den3c))^.5
    t23<-abs(abs(b2)-abs(b3)) / denc
    p23<-2*(1-(pt(q=abs(t23), df=df, lower.tail=TRUE)))
    t23<-round((t23),3)
    #2 vs 4
    pij5<-inv[2,4] #inv of cor between pred of interest 1 vs. 2
    pii5<-inv[2,2] #inv of cov, v1
    pjj5<-inv[4,4] #inv of cov, v2
    den1e<-seb2^2+seb4^2
    den2e<-2*seb2*seb4
    den3e<-pij5/(pii5+pjj5)
    denc<-(den1e-(den2e*den3e))^.5
    t24<-(b2-b4) / denc
    p24<-2*(1-(pt(q=abs(t24), df=df, lower.tail=TRUE)))
    t24<-round((t24),3)

    #2 vs 5
    pij7<-inv[2,5] #inv of cor between pred of interest 1 vs. 2
    pii7<-inv[2,2] #inv of cov, v1
    pjj7<-inv[5,5] #inv of cov, v2
    den1g<-seb2^2+seb5^2
    den2g<-2*seb2*seb5
    den3g<-pij7/(pii7+pjj7)
    deng<-(den1g-(den2g*den3g))^.5
    t25<-(b2-b5) / deng
    p25<-2*(1-(pt(q=abs(t25), df=df, lower.tail=TRUE)))
    t25<-round((t25),3)

    #3 vs 4
    pij8<-inv[3,4] #inv of cor between pred of interest 1 vs. 2
    pii8<-inv[3,3] #inv of cov, v1
    pjj8<-inv[4,4] #inv of cov, v2
    den1h<-seb3^2+seb4^2
    den2h<-2*seb3*seb4
    den3h<-pij8/(pii8+pjj8)
    denc<-(den1h-(den2h*den3h))^.5
    t34<-(b3-b4) / denc
    p34<-2*(1-(pt(q=abs(t34), df=df, lower.tail=TRUE)))
    t34<-round((t34),3)

    #3 vs 5
    pij9<-inv[3,5] #inv of cor between pred of interest 1 vs. 2
    pii9<-inv[3,3] #inv of cov, v1
    pjj9<-inv[5,5] #inv of cov, v2
    den1i<-seb3^2+seb5^2
    den2i<-2*seb3*seb5
    den3i<-pij9/(pii9+pjj9)
    deni<-(den1i-(den2i*den3i))^.5
    t35<-(b3-b5) / denc
    p35<-2*(1-(pt(q=abs(t35), df=df, lower.tail=TRUE)))
    t35<-round((t35),3)

    #4 vs 5
    pij10<-inv[4,5] #inv of cor between pred of interest 1 vs. 2
    pii10<-inv[4,4] #inv of cov, v1
    pjj10<-inv[5,5] #inv of cov, v2
    den1j<-seb4^2+seb5^2
    den2j<-2*seb4*seb5
    den3j<-pij10/(pii10+pjj10)
    denj<-(den1h-(den2h*den3h))^.5
    t45<-(b4-b5) / denc
    p45<-2*(1-(pt(q=abs(t45), df=df, lower.tail=TRUE)))
    t45<-round((t45),3)


    message("Pred 1 vs. Pred 2 "," : t = ", t12,", p = ", p12)
    message("Pred 1 vs. Pred 3 "," : t = ", t13,", p = ", p13)
    message("Pred 1 vs. Pred 4 "," : t = ", t14,", p = ", p14)
    message("Pred 1 vs. Pred 5 "," : t = ", t15,", p = ", p15)
    message("Pred 2 vs. Pred 3 "," : t = ", t23,", p = ", p23)
    message("Pred 2 vs. Pred 4 "," : t = ", t24,", p = ", p24)
    message("Pred 2 vs. Pred 5 "," : t = ", t25,", p = ", p25)
    message("Pred 3 vs. Pred 4 "," : t = ", t34,", p = ", p34)
    message("Pred 3 vs. Pred 5 "," : t = ", t35,", p = ", p35)
    message("Pred 4 vs. Pred 5 "," : t = ", t45,", p = ", p45)
  }

  }

