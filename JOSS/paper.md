---
title: 'BetterReg: An R package for Useful Regression Statistics'  
tags:  
  - R  
  - OLS Regression  
  - Logistic Regression  
authors:  
  - name: Christopher L. Aberson  
    orcid: 0000-0003-3481-7177  
    affiliation: 1  
affiliations:  
 - name: Cal Poly Humboldt  
   index: 1  
date: 20 February 2022  
bibliography: paper.bib

# Optional fields if submitting to a AAS journal too, see this blog post:
# https://blog.joss.theoj.org/2018/12/a-new-collaboration-with-aas-publishing
aas-doi: 10.3847/xxxxx <- update this with the DOI from AAS once you know it.
aas-journal: Astrophysical Journal <- The name of the AAS journal.
---

# Summary

Statistics such as squared semi partial correlations, tolerance, and
Mahalanobis Distances are useful for reporting the results of OLS
Regression (@tabachnick_using_2019?). Similarly values such as the
Likelihood Ratio Chi-square (@cohen_applied_2015)) and Likelihood
R-square [@menard_logistic_2010]. Such statistics are not part
of base R [@r_core_team_r_2022] popular packages such *car*
[fox_r_2019]. To fill these gaps, I developed BetterReg to
provide these values.

Squared semipartial correlations provide a measure of uniquely explained
variances that is on the same scale as *R*<sup>2</sup> values. Tolerance
values address multicollinearity by addressing variance unexplained in a
predictor. Mahalanabis Distances are a popular measure of multivariate
outliers that are presented on an *χ*<sup>2</sup> scale. The Likelihood
Ratio *χ*<sup>2</sup> provides a significance test that is more stable
than the commonly presented Wald Test for logistic regression. The
Likelihood Ratio *χ*<sup>2</sup> is the most widely recommended Pseudo
*R*<sup>2</sup> statistic for logistic.

# Useage

BetterReg functions require existing regression models (either OLS or
Logistic for most statistics), dataset names (for some approaches),
number of predictors (some functions), and desired amount of output
(Mahal function).

## *part* function for squared semipartial correlations

The *part* function requires an existing LM model and indication of
number of predictors.

    library(BetterReg)
    mymodel<-lm(y~x1+x2+x3+x4+x5, data=testreg)
    parts(model=mymodel, pred=5)

    ## Predictor 1: semi partial = 0.032; squared semipartial = 0.001

    ## Predictor 2: semi partial = 0.307; squared semipartial = 0.094

    ## Predictor 3: semi partial = 0.268; squared semipartial = 0.072

    ## Predictor 4: semi partial = 0.134; squared semipartial = 0.018

    ## Predictor 5: semi partial = 0.241; squared semipartial = 0.058

## *tolerance* function for multicollinearity assumptions

The *tolerance* function requires only a model.

    mymodel<-lm(y~x1+x2+x3+x4+x5, data=testreg)
    tolerance(model=mymodel)

    ##        x1        x2        x3        x4        x5 
    ## 0.9976977 0.9990479 0.9931082 0.9953317 0.9980628

## *Mahal* function for detecting multivariate outliers

The *Mahal* function requires model, predictors, and desired number of
values to output.

    mymodel<-lm(y~x1+x2+x3+x4+x5, data=testreg)
    Mahal(model=mymodel, pred=5, values=10)

    ##      537      770      342      760      299      982      446      174 
    ## 14.56342 15.03188 15.56224 15.60986 16.52869 16.80958 17.38597 18.11072 
    ##      458      530 
    ## 20.02762 25.09934

## *LRchi* function for Logistic Regression Coefficients

The *LRchi* function takes input for the dependent variable name (y), up
to 10 predictors (x1, x2, etc.), and the number of predictors.

    LRchi(data=testlog, y="dv", x1="iv1", x2="iv2",numpred=2)

    ## Predictor: iv1; LR squared 34.09, p= 0

    ## Predictor: iv2; LR squared 0.19, p= 0.67

## *Pseudo* function for Logistic Regression Effect Size

The *Psuedo* function takes an existing model as input

    mymodel<-glm(dv~iv1+iv2+iv3+iv4, testlog,family = binomial())
    pseudo(model=mymodel)

    ## Likelihood Ratio R-squared (McFadden, Recommended) = 0.26

    ## Cox-Snell R-squared) = 0.301

    ## Nagelkerk R-squared  = 0.402

# References
