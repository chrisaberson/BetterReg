# BetterReg Readme

Chris Aberson May 2, 2022

# BetterReg  

This package provides tools for statistics that are not provided in base
R packages for linear regression and logistic regression. Functions
provide squared semi-partial correlations, tolerance, Mahalanobis
Distances, Likelihood Ratio Chi-square, and Pseudo R-square. 

### Prerequisites  

I built this under R 4.1.1

## Authors  

-   **Chris Aberson** [chrisaberson](https://github.com/chrisaberson)

## Dependencies  

car (>= 3.0-0), stats (>= 3.5.0), dplyr (>= 0.8.0)

## Issues and Contributions  

Please post issues using the link above (titled "isssues"). Those interested in contributing to further development should create a pull request. 

## License  

This project is licensed under GNU General Public License version 3.

## Examples  

### part function for squared semipartial correlations  

The part function requires an existing LM model and indication of number of predictors.  

library(BetterReg)  
mymodel<-lm(y~x1+x2+x3+x4+x5, data=testreg)  
parts(model=mymodel, pred=5)  

 Predictor 1: semi partial = 0.032; squared semipartial = 0.001  
 Predictor 2: semi partial = 0.307; squared semipartial = 0.094  
 Predictor 3: semi partial = 0.268; squared semipartial = 0.072  
 Predictor 4: semi partial = 0.134; squared semipartial = 0.018  
 Predictor 5: semi partial = 0.241; squared semipartial = 0.058  

### tolerance function for multicollinearity assumptions  

The tolerance function requires only a model.  

mymodel<-lm(y~x1+x2+x3+x4+x5, data=testreg)  
tolerance(model=mymodel)  
 
        x1        x2        x3        x4        x5 
 0.9976977 0.9990479 0.9931082 0.9953317 0.9980628


### Mahal function for detecting multivariate outliers  

The Mahal function requires model, predictors, and desired number of values to output.  

mymodel<-lm(y~x1+x2+x3+x4+x5, data=testreg)  
Mahal(model=mymodel, pred=5, values=10)  

      537      770      342      760      299      982      446      174 
 14.56342 15.03188 15.56224 15.60986 16.52869 16.80958 17.38597 18.11072 
      458      530 
 20.02762 25.09934


### LRchi function for Logistic Regression Coefficients  

The LRchi function takes input for the dependent variable name (y), up to 10 predictors (x1, x2, etc.), and the number of predictors.  

LRchi(data=testlog, y="dv", x1="iv1", x2="iv2",numpred=2)  

Predictor: iv1; LR squared 34.09, p= 0  
Predictor: iv2; LR squared 0.19, p= 0.67  

### Pseudo function for Logistic Regression Effect Size  

The Psuedo function takes an existing model as input  

mymodel<-glm(dv~iv1+iv2+iv3+iv4, testlog,family = binomial())  
pseudo(model=mymodel)  

Likelihood Ratio R-squared (McFadden, Recommended) = 0.26  
Cox-Snell R-squared) = 0.301  
Nagelkerk R-squared  = 0.402  
