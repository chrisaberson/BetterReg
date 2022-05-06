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

Please post issues using the link above (titled "issues"). Those interested in contributing to further development should create a pull request. 

## License  

This project is licensed under GNU General Public License version 3.

## Example 1: OLS Regression

Using data from Aberson (2007), the analyses that follow predict support for Affirmative Action (AA) in year 4 of college from incomming attitudes, personal experience with discrimination, liberality, gender, and economic concern for the future. In a later example, I add perceptions of the prevalence of discrimination, endorsement of meritocracy, and partipation in campus diversity events. 

The part function requires an existing LM model and indication of number of predictors.  

First, build the model:
xx<-lm(formula = AA_DV ~ AA_Initial+pers_exp+liberal+female+economic, data = hand5)

Then, using the parts command, provide the model name and the number of predictors
parts(model=xx, pred=5)

Predictor 1: semi partial = 0.333; squared semipartial = 0.111 
Predictor 2: semi partial = 0.032; squared semipartial = 0.001 
Predictor 3: semi partial = 0.197; squared semipartial = 0.039 
Predictor 4: semi partial = 0.095; squared semipartial = 0.009 
Predictor 5: semi partial = 0.032; squared semipartial = 0.001 

Mahalanobis values require input of model and predictors as well as the number of values to return (10 is the default). 

Mahal(model=xx, pred=5, values=10) 
       60      247      639      703      133      157      129      431  
 11.08698 11.08698 11.77189 11.93773 12.34983 13.68620 14.15117 14.50515  
 
       24      655  
 14.72140 15.27508  

The tolerance command requires only the model name. 

tolerance(model=xx)  
AA_Initial   pers_exp    liberal     female   economic 
0.9464682  0.9904156  0.9058256  0.9418196  0.9910559  

R2change compares two models. Below, I added merit, discrimination, and diversity participation to the model (xx2).
the R2change command takes model1 (xx) and compares it to model2 (xx2). Note that this approach is only for models that are adding variables to a previous model.

xx2<-lm(formula = AA_DV ~ AA_Initial+pers_exp+liberal+female+economic+merit+discrim+div_part, data = hand5) 
R2change(model1=xx, model2=xx2) 

R-square change = 0.181 
F(3,704) = 70.537, p = 6.81538788796511e-40 

## Example 2: Logistic Regression functions

In this example, taken from Cohen, Cohen, West, and Aiken (2015), women's compliance with mammography recommendations (i.e., yearly screening in this case) is predicted from physicians recommendation, knowledge of mammography, perceived barriers, and perceived benifits. 

First, run a logistic regression model. 

Model4<-glm(comply~physrec+knowledg+benefits+barriers, data=logistic2, family = binomial())

The LRchi command requires the name of the dataset, definition of all variables in mode (y, x1, x2, etc.), and the number of model predictors. 

LRchi(data=logistic2, y="comply",x1="physrec", x2="knowledg", x3="benefits",x4="barriers", numpred=4) 
 Predictor: physrec; LR squared 16.67, p= 0 
 Predictor: knowledg; LR squared 0.01, p= 0.94 
 Predictor: benefits; LR squared 5.29, p= 0.02 
 Predictor: barriers; LR squared 13.77, p= 0 

The Psuedo function requires only an existing model as input.   

pseudo(model = Model4) 
 Likelihood Ratio R-squared (McFadden, Recommended) = 0.26 
 Cox-Snell R-squared) = 0.301 
 Nagelkerk R-squared  = 0.402 

