# LAB ASSIGNMENT 4 - STRUCTURE EQUATION MODELLING:  MENTAL ABILITY TEST SCORES
# Luis Felipe Villota Macías 

# Objective: to evaluate fit of a theoretical factor structure of mental ability 
# test indicators and relationships between different test scores. It is a
# theory-based confirmatory research. 



#Loading packages
library(lavaan) # for SEM fit and model functions	
library(semPlot) # for semPaths()	
library(semptools) # for set_sem_layout	
library(tidyverse) # for tidy code	
library(CompQuadForm) # for mvnorm.kur.test and mvnorm.skew.test (prerequisite)	
library(ICS) # for mvnorm.kur.test and mvnorm.skew.test	
library(psychTools)
library(psych)	
library(readr)
library(skimr)
library(Amelia)
library(MVN)
library(semTable)

# Setting working directory
getwd()
setwd("C:/Users/USER/Desktop/SSDA - LUND UNIVERSITY/- COURSES/SIMM61- Quantitative Data Analysis in R/Scripts/Zoltan")

# Loading the data 

mental_data = holzinger.swineford

# Checking the data
View(mental_data)
summary(mental_data)
skim(mental_data)
describe(mental_data) # 301 obs 33 vars

# Missing values
any(is.na(mental_data)) # TRUE
sum(is.na(mental_data)) # 312 missing cases in total
colSums(is.na(mental_data)) # missing values in "t25_frmbord2" and "t26_flags"
missmap(mental_data)
# We won't remove any NAs, it will reduce the dataset considerably.

# Variables of interest: tidying and recoding 
mental_data = mental_data %>% 
                     rename(sex = female) # female to sex

# _________________________

# Path diagrams of the MEASUREMENT MODEL: the latent variable (here exogenous)
# is the unobserved cause of manifest variables (here endogenous). It is a 
# latent variable loading in SEM.

# From lab assignment: "Your theoretical model indicates that there are three (3) 
# underlying factors that influence the mental ability test scores.These latent 
# factors can correlate with each other.

# 1) Visual perception ability is a latent factor that influences (causally determines) test scores 
# measured by t01_visperc, t02_cubes, t03_frmbord, and t04_lozenges, which are all related to 
# visual perception and mental rotation.


model_visper <- '
visper =~ t01_visperc + t02_cubes + t03_frmbord + t04_lozenges
'
fit_visper <- sem(model_visper, data = mental_data)
plot = semPaths(fit_visper, label.scale=F, nCharNodes = 8,
                sizeMan2=3.5, sizeMan=10, asize=3, edge.color="black", residuals = F, fixedStyle = 1)


# 2) Verbal ability is a latent factor that influences (causally determines) test scores measured 
# by t06_paracomp, t07_sentcomp, and t09_wordmean.

model_verbal <- '
verbal =~ t06_paracomp + t07_sentcomp + t09_wordmean 
'
fit_verbal <- sem(model_verbal, data = mental_data)
plot = semPaths(fit_verbal, label.scale=F, nCharNodes = 8,
                sizeMan2=3.5, sizeMan=10, asize=3, edge.color="black", residuals = F, fixedStyle = 1)



# 3) Processing speed is a latent factor that influences (causally determines) test scores 
# measured by t10_addition t12_countdot, and t13_sccaps." 

model_processing <- '
processing =~ t10_addition + t12_countdot + t13_sccaps 
'
fit_processing <- sem(model_processing, data = mental_data)
plot = semPaths(fit_processing, label.scale=F, nCharNodes = 8,
                sizeMan2=3.5, sizeMan=10, asize=3, edge.color="black", residuals = F, fixedStyle = 1)

#______________________________



# MODEL A

modelA<- '
        # measurement model
          visper =~ t01_visperc + t02_cubes + t03_frmbord + t04_lozenges
          verbal =~ t06_paracomp + t07_sentcomp + t09_wordmean 
          processing =~ t10_addition + t12_countdot + t13_sccaps
'

fit_modelA <- sem(modelA, data = mental_data)
fit_modelA

# unstandardized

summary(fit_modelA)

plot = semPaths(fit_modelA, label.scale=F, nCharNodes = 8,
                sizeMan2=3.5, sizeMan=10, asize=3, edge.color="black", residuals = F, fixedStyle = 1,
                whatLabels = "est")

semPaths(fit_modelA, whatLabels = "est")

# standardized 

summary(fit_modelA, standardized = T, rsquare = T)

plot = semPaths(fit_modelA, label.scale=F, nCharNodes = 8,
                sizeMan2=3.5, sizeMan=10, asize=3, edge.color="black", residuals = F, fixedStyle = 1,
                whatLabels = "std")

semPaths(fit_modelA, whatLabels = "std")

#______________________________

# Counting degrees of freedom: 

summary(fit_modelA) # df = 32, Model A is identified 

# From the notes: "difference between the total number of possible estimated 
# parameters of a "null model" with no latent variables, and the number of free parameters
# in our actual model". 

# 10 manifest variables
# p*(p+1)/2

10*(10+1)/2 # total possible parameters of the manifest variables = 55 



parTable(fit_modelA)
parameterEstimates(fit_modelA)

# The number of free parameters: 

# 10 latent factor loadings between:
# visper =~ t01_visperc + t02_cubes + t03_frmbord + t04_lozenges
# verbal =~ t06_paracomp + t07_sentcomp + t09_wordmean 
# processing =~ t10_addition + t12_countdot + t13_sccaps
# 3 covariances (between our exogenous variables)
# 3 variances (for each latent factor)
# 7 residuals

# 
# That gives us a df = 55 - 23 = 32   

#______________________________

# Checking assumptions: MULTIVARIATE NORMALITY 


## Correlations: between all items in the questionnaire

mental_tscores = mental_data %>% dplyr::select(t01_visperc:t26_flags)  #database with 26 test scores
cor_mental_tscores <- mental_tscores %>% # correlations between items
        cor()
cor_mental_tscores




# Multivariate normality through the Henze-Zirkler's test

mental_mvn <- mvn(mental_data[,c("t01_visperc", "t02_cubes", "t03_frmbord", "t04_lozenges", 
                                 "t06_paracomp", "t07_sentcomp", "t09_wordmean", 
                                 "t10_addition", "t12_countdot", "t13_sccaps")], mvnTest = "hz") 
mental_mvn$multivariateNormality   # p <0.05, MVN is violated


# Multivariate Normality Test Based on Kurtosis
mvnorm.kur.test(mental_data[,c("t01_visperc", "t02_cubes", "t03_frmbord", "t04_lozenges", 
                           "t06_paracomp", "t07_sentcomp", "t09_wordmean", 
                           "t10_addition", "t12_countdot", "t13_sccaps")])  # p<0.05, MVN is violated

# Multivariate Normality Test Based on Skewness
mvnorm.skew.test(mental_data[,c("t01_visperc", "t02_cubes", "t03_frmbord", "t04_lozenges", 
                               "t06_paracomp", "t07_sentcomp", "t09_wordmean", 
                               "t10_addition", "t12_countdot", "t13_sccaps")])  # p<0.05, MVN is violated

# All MVN tests show p values lower than 0.05, so the assumption of MVN is violated. 
# P values are not reliable, the standard errors are large, model fit indices are unreliable.
# We proceed with the robust variants of the statistics of the model fit: e.g Satorra-Bentler 
# (corrects Chi-squared fit), normality-adjusted robust standard errors. 


# ML with robust SE and test statistics
fit_modelA_MLM <- sem(modelA, data = mental_data, estimator = "MLM")
summary(fit_modelA_MLM, fit.measures = T)

# Bootstrapped ML with robust SE and test statistics
fit_modelA_BOOT <- sem(modelA, data = mental_data, se = "bootstrap", test = "bootstrap")
summary(fit_modelA_BOOT)





#_______________________________________


# MODEL B 

modelB<- '
        # measurement model
          visper =~ t01_visperc + t02_cubes + t03_frmbord + t04_lozenges
          verbal =~ t06_paracomp + t07_sentcomp + t09_wordmean 
          processing =~ t10_addition + t12_countdot + t13_sccaps
          t10_addition~~t12_countdot
'



fit_modelB_MLM <- sem(modelB, data = mental_data, estimator = "MLM")
fit_modelB_MLM


# unstandardized

summary(fit_modelB_MLM)

plot = semPaths(fit_modelB_MLM, label.scale=F, nCharNodes = 8,
                sizeMan2=3.5, sizeMan=10, asize=3, edge.color="black", residuals = F, fixedStyle = 1,
                whatLabels = "est")

semPaths(fit_modelB_MLM, whatLabels = "est")

# standardized 

summary(fit_modelB_MLM, standardized = T, rsquare = T)

plot = semPaths(fit_modelB_MLM, label.scale=F, nCharNodes = 8,
                sizeMan2=3.5, sizeMan=10, asize=3, edge.color="black", residuals = F, fixedStyle = 1,
                whatLabels = "std")

semPaths(fit_modelB_MLM, whatLabels = "std")


standardizedsolution(fit_modelB_MLM)


# Indices of Model B
summary(fit_modelB_MLM, fit.measures = T)


#_______________________________________________________


# Reproduction of Task 3 Path Diagram 


modelC<- '
        # measurement model
          t13_sccaps ~ c*t01_visperc + b*t12_countdot 
          t12_countdot ~ a*t01_visperc 
          
        # indirect effect (a*b)
            indirect := a*b
        
        # total effect
            total := c + (a*b)
          
'


fit_modelC <- sem(modelC, data = mental_data)

summary(fit_modelC)

plot = semPaths(fit_modelC, label.scale=F, nCharNodes = 8,
                sizeMan2=3.5, sizeMan=15, asize=3, edge.color="black", residuals = F, fixedStyle = 1,
                edge.label.cex = 1)



fit_modelC_MLM <- sem(modelC, data = mental_data, estimator = "MLM")
fit_modelC_MLM

semPaths(fit_modelC_MLM, whatLabels = "est")

summary(fit_modelC_MLM, fit.measures = T)

parameterestimates(fit_modelC)
