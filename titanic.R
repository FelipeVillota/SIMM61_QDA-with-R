
# LAB ASSIGNMENT 1 - LOGISTIC REGRESSION: TITANIC DATABASE 
# Luis Felipe Villota Macías 


# RQ: 
# Did Leonardo's decision of not accompanying Sue and Kate on the trip
# decreased their chances of survival?

# Antecedents:
#
## They were supposed to be a group of 3 but Leonardo did not embark.
#
# They bought 3rd class tickets for the three of them for 8 British Pounds each.
# (They did not get cabins with their 3rd class tickets.)
#
# They embarked in Southampton.
#
# Sue was only 4 at the time, and Kate was 20. 
#
# Sue survived but Kate did not. 


#Loading packages

library(tidyverse)	
library(psych)	
library(gridExtra)
library(readr)
library(skimr)
library(pscl)
library(lmtest) 
library(dominanceanalysis)
library(sjPlot)
library(finalfit)
library(dplyr)
library(knitr)

# Setting working directory

getwd()
setwd("C:/Users/USER/Desktop/SSDA - LUND UNIVERSITY/- COURSES/SIMM61- Quantitative Data Analysis in R/Scripts/Zoltan")

# Loading the data 

titanicdata = read_csv ("https://raw.githubusercontent.com/kekecsz/SIMM61-Course-materials/main/Home_assignment/Titanic%20-%20training%20set.csv")

# Basic descriptive and exploratory data analysis 

View(titanicdata)

titanicdata %>% 	
        summary()

describe(titanicdata)

skim(titanicdata)


# Checking for missing values before recoding

any(is.na(titanicdata)) # TRUE
sum(is.na(titanicdata)) # 866 cases in total
colSums(is.na(titanicdata)) # missing values in "Age", "Cabin" and "Embarked"

## Removing NA's from "Age"  

titanicdata <- titanicdata %>% drop_na(Age) ### 177 cases removed in total 

## Removing NA's from "Embarked"

titanicdata <- titanicdata %>% drop_na(Embarked) ### 2 cases removed in total 

# 179 passengers are removed due to missing values in the previous variables.

## We won't remove the rest of the missing cases from "Cabin" since it would 
# reduce a great amount of passengers from our database, - and we will not use
# this variable in the present exercise.

# Checking the data again

colSums(is.na(titanicdata))

skim(titanicdata)


## Variables of interest: tidying and recoding 

# To factors 

titanicdata = titanicdata %>% 	
        mutate(Sex = factor(recode(Sex,	                # Sex
                                   "1" = "male",	
                                   "0" = "female")),	
               Survived = factor(recode(Survived,	# Survived
                                  "1" = "Yes",	
                                  "0" = "No")),	
               Pclass = factor(recode(Pclass,	        # Pclass
                                   "1" = "1st",	
                                   "2" = "2nd",
                                   "3" = "3rd")),
               
               PassengerId = factor(PassengerId),       # Passenger Id
               
               Ticket = factor(Ticket),                 # Ticket
               
               Cabin = factor(Cabin),                   # Cabin 
                
               Embarked = factor(Embarked)              # Embarked 
                       )
        
## Now we have our clean database

# Checking the dataset again 

skim(titanicdata)


## Descriptive stats and visualization of the variables 

length(unique(titanicdata$PassengerId)) # 712 passengers in total 


# Our dependent variable: "Survived"

summary(titanicdata$Survived) # 288 (40%) passengers survived, 424 didn't (60%)

# Count of survivors vs deaths in total

titanicdata %>%                              	
        ggplot() +	
        aes(x = titanicdata$Survived) +	
        geom_bar()	

# Count of "Sex" of passengers in total

summary(titanicdata$Sex)    #  total males: 453 and females: 259  

titanicdata %>%                             
        ggplot() +	
        aes(x = titanicdata$Sex) +	
        geom_bar()

# Count "Pclass" of passengers in total

summary(titanicdata$Pclass)           # 1st:184 , 2nd: 173, 3rd: 355 
                                        

titanicdata %>% 	
        ggplot() +	
        aes(x = titanicdata$Pclass) +	
        geom_bar()


# "Age" distribution


titanicdata %>% 	
        ggplot() +	
        aes(x = titanicdata$Age) +	
        geom_histogram()


# Survival by "Age" of passengers

titanicdata %>% 	
        group_by(Survived) %>% 	
        summarize(mean = mean(Age),	
                  sd = sd(Age))

# Plot of survival and age by sex 

titanicdata %>% 	
        ggplot() +	
        aes(y = Age, x = Survived) +	
        geom_violin(aes(fill = Sex)) +	
        geom_boxplot(aes(color = Sex)) +	
        geom_jitter(width = 0.1, aes(color = Sex))



### Model 1: "Sex" as the only predictor for "Survived"
# Logistic regression

## For males

model1 = glm(Survived ~ Sex, 	
             family = binomial(), data = titanicdata)	

summary(model1)


predict(model1, newdata= data.frame(Sex = "male")) 

# Regression equation 

1.11 + (-2.47)*1 # -1.36 is the prediction for Y, the negative sign means that survival of males is less likely to occur. 

# Log odds

exp(-1.36) # 0.26 # natural logarithm of odds of the event of surviving as a male


# Probability 

(exp(-1.36) / (1 + exp(-1.36)))* 100 # 20% of probabilities of surviving if you are a male

## For females

predict(model1, newdata= data.frame(Sex = "female")) 

1.11 + (-2.47)*0 # 1.11 is the prediction for Y, meaning that survival of females is more likely to occur. 

exp(1.11) # 3.03 natural logarithm of odds of the event of surviving as a female

(exp(1.11) / (1 + exp(1.11)))* 100 # 75% of probabilities of surviving if you are a female


#---------------------


# FINAL MODEL



### Kate's survival probability 
#
# Without Leonardo


model2 = glm(Survived ~ Age + Sex + Pclass + SibSp + Parch, 	
             family = binomial(), data = titanicdata)


summary(model2)


kate <- 4.34 + (-0.04)*20 + (-2.63)*0 + (-1.40)*0 + (-2.64)*1 + (-0.36)*0 + (-0.03)*1

exp(kate)

(exp(kate) / (1 + exp(kate)))* 100     # 70% of Kate surviving without Leonardo



## With Leonardo 

kateL <- 4.34 + (-0.04)*20 + (-2.63)*0 + (-1.40)*0 + (-2.64)*1 + (-0.36)*1 + (-0.03)*1

exp(kateL)

(exp(kateL) / (1 + exp(kateL)))* 100   # 62% of Kate surviving with Leonardo


### Sue's survival probability 
#
# without Leonardo

sue <- 4.34 + (-0.04)*4 + (-2.63)*0 + (-1.40)*0 + (-2.64)*1 + (-0.36)*0 + (-0.03)*1

exp(sue)

(exp(sue) / (1 + exp(sue)))* 100     # 82% of Sue surviving without Leonardo


#
# with Leonardo 


sueL <- 4.34 + (-0.04)*4 + (-2.63)*0 + (-1.40)*0 + (-2.64)*1 + (-0.36)*0 + (-0.03)*2

exp(sueL)

(exp(sueL) / (1 + exp(sueL)))* 100     # 81% of Sue surviving with Leonardo


#################################


##### MODEL PERFORMANCE

## Goodness of fit

# Since R^2 is not an appropriate index for logistic regression
# We will use pseudo R-squared methods to assess the the proportion of explained 
# variance. In this case we will use the McFadden R^2 index.

pR2(model2)     #	log likelihood of the model.


# -2LL, deviance	

pR2(model2)["llh"] * -2	  # **-2 Log Likelihood (-2LL)**


# Comparison with the null model 

modelNULL = glm(Survived ~ 1, 	
                family = binomial(), data = titanicdata)	

summary(modelNULL)

pR2(modelNULL)

pR2(modelNULL)["llh"] * -2


# Our model:  llh -318 , -2llh 634
# Null model: llh -480 , -2llh 960

# Our model, after accounting for all the variance (explained by the predictors
# included) has a much smaller total error left (sums of differences in probability
# between the predicted outcome and the actual outcome for each observation). 


## Effectiveness: prediction accuracy for categorization	


# Predicted log odds for all passengers


titanicdata_p = titanicdata %>% 
        mutate(log_odds = predict(model2))

# Predicted probabilities for all passengers

titanicdata_p = titanicdata_p %>% 
        mutate(probs = exp(predict(model2)) / (1 + exp(predict(model2)))) %>% 
        mutate(probs = probs *100 )

hist(titanicdata_p$probs)

# Determining cutoffs for predictions on survival


titanicdata_pa = titanicdata_p %>% 	
        mutate(pr_mod2 = exp(predict(model2)) / (1 + exp(predict(model2)))) %>% 
        mutate(pr_mod2 = pr_mod2*100 ) %>% 
        mutate(pr_mod2 = case_when(probs <= 50 ~ "No",	
                                   probs > 50 ~ "Yes"))




# Model comparison with to see how many times did our model correctly categorized
# the observations the for survival status. 


# Coding correct guesses to assess how frequently our model classified correctly 
# the cases.


titanicdata1 = titanicdata_pa %>%	
        mutate(correct_prediction = case_when(pr_mod2 == Survived ~ "correct",	
                                              pr_mod2 != Survived ~ "incorrect"))




titanicdata1 %>%	
        group_by(correct_prediction) %>%	
        summarise(count = n()) %>%	
        mutate(freq = count / sum(count))	



# The results indicate that our model has a total correct 
# prediction for 573 correct cases or a 80% and 139 incorrect cases or a 20% of 
# total error. 



# Categorizing correct predictions for survivors  

titanicdata1 %>%	
        filter(Survived == "Yes") %>% 
        group_by(correct_prediction) %>%	
        summarise(count = n()) %>%	
        mutate(freq = count / sum(count))  

# 210 out of 288 actual survivors were correct predictions with our model, this 
# is to say that ~ 73% is the total correct prediction percentage for survivors. 




# Categorizing correct predictions for the deceased.  

titanicdata1 %>%
        filter(Survived == "No") %>% 
        group_by(correct_prediction) %>%	
        summarise(count = n()) %>%	
        mutate(freq = count / sum(count)) 


# 363 our of 424 actual deaths were correct predictions with our model, this 
# is to say that ~ 86% is the total correct prediction percentage for deaths.




# Comparing our final model with the null model: which is significantly better to
# answer the court case question on Leonardo's presence affecting Sue's and Kate's
# chances of survival. 



# Likelihood ratio test

lrtest(modelNULL, model2) # The models are significantly different from each other 
# in terms of prediction accuracy.The model with 
# predictors is significantly better.	


# Akaike Information Criterion (AIC)

AIC(modelNULL, model2)	# Our model has an AIC of more than 2 points lower 
# than the Null model, which makes it significantly better
# at the prediction accuracy. 


# Estimates in our model 


summary(model2)	# not in the scale of the outcome variable

confint(model2)	



## Contributions of predictors in the model



# Age, Sex, Pclass and SibSp show significant p-values and according to our 
# confidence intervals and these are different from zero so they add predictive 
# value to the model.



# Dominance analysis: what is the relative contribution of predictors to our model?	



dom_model2<-dominanceAnalysis(model2)	

contributionByLevel(dom_model2, fit.functions="r2.m")


plot(dom_model2, which.graph ="conditional",fit.function = "r2.m")	

averageContribution(dom_model2,fit.functions = "r2.m")	

plot(dom_model2, which.graph ="general",fit.function = "r2.m") + coord_flip()	

# Sex, Pclass and Sex are the most influential predictors in our model. 


library(modelsummary) # 

msummary(model2, stars = TRUE)


coef_table <- tab_model(model2, show.ci = 0.95, show.se =  FALSE, show.std = TRUE, show.stat = TRUE,show.est = TRUE, 
          show.aic = TRUE,show.dev = TRUE, 
          show.loglik=TRUE, p.style = c("numeric_stars"))


coef_table



#  ________________________________ #	



