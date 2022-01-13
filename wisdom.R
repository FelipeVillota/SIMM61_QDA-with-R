# LAB ASSIGNMENT 2 - MIXED LINEAR MODELS: WISDOM TOOTH SURGERY DATA 
# Luis Felipe Villota Macías 

# RQ: 
# What factors influence postoperative pain after wisdom tooth surgery?

#Loading packages

library(tidyverse)	
library(psych)	
library(readr)
library(skimr)
library(cAIC4) 
library(r2glmm) 
library(lme4) 
library(lmerTest) 
library(MuMIn)
library(sjPlot)
library(r2glmm)
library(influence.ME)
library(lattice)
library(stargazer)
library(sandwich)
library(gridExtra)

# Setting working directory

getwd()
setwd("C:/Users/USER/Desktop/SSDA - LUND UNIVERSITY/- COURSES/SIMM61- Quantitative Data Analysis in R/Scripts/Zoltan")

# Loading the data 

Dataset_A = read_csv("https://raw.githubusercontent.com/kekecsz/SIMM61-Course-materials/main/Home_assignment/surgery_data_1.csv")

Dataset_B = read_csv("https://raw.githubusercontent.com/kekecsz/SIMM61-Course-materials/main/Home_assignment/surgery_data_2.csv") 


# Custom function from the assingment description

stdCoef.merMod <- function(object) {
        sdy <- sd(getME(object, "y"))
        sdx <- apply(getME(object, "X"), 2, sd)
        sc <- fixef(object) * sdx/sdy
        se.fixef <- coef(summary(object))[, "Std. Error"]
        se <- se.fixef * sdx/sdy
        return(data.frame(stdcoef = sc, stdse = se))
}



### EDA

# Dataset A
skim(Dataset_A)
describe(Dataset_A)

any(is.na(Dataset_A)) # No missing values 

Dataset_A %>% 	           # plotting linear model of pain and age	
             ggplot() +		
             aes(y = pain, x = age) +		
             geom_point(aes(color = hospital), size = 4) +		
             geom_smooth(method = "lm", se = F)

# Dataset B

skim(Dataset_B)
describe(Dataset_B)

any(is.na(Dataset_B)) # No missing values 

Dataset_A %>% 	           # plotting linear model of pain and age	
        ggplot() +		
        aes(y = pain, x = age) +		
        geom_point(aes(color = hospital), size = 4) +		
        geom_smooth(method = "lm", se = F)



### Cleaning the data


# Changing characters into factors in Dataset A and B: ID, sex, hospital

Dataset_A <- Dataset_A %>% mutate(ID=as_factor(ID),
                                  sex=as_factor(sex),
                                  hospital= as_factor(hospital))

Dataset_B <- Dataset_B %>% mutate(ID=as_factor(ID),
                                  sex=as_factor(sex),
                                  hospital= as_factor(hospital))



# We identify in Dataset A a level called "woman" in the "Sex" variable and we collapse 
# it into "female"  

Dataset_A <- Dataset_A %>% 
        mutate(sex = fct_collapse(sex, 
                                      male = c("male"), 
                                      female = c("female", "woman"))) 
               
# We have now 97 females and 103 males for a total of 200 patients in Dataset A.               




#---- As an exploration
# Simple regression model: Explaining variation of pain with age by hospital 

painage_plot = Dataset_A %>%
        ggplot() + aes(y = pain, x = age) +
        geom_point(aes(color=hospital)) + geom_smooth(method = "lm", se = F,   # There's a negative  relationship between age and pain.
                                           fullrange = TRUE)
painage_plot


painage_plot + xlim(-1, 50) + geom_hline(yintercept = 0) + geom_vline(xintercept = 0)



slp_plot = Dataset_A %>%
        ggplot() + aes(y = pain, x = age, color = hospital) +
        geom_point(size = 4) + geom_smooth(method = "lm", se = F,
                                           fullrange = TRUE) + xlim(-1, 50) + geom_hline(yintercept = 0) +
        geom_vline(xintercept = 0)

slp_plot
#----


### Linear mixed model: 

# estimating postoperative pain on Dataset A with (6 fixed
# effect predictors): "age", "sex", "STAI_trait", "pain_cat", "mindfulness", "cortisol_serum"
# This model also has to account for the clusters in the different hospitals. 

# After an exploratory data analysis and visualization we saw that data points (by colors) were 
# grouped or clustered on the scatterplots. This strongly suggests that group membership
# (in this case hospital of treatment) can explain variability in the outcome variable "pain", 
# and therefore some observations might not be independent from each other. Therefore, 
# in our model, "hospital" will be the random effect predictor as it seems to be a key 
# element to take into account in order to make accurate predictions on 
# postoperative pain on this dataset and outside it. 



### Models in DATASET A


# Null model 

Null_model = lm(pain~1 , Dataset_A)

Null_model

summary(Null_model)

### random intercept model ---> a separate regression line for all clustering levels, but same slope for all regression lines
# Here we suspect that hospital membership has no interaction with the fixed effect predictors. 

Rint_modA = lmer(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + (1 | hospital), data = Dataset_A)

Rint_modA 

summary(Rint_modA) # cortisol_serum is the most influential predictor	

### random slope model ---> a separate regression line for all clustering levels with no restriction for the slopes or the intercept of all regression lines
# Here we suspect that hospital membership has effect on pain and interacts with the fixed predictors.

Rslo_modA = lmer(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + (cortisol_serum | hospital), data = Dataset_A)

Rslo_modA 

summary(Rslo_modA)

# random slope model with Nelder_Mead optimizer	---> in order to decrease the complexity of the model

Rslo_modA_NM  = lmer(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + (cortisol_serum | hospital), control = lmerControl(optimizer = "Nelder_Mead"),
                       data = Dataset_A)

Rslo_modA_NM 

summary(Rslo_modA_NM) 



## Model comparison and deciding which one to use based on model fit

# Saving predictions
Dataset_A = Dataset_A %>%
        mutate(pr_int = predict(Rint_modA), pr_slo = predict(Rslo_modA_NM))

# Plot of the random intercept model 

Dataset_A %>%
        ggplot() + aes(y = pain, x = age , group = hospital) +
        geom_point(aes(color = hospital), size = 4) + geom_line(color = "red",
        aes(y = pr_int, x = age)) + facet_wrap(~hospital, ncol = 2)

# Plot of the random slope model 

Dataset_A %>%
        ggplot() + aes(y = pain, x = age , group = hospital) +
        geom_point(aes(color = hospital), size = 4) + geom_line(color = "red",
        aes(y = pr_slo, x = age)) + facet_wrap(~hospital, ncol = 2)


# Sum of residuals 

sum(residuals(Null_model)^2) # 440.72

sum(residuals(Rint_modA)^2) # 224.31

sum(residuals(Rslo_modA_NM)^2) # 217.34, has the smallest RSS ***


# Model fit indices

# Conditional AIC 

cAIC(Null_model)$caic # Null   729.59

cAIC(Rint_modA)$caic  # Intercept ---> is the better fit    621.42 ***

cAIC(Rslo_modA_NM)$caic  # Random slope model      623.73


# Anova
anova(Rint_modA, Rslo_modA_NM)  # AIC int: 629.12 ***, slope: 631.15



# Although the random slope model had the smallest RSS (217.34 against 224.31 of the random intercept model), in the cAIC and AIC indices 
# the random intercept model is smaller by less than 2, so it seems to be better fit.



### Variance explained in Dataset A
        
        
# Fixed effects: Marginal R2 

# r2beta --> variance for the fixed effects alone, significance can be interpreted from CI, 95% does not contain 0

r2beta(Rint_modA, method = "nsj", data = Dataset_A) # cortisol_serum accounts for 12% of the variance, it is the most influential predictor       


# Fixed effects: Marginal and Conditional R2

r.squaredGLMM(Rint_modA) # marginal 38.52% , conditional 46.32%

# Intercept and Random Slope combined model: Marginal and Conditional R2 
        
r.squaredGLMM(Rslo_modA_NM) # marginal 38.47% , conditional 47.89%


# The random intercept model proved to be a better fit according to the 
# likelihood ratio test (Chi^2 = 1.9647, df = 2, p = 0.3744) and the cAIC (cAIC 
# intercept = 629.12, cAIC slope = 631.15). 

# The results of the random intercept model:
 
summary(Rint_modA)
confint(Rint_modA) # Confidence intervals for the model coefficients
stdCoef.merMod(Rint_modA) # Standardized beta for each predictor

tab_model(Rint_modA, Rslo_modA_NM)

        
### Prediction of "pain" in DATASET B with our model of Dataset A



prediction_B <- predict(Rint_modA, newdata= Dataset_B, allow.new.levels = TRUE) 

prediction_B


# ----------

### Variance explained in Dataset B


RSS <- sum((Dataset_B$pain-prediction_B)^2) # 307.3396
RSS



TSS= sum((Dataset_B$pain- predict(Null_model))^2)  # 495.68
TSS


1-(RSS/TSS) # 0.3799


# COMPARISON 

# 37.99% in Dataset B and Marginal R^2 38.52% and Conditional R^2 46.32% in Dataset A.


        
### New model having only the most influential predictor (cortisol_serum) with the (random slope + intercept model)

New_model = lmer(pain ~ cortisol_serum  + ( cortisol_serum|hospital),data = Dataset_A)

summary(New_model)


New_model_opt  = lmer(pain ~ cortisol_serum + (cortisol_serum | hospital), control = lmerControl(optimizer = "Nelder_Mead"),
                      data = Dataset_A)


summary(New_model_opt)

tab_model(New_model,New_model_opt, dv.labels = c("cor_rslint", "cor_rslint_opt"))


## Visualizing (cortisol_serum only predictor): fitted regression lines for each hospital (random + intercept) 


slint_pain_predsA <- predict(New_model_opt, newdata= Dataset_A, allow.new.levels=TRUE)


cortplot1 <- ggplot(Dataset_A, aes(y = pain, x = cortisol_serum,
                                 group = hospital)) + geom_point(size = 3) + geom_line(color = "red",
                                 aes(y = slint_pain_predsA , x = cortisol_serum)) + facet_wrap(~hospital, ncol = 5) 

cortplot1


# Answers


#1. ) The R^2 obtained in Dataset B was 0.3799 (or 37.99% of explained variance in pain) 
# and was closer to the Marginal R^2 (38.52%) in Dataset A, rather than to the 
# Conditional R^2 in Dataset A (46.32%). This is due to the fact the we 
# were applying the model that takes into account the fixed effects and not the random effects.
# Marginal R^2 explains for fixed effects only. Conditional R^2 explains the variance 
# of the combined model (random slope and random intercept).


# 2.) Based on the graph of the new model in which cortisol_serum is the only predictor, 
# the difference between the random intercept model and the random slope (which includes the
# random intercept) is hardly noticeable. Nevertheless, we compared model fit indices for both
# and we found (Chi^2 = 2.15, df = 2, p < 0.3411, AIC of the random slope model = 674.35, 
# AIC of random intercept model = 672.50). So the random slope model has an AIC 
# of less than 2 smaller than the one of the random intercept model, and it is 
# not at a significant level. Nevertheless, we also found that for the cAIC we do have a (661.37 for the random slope model)      
# and (664.5371 for the random intercept model), making the random slope model slightly better fit.

# Yet, for our data, it was is not fruitful to expect different slopes of cortisol_serum for each hospital, 
# because we assumed that hospitals had effect on pain but no on cortisol_serum. In conclusion, 
# the random intercept model is a better fit for this data at explaining variation of pain in Dataset A. 




### New model having only the most influential predictor (cortisol_serum) with the (intercept model)

intnouvelle = lmer(pain ~ cortisol_serum + (1 | hospital),
                       data = Dataset_A)

intnouvelle 

summary(intnouvelle)



int_pain_predsA<- predict(intnouvelle, newdata= Dataset_A, allow.new.levels=TRUE)


cortplot2 <- ggplot(Dataset_A, aes(y = pain, x = cortisol_serum,
                      group = hospital, color = hospital)) + geom_point(size = 3) + geom_line(color = "black",
                                                                            aes(y = int_pain_predsA , x = cortisol_serum)) + facet_wrap(~hospital, ncol = 5) 
cortplot2 # this one we keep the intercept model


grid.arrange(cortplot1,cortplot2, nrow=2)


cAIC(New_model_opt)$caic # 661.3712
cAIC(intnouvelle)$caic # 664.5371
anova(New_model_opt,intnouvelle) # intnouvelle 672.50     New_model_opt 674.35


#------



# Recap last plots

lastint_plot = Dataset_A %>%
        ggplot() + aes(y = pain, x = cortisol_serum, color = hospital) +
        geom_point(size = 4) + geom_smooth(method = "lm", se = F,
                                           fullrange = TRUE)
lastint_plot



lastslope_plot = Dataset_A %>%
        ggplot() + aes(y = pain, x = cortisol_serum, color = hospital) +
        geom_point(size = 4) + geom_smooth(method = "lm", se = F,
                                           fullrange = TRUE) + xlim(-1, 50) + geom_hline(yintercept = 0) +
        geom_vline(xintercept = 0)

lastslope_plot





