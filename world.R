# INDIVIDUAL PAPER - MULTIPLE LINEAR REGRESSION: WORLD VALUE SURVEY DATA 
# Luis Felipe Villota Macías 


# Loading packages

library(tidyverse)
library(psych)
library(haven)
library(readr)
library(skimr)
library(sjPlot)
library(olsrr)
library(ggplot2)
library(forcats)
library(Amelia)
library(naniar)
library(car)
library(likert)
library(GGally)
library(ggcorrplot)
library(GGally) 	
library(corrr) 	
library(ggcorrplot) 
library(FactoMineR) 	
library(factoextra)	
library(paran) 	
library(car)	
library(GPArotation) 	
library(MVN) 
library(ICS) 
library(corrplot)
library(devtools)

# Setting working directory

getwd()
setwd("C:/Users/USER/Desktop/SSDA - LUND UNIVERSITY/- COURSES/SIMM61- Quantitative Data Analysis in R/Scripts/Anna")
getwd()

# Loading the data from the working directory

load("WVS7.R")


# Basic exploratory data analysis

dim(WVS_Cross_National_Wave_7_v2_0) # 76897 observations, 548 variables
describe(WVS_Cross_National_Wave_7_v2_0)
skim(WVS_Cross_National_Wave_7_v2_0)
summary(WVS_Cross_National_Wave_7_v2_0)
str(WVS_Cross_National_Wave_7_v2_0)



# Checking for missing values of the raw data before recoding

any(is.na(WVS_Cross_National_Wave_7_v2_0)) # TRUE
sum(is.na(WVS_Cross_National_Wave_7_v2_0)) # 1,364,682 cases in total
colSums(is.na(WVS_Cross_National_Wave_7_v2_0)) 
summary(is.na(WVS_Cross_National_Wave_7_v2_0))
pct_miss_case(WVS_Cross_National_Wave_7_v2_0) # 65% of cases that contain a missing or incomplete value 
pct_miss_var(WVS_Cross_National_Wave_7_v2_0) # 12% of variables that contain NA's

# We plan to remove missing values after the data tidying phase.
       

# Raw data into an object

WVS7 <- WVS_Cross_National_Wave_7_v2_0


# New database with selected variables.We put everything as factors because we need to drop undesired levels.

WVS7_SELECTION = WVS7 %>% 
        
        mutate( age = as_factor(Q262),            # Age
                gender = as_factor(Q260),         # Gender
                country = as_factor(B_COUNTRY),   # Country
                edu = as_factor(Q275),            # Level of education
                income = as_factor(Q288R),        # Income level
                employ = as_factor(Q279),         # Employment status
                infnp = as_factor(Q201),          # Info source: Daily newspaper
                inftv = as_factor(Q202),          # Info source: TV news
                infrad= as_factor(Q203),          # Info source: Radio news
                infmob= as_factor(Q204),          # Info source: Mobile phone
                infeml = as_factor(Q205),         # Info source: Email
                infnet = as_factor(Q206),         # Info source: Internet
                infsoc = as_factor(Q207),         # Info source: Social media
                inftal = as_factor(Q208))%>%      # Info source: Friends
        
        
        
        select( age,gender,country,edu,
                income,employ,infnp,inftv, 
                infrad, infmob, infeml, infnet,infsoc, inftal
        ) 

# Look at the data 

dim(WVS7_SELECTION) # 76897 obs, 14 vars
View(WVS7_SELECTION)
str(WVS7_SELECTION)




### Recoding variables ###

# Age: we remove negative values from the scale because they represent Na´s in the questionnaire and 
# transform variable into a numeric one. 

WVS7_SELECTION$age <- droplevels(WVS7_SELECTION$age, exclude = c("-5","-2")) 

WVS7_SELECTION = WVS7_SELECTION %>% mutate(age = as.numeric(as.character(age)))

# Gender: two (2) sexes

WVS7_SELECTION$gender <- fct_recode(WVS7_SELECTION$gender, Male = "1", Female ="2") %>% 
        droplevels(WVS7_SELECTION$gender, except = c("Male", 
                                               "Female"))

# Countries:

length(unique(WVS7_SELECTION$country)) # 51 unique cases 

WVS7_SELECTION$country <- fct_recode(WVS7_SELECTION$country, Andorra = "20", Macao_SAR_PRC ="446",
                               Argentina = "32", Malaysia = "458", Australia= "36",
                               Mexico = "484", Bangladesh = "50", Myanmar = "104",
                               Bolivia ="68", New_Zealand = "554", 
                               Brazil ="76", Nicaragua= "558",
                               Chile = "152", Nigeria = "566",
                               China = "156", Pakistan ="586",
                               Colombia = "170", Peru = "604",
                               Cyprus = "196", Philippines="608",
                               Ecuador = "218", Puerto_Rico ="630",
                               Egypt="818", Romania = "642",
                               Ethiopia = "231", Russia= "643",
                               Germany="276", Serbia="688",
                               Greece="300", South_Korea ="410",
                               Guatemala="320",  Taiwan_ROC="158", 
                               Hong_Kong_SAR_PRC ="344", Tajikistan="762", 
                               Indonesia="360", Thailand="764",
                               Iran="364", Tunisia="788", 
                               Iraq="368", Turkey= "792",
                               Japan="392", Ukraine="804",
                               Jordan="400", United_States="840",
                               Kazakhstan="398", Vietnam="704",
                               Kyrgyzstan="417", Zimbabwe="716",
                               Lebanon="422", Canada ="124", Singapore= "702")



# Note: In the WVS7 Codebook/Variables Report, the B_COUNTRY variable
# does not include  "124" and "702", although they are part of dataset actually. 
# We include them according to the ISO 3166-1 numeric country codes as Canada
# and Singapore, respectively.





# Level of education: levels from 0 to 8 according to ISCED 2011

WVS7_SELECTION$edu <- droplevels(WVS7_SELECTION$edu, exclude = c("-5","-2","-1"))


WVS7_SELECTION = WVS7_SELECTION %>% mutate(edu = as_factor(as.character(edu)))


# Reduce the number of categories in education.

WVS7_SELECTION <- WVS7_SELECTION %>% 
        mutate(edu = fct_collapse(edu, 
                                  High_edu = c("6", "7", "8"),
                                  Mid_edu = c("3","4","5"), 
                                  Low_edu = c("0","1","2")))

# Income level: we recode the 3 categories

WVS7_SELECTION$income <- fct_recode(WVS7_SELECTION$income, Low_in = "1", Medium_in ="2", High_in = "3") %>% 
        droplevels(WVS7_SELECTION$income, exclude = c("-5","-2","-1"))



# Employment status 




WVS7_SELECTION <- WVS7_SELECTION %>% 
        mutate(employ = fct_collapse(employ, 
                                     Employed = c("1", "2", "3"), 
                                     Other = c("4", "5", "6", "8"), 
                                     Unemployed = c("7")))
        
        
WVS7_SELECTION$employ <- droplevels(WVS7_SELECTION$employ, exclude = c("-5","-2","-1"))
               

#__________________________

## DEPENDENT VARIABLE: Index of frequency of usage information sources

# 8 items: 
# Q201,         # Info source: Daily newspaper
# Q202,         # Info source: TV news
# Q203,         # Info source: Radio news
# Q204,         # Info source: Mobile phone
# Q205,         # Info source: Email
# Q206,         # Info source: Internet
# Q207,         # Info source: Social media
# Q208,         # Info source: Friends

# They have the same scale and direction: 1 (Daily usage) to 5 (Never).

# Originally, higher values represent less frequent obtention of info from all sources.



# We remove negative values from the scale because they represent Na´s

WVS7_SELECTION$infnp <- droplevels(WVS7_SELECTION$infnp, exclude = c("-1", # Don´t know
                                                         "-2", # No answer
                                                         "-3", # Not applicable
                                                         "-4", # Not asked
                                                         "-5")) # Missing; Not available))

WVS7_SELECTION$inftv  <- droplevels(WVS7_SELECTION$inftv,  exclude = c("-1","-2","-3","-4","-5"))
WVS7_SELECTION$infrad <- droplevels(WVS7_SELECTION$infrad, exclude = c("-1","-2","-3","-4","-5"))
WVS7_SELECTION$infmob <- droplevels(WVS7_SELECTION$infmob, exclude = c("-1","-2","-3","-4","-5"))
WVS7_SELECTION$infeml <- droplevels(WVS7_SELECTION$infeml, exclude = c("-1","-2","-3","-4","-5"))
WVS7_SELECTION$infnet <- droplevels(WVS7_SELECTION$infnet, exclude = c("-1","-2","-3","-4","-5"))
WVS7_SELECTION$infsoc <- droplevels(WVS7_SELECTION$infsoc, exclude = c("-1","-2","-3","-4","-5"))
WVS7_SELECTION$inftal <- droplevels(WVS7_SELECTION$inftal, exclude = c("-1","-2","-3","-4","-5"))


# We transform theses variables into numeric 

WVS7_SELECTION <- WVS7_SELECTION %>%
        
        mutate(
                infnp = as.numeric(infnp),
                inftv= as.numeric(inftv), 
                infrad= as.numeric(infrad),
                infmob= as.numeric(infmob), 
                infeml= as.numeric(infeml), 
                infnet= as.numeric(infnet),
                infsoc= as.numeric(infsoc), 
                inftal= as.numeric(inftal)
        )    


# We create the index for the focal independent variable with a scale from from 8 to 40. 
# We reverse the scale in order to have higher values meaning more frequent 
# usage of information sources.

WVS7_SELECTION <- WVS7_SELECTION %>%
        mutate(yindex = 48 - (infnp + inftv + infrad + infmob + infeml + infnet + infsoc + inftal))



## Reliability Analysis of Index (Focal independent variable)

an_yindex <- data.frame(WVS7_SELECTION$infnp, 
                        WVS7_SELECTION$inftv, 
                        WVS7_SELECTION$infrad, 
                        WVS7_SELECTION$infmob, 
                        WVS7_SELECTION$infeml,
                        WVS7_SELECTION$infnet,
                        WVS7_SELECTION$infsoc,
                        WVS7_SELECTION$inftal)

alph_yindex <- alpha(an_yindex)
summary(alph_yindex)



#----------------------------------------------


# Since we recoded, renamed, collapsed and dropped various levels in our variables of interest
#(understood as NA's in the questionnare) we need to remove incomplete cases. 

dim(WVS7_SELECTION)
any(is.na(WVS7_SELECTION)) # TRUE
sum(is.na(WVS7_SELECTION)) # 23051 cases in total
colSums(is.na(WVS7_SELECTION)) 
summary(is.na(WVS7_SELECTION))
pct_miss_case(WVS7_SELECTION) # 9.93% of cases that contain a missing or incomplete value 
pct_miss_var(WVS7_SELECTION) # 93.75% of variables that contain NA's


# we proceed to remove all NA's in our selected dataset

WVS7_CLEAN = WVS7_SELECTION %>% drop_na()

dim(WVS7_CLEAN)

# ------------------------



## Correlations: between age and frequencies in obtainment of info (by media) 

age_medias = WVS7_CLEAN %>% dplyr::select(age, infrad, inftal, infsoc, infnet, infeml, infmob, inftv, infnp)


cor_age_medias <- age_medias %>% # correlations between items
        cor()

cor_age_medias

# visualization of correlation of items

ggcorr(cor_age_medias) 


ggcorrplot(cor(age_medias), p.mat = cor_pmat(age_medias), 
           hc.order = TRUE, type = "lower")


cor(age_medias) %>%
        network_plot(min_cor = 0.1)   


palette = colorRampPalette(c("green", "white", "red")) (20)  # heatmap
heatmap(x = cor_age_medias, col = palette, symm = TRUE)

corrplot(cor_age_medias)                                 # correlogram



# Distribution of focal variables


# DEPENDENT YINDEX: Index of frequency of usage information sources


### Plotting focal Y
ggplot(WVS7_CLEAN, aes(yindex)) + geom_bar() + theme_minimal() + 
        ggtitle("Focal Dependent Variable: Frequency in the obtainment of 
                information (all sources)")


# Summary stats for focal Y 
summary(WVS7_CLEAN$yindex)

skim(WVS7_CLEAN$yindex)


# age 

### Plotting focal X
ggplot(WVS7_CLEAN, aes(age)) + geom_bar() + theme_minimal() + 
        ggtitle("Focal Independent Variable: Age of respondents")


# Summary stats for focal X 
summary(WVS7_CLEAN$age)

skim(WVS7_CLEAN$age)


##----------------

### BIVARIATE MODEL ### 

model.null <- lm(yindex~ 1,  
                 data = WVS7_CLEAN)
summary(model.null)     # The estimate for the intercept is the mean = 25.8 
confint(model.null)

model.1 <- lm(yindex~ age,  
              data = WVS7_CLEAN)

summary(model.1)
confint(model.1)
plot(model.1)

anova(model.null, model.1)

scatterplot(WVS7_CLEAN$age,WVS7_CLEAN$yindex)


ggplot(data = WVS7_CLEAN, aes(WVS7_CLEAN$age, WVS7_CLEAN$yindex)) +
        geom_jitter(na.rm=TRUE, width = 8, height = 15, alpha=0.2, size=0.29) +
        xlab("Age of respondents") + 
        ylab("Frequency in the obtainment of 
                information") + 
        coord_equal()+
        geom_smooth(method=lm)


#------------------

### EXPANDED MODEL: ### 
## Introduction of the "third variable": exclusionary 


model.2 <- lm(yindex ~ age + edu,
              data = WVS7_CLEAN)

summary(model.2)



### Inclusive of more explanatory variables

model.3 <- lm(yindex ~ age + 
                      gender + 
                      edu + 
                      income+
                      employ,
              data = WVS7_CLEAN)

summary(model.3)



### Final model with interaction effects 

model.4 <- lm(yindex ~ age + 
                      gender + 
                      edu + 
                      income +
                      employ+
                      age*edu, # interaction effect
              data = WVS7_CLEAN)


summary(model.4)


plot(model.4)


plot(model.4, which = 5)


## Assumptions

# Highest numbers of Cook's D
cd <- cooks.distance(model.4)
sort(cd, decreasing = TRUE) %>% head()



# Linearity
plot(model.4, which = 1)

# Homogeneity of variance

plot(model.4, which = 3)

# Normality of the residuals
plot(model.4, which = 2)


# Multicollinearity

vif(model.4)


# Visualization 

library(visreg)

visreg(model.3, "age", # focal independent variable
       ylab = "Frequency in the obtainment of information(all sources)", 
       xlab = "Age of respondents",
       gg = TRUE, # ggplot
       band = TRUE) + 
        theme_classic() + 
        ggtitle("Final model")

# Model with interaction
visreg(model.4, "age", by = "edu", 
       overlay = TRUE,
       ylab = "Frequency in the obtainment of information(all sources)", 
       xlab = "Age of respondents",
       legend = FALSE,
       gg = TRUE, 
       band = FALSE) + 
        theme_classic() + 
        ggtitle("Final model: The interaction effect of age and education")

# Model summary

library(modelsummary)
msummary(list(model.1, model.2, model.3, model.4), # Our 4 models
         stars = TRUE)




