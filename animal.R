# LAB ASSIGNMENT 3 - DIMENSION REDUCTION: ANIMAL RIGHTS SCALE 
# Luis Felipe Villota Macías 

# In the present report, we apply exploratory factor analysis (EFA) to the
# elements in "The Animal Rights Scale (ARS): Measuring Attitudes About Animal Rights and Animal Research"
# database to detect which sets of latent factors (not observable) are underneath the answers of the participants.



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
library(Amelia)
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

# Custom function for visualization of PCA and EFA results. It is extracted   
# exactly as provided in "Exercise_06_PCA_and_EFA.R" by professor Zoltan:

# fviz_loadnings_with_cor() 


fviz_loadnings_with_cor <- function(mod, axes = 1, loadings_above = 0.4){	
        require(factoextra)	
        require(dplyr)	
        require(ggplot2)	
        
        
        
        if(!is.na(as.character(mod$call$call)[1])){	
                if(as.character(mod$call$call)[1] == "PCA"){	
                        contrib_and_cov = as.data.frame(rbind(mod[["var"]][["contrib"]], mod[["var"]][["cor"]]))	
                        
                        vars = rownames(mod[["var"]][["contrib"]])	
                        attribute_type = rep(c("contribution","correlation"), each = length(vars))	
                        contrib_and_cov = cbind(contrib_and_cov, attribute_type)	
                        contrib_and_cov	
                        
                        plot_data = cbind(as.data.frame(cbind(contrib_and_cov[contrib_and_cov[,"attribute_type"] == "contribution",axes], contrib_and_cov[contrib_and_cov[,"attribute_type"] == "correlation",axes])), vars)	
                        names(plot_data) = c("contribution", "correlation", "vars")	
                        
                        plot_data = plot_data %>% 	
                                mutate(correlation = round(correlation, 2))	
                        
                        plot = plot_data %>% 	
                                ggplot() +	
                                aes(x = reorder(vars, contribution), y = contribution, gradient = correlation, label = correlation)+	
                                geom_col(aes(fill = correlation)) +	
                                geom_hline(yintercept = mean(plot_data$contribution), col = "red", lty = "dashed") + scale_fill_gradient2() +	
                                xlab("variable") +	
                                coord_flip() +	
                                geom_label(color = "black", fontface = "bold", position = position_dodge(0.5))	
                        
                        
                }	
        } else if(!is.na(as.character(mod$Call)[1])){	
                
                if(as.character(mod$Call)[1] == "fa"){	
                        loadings_table = mod$loadings %>% 	
                                matrix(ncol = ncol(mod$loadings)) %>% 	
                                as_tibble() %>% 	
                                mutate(variable = mod$loadings %>% rownames()) %>% 	
                                gather(factor, loading, -variable) %>% 	
                                mutate(sign = if_else(loading >= 0, "positive", "negative"))	
                        
                        if(!is.null(loadings_above)){	
                                loadings_table[abs(loadings_table[,"loading"]) < loadings_above,"loading"] = NA	
                                loadings_table = loadings_table[!is.na(loadings_table[,"loading"]),]	
                        }	
                        
                        if(!is.null(axes)){	
                                
                                loadings_table = loadings_table %>% 	
                                        filter(factor == paste0("V",axes))	
                        }	
                        
                        
                        plot = loadings_table %>% 	
                                ggplot() +	
                                aes(y = loading %>% abs(), x = reorder(variable, abs(loading)), fill = loading, label =       round(loading, 2)) +	
                                geom_col(position = "dodge") +	
                                scale_fill_gradient2() +	
                                coord_flip() +	
                                geom_label(color = "black", fill = "white", fontface = "bold", position = position_dodge(0.5)) +	
                                facet_wrap(~factor) +	
                                labs(y = "Loading strength", x = "Variable")	
                }	
        }	
        
        
        
        
        
        
        return(plot)	
        
}	


# Setting working directory


getwd()
setwd("C:/Users/USER/Desktop/SSDA - LUND UNIVERSITY/- COURSES/SIMM61- Quantitative Data Analysis in R/Scripts/Zoltan")

# Loading the data 

animal_data = read_csv("https://raw.githubusercontent.com/kekecsz/SIMM61-Course-materials/main/Exercise_06%20-%20CFA%20and%20EFA/animalrights.csv")

# Checking the data

view(animal_data)

skim(animal_data)


# Checking for missing values before recoding

any(is.na(animal_data)) # TRUE
sum(is.na(animal_data)) # 11 cases in total
colSums(is.na(animal_data)) # missing values in "ar1", "ar2", "ar10", "ar14", "ar18"
                            #                    "ar19", "ar22", "sex", "party", "liberal"
missmap(animal_data)

# Removing all Na's

animal_data <- animal_data %>% drop_na()  # 11 cases were dropped in total 


## Variables of interest: tidying and recoding 

# To factors 

# Changing numeric variables into factors in animal_data: "sex" and "party".

animal_data <- animal_data %>% mutate(sex=as_factor(sex),
                                  party=as_factor(party))

# Recoding for "sex" and "party"

animal_data = animal_data %>% 	
               mutate(sex = factor(dplyr::recode(sex,	                # sex
                                   "1" = "female",	
                                   "2" = "male")),	
               party = factor(dplyr::recode(party,	       # party
                                        "1" = "democrat",	
                                        "2" = "republican",
                                        "3" = "other",
                                        "4" = "none")))


# Items from "ar1" to "ar28" are on a 1-5 Likert scale, and were treated as numerical 
# variables here. Same is the case for "party". 

# Recap

str(animal_data)
skim(animal_data)  # 149 observations and 31 variables in total in animal_data
summary(animal_data)
summary(animal_data$sex) # 120 females (~81%) ; 29 males (19%)
summary(animal_data$party) # democrat republican      other       none 
                           #     30         34          8         77 




# We had 11 missing cases in total in items:"ar1", "ar2", "ar10", "ar14", "ar18", "ar19", "ar22", "sex", "party", "liberal"
# All of them are removed from the database leaving us with 149 observations and 31 variables in total. 
# We highlight the remarkable amount of females in the database ~81% (120) vs 19% (29) of males, which maybe points
# to a sample bias. 
# We changed numeric variables into factors in animal_data for: "sex" and "party". We recoded "sex" with "1" = "female" and "2" = "male".
# and "party" with  "1" = "democrat", "2" = "republican", "3" = "other", "4" = "none".
# Items from "ar1" to "ar28" are on a 1-5 Likert scale, and were treated as numerical (continuous)
# variables here. Same is the case for "liberal". 
# Some items show great skewness.

# END OF EDA AND TIDYING --------------------------------


## Correlations: "liberal" as the outcome and the 28 items as the predictors


model_ars_null = lm(liberal ~ 1, data=animal_data)

summary(model_ars_null)


model_ars_items = lm(liberal ~ ar1 + ar2 + ar3 + ar4 + ar5 + ar6 + ar7 + ar8 + ar9 + ar10 + ar11 + ar12 + ar13 + 
                                ar14 + ar15 + ar16 + ar17 + ar18 + ar19 + ar20 + ar21 + ar22 + ar23 + ar24 + ar25 + 
                                ar26 + ar27 + ar28, data=animal_data)

summary(model_ars_items) 

vif_items <- vif(model_ars_items)

sort(vif_items, decreasing = TRUE)  # VIF are above 3 in ar13 and ar5 


## Correlations: between all items in the questionnaire


ars_only_items = animal_data %>% dplyr::select(ar1:ar28)  #database with only the 28 items 

cor_ars_only_items <- ars_only_items %>% # correlations between items
        cor()

cor_ars_only_items

# visualization of correlation of items

ggcorr(cor_ars_only_items) 


ggcorrplot(cor(ars_only_items), p.mat = cor_pmat(ars_only_items), 
           hc.order = TRUE, type = "lower")


cor(ars_only_items) %>%
        network_plot(min_cor = 0.6)   


palette = colorRampPalette(c("green", "white", "red")) (20)  # heatmap
heatmap(x = cor_ars_only_items, col = palette, symm = TRUE)

corrplot(cor_ars_only_items)                                 # correlogram


# These items show a strong positive correlation of 0.83:
# ar 5    It is wrong to wear leather jackets and pants. 
# ar13    It is wrong to wear leather belts and shoes. 

# END OF CORRELATIONS-----------------------------------------------------





### EFA: Checking assumptions
## Is there enough correlation between the observations in order to use EFA?


# Factorability through the Bartlett sphericity test

 
bft_factorability <- cortest.bartlett(cor_ars_only_items)

bft_factorability

# The results of the Bartlett sphericity test show a  
# Chi^2 is 1128.934, p < 0.001, df 378 for the correlation matrix of all 28 items.
# We can see that it is significantly different from the identity matrix (null-correlation matrix). 
# This, meaning that the observed variables are correlated with one another and are factorable. 
# Nevertheless, in our database the ratio of number of observations and number of observed 
# variables: 149/28 = 5.32. So it is slightly higher than the desirable 
# threshold of 5, so the test is not reliable.

# 

# To have a definitive indicator of factorability we run a Kaiser-Meyer-Olkin (KMO) test,
# to see the contrast between the partial correlation matrix and the regular correlation matrix. So, 
# in the instances where our variables have a remarkable common variance (a good indication that they are influenced by 
# the same latent factors), the partial correlations are low. Leaving us with large KMO indices for the variables.



KMO_cor_ars_only_items <- KMO(cor_ars_only_items)

KMO_cor_ars_only_items

sort(KMO_cor_ars_only_items$MSAi, decreasing = TRUE) # All observed variables (including 
                                                     # the overall MSA) have a KMO
                                                     # above the 0.6 threshold and are close to 1, 
                                                     # an indication of reasonable factorability of 
                                                     # our data. 
          



# Multivariate normality through the Henze-Zirkler's test

ars_mvn <- mvn(animal_data[, 1:28], mvnTest = "hz")

ars_mvn$multivariateNormality   # p value lower than 0.05, it is a violation of the
                                # multivariate normality assumption. 

# Multivariate Normality Test Based on Kurtosis
mvnorm.kur.test((animal_data[, 1:28]))

# Multivariate Normality Test Based on Skewness
mvnorm.skew.test((animal_data[, 1:28]))

# All MVN tests show p values lower than 0.05, so the assumption of MVN is violated. 
# We will proceed, then, with the Principal Axis Factoring extraction method.
# END OF CHECKING ASSUMPTIONS----------------------------------
                  


## Factor extraction 

EFA_1 <- fa(cor_ars_only_items, nfactors = 6, fm = "pa") # we use the paf method
                                                         # Number of factors: 6 (starting point) 

EFA_1_commun <- as.data.frame(sort(EFA_1$communality, decreasing = TRUE))

EFA_1_commun

# Communalities: highest and lowest

# The first EFA model shows that item ar5 "It is wrong to wear leather jackets and pants"
# has the highest proportion of its variance explained (95%) by the extracted factors
# in the 6-factor structure. As the least represented in this scheme, we find 
# item ar3 "It is morally wrong to drink milk and eat eggs" with ~23% of its variance
# explained by the new factors.  




# Average communality 


mean(EFA_1$communality) # 0.46 (46%) 


# Exclusion of low communality items
# According to the literature cited in the course notes, MacCallum et al. suggest
# that the average communality should meet the threshold of 0.6 when the sample size 
# is small (<250 data points). In our case, with the 6-factor structure we have 
# an average of 0.46, so we consider to exclude the (8) worst represented items
# by the extracted factors: ar3, ar16, ar22, ar25, ar28, ar1, ar18, ar8.       


# we repeat the same process but for only 20 items 


ars_only_20 = animal_data %>% dplyr::select(-c("ar3", "ar16", "ar22", "ar25",    #database with only the 20 items
                                                  "ar28", "ar1", "ar18", "ar8","sex", "party","liberal")) 

cor_ars_only_20 <- ars_only_20 %>% # correlations between 20 items
        cor()

cor_ars_only_20


EFA_2 <- fa(cor_ars_only_20, nfactors = 6, fm = "pa") # we use the paf method
                                                         # Number of factors: 6 (starting point)
EFA_2_commun <- as.data.frame(sort(EFA_2$communality, decreasing = TRUE))

EFA_2_commun

mean(EFA_2$communality) # 0.55 (55%) 

# After excluding the 8 factors with the lowest communality we still have an 
# average of 0.55. We point this as a limitation in our study (the sample size is small).  
# END OF FACTOR EXTRACTION----------------------------------



### Selection of the ideal number of factors (we go with 20 items in total)


## Scree test

pca_ars<- PCA(ars_only_20) # dim 1 (38.25%), dim2(8.09%)

fviz_screeplot(pca_ars, addlabels = TRUE, ylim = c(0, 85)) # This criteria tells us to keep 2 factors. 
                                                           # We find the last substantial break of the slope in the 3th factor.
         
summary(pca_ars)


## "error": sjt.plot function is not found                                      ### failed plot
sjt.pca(pca_ars, rotation = c("varimax", "oblimin"), nmbr.fctr = NULL,
        fctr.load.tlrn = 0.1, title = "Principal Component Analysis",
        var.labels = NULL, wrap.labels = 40, show.cronb = TRUE,
        show.msa = FALSE, show.var = FALSE, altr.row.col = FALSE, digits = 2,
        string.pov = "Proportion of Variance",
        string.cpov = "Cumulative Proportion", CSS = NULL, encoding = NULL,
        file = NULL, use.viewer = TRUE, no.output = FALSE,
        remove.spaces = TRUE)


# test with 28 items
which(names(animal_data) == "liberal") # column 31
which(names(animal_data) == "sex") # column 29
which(names(animal_data) == "party") # column 30

pca_ars_28<- PCA(animal_data, quanti.sup = 31, quali.sup = c(29, 30))

fviz_screeplot(pca_ars_28, addlabels = TRUE, ylim = c(0, 85))
                      

## Kaiser-Guttman criterion: keep elements with Eigenvalue > 1

get_eigenvalue(pca_ars) # we keep 4 factors, since the fifth is < 1 


# Parallel test: keep that which is > random data eigenvalue line on the scree plot

pca_par_20 = paran(ars_only_20, graph = TRUE)

pca_par_20$Retained # 2 factors


fa.parallel(cor_ars_only_20, n.obs = nrow(ars_only_20), fa = "fa", fm = "pa") # Parallel analysis suggests that the
                                                                        # number of factors =  3  and the number of components =  NA

# (VSS) criterion

nfactors(cor_ars_only_20, n.obs = nrow(ars_only_20)) # 2 factors


# Wayne Velicer's Minimum Average Partial (MAP) criterion 

# The Velicer MAP achieves a minimum of 0.02  with  2  factors





EFA_3 <- fa(cor_ars_only_20, nfactors = 2, fm = "pa")
EFA_3
EFA_3_commun <- as.data.frame(sort(EFA_3$communality, decreasing = TRUE))
EFA_3_commun

# ar5 = ~70% highest communality in the 2-factor structure
# ar24 = ~21% lowest

mean(EFA_3$communality) # average communality is  ~41%

## Table: code for costumized from Francisco Wilhem (adpating Anthony Smith)
# https://www.franciscowilhelm.com/post/exploratory-factor-analysis-table/
# https://www.anthonyschmidt.co/post/2020-09-27-efa-tables-in-r/


library(flextable)
library(gt)

names(EFA_3)

EFA_3_tb <- fa(cor_ars_only_20, nfactors = 2, fm = "pa", rotate = "oblimin")

fa_table <- function(x, varlabels = NULL, title = "Factor analysis results", diffuse = .10, small = .30, cross = .20, sort = TRUE) {
        #get sorted loadings
        require(dplyr)
        require(purrr)
        require(tibble)
        require(gt)
        if(sort == TRUE) {
                x <- psych::fa.sort(x)
        }
        if(!is.null(varlabels)) {
                if(length(varlabels) != nrow(x$loadings)) { warning("Number of variable labels and number of variables are unequal. Check your input!",
                                                                    call. = FALSE) }
                if(sort == TRUE) {
                        varlabels <- varlabels[x$order]
                }
        }
        if(is.null(varlabels)) {varlabels <- rownames(x$loadings)}
        
        loadings <- data.frame(unclass(x$loadings))
        
        #make names
        factornamer <- function(nfactors) {
                paste0("Factor_", 1:nfactors)}
        
        nfactors <- ncol(loadings)
        fnames <- factornamer(nfactors)
        names(loadings) <- fnames
        
        # prepare locations
        factorindex <- apply(loadings, 1, function(x) which.max(abs(x)))
        
        # adapted from from sjplot: getremovableitems
        getRemovableItems <- function(dataframe, fctr.load.tlrn = diffuse) {
                
                # clear vector
                removers <- vector(length = nrow(dataframe))
                
                # iterate each row of the data frame. each row represents
                # one item with its factor loadings
                
                for (i in seq_along(removers)) {
                        # get factor loadings for each item
                        rowval <- as.numeric(abs(dataframe[i, ]))
                        # retrieve highest loading
                        maxload <- max(rowval)
                        # retrieve 2. highest loading
                        max2load <- sort(rowval, TRUE)[2]
                        # check difference between both
                        if (abs(maxload - max2load) < fctr.load.tlrn) {
                                # if difference is below the tolerance,
                                # remeber row-ID so we can remove that items
                                # for further PCA with updated data frame
                                removers[i] <- TRUE
                        }
                }
                # return a vector with index numbers indicating which items
                # have unclear loadings
                return(removers)
        }
        if(nfactors > 1) {
                removable <- getRemovableItems(loadings)
                cross_loadings <- purrr::map2(fnames, seq_along(fnames), function(f, i) {
                        (abs(loadings[,f] > cross)) & (factorindex != i) 
                })
        }
        
        small_loadings <- purrr::map(fnames, function(f) {
                abs(loadings[,f]) < small
        })
        
        ind_table <- dplyr::tibble(varlabels, loadings) %>%
                dplyr::rename(Indicator = varlabels) %>% 
                dplyr::mutate(Communality = x$communality, Uniqueness = x$uniquenesses, Complexity = x$complexity) %>% 
                dplyr::mutate(across(starts_with("Factor"), round, 3))  %>%
                dplyr::mutate(across(c(Communality, Uniqueness, Complexity), round, 2))
        
        
        ind_table <- ind_table %>% gt(rowname_col = "Indicator") %>% tab_header(title = title)
        # mark small loadiongs
        for(f in seq_along(fnames)) {
                ind_table <- ind_table %>%  tab_style(style = cell_text(color = "#D3D3D3", style = "italic"),
                                                      locations = cells_body(columns = fnames[f], rows = small_loadings[[f]]))
        }
        # mark cross loadings
        
        if (nfactors > 1) {
                for (f in seq_along(fnames)) {
                        ind_table <-
                                ind_table %>%  tab_style(
                                        style = cell_text(style = "italic"),
                                        locations = cells_body(columns = fnames[f], rows = cross_loadings[[f]])
                                )
                }
                # mark non-assignable indicators
                ind_table <-
                        ind_table %>%  tab_style(style = cell_fill(color = "#D93B3B"),
                                                 locations = cells_body(rows = removable))
        }
        
        # adapted from https://www.anthonyschmidt.co/post/2020-09-27-efa-tables-in-r/
        Vaccounted <- x[["Vaccounted"]]
        colnames(Vaccounted) <- fnames 
        if (nfactors > 1) {
                Phi <- x[["Phi"]]
                rownames(Phi) <- fnames
                colnames(Phi) <- fnames
                f_table <- rbind(Vaccounted, Phi) %>%
                        as.data.frame() %>% 
                        rownames_to_column("Property") %>%
                        mutate(across(where(is.numeric), round, 3)) %>%
                        gt() %>% tab_header(title = "Eigenvalues, Variance Explained, and Factor Correlations for Rotated Factor Solution")
        }
        else if(nfactors == 1) {
                f_table <- rbind(Vaccounted) %>%
                        as.data.frame() %>% 
                        rownames_to_column("Property") %>%
                        mutate(across(where(is.numeric), round, 3)) %>%
                        gt() %>% tab_header(title = "Eigenvalues, Variance Explained, and Factor Correlations for Rotated Factor Solution")
        }
        
        return(list("ind_table" = ind_table, "f_table" = f_table))
        
}


tables <- fa_table(EFA_3_tb)
tables$ind_table
tables$f_table


#END OF SELECTION OF IDEAL NUMBER OF FACTORS---------------------------------- 


### Factor rotation: improving the interpretation of factors


# Orthogonal rotation: Varimax rotation  It minimizes the number of items with extreme loadings.

EFA_3_varimax <- fa(ars_only_20, nfactors = 2, fm = "pa", rotate = "varimax")
EFA_3_varimax


# Oblique rotation: Promax, Oblimin 

EFA_3$rotation # "oblimin" 


EFA_3_promax <- fa(ars_only_20, nfactors = 2, fm = "pa", rotate = "promax")
EFA_3_promax 

tables1 <- fa_table(EFA_3_promax) # promax
tables1$ind_table
tables1$f_table

# Interpretation of the factors

fa.diagram(EFA_3)

fviz_loadnings_with_cor(EFA_3, axes = 1, loadings_above = 0.4)
fviz_loadnings_with_cor(EFA_3, axes = 2, loadings_above = 0.4)


## NAMING THE FACTORS

## SAVING FACTOR SCORES


factor_scores = factor.scores(ars_only_20[,1:20], EFA_3)$scores

ars_f_scores = cbind(animal_data, factor_scores)

view(ars_f_scores)

# Which factor is the most influential for predicting "liberal"? None

model_ars_LIB = lm(liberal ~ PA1 + PA2, data=ars_f_scores)

summary(model_ars_LIB) 

vif_items_PALIB <- vif(model_ars_LIB)

sort(vif_items_PALIB, decreasing = TRUE)  # VIF are not above 3 


