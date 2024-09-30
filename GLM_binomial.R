# trying to use predict choice of the product (logit) with GLM package

# Questions: 

# 1. I thought it would be our goal to run a Multinominal Model with the Alternative Chosen being our Y (categorical), 
# or do you think a logit with Choice or Non Choice (Binomial ) is better suited?

# 2. loss of information 
#don't know how if the GLM package is perfect for our data structure. 
#With this i mean that if GLM is used for a logit model, it usually has a wide format indicating information about if  
#a participant has chosen a specific product or not. Other then for the mlogit there is no possibility to give the information 
#which choices are made by the same individual (see chid in the long Format data) and thus, no information on the not
#chose alternative is given. 
#I could, theoretically just use every decision (choice or non choice) as a case (meaning that i treat each decision as 
#an individual). 
#However, in this case, information the choice behavior of the individual would be lost. 
#Furthermore, compared to mlogit, there is no possibility to define  which variables are alternative specific.

# run glm logit model with choice being our Y and each row is treated a case 

library(readxl)
MILK_R_long <- read_excel("D:/S2F/Strenght2Food 8.3/Scibo Hochladen/Meeting_131222/Long Format/MILK_R_long.xlsx", 
                          sheet = "No_NA_Milk_chid")
View(MILK_R_long)

#check data structure 
library(glm2)
str(MILK_R_long)

library(dplyr)
MILK_R_long <- MILK_R_long%>%mutate(CHOICE = as.factor(CHOICE))
MILK_R_long <- MILK_R_long%>%mutate(INT_RECOG = as.factor(INT_RECOG))
MILK_R_long <- MILK_R_long%>%mutate(GEN = as.factor(GEN))
MILK_R_long <- MILK_R_long%>%mutate(RESP = as.factor(RESP))
str(MILK_R_long)

#standardize continuous variables (but i guess thats not nessecary as results are the same)
MILK_R_long_rescale <- MILK_R_long%>% mutate_at(c("INCOME_DE", "PRICE","EDU"), ~(scale(.) %>% as.vector))
str(MILK_R_long_rescale)
head(MILK_R_long_rescale)

#Modeling 
model.glm.1<-glm(CHOICE~INCOME_DE+AGE+EDU+PRICE,family = binomial,data=MILK_R_long)
summary(model.glm.1) #the model is not a good fit 

# I would be glad if we could disscuss if such a model is suitable for our data


# Just for your information, I found an article which summaries packages and model that might be useful for us.
# https://www.msarrias.com/uploads/3/7/7/8/37783629/gmnl_v2_vignette.pdf (page 2 and 3)
#i also did some further research and made this list of packages to consider: 
Pglm 
lme4
glmer
gmnl
Rchoice
nnet (multinom)