#Long format data analysis 

library("mlogit")
library(readxl) 

#please import the excel file called "Milk_R_long". Please import the tab NoNa or No_Na_chid. They are the same however, in the latter i subsequently named the choice/participant id.
View(MILK_R_long)

# the Choice variable needs to be a logical vector
library(dplyr)
MILK_R_long <- MILK_R_long%>%mutate(CHOICE = as.logical(CHOICE))
head(MILK_R_long,20)

# i converted the data into a df instead of tbl_df. However, i guess i would not matter
Milk_long=as.data.frame(MILK_R_long)

#defining the mlogit.data 
mldata_long <- mlogit.data(Milk_long,choice='CHOICE',shape='long',alt.var='ALT', chid.var = "chid")
head(mldata,20)

#defining the ml model 
#no alternative specific variable
Model.1<-mlogit(CHOICE~1|INCOME_DE,data = mldata_long,reflevel = "jafull")
summary(Model.1) 

#no individual specific variable,just price as the alternative specific variable 
Model.2<-mlogit(CHOICE~PRICE,data=mldata_long,reflevel = "jafull") # Error in solve.default(H, g[!fixed]) : system is computationally singular: reciprocal condition number = 2.20987e-16

#check for multicolinarity, as this Error hints to MC Problem 
library(tidyverse)
library(caret)
library(car)

MCTestmodel <- lm(CHOICE ~ GEN + AGE + RESP +EDU+ INCOME_DE+ HH_SIZE+INT_RECOG+PRICE, data=MILK_R_long)
vif(MCTestmodel) # VIF around 1 no MC. 

#correlation matrix 
library(dplyr)
Milk_cor <-Milk_long %>% select(-(ALT))
cor(Milk_cor) #no high correlation 

#some more models i have tried  
Model.3<-mlogit(CHOICE~PRICE |-1,data=mldata_long,reflevel = "jafull") #no intercept model   
summary(Model.3) #works, however, is this really logical?

Model.4<-mlogit(CHOICE~PRICE |INCOME_DE,data=mldata_long) #same error of "system is computationally singular: reciprocal condition number = 3.72358e-17"

Model.5<-mlogit(CHOICE~PRICE |INCOME_DE -1,data=mldata_long) #without intercept my model works, but i doubt that this is right
summary(Model.5) # no error when running model without intercept

#looking for different solutions to the problem 

#there is one discussing right here: https://stackoverflow.com/questions/29849640/r-mlogit-model-computationally-singular, where a user suggests defining the alt levels. 
mldata <- mlogit.data(Milk,choice='CHOICE',shape='long',alt.var='ALT',alt.levels = c("jafull", "jalight","WSlight","WSfull","Tuffull","Tuflight","AndBiofull","AndBiolight","WSlight","WSfull","ReBiofull","ReBiolight"),chid.var = 'chid',drop.index = TRUE)
#Which does not work either getting error: Error in dfidx::dfidx(data = data, dfa$idx, drop.index = dfa$drop.index,  : the data must be balanced in order to use the levels argument.
# someone here had the same problem and reproducing the df i used the same code now:
mldata <- mlogit.data(Milk,choice='CHOICE',shape='long',alt.var='ALT', id.var = "chid") #still singularity issue remains


# example data for long format is for example this: 
library(AER)
data("TravelMode", package = "AER")
