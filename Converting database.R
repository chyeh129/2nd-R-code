####BWS trial 2.13.2022.####

library(readxl)
library(writexl)
library(dplyr)
library(tidyr)
library(factoextra)
library(psych)
library(support.BWS)
library(crossdes)
library(dfidx)
library(mlogit)


#My point of reference is: http://lab.agr.hokudai.ac.jp/nmvr/logit.html#the-linear-probablity-model
#I still have to explore Apollo package!!!

#I used one set of data: Serbia, survey 2, processed vegetables. 
#I cleaned the data quickly in excell and saved the file only with bw responses and Design type. 
#My first goal was to recreate the table you showed me on in the example you 
#prepared for your collegues: 
browseURL("https://uni-bonn.sciebo.de/s/sl4ADkqdbRjj03P")

#Getting the data:
data<-read_xlsx(file.choose()) #Please choose RS_ProcessedVegetables_Group2.xlsl

data<-data.frame(data)

#Getting the design file
Design<-read_xlsx(file.choose()) #Please choose Design.xlsx
Design<-data.frame(Design)

#Preparing the data for the requirements of the function bws.dataset from the package support.BWS

response.vars <- colnames(data)[2:13]

data_bws<- bws.dataset(
  data = data, # data frame containing response dataset
  response.type = 2, # format of response variables
  choice.sets = Design, # choice sets
  design.type = 2, # BIBD
  item.names = NULL, # this could be a vector with item names; NULL gives default ITEM1:ITEMn
  id = "ID", # the name of respondent id variable
  response = response.vars, # the names of response variables
  model = "marginal",# the type of dataset created by the function, alternatively "maxdiff" and "sequential"
  version = "Version" #!!!Problem!!! This should automatically reckognize the version variable in data set and 
  #design data set, however it won't work.  
  )

#What does work is the following:
#If I subset the data and the desing only to desing 1 it works. 

data_des1<-data %>% 
  filter(Version==1)

Design1<-Design %>% 
  filter(Version==1) %>% 
  select(Item1:Item5)

#Now, re-running the code only for participants who wad desing one:

data_bws1<- bws.dataset(
  data = data_des1, # data frame containing response dataset
  response.type = 2, # format of response variables
  choice.sets = Design1, # choice sets
  design.type = 2, # BIBD
  item.names = NULL, # this could be a vector with item names; NULL gives default ITEM1:ITEMn
  id = "ID", # the name of respondent id variable
  response = response.vars, # the names of response variables
  model = "marginal",# the type of dataset created by the function, alternatively "maxdiff" and "sequential"
  #version = "Version" #!!!Problem!!! This should automatically recognize the version variable in data set and 
  #design data set, however it won't work.  
)

head(data_bws1)

#The obtained table looks identical as the one you showed to your collegues. 
#10 participants turns into 600 rows of observations, etc...
#It also works fine in subsequent analysis with the package I used. 

#However, it is just for one design :)
#Which means the procedure from lines 53:73 it would have to be repeated 40 times, and then tables stacked together. 

#I assume I could figure out how to write the loop function and make this more automatized, 
#or in the works case just make long script with 40 repetitions that can be re-used for every dataset. 

#however I would like to ask you to go back to the line 46 and give me your opinion on why 
#the design feature returns an error. 

_______________________________________________________________________________________________________________________

#Just a demo, that other stuff work:

####The counting approach####

cs <- bws.count(data_bws1, cl = 2)
dim(cs)
names(cs)

plot(
  x = cs, 
  score = "bw",       # BW scores are used 
  pos = 3,            # Labels are added to the right of the points
  ylim = c(0.5, 2), # the y axis ranges from 1.6 to 2.3
  xlim =c(-1.5, 1.5)  
)


barplot(
  height = cs,
  score = "bw",    
  mfrow = c(2, 7))

dev.off()

barplot(
  height = cs,
  score = "sbw", # Standardized BW scores are used
  mean = TRUE,   # Bar plot of mean scores is drawn
  las = 1)

summary(cs)

####The modelling approach - overall####

# Dataset for the marginal model
mr.data.dfidx <- dfidx(data = data_bws1,
                       idx = list(c("STR", "ID"), "ALT"),
                       choice = "RES")

mf<-RES~ITEM1+ITEM2+ITEM3+ITEM4+ITEM5+ITEM6+ITEM7+ITEM8+ITEM9+ITEM10+ITEM11+ITEM12+ITEM13-1

mr.out <- mlogit(formula = mf, data = mr.data.dfidx)
summary(mr.out)#This also works fine

sp.md <- bws.sp(mr.out, base = "ITEM14")
sp.md

####The modelling approach - individual preferences####

library(gmnl)

#This owuld be the next step I would have to work on and would prbably have additional quesions about...