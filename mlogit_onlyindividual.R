# multinominal logit treating all variables as individual specific 
MILK_R_wide <- read_excel("D:/S2F/Strenght2Food 8.3/Scibo Hochladen/Meeting_131222/Wide Format/MILK_R_wide.xlsx", 
                          +     sheet = "NO_NA_chid_ID")

Milk=as.data.frame(MILK_R_wide)
str(Milk)

#defininf the data 
Milk <- Milk%>%mutate(ALT = as.factor(ALT))
Milk <- Milk%>%mutate(RESP = as.factor(RESP))
Milk <- Milk%>%mutate(INT = as.factor(INT))

#defining the mlogit.data, but not using the varying argument
mldata<-mlogit.data(Milk,choice = "ALT",shape="wide")

#only individual specific variables 
model.1<-mlogit(ALT~1|INCOME_DE,data=mldata) 
summary(model.1)

model.2<-mlogit(ALT~1|INCOME_DE+AGE+EDU+RESP+VIDGAMES+PRICE,data=mldata)  
summary(model.2)
