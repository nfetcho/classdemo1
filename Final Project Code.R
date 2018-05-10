#Final Project Code
#Importing dataset from the excel set and loading libraries

library(readxl)
opiodemscalls <- read_excel("Downloads/opiodemscalls.xls", 
                            col_types = c("numeric", "numeric", "text", 
                                          "text", "numeric", "numeric", "text", 
                                          "text", "numeric", "numeric", "text", 
                                          "text", "text", "numeric", "numeric", 
                                          "text", "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                           "blank", "blank"))
View(opioidemscalls)


library(haven)
library(MASS)
library(lmtest)
library(stargazer)
library(sandwich)
library(margins)
library(ggplot2)
library(knitr)
library(tidyverse) # for various packages
library(nnet)
install.packages(erer)
library(erer)

#Data exploring & management
table(opiodemscalls$Opoid_Use_Probable)
table(opiodemscalls$Patient_Age)
table(opiodemscalls$Patient_Gender)
table(opiodemscalls$Psych_Diag)
table(opiodemscalls$Total_Prescript)
table(opiodemscalls$Opioid_Prescript)
table(opiodemscalls$AntiDepress_Prescript)
table(opiodemscalls$Benzo_Prescript)
table(opiodemscalls$Psych_Binary)
summary(opioidemscalls$Age)

opioid <- opiodemscalls
opioid1 <- opioid$Psy[opioid$Psych_Diag==0] <- "No Positive Diagnosis"
            opioid$Psy[opioid$Psych_Diag==1] <- "One Diagnosis"
            opioid$Psy[opioid$Psych_Diag==2] <- "Two Diagnosis"
            opioid$Psy[opioid$Psych_Diag==3] <- "Three or More Diagnosis"
            
            opioid$Total[opioid$Total_Prescript==0] <- 0
            opioid$Total[opioid$Total_Prescript==1] <- 1
            opioid$Total[opioid$Total_Prescript==2] <- 2
            opioid$Total[opioid$Total_Prescript==3] <- 3
            opioid$Total[opioid$Total_Prescript==4] <- 4
            opioid$Total[opioid$Total_Prescript>=5] <- 5
            
            opioid$Age_cat[opioid$Age=="15 to 19" | opioid$Age== "5 to 9"] <- 1
            opioid$Age_cat[opioid$Age=="20 to 24"] <- 2
            opioid$Age_cat[opioid$Age=="25 to 29"] <- 3
            opioid$Age_cat[opioid$Age=="30 to 34"] <- 4
            opioid$Age_cat[opioid$Age=="35 to 39"] <- 5
            opioid$Age_cat[opioid$Age=="40 to 44"] <- 6
            opioid$Age_cat[opioid$Age=="45 to 49" | opioid$Age=="50 to 54"] <- 7
            opioid$Age_cat[opioid$Age=="55 to 59" | opioid$Age=="60 to 64" | opioid$Age=="65 to 69"
                           | opioid$Age=="70 to 74"| opioid$Age=="75 to 79"| opioid$Age=="80 to 84"
                           | opioid$Age=="85 to 89"| opioid$Age=="90 to 94"| opioid$Age=="95 to 99"] <- 8
            opioid$Opioid_Use[opioid$Opoid_Use_Probable==1] <- "Yes"
            opioid$Opioid_Use[opioid$Opoid_Use_Probable>=2] <- "No"
opioid_re <- opioid %>% 
  mutate(Psy_cat=factor(Psy), Use_cat=factor(Opioid_Use))
View(opioid_re)

#Binomial logistic regression
#Testing Objective 1: If psychiatric status is associated with opioid use
mod7 <-glm(Use_cat ~ Psych_Binary, data=opioid_re, family="binomial")
summary(mod7)
OR5<-exp(cbind(OR = coef(mod7), confint(mod7))) #calculate ORs and 95% CIs
OR5 #print ORs and 95% CIs

#Binomial logistic regression
#Testing Objective 2: if comorbidity of psychiatric diagnoses has an effect on opioid use
mod2 <-glm(Use_cat ~ Psy_cat, data=opioid_re, family="binomial")
summary(mod2)
ORPsy<-exp(cbind(OR = coef(mod2), confint(mod2))) #calculate ORs and 95% CIs
ORPsy 


#Multinomial logistic regression
#Testing objective 3: measuring cofounders and effects of variables
mod8 <-glm(Use_cat ~ Psych_Binary + Patient_Age + Patient_Gender + Opioid_Prescript + 
             Benzo_Prescript + AntiDepress_Prescript, data=opioid_re, family="binomial")
summary(mod8)
OR6<-exp(cbind(OR = coef(mod8), confint(mod8)))
OR6 

#Because Age was significant in the previous model, ran a logistic regression with just continuous age variable
mod9 <-glm(Use_cat ~ Patient_Age, data=opioid_re, family="binomial")
summary(mod9)
OR7<-exp(cbind(OR = coef(mod9), confint(mod9)))
OR7

#Age and gender are statistically significant with individuals who are older being 20% less likely
#to use opioids than younger individuals and males being more likely to suffer from opioid abuse
#There was no statistically significant association between varying degrees of comobidity
#for psychiatric diagnosis. 

#Checking assumptions for linearity for age as a continuous variable
#linearity
Age.times.log<-opioid_re$Patient_Age*log(opioid_re$Patient_Age)

mylogit1<-glm(Use_cat ~ Patient_Age + Age.times.log, data=opioid_re, family="binomial")
summary(mylogit1)

#Age is appropriate to use as a continuous variable because it meets the assumption of linearity

mylogit2<-glm(Use_cat ~ Patient_Age, data=opioid_re, family="binomial") 
plot(mylogit2, which=4, id.n=5, col="red") 

#no outliers with Age based off Cook's Distance plot

xt<-table(opioid_re$Psych_Binary, opioid_re$Use_cat, opioid_re$Opoid_Use_Probable, opioid_re$Patient_Gender,
          opioid_re$Psych_Diag, opioid_re$Total_Prescript, opioid_re$AntiDepress_Prescript, opioid_re$Opioid_Prescript,
          opioid_re$Benzo_Prescript) 
xt
prop.table(xt, 1)





##Other models 
#Re-leveling the Psy variable to increase sample size for the two factors
opioid2 <- opioid_re$Psy[opioid_re$Psych_Diag==0] <- "No Positive Diagnosis"
opioid_re$Psy[opioid_re$Psych_Diag==1] <- "One Diagnosis"
opioid_re$Psy[opioid_re$Psych_Diag>=2] <- "Two or More Diagnosis"

attach(opioid2)
##Re-leveling data, choose stage IV as reference
Psy_re <- relevel(Psy_cat, ref = "No Positive Diagnosis")

mod5 <- multinom(Use_cat ~ Psy_cat + Opioid_Prescript)
summary(mod5)

exp(coef(mod5))
exp(summary(mod5)$coefficients["No",2] + qnorm(c(0.025,0.5,0.975))*summary(mod5)$standard.errors["No",2])
exp(confint(mod5, level=0.95))

attach(opioid)
#Attach the data to avoid typing the dataset name 
attach(opioid_re)
##Re-leveling data, choose stage IV as reference
Use_re <- relevel(Use_cat, ref = "No")
##Multinomial Regression
mod <- multinom(Use_re ~ Psy_cat + Age_cat + Opioid_Prescript)
summary(mod)

exp(coef(mod))
exp(summary(mod)$coefficients["No",2] + qnorm(c(0.025,0.5,0.975))*summary(mod)$standard.errors["No",2])
exp(confint(mod, level=0.95))


#oridinal regression
mod5 <- polr(Use_cat ~ Psy_re + Opioid_Prescript, Hess=TRUE)
summary(mod5)


