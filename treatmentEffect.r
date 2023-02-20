library(tidyverse)
library(tidyr)

dat00 <- read_csv("data/DATA20220202_4092.csv",na = c("", "NA","#NULL!"))

# define attrition and treatment variables
dat00 <- dat00%>%
  mutate(
    hasPretest=is.finite(pre.total_math_score),
    hasPosttest=is.finite(post.total_math_score),
    naPretest = is.na(pre.total_math_score),
    naPosttest = is.na(post.total_math_score),
  )%>%
  mutate(
    # Make sure all teacher have a teacher ID assigned, etc
    teach=ifelse(is.na(TeaIDPre),TeaIDEnd,TeaIDPre),
    class=ifelse(is.na(ClaIDPre),ClaIDEnd,ClaIDPre),
  )

sum(dat00$DROPSCH1)
dat00%>%group_by(DROPSCH1)%>%summarize(n_distinct(StuID),n_distinct(TeaIDPre))

# drop the first school and resource students
dat01 <- filter(dat00,DROPSCH1==0)
dat02 <- filter(dat01,RESOURCE==0)

#make dummy variables for pretest, posttest
dat02 <- dat02%>%
  mutate(
    Pretest = ifelse(hasPretest == TRUE, 1, 0),
    Posttest = ifelse(hasPosttest == TRUE, 1, 0)
  )

# find the mean for the pretest math scores and the pretest math anxiety scores
score1 <- na.omit(dat02$pre.total_math_score)
mean1 = mean(score1)

score3 <- na.omit(dat02$pre.avg_time_on_tasks)
logscore3 <- log(score3)
mean3 <- mean(logscore3)

# Fill in the NA data with the mean scores of the data that we do have
dat02 <- dat02%>%
  mutate(
    mathscores=ifelse(is.na(pre.total_math_score),mean1,pre.total_math_score),
    times=ifelse(is.na(pre.avg_time_on_tasks), mean3, log(pre.avg_time_on_tasks))
  )

# Create dummy variables for the schools and the teachers
## do not include schools 7 and 10 because they are dropped from the study
dat02 <- dat02%>%
  mutate(
    S1 = ifelse(SchIDPre == 1, 1, 0),
    S2 = ifelse(SchIDPre == 2, 1, 0),
    S3 = ifelse(SchIDPre == 3, 1, 0),
    S4 = ifelse(SchIDPre == 4, 1, 0),
    S5 = ifelse(SchIDPre == 5, 1, 0),
    S6 = ifelse(SchIDPre == 6, 1, 0),
    S7 = ifelse(SchIDPre == 7, 1, 0),
    S8 = ifelse(SchIDPre == 8, 1, 0),
    S9 = ifelse(SchIDPre == 9, 1, 0),
    S10 = ifelse(SchIDPre == 10, 1, 0)
  )

# drop the second school that left after assignment
## drop because still will not predict attrition bc all students in this school did not have choice to leave
dat03 <- dat02%>%filter(DROPSCH2==0)
dat03 <- dat03%>%
  mutate(
    drag = ifelse( rdm_condition == 'Dragon', 1, 0)
  )

#DragonBox and BAU specific
datDrag <- dat03%>%filter(dat03$rdm_condition == 'Dragon')
datBAU <- dat03%>%filter(dat03$rdm_condition == 'BAU')

datsubD <- datDrag[ , c("rdm_condition", "post.total_math_score", "hasPosttest", "drag", "mathscores", "Pretest", "times", "GIFTED",
                        "S1","S2","S3","S4","S5","S6","S8","S9")]
datsubB <- datBAU[ , c("rdm_condition", "post.total_math_score", "hasPosttest", "drag", "mathscores", "Pretest", "times", "GIFTED",
                        "S1","S2","S3","S4","S5","S6","S8","S9")]
datDragBAU <- rbind(datsubD, datsubB)


### separate datasets for each condition
dbDat <- mutate(datDragBAU,drag=1)
bauDat <- mutate(datDragBAU,drag=0)

### best fit model
model4 <- glm(hasPosttest~drag+Pretest+mathscores
              +S1+S2+S3+S4+S5+S6+S8+S9
              +times*drag, family="binomial", data=datDragBAU)

prAP <- predict(model4,dbDat,type='response') ## Pr(AP|X)
prO <- predict(model4,bauDat,type='response') ## Pr(0bs. Posttest |X)

prAPO <- prAP / prO

prob <- ifelse(prAPO > 1, 1, prAPO)

estprobs <- (1 - prob)

#define estprobs as 0 for everyone in DragonBox
probsadj0 <- ifelse(datDragBAU$drag == 1, 0, estprobs)


###############################################################

#fit OLS regression model
modelTrial <- lm(post.total_math_score~probsadj0+drag+Pretest+#mathscores+
times+GIFTED
                 +S1+S2+S3+S4+S5+S6+S8+S9, data=datDragBAU)

summary(modelTrial)


require(broom)    
tidy(modelTrial)
treatmentEffect <- tidy(modelTrial)[3, 2]
print(paste0("The treatment effect is ", treatmentEffect))

adjmodelTrial <- lm(post.total_math_score~drag+Pretest+mathscores+times+GIFTED
                    +S1+S2+S3+S4+S5+S6+S8+S9, data=datDragBAU)## refits the model without estprobs
theta0 <- coef(adjmodelTrial)['drag']
print(paste0("The treatment effect without estprobs is ", theta0))


### bootstrap

thetaBS <- NULL
thetaBS0 <- NULL
for(i in 1:1000){
  #create datset with replacement
  bsRows <- sample(1:nrow(datDragBAU),nrow(datDragBAU),replace=TRUE)
  bsDat <- datDragBAU[bsRows,]
  
  #split new data into subsets 
  db_bsDat <- mutate(bsDat,drag=1)
  bau_bsDat <- mutate(bsDat,drag=0)
  
  
  #logistic regression for the predicted probabilities 
  modelbs <- glm(hasPosttest~drag+Pretest+mathscores
                 +S1+S2+S3+S4+S5+S6+S8+S9
                 +times*drag, family="binomial", data=bsDat)
  
  #predicted probabilities with the data subsets
  bs_prAP <- predict(modelbs,db_bsDat,type='response') 
  bs_prO <- predict(modelbs,bau_bsDat,type='response') 
  
  #Find the probability of interest
  bs_prAPO <- bs_prAP / bs_prO
  
  #make all probabilities <=1
  bsprob <- ifelse(bs_prAPO > 1, 1, bs_prAPO)
  
  #bsprob is the probability of AP given observed -- 
  #b/c of setup of OLS regression, need probability this event does not occur
  bsestprobs <- (1 - bsprob)
  
  #define bsestprobs as 0 for everyone in DragonBox
  probsadj <- ifelse(bsDat$drag == 1, 0, bsestprobs)
  
  #fit OLS regression model
  modelTrialbs <- lm(post.total_math_score~probsadj+drag+Pretest+mathscores+
  times+GIFTED+
                       +S1+S2+S3+S4+S5+S6+S8+S9, data=bsDat)
  
  #store Treatment Effect 1
  thetaBS[i] <- coef(modelTrialbs)['drag']
  
  #OLS regression model without probability 
  adjmodelTrialbs <- lm(post.total_math_score~drag+Pretest+mathscores+times+GIFTED
                        +S1+S2+S3+S4+S5+S6+S8+S9, data=bsDat)## refits the model without estprobs
  
  #store Treatment Effect 2
  thetaBS0[i] <- coef(adjmodelTrialbs)['drag']
}



