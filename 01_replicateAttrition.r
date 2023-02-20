## run from "MQP Resources" folder
library(tidyverse)

dat00 <- read_csv("data/DATA20220202_4092.csv",na = c("", "NA","#NULL!"))

### define attrition and treatment variables
dat00 <- dat00%>%
  mutate(
    hasPretest=is.finite(pre.total_math_score),
    hasPosttest=is.finite(post.total_math_score),
    Z =as.character(rdm_condition)
  )%>%
  mutate(
### TeaIDPre is the ID for the teacher they started with
### when this is missing, impute TeaIDEnd, their teacher at the end of the study
    teach=ifelse(is.na(TeaIDPre),TeaIDEnd,TeaIDPre),
    class=ifelse(is.na(ClaIDPre),ClaIDEnd,ClaIDPre))

########################################################
### A total of 52 seventh-grade mathematics teachers and their students from 11 middle
## schools (10 in-person schools and one virtual academy) were recruited from a large, suburban
###district in the Southeastern United States in the summer of 2020. Together, these teachers taught
### 190 mathematics classes and 4,092 students
########################################################

n_distinct(dat00$TeaIDPre)
n_distinct(dat00$SchIDPre)
n_distinct(dat00$ClaIDPre)
n_distinct(dat00$StuID)
nrow(dat00)


########################################################
### Prior to random assignment, one school declined to
### participate. This school included 4 teachers and 377 students.
########################################################

### this is recorded in DROPSCH1 =1 if dropped, 0 otherwise
### Group by has the action performed by summarize on this column
### summarize function gives an appropriate action to perform
# like a pivot table in excel
sum(dat00$DROPSCH1)
dat00%>%group_by(DROPSCH1)%>%summarize(n_distinct(StuID),n_distinct(TeaIDPre))

dat01 <- filter(dat00,DROPSCH1==0)

########################################################
### Students enrolled in resource
### settings were not included in this study. 
########################################################
dat02 <- filter(dat01,RESOURCE==0)


########################################################
### This resulted in 10 schools (9 in-person and 1 virtual),
### 37 teachers, 156 classes, and 3,612 students.
########################################################
n_distinct(dat02$TeaIDPre)
n_distinct(dat02$SchIDPre)
n_distinct(dat02$ClaIDPre)
n_distinct(dat02$StuID)


########################################################
### One school dropped out following the pretest assessment, 
########################################################
# schools that did not drop will be FALSE or 0
dat03 <- dat02%>%filter(DROPSCH2==0)

########################################################
### resulting in a final pool of 9
### schools (8 in-person and 1 virtual), 34 teachers, 143 classes, and 3,271 students participating at
### the start of the interventions.
########################################################
n_distinct(dat03$TeaIDPre)
n_distinct(dat03$SchIDPre)
n_distinct(dat03$ClaIDPre)
n_distinct(dat03$StuID)

########################################################
### From this pool of 3,271 students, 1,850 had both pretest and
### posttest assessments and constituted the analytic sample for this study. These students were
### enrolled in 127 classes across 34 teachers
########################################################
dat04 <- dat03%>%filter(hasPretest&hasPosttest)
n_distinct(dat04$TeaIDPre)
n_distinct(dat04$SchIDPre)
n_distinct(dat04$ClaIDPre)
n_distinct(dat04$StuID)

########################################################
### attrition rates
########################################################

# Attrition Rate = # ppl that left / total # randomized

########################################################
# for the purpose of this study, we will consider dat03 as indiviuals that 
# dropped out of the study b/c after pretest assignment

attritprepost <- (n_distinct(dat02$StuID) - n_distinct(dat04$StuID))/n_distinct(dat02$StuID)

########################################################
### if we didn’t exclude students w/ missing pretests
########################################################

dat05 <- dat03%>%filter(hasPosttest)
n_distinct(dat05$TeaIDPre)
n_distinct(dat05$SchIDPre)
n_distinct(dat05$ClaIDPre)
n_distinct(dat05$StuID)

# attrition rates considering those with posstests(including those with missing prettests)
attritpost <- (n_distinct(dat02$StuID) - n_distinct(dat05$StuID))/n_distinct(dat02$StuID)


#filter by virtual
datV <- filter(dat02, virtual == 1)
datV1 <- filter(dat05, virtual == 1)
attritpostV <- (n_distinct(datV$StuID) - n_distinct(datV1$StuID))/n_distinct(datV$StuID)

#stay virtual 
datSV <- filter(dat02, stay_VIRTUAL == 1)
datSV1 <- filter(dat05, stay_VIRTUAL == 1)
attritpostSV <- (n_distinct(datSV$StuID) - n_distinct(datSV1$StuID))/n_distinct(datSV$StuID)

#in person
datNV <- filter(dat02, virtual == 0)
datNV1 <- filter(dat05, virtual == 0)
attritpostNV <- (n_distinct(datNV$StuID) - n_distinct(datNV1$StuID))/n_distinct(datNV$StuID)

#stay in person
datIP <- filter(dat02, stay_INPERSON == 1)
datIP1 <- filter(dat05, stay_INPERSON == 1)
attritpostIP <- (n_distinct(datIP$StuID) - n_distinct(datIP1$StuID))/n_distinct(datIP$StuID)


########################################################
### for students w/ pretest scores, mean(pretest |has Posttest) vs mean (pretest | no Posttest)
########################################################

#mean for pretest total math score for those who have the Posttest
meanprepoest <- mean(dat04$pre.total_math_score)

dat03%>%group_by(hasPosttest)%>%summarize(mean(pre.total_math_score, na.rm = TRUE))


#mean for pretest total math score for those who do not have the Posttest
dat06 <- dat03%>%filter(hasPretest&is.na(post.total_math_score))
meanprenopost <- mean(dat06$pre.total_math_score)

#dat03%>%filter(hasPretest)%>%group_by(Z)%>%summarize(mean(pre.total_math_score))

########################################################
### mean(pretest |has Posttest) vs mean (pretest | no Posttest)
### Under the separate treatment conditions
########################################################

## Attrition rates (has pretest only)

#BAU
dat07a <- dat02%>%filter(dat02$Z == 'BAU')
dat07 <- dat04%>%filter(dat04$Z == 'BAU')
meanprepostBAU <- mean(dat07$pre.total_math_score)
attritprepostBAU <- (n_distinct(dat07a$StuID) - n_distinct(dat07$StuID))/n_distinct(dat07a$StuID)

#Dragon
dat08a <- dat02%>%filter(dat02$Z == 'Dragon')
dat08 <- dat04%>%filter(dat04$Z == 'Dragon')
meanprepostDB <- mean(dat08$pre.total_math_score)
attritprepostDB <- (n_distinct(dat08a$StuID) - n_distinct(dat08$StuID))/n_distinct(dat08a$StuID)

#FH2T
dat09a <- dat02%>%filter(dat02$Z == 'FH2T')
dat09 <- dat04%>%filter(dat04$Z == 'FH2T')
meanprepostFH <- mean(dat09$pre.total_math_score)
attritprepostFH <- (n_distinct(dat09a$StuID) - n_distinct(dat09$StuID))/n_distinct(dat09a$StuID)

#ASSIST
dat10a <- dat02%>%filter(dat02$Z == 'ASSISTments')
dat10 <- dat04%>%filter(dat04$Z == 'ASSISTments')
meanprepostAS <- mean(dat10$pre.total_math_score)
attritprepostAS <- (n_distinct(dat10a$StuID) - n_distinct(dat10$StuID))/n_distinct(dat10a$StuID)

## Attrition rates (pretest not necessary)

#BAU
dat11a <- dat02%>%filter(dat02$Z == 'BAU')
dat11b <- dat05%>%filter(dat05$Z == 'BAU')
attritpostBAU <- (n_distinct(dat11a$StuID) - n_distinct(dat11b$StuID))/n_distinct(dat11a$StuID)

#Dragon
dat12a <- dat02%>%filter(dat02$Z == 'Dragon')
dat12b <- dat05%>%filter(dat05$Z == 'Dragon')
attritpostDB <- (n_distinct(dat12a$StuID) - n_distinct(dat12b$StuID))/n_distinct(dat12a$StuID)

#FH2T
dat13a <- dat02%>%filter(dat02$Z == 'FH2T')
dat13b <- dat05%>%filter(dat05$Z == 'FH2T')
attritpostFH <- (n_distinct(dat13a$StuID) - n_distinct(dat13b$StuID))/n_distinct(dat13a$StuID)

#ASSIST
dat14a <- dat02%>%filter(dat02$Z == 'ASSISTments')
dat14b <- dat05%>%filter(dat05$Z == 'ASSISTments')
attritpostFH <- (n_distinct(dat14a$StuID) - n_distinct(dat14b$StuID))/n_distinct(dat14a$StuID)

## Has pretest and does not have posttest

#BAU
dat11 <- dat06%>%filter(dat06$Z == 'BAU')
meanprenopostBAU <- mean(dat11$pre.total_math_score)

#Dragon
dat12 <- dat06%>%filter(dat06$Z == 'Dragon')
meanprenopostDB <- mean(dat12$pre.total_math_score)

#FH2T
dat13 <- dat06%>%filter(dat06$Z == 'FH2T')
meanprenopostFH <- mean(dat13$pre.total_math_score)

#ASSIST
dat14 <- dat06%>%filter(dat06$Z == 'ASSISTments')
meanprenopostAS <- mean(dat14$pre.total_math_score)

