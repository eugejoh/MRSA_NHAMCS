#BS845_mod_FINAL
library(devtools)
library(foreign)
library(readstata13)
library(survey)
library(ggplot2)
library(data.table)
library(MASS)

rm(list=ls())
ls()
cat("\014") # clear console in R Studio
R.Version()$version.string
sessionInfo()

####################################
# FUNCTION TO SETWD BY A FUNCTION #
####################################
# edit of the function to read in the files after setting the working directory

set.directory <- function(z){
  z <-  readline('Enter File Path (e.g. "C:/Users/Directory") : ')
  if(is.null(z)){
    stop("Empty Input")}
  else{
    setwd(z)}
  filenames <- list.files(pattern="ed+.*dta")
  message("File names within the selected working directory")
  print(filenames)
  ed.list <<- list() # empty list that will be filled with dataframes from working directory
  for (i in 1:length(filenames)){ # for loop that based on the number of files in the folder with filenames criterion
    ed.list[[i]]<<-read.dta13(filenames[i],generate.factors = T) ## creating "ed" a list of STATA 13 files based on file name
    names(ed.list)[i]<<-sub(pattern = "(.*)\\..*$", replacement = "\\1",filenames[i]) # Google regex to understand syntax
    message(names(ed.list[i])," (dataframe dimensions: rows x cols)")
    if ( T==all(grepl("[[:upper:]]", names(ed.list[[i]]))) ){ #visual check of which dataframes have upper/lower-case column names
      print("Uppercase Column Variables")}
    else {
      print("Lowercase Column Variables")
      names(ed.list[[i]])<<-toupper(names(ed.list[[i]]))
    }
    print(dim(ed.list[[i]]))
    assign(paste0("summary.",names(ed.list[i])),summary(ed.list[[i]]),envir = .GlobalEnv)
  }
}

# /Users/eugenejoh/Documents/BU Graduate School/BU SPH/Practicum Summer/Online Dropbox Folder/STATA Files

###############################
# SURVEY DESIGN and WEIGHTING #
###############################
# PATWT - patient weight visits
# EDWT - allows for department-level estimation
# CSTRATM – clustered PSU Stratum Marker 
# CPSUM – clustered PSU Marker

svy.var <- c("PATWT","CSTRATM","CPSUM") #names of survey variables

########################################################
# CHECK FOR PROPER SURVEY DESIGN INPUT FOR EACH NHAMCS #
########################################################
svy.var <- c("PATWT","CSTRATM","CPSUM") #names of survey variables
edw<-lapply(names(ed.list),function(x) {which(names(ed.list[[x]]) %in% svy.var)}) #creates list column numbers for each survey variables in ed.list (of each year)
for (i in 1:length(names(ed.list))){ # loop from number 1 to 5 (2005-2009)
  assign(paste0("svy.ed",i), #assign values to varaible svy.ed number of loop
         svydesign(data=ed.list[i], # dataset from ed.list by appropriate year
                   weights=ed.list[[i]][,edw[[i]][1]], #weights=PATWT #check by edw; names(ed.list[[1]])[140]
                   ids=ed.list[[i]][,edw[[i]][3]], #ids=CPSUM #check by names(ed.list[[1]])[278]
                   strata=ed.list[[i]][,edw[[i]][2]], #strata=CSTRATM #check by names(ed.list[[1]])[277]
                   nest=T))}

#####################################
# MERGING OF NHAMCS YEARS 2005-2009 #
#####################################
# http://adv-r.had.co.nz/Functionals.html#functionals-fp
ed0509 <- Reduce(function(...) {
  message("processing...",appendLF = F)
  merge(..., all=T)} ,ed.list)

# Old Method
############
system.time(ed0506<-merge(ed$ed05,ed$ed06,all=T)) #merge 2005 with 2006
# 9.081
dim(ed0506);table(ed0506$VYEAR) #check the dimensions and years included in the study
system.time(ed0507<-merge(ed0506,ed$ed07,all=T)) #merge 2005-2006 with 2007
# 20.879
dim(ed0507);table(ed0507$VYEAR)
system.time(ed0508<-merge(ed0507,ed$ed08,all=T)) #merge 2005-2007 with 2008
# 37.765
dim(ed0508);table(ed0508$VYEAR)
system.time(ed0509<-merge(ed0508,ed$ed09,all=T)) #merge 2005-2008 with 2009
############

###########################
# DUMMY VARIABLE CREATION #
###########################
table(ed0509$RESIDNCE)
# Merging all "other" categories in residence frmo 2005-2008 to "Other" in 2009
levels(ed0509$RESIDNCE) <- c("Private residence","Nursing home","Other","Other","Homeless","Unknown","Blank","Other")
levels(ed0509$RESIDNCE)
table(ed0509$RESIDNCE)

## DON'T RUN IF USING STEPAIC
# # assigns NA for unknown, blank and other from residence variable
# ed0509$NRESIDNCE<-ifelse(ed0509$RESIDNCE=="Blank",NA,ed0509$RESIDNCE) # ONLY DO THIS ONCE
# table(ed0509$NRESIDNCE)
# # relabels the levels for residence variable
# ed0509$NRESIDNCE<-factor(ed0509$NRESIDNCE,levels=c(1,2,3,4,5),labels=c("Private residence","Nursing home","Other","Homeless","Unknown")) # ONLY DO THIS ONCE
# table(ed0509$NRESIDNCE)

# create dummy binary variable for homelessness from residence variable
ed0509$HOMELESS.cat <- ifelse(ed0509$RESIDNCE=="Homeless",1,0)
table(ed0509$HOMELESS.cat)
# relabels the levels
ed0509$HOMELESS.cat <- factor(ed0509$HOMELESS.cat,levels=c(0,1),labels=c("Non-homeless","Homeless")) #ONLY DO THIS ONCE
table(ed0509$AGER,ed0509$HOMELESS.cat) #non-homeless includes prviate residence, nursing home, other and unknown
table(ed0509$SEX,ed0509$HOMELESS.cat)

# create dummy binary variable for race, white vs. non-white
ed0509$RACE.cat <- ifelse(ed0509$RACE=="White Only",1,0)
ed0509$RACE.cat <- factor(ed0509$RACE.cat,levels=c(0,1),labels=c("Non-White","White"))
table(ed0509$AGER,ed0509$RACE.cat)

#vector that contains ICD-9 codes of interest
# CREATE FUNCTION FOR READING IN ICD-9 CODES FROM SPREADSHEET FILE
icd9<-read.table(file="/Users/eugenejoh/Documents/BU Graduate School/BU SPH/2016 Fall/BS 845/Final Project/Data/icd9codes_final.txt",sep = ",",header=T,colClasses=c(rep("character",2)))
# /Users/eugenejoh/Documents/BU Graduate School/BU SPH/Practicum Summer/Online Dropbox Folder/STATA Files

icd9<-icd9[-25,1] #removes MRSA diagnosis
icd9.list<-sort(gsub("\\.","",icd9)) #removes period, \\. is for special characters... "." removes any character and sorts by numberical value

# From Practicum Code
#SSTI.list <- c("035", "566", "60883", "6110", "6752", "6751", "680","681","682","683","684","685","686", "7048", "70583","7070", "7078", "7079", "7071", "7280", "72886", "7714", "7715", "7854")

diag.var <- data.frame(ed0509$DIAG1,ed0509$DIAG2,ed0509$DIAG3) #dataframe of DIAG1, DIAG2, DIAG3 variables
SSTIfunction <- function(i) any(grepl(paste(paste0("^",icd9.list), collapse="|"), i)) #function that selects 
# any(): returnsTRUE/FALSE if logic condition is satisfied/not
# grepl(): pattern recognition returns logic condition
# paste(,collapse): pasting values and collapse-assigns all values of SSTI.list into a vector sequence
# paste0(): pastes the numbers in SSTI.list without spaces between ^ operator and char
# i: index of grepl so it runs for all values in SSTI.list
diags <- which(apply(diag.var,1,SSTIfunction)) #returns vector of rows that satisfy condition of SSTI in DIAG1, DIAG2, DIAG3
# apply(diags1,1,SSTIfunction): returns TRUE/FALSE if row contains at least one SSTI.list character from the DIAG1, DIAG2, DIAG3
# which(apply(diags1,1,SSTIfunction)): returns the actual row number that contains at least one SSTI.list character from the DIAG1, DIAG2, DIAG3 variable

setDT(ed0509)[,SSTI.cat:=0][diags,SSTI.cat:=1]
ed0509$SSTI.cat <- factor(ed0509$SSTI.cat, levels=c(0,1),labels=c("Non-SSTI","SSTI")) #assign labels SSTI and non-SSTI to levels for DIAG1, DIAG2, DIAG3

names(ed0509) #doublecheck that the new variables are added to the dataset
class(ed0509) #need to convert to data.frame
ed0509<-as.data.frame(ed0509) #convert to data.frame
# select relevant variables for analysis (minimize processing time)
myvars <- c("AGE","SEX","AGER","RESIDNCE","RACE","ETHUN","DIAG1","DIAG2","DIAG3","PAYPRIV","PAYSELF","PAYTYPE","REGION","HOMELESS.cat","RACE.cat","SSTI.cat","PATWT","CPSUM","CSTRATM")
ed0509.1<-ed0509[,myvars] #smaller dataset with columns of interest to reduce processing time
ed0509<-ed0509[,myvars]

#################################
# APPLY SURVEY DESIGN TO ed0509 #
#################################
wt<-svydesign(data=ed0509, # dataset from ed.list by appropriate year
          weights=~PATWT, #weights=PATWT #check by edw; names(ed.list[[1]])[140]
          ids=~CPSUM, #ids=CPSUM #check by names(ed.list[[1]])[278]
          strata=~CSTRATM, #strata=CSTRATM #check by names(ed.list[[1]])[277]
          nest=T) #forces nesting in the design


#############################################
# DESCRIPTIVES FOR ENTRIES OF POOLED NHAMCS #
#############################################

# USE ftable() to convert survey output into table form
# then use ggplot2 to create plots for each descriptive

#AGE
ed0509$AGE<-as.numeric(ed0509$AGE)
svymean(as.numeric(ed0509$AGE),design=wt) #mean age of entire NHAMCS
#svytotal(~DIAG1,wt)

#RESIDENCE
summary(ed0509$NRESIDNCE) #summary statistics of NHAMCS entries for housing status
round(prop.table(table(ed0509$RESIDNCE))*100,2) #percentages of NHAMCS entries
length(ed0509$RESIDNCE) #double-check the sum

ftable(svytotal(~RESIDNCE,design=wt,na.rm=T)) #estimated visits totals and SE
round(svymean(~RESIDNCE,wt,na.rm=T),3) #proportions of residence levels and SEs
svyciprop(~RESIDNCE=="Private residence",wt,level=0.95) #95 CIs
svyciprop(~RESIDNCE=="Nursing home",wt,level=0.95)
svyciprop(~RESIDNCE=="Other institution",wt,level=0.95)
svyciprop(~RESIDNCE=="Other residence",wt,level=0.95)
svyciprop(~RESIDNCE=="Homeless",wt,level=0.95)

svytable(~RESIDNCE,wt) #totals for each residence levels
str(ed0509$AGE) # use as.numeric to avoid problems 
svytotal(~ed0509$AGER,wt) #estimated totals for each residence and SE

##############################################
# DESCRIPTIVES FOR HOMELESS VS. NON-HOMELESS #
##############################################
table(ed0509$RESIDNCE=="Homeless") #those homeless=T, all the other=F
svytotal(~RESIDNCE=="Homeless",wt,na.rm=T) #estimated visit totals for homeless and all other

### AGE ###
svyby(~as.numeric(AGE),~RESIDNCE=="Homeless",wt,svymean) #
svyttest(as.numeric(AGE)~HOMELESS.cat,design=wt) #plots require separate weighting to separate values

### SEX ###
### homeless & non-homeless
svyby(~SEX,~RESIDNCE=="Homeless",wt,svytotal) #Estimated totals for male and female with SEs
svyby(~SEX,~RESIDNCE=="Homeless",wt,svymean) #proportions for male and female with SEs
svyby(~SEX=="Male",~RESIDNCE=="Homeless",wt,svyciprop,vartype="ci") #95% CIs
svyby(~SEX=="Female",~RESIDNCE=="Homeless",wt,svyciprop,vartype="ci") #95% CIs

### RACE ###
### homeless & non-homeless
svyby(~RACE,~RESIDNCE=="Homeless",wt,svytotal,na.rm=T) #Estimated totals by race with SEs
svyby(~RACE,~RESIDNCE=="Homeless",wt,svymean,na.rm=T) #proportions by race
# 95% CIs
svyby(~RACE=="White Only",~RESIDNCE=="Homeless",wt,svyciprop,vartype="ci",na.rm=T)
svyby(~RACE=="Black/African American only",~RESIDNCE=="Homeless",wt,svyciprop,vartype="ci",na.rm=T)
svyby(~RACE=="Asian only",~RESIDNCE=="Homeless",wt,svyciprop,vartype="ci",na.rm=T)
svyby(~RACE=="Native Hawaiian/Oth Pac Isl only",~RESIDNCE=="Homeless",wt,svyciprop,vartype="ci",na.rm=T)
svyby(~RACE=="American Indian/Alaska Native only",~RESIDNCE=="Homeless",wt,svyciprop,vartype="ci",na.rm=T)
svyby(~RACE=="More than one race reported",~RESIDNCE=="Homeless",wt,svyciprop,vartype="ci",na.rm=T)

### ETHNICITY ###
svyby(~ETHUN,~RESIDNCE=="Homeless",wt,svytotal,na.rm=T) #estimated totals by ethnicity
svyby(~ETHUN,~RESIDNCE=="Homeless",wt,svymean,na.rm=T) #proportions by ethnicity

### PAYMENT OF CARE ###
table(ed0509$PAYPRIV) #private insurance entries
table(ed0509$PAYSELF) #selfpay entries

### PRIVATE INSURANCE ###
svyby(~PAYPRIV,~RESIDNCE=="Homeless",wt,svytotal,na.rm=T) #estimated totals of private insurance by homelessness with SEs
svyby(~PAYPRIV,~RESIDNCE=="Homeless",wt,svymean,na.rm=T) #proportions
svyby(~PAYPRIV=="No",~RESIDNCE=="Homeless",wt,svyciprop,vartype="ci",na.rm=T) #95% CIs
svyby(~PAYPRIV=="Yes",~RESIDNCE=="Homeless",wt,svyciprop,vartype="ci",na.rm=T)

### SELF-PAY ###
svyby(~PAYSELF,~RESIDNCE=="Homeless",wt,svytotal,na.rm=T) #estimated totals of self-pay by homelesness with SEs
svyby(~PAYSELF,~RESIDNCE=="Homeless",wt,svymean,na.rm=T) #proportions
svyby(~PAYSELF=="No",~RESIDNCE=="Homeless",wt,svyciprop,vartype="ci",na.rm=T) #95% CIs
svyby(~PAYSELF=="Yes",~RESIDNCE=="Homeless",wt,svyciprop,vartype="ci",na.rm=T)

# REGION # *LOOK INTO REGION AFTER OTHER ANALYSES
prop.table(table(ed0509$REGION,ed0509$HOMELESS.cat),1)*100
prop.table(table(ed0509$REGION,ed0509$HOMELESS.cat),2)*100

svytable(~REGION+HOMELESS.cat,wt) #estimates of homelessness by region
svyby(~REGION,~RESIDNCE=="Homeless",wt,svytotal,na.rm=T) #estimated totals
svyby(~REGION,~RESIDNCE=="Homeless",wt,svymean,na.rm=T) #proportions
prop.table(svytable(~REGION+HOMELESS.cat,wt),1)*100
prop.table(svytable(~REGION+HOMELESS.cat,wt),2)*100

#################
# SSTI ANALYSIS #
#################

# Age
svyby(~as.numeric(AGE),~SSTI.cat=="SSTI",wt,svymean) #estimated mean age for SSTIs and non-SSTIs with SEs
svyttest(as.numeric(AGE)~SSTI.cat,wt)

# AGER
summary(ed0509$AGER) #summmary of NHAMCS entries
svyby(~AGER,~SSTI.cat,wt,svytotal) #estimated totals of age categories by SSTI/non-SSTI with SEs
svyby(~AGER,~SSTI.cat3,wt,svymean) #proportions
svyby(~AGER=="Private residence",~SSTI.cat,wt,svyciprop,vartype="ci",na.rm=T) #95% CIs
svyby(~AGER=="Nursing home",~SSTI.cat,wt,svyciprop,vartype="ci",na.rm=T)
svyby(~AGER=="Other institution",~SSTI.cat,wt,svyciprop,vartype="ci",na.rm=T)
svyby(~AGER=="Other residence",~SSTI.cat,wt,svyciprop,vartype="ci",na.rm=T)
svyby(~AGER=="Homeless",~SSTI.cat,wt,svyciprop,vartype="ci",na.rm=T)

# Residence
svyby(~RESIDNCE,~SSTI.cat,wt,svytotal,na.rm=T)
svyby(~RESIDNCE,~SSTI.cat,wt,svymean,na.rm=T)
svyby(~AGER=="Under 15 years",~SSTI.cat,wt,svyciprop,vartype="ci",na.rm=T)
svyby(~AGER=="15-24 years",~SSTI.cat,wt,svyciprop,vartype="ci",na.rm=T)
svyby(~AGER=="25-44 years",~SSTI.cat,wt,svyciprop,vartype="ci",na.rm=T)
svyby(~AGER=="45-64 years",~SSTI.cat,wt,svyciprop,vartype="ci",na.rm=T)
svyby(~AGER=="65-74 years",~SSTI.cat,wt,svyciprop,vartype="ci",na.rm=T)
svyby(~AGER=="75 years and over",~SSTI.cat,wt,svyciprop,vartype="ci",na.rm=T)
# Homeless Category
svyby(~HOMELESS.cat,~SSTI.cat,wt,svytotal,na.rm=T)
svyby(~SSTI.cat3,~HOMELESS.cat,wt,svymean,na.rm=T)

# Private Pay
svyby(~PAYPRIV,~SSTI.cat,wt,svytotal,na.rm=T)
svyby(~PAYPRIV,~SSTI.cat,wt,svymean,na.rm=T)
svyby(~PAYPRIV=="No",~SSTI.cat,wt,svyciprop,vartype="ci",na.rm=T)
svyby(~PAYPRIV=="Yes",~SSTI.cat,wt,svyciprop,vartype="ci",na.rm=T)

# Self Pay
svyby(~PAYSELF,~SSTI.cat,wt,svytotal,na.rm=T)
svyby(~PAYSELF,~SSTI.cat,wt,svymean,na.rm=T)
svyby(~PAYSELF=="No",~SSTI.cat,wt,svyciprop,vartype="ci",na.rm=T)
svyby(~PAYSELF=="Yes",~SSTI.cat,wt,svyciprop,vartype="ci",na.rm=T)

# Sex
svyby(~SEX,~SSTI.cat,wt,svytotal,na.rm=T)
svyby(~SEX,~SSTI.cat,wt,svymean,na.rm=T)
svyby(~SEX=="Male",~SSTI.cat,wt,svyciprop,vartype="ci",na.rm=T)
svyby(~SEX=="Female",~SSTI.cat,wt,svyciprop,vartype="ci",na.rm=T)

# Race
svyby(~RACE,~SSTI.cat,wt,svytotal,na.rm=T)
svyby(~RACE,~SSTI.cat,wt,svymean,na.rm=T)
svyby(~RACE=="White Only",~SSTI.cat,wt,svyciprop,vartype="ci",na.rm=T)
svyby(~RACE=="Black/African American only",~SSTI.cat,wt,svyciprop,vartype="ci",na.rm=T)
svyby(~RACE=="Asian only",~SSTI.cat,wt,svyciprop,vartype="ci",na.rm=T)
svyby(~RACE=="Native Hawaiian/Oth Pac Isl only",~SSTI.cat,wt,svyciprop,vartype="ci",na.rm=T)
svyby(~RACE=="American Indian/Alaska Native only",~SSTI.cat,wt,svyciprop,vartype="ci",na.rm=T)
svyby(~RACE=="More than one race reported",~SSTI.cat,wt,svyciprop,vartype="ci",na.rm=T)

#double check with all RACE levels
svytable(~RACE.cat+SSTI.cat,design=wt)
svyby(~RACE.cat,~SSTI.cat,wt,svytotal,na.rm=T)
svyby(~RACE.cat,~SSTI.cat,wt,svymean,na.rm=T)

svyby(~SSTI.cat,~HOMELESS.cat,wt,svymean,na.rm=T)

#####################
# CHI-SQUARED TESTS #
#####################

# Homeless vs. Non-homeless 1
table(ed0509$SSTI.cat,ed0509$HOMELESS.cat) #check if entries are <5 for X2-test
svytable(~SSTI.cat+HOMELESS.cat,design=wt)

svychisq(~SSTI.cat+HOMELESS.cat,design=wt,statistic="F") #X2 test for homelessness and SSTI/non-SSTI
# Homeless vs. Private Residence
table(ed0509$SSTI.cat,ed0509$HOMELESS.cat2) #check if entries are <5 for X2-test
svytable(~SSTI.cat+HOMELESS.cat2,design=wt)
svychisq(~SSTI.cat+HOMELESS.cat2,design=wt,statistic="F")
# Private Pay
table(ed0509$SSTI.cat,ed0509$PAYPRIV)
svytable(~SSTI.cat+PAYPRIV,design=wt)
svychisq(~SSTI.cat+PAYPRIV,design=wt,statistic="F")
# Self Pay
table(ed0509$SSTI.cat,ed0509$PAYSELF)
svytable(~SSTI.cat+PAYSELF,design=wt)
svychisq(~SSTI.cat+PAYSELF,design=wt,statistic="F")
# Sex
table(ed0509$SSTI.cat,ed0509$SEX)
svytable(~SSTI.cat+SEX,design=wt)
svychisq(~SSTI.cat+SEX,design=wt,statistic="F")
# Race
table(ed0509$SSTI.cat,ed0509$RACE.cat)
svychisq(~SSTI.cat+RACE,design=wt)
svytable(~SSTI.cat+RACE.cat,design=wt)
svyby(~SSTI.cat,~RACE.cat,design=wt,svymean)

##################################
# UNIVARIATE LOGISTIC REGRESSION #
##################################
# Homeless 1
LMhomeless <- svyglm(SSTI.cat~HOMELESS.cat,data=ed0509,design=wt,family=quasibinomial(link="logit"));LMhomeless
summary(LMhomeless)
regTermTest(LMhomeless,~HOMELESS.cat,method="Wald")
round(exp(coef(LMhomeless)),3);round(exp(confint(LMhomeless)),3)
# Homeless 2
LMhomeless2 <- svyglm(SSTI.cat~HOMELESS.cat2,data=ed0509,design=wt,family=quasibinomial(link="logit"),maxit=50);LMhomeless2
summary(LMhomeless2)
regTermTest(LMhomeless2,~HOMELESS.cat2,method="Wald")
round(exp(coef(LMhomeless2)),3);round(exp(confint(LMhomeless2)),3)
# Private Pay
LMpaypriv <- svyglm(SSTI.cat~PAYPRIV,data=ed0509,design=wt,family=quasibinomial,maxit=50);LMpaypriv
summary(LMpaypriv)
round(exp(coef(LMpaypriv)),3);round(exp(confint(LMpaypriv)),3)
# Self Pay
LMpayself <- svyglm(SSTI.cat~PAYSELF,data=ed0509,design=wt,family=quasibinomial,maxit=50);LMpayself
summary(LMpayself)
round(exp(coef(LMpayself)),3);round(exp(confint(LMpayself)),3)
# Sex
LMsex <- svyglm(SSTI.cat~SEX,data=ed0509,design=wt,family=quasibinomial,maxit=50);LMsex
summary(LMsex)
regTermTest(LMsex,~SEX,method="Wald")
round(exp(coef(LMsex)),3);round(exp(confint(LMsex)),3)
# Race
LMrace <- svyglm(SSTI.cat~RACE.cat,data=ed0509,design=wt,family=quasibinomial,maxit=50);LMrace
summary(LMrace)
regTermTest(LMrace,~RACE.cat,method="Wald")
round(exp(coef(LMrace)),3);round(exp(confint(LMrace)),3)

###############################
# MULTIVARIATE LOG REGRESSION #
###############################
glm1 <- svyglm(SSTI.cat~AGER+SEX+RACE.cat+HOMELESS.cat+PAYPRIV+PAYSELF+REGION,
               design=wt,
               family=quasibinomial(link="log"))
summary(glm1)
glm1.step <- stepAIC(glm1)
anova(glm1,glm1.step)

glm2<-svyglm(SSTI.cat~as.numeric(AGE)+SEX+RACE.cat+HOMELESS.cat+PAYPRIV+PAYSELF+REGION,
             design=wt,
             family=quasibinomial(link="logit"))
summary(glm2)
glm2.step <- stepAIC(glm2)
summary(glm2)
anova(glm2,glm2.step)

sum(resid(glm1, type = "pearson")^2) / df.residual(glm1)
glm.diag.plots(glm1)
cit
stepAIC(glm1) #error of missing values
stepAIC(glm.age)
na.omit(ed0509)
anova(ALL)
summary(ALL)
round(exp(coef(ALL)),3);round(exp(confint(ALL,level=0.95)),3)

regTermTest(ALL,~AGE,method="Wald")
regTermTest(ALL,~SEX,method="Wald")
regTermTest(ALL,~RACE.cat,method="Wald")
regTermTest(ALL,~HOMELESS.cat,method="Wald")
regTermTest(ALL,~PAYPRIV,method="Wald")
regTermTest(ALL,~PAYSELF,method="Wald")
