
# code audited 2020-6-17

################################## PRELIMINARIES ################################## 

library(dplyr)
library(tidyverse)
library(testthat)
library(readxl)
library(metafor)
library(robumeta)

raw.data.dir = "~/Dropbox/Personal computer/Independent studies/2020/Meta-regression metrics (MRM)/Applied example/Raw data"
prepped.data.dir = "~/Dropbox/Personal computer/Independent studies/2020/Meta-regression metrics (MRM)/Applied example/Prepped data"
code.dir = "~/Dropbox/Personal computer/Independent studies/2020/Meta-regression metrics (MRM)/Code (git)/Applied example"

setwd(code.dir)
source("helper_applied_MRM.R")


################################## PREP HU DATA ################################## 

setwd(raw.data.dir)
dh = read.csv("TMRMetaData_OutcomeEffectSizes.csv")

# quick checks
any(is.na(dh$Hedges..g))
nrow(dh)  # larger than reported 212 because haven't yet made exclusions

# understand the clustering situation
View(dh %>% filter(StudyID ==1))
# this has different DVs from same subjects
# StudyID corresponds to the subject sample

ICCbareF(x = dh$StudyID, y = dh$Hedges..g)

# data cleaning and exclusion steps from their script (00File_Processing.R)
##### BEGIN VERBATIM FROM THEIR SCRIPT:
#StudyID is just another version of ExperimentalID 
#(the experimentalID column only has "1". 
#whilst the StudyID column will have "1","2","3")
#this function will return a df with one more column "StudyID" added to the original df
computeStudyID <- function(df){
  counter = 0
  for (i in 1:nrow(df)){
    #whenever the experimentID is 1, increase the counter
    if (df$ExperimentID[i] == 1 & !is.na(df$ExperimentID[i])){
      counter = counter+1
    }
    df$StudyID[i] = counter
  }
  return(df)
}

#Change those new name back into old names
#names(dh)[names(dh) == 'nameInNewDF'] <- 'nameInOldFile'
names(dh)[names(dh) == 'TMRSleepWake'] <- 'Effect.Size.Type'
names(dh)[names(dh) == 'TMRTime'] <- 'CuingCondition'
names(dh)[names(dh) == 'TMR_CueingStage'] <- 'TMRCueingStages'
names(dh)[names(dh) == 'TMR_Focal'] <- 'Focal'
names(dh)[names(dh) == 'TMR_Design'] <- 'TMR.Design'
names(dh)[names(dh) == 'TMR_OutcomeType'] <- 'DVModerator'
names(dh)[names(dh) == 'TMR_SensoryModality'] <- 'TMR_Modality'

#Use the TMRCueingStages to compute the effect.size.type column
dh$Effect.Size.Type = ifelse(dh$TMRCueingStages=="Wake","WakeTMR","SleepTMR")

#compute the studyID
dh = computeStudyID(dh)

#create a separte file for sleepTMR and WakeTMR
sleepdf_withTactile = subset(dh, Effect.Size.Type == "SleepTMR")
sleepdf = subset(dh, Effect.Size.Type == "SleepTMR")

#Exclude the tactile study and study with unspecified sleep stage from the sleep file
sleepdf = subset(sleepdf, EffectSizeID != "155" & EffectSizeID != "156")
sleepdf = subset(sleepdf, TMRCueingStages != "Unspecified")

wakedf = subset(dh, Effect.Size.Type == "WakeTMR")
##### END VERBATIM FROM THEIR SCRIPT




# per their main_anlysis.R (sic) file, StudyID is their clustering variable:
# meta <- rma.mv(yi=Hedges..g, V = Variances, data=df, slab=StudyID, random = ~ 1 | StudyID/DependentVariables)

# sanity checks on number of estimates and of papers
# reported ("Study and Sample Characteristics"):
#  73 articles/datasets
#  91 experiments
#  111 independent samples
#  212 effect sizes
expect_equal( length(unique(dh$Reference)), 73 )  # I get 72
expect_equal( length(unique(dh$StudyID)), 111 ) # yes :)
expect_equal( nrow(dh), 212 )  # I get 244
# seems like we need one of these two files per the processing script:
# AllTmrFile <- "R2_TMRMetaData_OutcomeEffectSizes_beforeProcess.csv"
# NameAllTmrFile <- "R2_TMRMetaData_AllOutcomeEffectSizes.csv"


# I did this to get the right number of estimates
dh = dh %>% filter( Effect.Size.Type == "SleepTMR" )
# this results in 212 rows and 91 StudyIDs
# unlike their "sleepdf" dataframe, this does not remove outliers yet

# sanity check
# reported (pg 229):
# 0.29 [0.21, 0.38], Z = 6.711
# exactly reproduces with the above filtering
rma.mv(yi=Hedges..g,
       V = Variances,
       data=dh,
       slab=StudyID,
       random = ~ 1 | StudyID/DependentVariables)


##### Rename Variables #####
# verbatim from their script
names(dh)[names(dh) == 'TMRSleepWake'] <- 'Effect.Size.Type'
names(dh)[names(dh) == 'TMRTime'] <- 'CuingCondition'
names(dh)[names(dh) == 'TMR_CueingStage'] <- 'TMRCueingStages'
names(dh)[names(dh) == 'TMR_Focal'] <- 'Focal'
names(dh)[names(dh) == 'TMR_Design'] <- 'TMR.Design'
names(dh)[names(dh) == 'TMR_OutcomeType'] <- 'DVModerator'
names(dh)[names(dh) == 'TMR_SensoryModality'] <- 'TMR_Modality'

# # look at the moderators
# table(dh$Effect.Size.Type)  # sleep/wake
# table(dh$CuingCondition) # not good
# table(dh$TMRCueingStages)  # **good
# table(dh$TMR.Design)
# table(dh$DependentVariables)  # not good
# table(dh$DVModerator) # not good (missing data)
# table(dh$TMR_Modality)  # *maybe good
# summary(dh$Sleep.Time)   # **nap vs. night vs. wake
# summary(dh$Sleep.Length)  # continuous :)
# summary(dh$Age)  # continuous :)


##### Remove Outliers to Match Their k=208 #####
##### BEGIN VERBATIM FROM THEIR SCRIPT:
# ~~~ outlier (verbatim):
#Compute studentized residuals
meta <- rma(yi = Hedges..g, vi = Variances, data = dh)
dh$StudentizedResiduals = rstudent(meta)$z

#Get a subset of the dataset with only outliers
#Studentized Residuals +-3
df_Studentized_Outliers <- subset(dh, StudentizedResiduals < -3 | StudentizedResiduals > 3)
##### END VERBATIM FROM THEIR SCRIPT

dh2 = dh %>% filter( abs(StudentizedResiduals) < 3 )
expect_equal( nrow(dh2), 208 )  # should be 208 per page 231


##### Make and Rename Variables #####
# make my own moderators
dh2$sws = (dh2$TMRCueingStages == "SWS")
dh2$conditioning = (dh2$TMR_LearningType == "Conditioning")

# rename variables
dh2$yi = dh2$Hedges..g
dh2$vi = dh2$Variances
dh2$study = dh2$StudyID

# save prepped data
setwd(prepped.data.dir)
write.csv(dh2, "hu_data_prepped.csv")




################################## PREP MATHUR DATA ################################## 

setwd(raw.data.dir)
dm = read.csv("mathur_prepped_data_lean.csv")
dm = dm %>% filter( exclude.main == FALSE )
expect_equal( nrow(dm), 100 )

qual.vars.raw = c("qual.y.prox",
                  "qual.missing",
                  "qual.exch",
                  "qual.gen",
                  "qual.sdb",
                  "qual.y.prox",
                  "qual.gen",
                  "qual.sdb",
                  "qual.prereg",
                  "qual.public.data")

# recode quality variables as binary
dm$qual.y.prox2 = binary_recode(dm$qual.y.prox, "b.Self-reported"); table(dm$qual.y.prox2, dm$qual.y.prox)
dm$low.miss = dm$qual.missing < 15
dm$qual.exch2 = binary_recode(dm$qual.exch, "a.Low"); table(dm$qual.exch2, dm$qual.exch)
dm$qual.sdb2 = binary_recode(dm$qual.sdb, "a.Low")
dm$qual.gen2 = binary_recode(dm$qual.gen, "a.Low")
dm$qual.prereg2 = binary_recode(dm$qual.prereg, "Yes")
dm$qual.public.data2 = binary_recode(dm$qual.public.data, "Yes")

qual.vars = c("qual.y.prox2",
              "low.miss",
              "qual.exch2",
              "qual.gen2",
              "qual.sdb2",
              "qual.prereg2",
              "qual.public.data2")

# sanity check: check counts against Table 2 from AWR
apply( dm %>% select(qual.vars), 2, function(x) sum(x, na.rm = TRUE) )


setwd(prepped.data.dir)
write.csv(dm, "mathur_data_prepped.csv")
              
              