
# code audited 2020-6-17

################################## READ IN DATA ################################## 

library(dplyr)
library(tidyverse)
library(testthat)
library(robumeta)
library(MetaUtility)
library(boot)
library(metafor)
library(ICC)

prepped.data.dir = "~/Dropbox/Personal computer/Independent studies/2020/Meta-regression metrics (MRM)/Applied example/Prepped data"
code.dir = "~/Dropbox/Personal computer/Independent studies/2020/Meta-regression metrics (MRM)/Code (git)/Applied example"

setwd(code.dir)
source("helper_applied_MRM.R")

setwd(prepped.data.dir)
dr = read.csv("ritchie_data_prepped.csv")

################################## DESCRIPTIVE ################################## 

# effect modifier of interest
summary(dr$age.fu)

# just curious how early the education clock started in these studies:
# for control-prior-IQ studies, the age of baseline IQ (max 20)
summary(dr$Age.at.early.test)
# for policy-change studies, age at intervention (max 19)
summary(dr$Age.at.intervention)
dr %>% filter( !is.na(Age.at.early.test) & Age.at.early.test>19 )

# ICC of estimates within "studies" (datasets)
ICCbareF(dr$study, dr$yi)

################################## THEIR ANALYSES ################################## 

# get CI for their estimates based on reported SEs
# page 1362: overall estimate of education effect
3.394 + c(-1, 1) * 0.503 * qnorm(.975)

# sanity check: reproduce this
# note that they used SEM to fit the meta-analysis, so won't match exactly
robu( yi ~ 1,
      data = dr, 
      studynum = study,  
      var.eff.size = vi )
rma.uni(yi, vi, data = dr, method = "DL")

# page 1363: estimate of age decline among longitudinal studies
-0.026 + c(-1, 1) * 0.012 * qnorm(.975)

# reproduce this
robu( yi ~ age.fu,
      data = dr %>% filter(Study.design == "Control Prior IQ"), 
      studynum = study,  
      var.eff.size = vi )


################################## REGULAR PHAT ################################## 

# threshold (IQ points/yr of education)
q = 1 

# describe threshold in terms of SMDs
iq.sd = 15  # by definition

# convert q to SMD for a given number of additional years of education
yrs = 5
yrs*q / iq.sd
# or solve for the number of years equal to a certain d
d = .2
(d*iq.sd)/q

boot.reps = 1000
( Phat = prop_stronger(q = q, 
                       tail = "above",
                       dat = dr,
                       R = boot.reps,
                       yi.name = "yi",
                       vi.name = "vi") )


################################## META-REGRESSIVE PHAT AND DIFFERENCE ################################## 

# compare effect at age 50 to effect at age 10
z = 50
z0 = 10
( stats = get_phat_ritchie(dat = dr,
                           q = q,
                           z = z,
                           z0 = z0,
                           return.meta = TRUE) )

# look at meta-regression coefficients
stats[[1]]

# and Phats
stats[[2]]

# bootstrapped inference
boot.res = boot( data = dr, 
                 parallel = "multicore",
                 R = boot.reps, 
                 statistic = function(original, indices) {
                   b = original[indices,]
                   
                   get_phat_ritchie(dat = b,
                                   q = q,
                                   z = z,
                                   z0 = z0,
                                   return.meta = FALSE)
                 } )

( bootCIs = get_boot_CIs(boot.res, n.ests = 3) )
