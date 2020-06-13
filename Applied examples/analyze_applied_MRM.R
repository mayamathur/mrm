
##### Possible Applied Examples #####
# - PS2: Education & IQ (k=142, very nice dataset, continuous age was a moderator of interest
#  could also fix levels of control for prior IQ and mixed-sex)

library(dplyr)
library(tidyverse)
library(testthat)
library(robumeta)
library(MetaUtility)
library(boot)

prepped.data.dir = "~/Dropbox/Personal computer/Independent studies/2020/Meta-regression metrics (MRM)/Applied examples/Prepped data"
code.dir = "~/Dropbox/Personal computer/Independent studies/2020/Meta-regression metrics (MRM)/Code (git)/Applied examples"

setwd(code.dir)
source("helper_applied_MRM.R")


################################## RITCHIE ################################## 

summary(dr$age.fu)

# older age at F/U decreases effect size
q = 2  # IQ pts/year
z = 50
z0 = 10
( stats = get_phat_ritchie(dat = dr,
                           q = q,
                           z = z,
                           z0 = z0,
                           return.meta = TRUE) )

boot.reps = 1000

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

# age 10: 60% [52%, 76%] above 2 IQ pts/yr
# age 50: 92% [84%, 94%]
# difference (50 vs. 10): -32% [-40%, -16%]

