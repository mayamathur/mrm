
################################## READ IN DATA ################################## 

library(dplyr)
library(tidyverse)
library(testthat)
library(robumeta)
library(MetaUtility)
library(boot)
library(metafor)

prepped.data.dir = "~/Dropbox/Personal computer/Independent studies/2020/Meta-regression metrics (MRM)/Applied examples/Prepped data"
code.dir = "~/Dropbox/Personal computer/Independent studies/2020/Meta-regression metrics (MRM)/Code (git)/Applied examples"

setwd(code.dir)
source("helper_applied_MRM.R")

setwd(prepped.data.dir)
dr = read.csv("ritchie_data_prepped.csv")

################################## SUMMARY STATS ################################## 

summary(dr$age.fu)
nrow(dr)
length(unique(dr$study))

################################## THEIR ANALYSES ################################## 

##### Get CI for Their Overall Estimate Based on CI #####

# page 1362
3.394 + c(-1, 1) * 0.503 * qnorm(.975)

# page 1363
-0.026 + c(-1, 1) * 0.012 * qnorm(.975)


################################## REGULAR PHAT ################################## 

q = 2  # IQ pts/year
boot.reps = 1000

Phat = prop_stronger(q = q, 
                     tail = "above",
                     dat = dr,
                     R = boot.reps,
                     yi.name = "yi",
                     vi.name = "vi")


################################## META-REGRESSIVE PHAT AND DIFFERENCE ################################## 

# older age at F/U decreases effect size

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


################################## META-REGRESSIVE PHAT AND DIFFERENCE ################################## 

##### Method #1 (As in Sim Study and Code Above) #####
# regress effect size on age at F/U
mod = robu( yi ~ age.fu,
            data = dr, 
            #studynum = 1:nrow(dr),
            studynum = study,  
            var.eff.size = vi )
# coefficient estimate for age at F/U
bhat = mod$b.r[2]
# estimated residual heterogeneity
t2 = mod$mod_info$tau.sq

# calculate beta_1'Z
dr$linpredZ = c(bhat) * dr$age.fu

# calculate point estimates shifted to "set" effect modifiers to 0
# i.e., Equation (S.2) in Appendix
dr$yi.shift = dr$yi - dr$linpredZ  # shifted to have moderators set to 0
ens.shift = MetaUtility::calib_ests(yi = dr$yi.shift,
                                    sei = sqrt(dr$vi) )

q.shift = q - c(bhat) * z

# as in function above
mean(ens.shift > q.shift)


##### Method #2 (Use Meta-Regressive Estimates) #####
meta.DL = rma.uni(yi.shift ~ 1, 
                  vi = vi,
                  data = dr,
                  method =  "DL")

# compare the mu and t2 used in calibrated estimates above vs. 
#  meta-regression
mod$b.r[1]; meta.DL$b
t2; meta.DL$tau2
# ~~~ huh...not very similar! 

# calibrate based on these 
ens.shift2 = c(mod$b.r[1]) + sqrt( c(t2) / ( c(t2) + dr$vi) ) * ( dr$yi.shift - c(mod$b.r[1]) )
mean(ens.shift2 > q.shift); stats[[2]]
# **56% vs. 60%



##### Method #3: No Clustering in Meta-Regression #####
meta.ind = rma.mv( yi,
                   V = vi,
                   mods = age.fu,
                   data = dr )

meta.ind$b[1]; meta.DL$b; mod$b.r[1]
meta.ind$tau2; meta.DL$tau2; t2

# calibrate based on these
ens.shift3 = c(meta.ind$b[1]) + sqrt( c(meta.ind$tau2) / ( c(meta.ind$tau2) + dr$vi) ) * ( dr$yi.shift - c(meta.ind$b[1]) )
mean(ens.shift3 > q.shift); stats[[2]]
# all exactly the same








