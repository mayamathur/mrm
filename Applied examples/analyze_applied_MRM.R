
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

################################## MATHUR ################################## 

##### Descriptives #####
setwd(prepped.data.dir)
dm = read.csv("mathur_data_prepped.csv")

qual.vars = c("qual.y.prox2",
              "low.miss",
              "qual.exch2",
              "qual.gen2",
              "qual.sdb2",
              "qual.prereg2",
              "qual.public.data2")

# proportion high on each quality variable
colMeans( dm %>% select(qual.vars) )

# sum of ROB metrics for each study (max 4 of 7)
apply( dm %>% select(qual.vars), 1, sum )  


##### Fit Meta-Regression #####

# BE CAREFUL ABOUT REORDERING OR ADDING VARIABLES HERE - WILL AFFECT BETA'Z BELOW
m = robu( logRR ~ #randomized +  # too collinear with exch
            qual.y.prox2 +
            low.miss +
            qual.exch2 +
            qual.gen2 +
            qual.sdb2 +
            qual.prereg2 +
            qual.public.data2,
          data = dm, 
          studynum = authoryear,  # ~~~ clustering
          var.eff.size = varlogRR )

t2 = m$mod_info$tau.sq

# linear predictor for being low on all ROB criteria
# ~~~ report this somewhere?
exp( sum(m$b.r[2:7]) )

# bm :)

##### Consider a Hypothetical Study with Optimal Risks of Bias #####
# design matrix of only the moderators
Z = as.matrix( dm %>% select(qual.vars) )
head(Z)

# confirm same ordering
colnames(Z); m

# moderator coefficients
# exclude intercept
bhat = as.matrix( m$b.r[ 2:( length(qual.vars) + 1 ) ], ncol = 1 )

dm$linpredZ = Z %*% bhat


##### Try Shifting the yis Themselves to Use Existing Package and Sims #####
dm$yi.shift = dm$logRR - dm$linpredZ  # shifted to have moderators set to 0
ens.shift = MetaUtility::calib_ests(yi = dm$yi.shift,
                               sei = sqrt(dm$varlogRR) )


# yes, agrees with the above! 
q = log(1.2)
# sum because all quality vars are coded such that 1 is good
q.shift = q - ( sum(m$b.r) - m$b.r[1] )  # remove the intercept from sum(m$b.r)
( Phat = mean(ens.shift > q.shift) )


boot.reps = 1000

boot.res = boot( data = dm, 
                 parallel = "multicore",
                 R = boot.reps, 
                 statistic = function(original, indices) {
                   b = original[indices,]
                   
                   b.stats = get_phat_mathur(dat = b,
                                              q = q)
                  
                 } )

# for debugging
#head( boot.res$t )

# **it's the bootstrapping wrt q.shift, NOT so much yi.shift, that really widens the CI

boot.ci(boot.res, type = "bca")

bootCIs = get_boot_CIs(boot.res, "bca", n.ests = 1)


# c.f. main analysis in paper for RR > 1.2 (page 19): 53% (95% CI: [37%, 65%])



get_phat_mathur(dm, q)  # should match the direct approach

# with whole bootstrapping: [7%, 100%]
# without bootstrapping the meta-regression part (i.e., reuse initial estimates): [1%, 100%]

# just out of curiosity, compare to inference that ignores the meta-regressive
#  estimation of regression coefficients
# Proportion of completely high-quality studies above RR = 1.2:
# **82% [65%, 95%]
# main analysis in paper (page 19): 53% (95% CI: [37%, 65%])
MetaUtility::prop_stronger(q = q.shift,
                           tail = "above",
                           dat = dm,
                           yi.name = "yi.shift",
                           vi.name = "varlogRR",
                           R = 1000) 
# CI basically uninformative here :(


# note that the linear predictor assumes that biases are essentially additive on the log-RR scale
#  i.e., multiplicative on the RR scale






