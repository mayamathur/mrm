
# Are the one- and two-stage methods exactly equivalent?

library(crayon)
library(dplyr)
library(foreach)
library(doParallel)
library(boot)
library(metafor)
library(robumeta)
library(data.table)
library(purrr)
library(metRology)
library(fansi)
library(MetaUtility)
library(ICC)
library(cfdecomp)

setwd("~/Dropbox/Personal computer/Independent studies/2020/Meta-regression metrics (MRM)/Code (git)/Simulation study")
source("helper_MRM.R")

# read in the saved example that shows non-equivalence
setwd("~/Dropbox/Personal computer/Independent studies/2020/Meta-regression metrics (MRM)/Code (git)/Auxiliary code/2020-9-15 One-stage vs. two-stage nonequivalence")
d = read.csv("simulated_meta_with_nonequivalence.csv")


# make scen params because prop_stronger_mr expects p to exist
( p = make_scen_params( method = "boot.whole",
                                  calib.method = "MR",

                                  k = c(10),
                                  m = 10,
                                  #m = 100,

                                  b0 = 0, # intercept
                                  bc = 0, # effect of continuous moderator
                                  bb = 0, # effect of binary moderator

                                  zc.star = 0.5,  # level of moderator to consider
                                  zb.star = 1,

                                  zc.ref = 2,  # comparison levels of moderator to consider
                                  zb.ref = 0,

                                  V = c( .01 ), # residual variance
                                  Vzeta = 0,

                                  muN = NA,  # just a placeholder; to be filled in later
                                  minN = c(50),
                                  sd.w = c(1),
                                  tail = "above",
                                  true.effect.dist = c("expo"),
                                  TheoryP = c(0.2),
                                  start.at = 1 ) )
# override the default choice of q (from sim study) for illustrative purposes
# make_prop_stronger looks for q as p$q
p$q = 0.10
p$TheoryP = NA

# # SAVE: code used to generate the example with non-equivalence
# d = sim_data2( k = p$k, 
#                m = p$m,
#                b0 = p$b0, # intercept
#                bc = p$bc, # effect of continuous moderator
#                bb = p$bb, # effect of binary moderator 
#                V = p$V,
#                Vzeta = p$Vzeta,
#                muN = p$muN, 
#                minN = p$minN,
#                sd.w = p$sd.w,
#                true.effect.dist = p$true.effect.dist )




##### Calibrated Estimates Are Non-Equivalent #####
# code taken from prop_stronger_mr
dat = d
yi = dat$yi
vyi = dat$vyi
Zc = dat$Zc
Zb = dat$Zb

m = robu( yi ~ Zc + Zb, 
          data = dat, 
          studynum = cluster,
          var.eff.size = vyi )
bhat0 = m$b.r[1]
bhatc = m$b.r[2]
bhatb = m$b.r[3]
t2 = m$mod_info$tau.sq



# point estimates shifted to have Z = 0
dat$yi.shift = yi - (bhatc*Zc + bhatb*Zb) 

# DL method shifted calibrated estimates
ensDL = calib_ests(yi = dat$yi.shift,
                   sei = sqrt(vyi) )

# MR method shifted calibrated estimates
ensMR = c(bhat0) + sqrt( c(t2) / ( c(t2) + vyi) ) * ( dat$yi.shift - c(bhat0) )

cbind(ensDL, ensMR)

# the non-equivalence here arises because the one-stage approach estimates t2=0,
#  so all calibrated estimates are the same (=0.09), but the two-stage approach
#  estimates t2>0, so the calibrated estimates differ
# and we deliberately chose q to be just above 0.09 



##### Get Meta-Regressive Phat for This Dataset #####
# one-stage
MRres = prop_stronger_mr(d,
                         zc.star = p$zc.star,
                         zb.star = p$zb.star,
                         zc.ref = p$zc.ref,
                         zb.ref = p$zb.ref,
                         calib.method = "MR" )

# two-stage
DLres = prop_stronger_mr(d,
                         zc.star = p$zc.star,
                         zb.star = p$zb.star,
                         zc.ref = p$zc.ref,
                         zb.ref = p$zb.ref,
                         calib.method = "DL" )

# using true parameters
paramsRes = prop_stronger_mr(d,
                 zc.star = p$zc.star,
                 zb.star = p$zb.star,
                 zc.ref = p$zc.ref,
                 zb.ref = p$zb.ref,
                 calib.method = "params" )


# compare the estimates
rbind(MRres, DLres, paramsRes)



