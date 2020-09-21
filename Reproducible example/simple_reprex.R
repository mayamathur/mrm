# code audited 2020-6-17

############################ LOAD PACKAGES ############################
library(robumeta)
library(metafor)
library(boot)
library(dplyr)
library(tidyr)

############################ HELPER FUNCTIONS ############################
##### Get BCa-bootstrapped confidence intervals for a vector of parameters (not just one) #####
# boot.res: an object from boot()
# type: what kind of bootstrapping to use? (as in boot.ci())
# n.ests: how many parameters were estimated?
get_boot_CIs = function(boot.res,
                        type = "bca",
                        n.ests) {
  bootCIs = lapply( 1:n.ests, function(x) boot.ci(boot.res, type = type, index = x) )
  
  # list with one entry per estimate
  # the middle index "4" on the bootCIs accesses the stats vector
  # the final index chooses the CI lower (4) or upper (5) bound
  bootCIs = lapply( 1:n.ests, function(x) c( bootCIs[[x]][[4]][4],
                                             bootCIs[[x]][[4]][5] ) )
}

##### Return Phat(z), Phat(z0), and Phat(z) - Phat(z0) #####
# important: this function would need to be modified somewhat to use it on another dataset (see comments therein)
# dat: the dataset
# cluster.var: the name of the variable denoting clusters, if any
#  if left to default NA, assumes studies are independent
# q: threshold for a meaningfully strong effect size (function assumes we are considering effects ABOVE the threshold)
# z: level of effect modifiers of interest (vector)
# z0: reference level of effect modifiers (vector)
# return.meta: should the meta-regression itself be returned, or just the percentages?
get_phat_reprex = function(dat,
                           cluster.var = NA,
                           q,
                           z,
                           z0,
                           return.meta = FALSE){
  
  if ( is.na(cluster.var) ) dat$cluster = 1:nrow(dat) else dat$cluster = dat[[cluster.var]]
  
  # fit the meta-regression
  # linear predictor would need to be modified for other datasets
  mod = robu( yi ~ Zc + Zb,
              studynum = cluster,  # account for clustering
              data = dat, 
              var.eff.size = vyi,
              small = TRUE )
  
  bhat = mod$b.r  # vector of coefficient estimates 
  t2 = mod$mod_info$tau.sq  # heterogeneity estimate
  
  # calculate the linear predictor (minus intercept) for each study
  # would need to be modified for other datasets
  dat$linpredZ = ( c(bhat[2]) * dat$Zc ) + ( c(bhat[3]) * dat$Zb )
  
  ##### Phat(z) #####
  # point estimate, shifted to set effect modifiers to 0
  dat$yi.shift = dat$yi - dat$linpredZ  
  
  # calibrated estimate, shifted to set effect modifiers to 0
  calib.shift = c(bhat[1]) + sqrt( c(t2) / ( c(t2) + dat$vyi) ) * ( dat$yi.shift - c(bhat[1]) )
  
  # threshold, shifted to set effect modifiers to 0
  # would need to be modified for other datasets
  q.shift = q - ( bhat[2]*z[1] + bhat[3]*z[2] )  
  # note: below assumes we are considering effects ABOVE the threshold
  Phat = mean( calib.shift > c(q.shift) )
  
  ##### Phat(z0) #####
  # reference level 
  # would need to be modified for other datasets
  q.shift.ref = q - ( bhat[2]*z0[1] + bhat[3]*z0[2] )
  Phat.ref = mean( calib.shift > c(q.shift.ref) )
  
  ##### Return Results #####
  if ( return.meta == FALSE){
    return( c(Phat, Phat.ref, Phat - Phat.ref) )
  } else {
    return( list(mod,
                 c(Phat, Phat.ref, Phat - Phat.ref)) )
  }
}


############################ ANALYZE FAKE DATA ############################
# read in the fake data
# these were generated similarly to data from the simulation study
#  with tau^2 = 0.04, E[N] = 100, normal true effects, and clustering
# variable names:
# yi: point estimate
# vyi: variance
# Zc: continuous effect modifier
# Zb: binary effect modifier

# set your working directory to contain the fake data:
d = read.csv("reprex_fake_data.csv")
head(d)

# define comparisons of interest
q = .2  # threshold of interest
z = c(1, 1)  # effect modifier level of interest
z0 = c(2.5, 0)  # effect modifier reference level to be compared to level of interest

##### Point Estimates #####
# fit meta-regression and get point estimates for Phat(z), Phat(z0), and Phat(z) - Phat(z0)
stats = get_phat_reprex(dat = d,
                        cluster.var = "cluster",
                        q = q,
                        z = z,
                        z0 = z0,
                        return.meta = TRUE)

# look at meta-regression coefficients
stats[[1]]

# Phat(z), Phat(z0), and Phat(z) - Phat(z0) respectively
stats[[2]]

##### Bootstrapped Inference #####
# number of iterates: should be much higher in practice (e.g., 1,000),
#  but kept small here for illustrative purposes
boot.reps = 300  

# to easily allow for cluster-bootstrapping, nest the data
# now has one row per cluster
# works whether there is clustering or not
# because without clustering, the clusters are 1:nrow(data)
dNest = d %>% group_nest(cluster)
boot.res = boot( data = dNest, 
                 parallel = "multicore",
                 R = boot.reps, 
                 statistic = function(original, indices) {
                   # resample clusters with replacement
                   bNest = original[indices,]
                   b = bNest %>% unnest(data)
                   
                   get_phat_reprex(dat = b,
                                   cluster.var = "cluster",
                                   q = q,
                                   z = z,
                                   z0 = z0,
                                   return.meta = FALSE) } )
# note: this simple bootstrapping script could, for some datasets, hit errors if the meta-regression 
#  model can't be fit for some resamples
# for a more sophisticated bootstrapping script that can proceed even if certain iterates fail, 
#  or if the BCa confidence interval construction works for some of the 3 estimators but not all,
#  see the simulation study script "helper_MRM.R" and "doParallel.R"

# get the bootstrapped CIs for all 3 statistics respectively using BCa method
( bootCIs = get_boot_CIs(boot.res, n.ests = 3) )

##### All Results #####
# Phat(z) and its CI limits
stats[[2]][1]; bootCIs[[1]]

# Phat(z0) and its CI limits
stats[[2]][2]; bootCIs[[2]]

# Phat(z) - Phat(z0) and its CI limits
stats[[2]][3]; bootCIs[[3]]