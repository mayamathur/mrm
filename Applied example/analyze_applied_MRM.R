
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
library(ggplot2)

prepped.data.dir = "~/Dropbox/Personal computer/Independent studies/2020/Meta-regression metrics (MRM)/Applied example/Prepped data"
code.dir = "~/Dropbox/Personal computer/Independent studies/2020/Meta-regression metrics (MRM)/Code (git)/Applied example"
overleaf.dir = "~/Dropbox/Apps/Overleaf/Moderators in meta-regression"
results.dir = "~/Dropbox/Personal computer/Independent studies/2020/Meta-regression metrics (MRM)/Applied example/Results from R"

setwd(code.dir)
source("helper_applied_MRM.R")

setwd(prepped.data.dir)
dh = read.csv("hu_data_prepped.csv")


################################## BASIC STATS ################################## 

# basic stats for reporting
nrow(dh)  # k
length(unique(dh$StudyID))  # 87 samples

# overall meta-regression
# d = 0.27 [0.19, 0.35]; p<0.0001
robu( yi ~ 1,
      data = dh, 
      studynum = study,  
      var.eff.size = vi )

# their model
# but won't exactly reproduce their reported overall results because we've excluded
#  4 outliers per their moderator analyses
rma.mv(yi=Hedges..g, V = Variances, data=dh, slab=StudyID, random = ~ 1 | StudyID/DependentVariables)


################################## REGULAR PHAT ################################## 

# threshold (Hedges' g)
q = .2

boot.reps = 1000
( Phat = prop_stronger(q = q, 
                       tail = "above",
                       dat = dh,
                       R = boot.reps,
                       yi.name = "yi",
                       vi.name = "vi") )

# sanity check
ens = calib_ests(dh$yi,
                 sqrt(dh$vi))
mean(ens)  # 0.23: lower than meta-analytic estimate, but still > q
quantile(ens, c(.25, .5, .75))  
mean(ens>q)  # less than 0.5 because of skewness
plot(density(ens))

################################## META-REGRESSIVE PHAT AND DIFFERENCE #################################

# compare (SWS, 8h sleep) to (not SWS, 2h sleep)
z = c(1, 8)
z0 = c(0, 2)
stats = get_phat_hu(dat = dh,
                      q = q,
                      z = z,
                      z0 = z0,
                      return.meta = TRUE)

# look at meta-regression coefficients
stats[[1]]

# and Phats
stats[[2]]

# calibrated estimates 
dh$ens.shift = stats[[3]]  # shifted to Z=0
dh$ens.unshift = stats[[4]]  # meta-regressive (unshifted)
dh$ens.std = stats[[5]]  # standard (unshifted)

# shifted threshold for level z
q.shift = stats[[6]]
q.shift.ref = stats[[7]]

# bootstrapped inference
boot.res = boot( data = dh, 
                 parallel = "multicore",
                 R = boot.reps, 
                 statistic = function(original, indices) {
                    b = original[indices,]
                    
                    get_phat_hu(dat = b,
                                     q = q,
                                     z = z,
                                     z0 = z0,
                                     return.meta = FALSE)
                 } )

( bootCIs = get_boot_CIs(boot.res, n.ests = 3) )


################################## PLOT MARGINAL AND CONDITIONAL CALIBRATED ESTIMATES #################################

ggplot( data = dh ) +
   
   # shifted threshold for Z=z
   geom_vline(xintercept = q.shift,
              color = "red",
              lty = 1) +
   
   # shifted threshold for Z=z0
   geom_vline(xintercept = q.shift.ref,
              color = "red",
              lty = 2) +
   
   # ensemble estimates shifted to Z=0
   geom_density( aes( x = ens.shift ),
                 color = "orange",
                 fill = "orange",
                 alpha = 0.3) +
   # geom_density( aes( x = ens.unshift ),
   #               color = "blue" ) +
   
   # regular ensemble estimates
   geom_density( aes( x = ens.std ),
                 color = "black",
                 fill = "black",
                 alpha = 0.3) +

   
   theme_bw() +
   
   xlab("Calibrated estimate") +
   scale_x_continuous( limits = c(-1.25, 1.5), breaks = seq(-1.25, 1.5, 0.25)) +
   
   ylab("Density") +

   theme(axis.text.y = element_blank(),
         axis.ticks = element_blank(),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank())
   


my_ggsave("calib_plot.pdf",
          width = 8,
          height = 1)



##### RESURRECTED FROM BEFORE:

# ################################## MATHUR ################################## 
# 
# ##### Descriptives #####
# setwd(prepped.data.dir)
# dm = read.csv("mathur_data_prepped.csv")
# 
# qual.vars = c("qual.y.prox2",
#               "low.miss",
#               "qual.exch2",
#               "qual.gen2",
#               "qual.sdb2",
#               "qual.prereg2",
#               "qual.public.data2")
# 
# # proportion high on each quality variable
# colMeans( dm %>% select(qual.vars) )
# 
# # sum of ROB metrics for each study (max 4 of 7)
# apply( dm %>% select(qual.vars), 1, sum )  
# 
# 
# ##### Fit Meta-Regression #####
# 
# # BE CAREFUL ABOUT REORDERING OR ADDING VARIABLES HERE - WILL AFFECT BETA'Z BELOW
# m = robu( logRR ~ #randomized +  # too collinear with exch
#              qual.y.prox2 +
#              low.miss +
#              qual.exch2 +
#              qual.gen2 +
#              qual.sdb2 +
#              qual.prereg2 +
#              qual.public.data2,
#           data = dm, 
#           studynum = authoryear,  # ~~~ clustering
#           var.eff.size = varlogRR )
# 
# t2 = m$mod_info$tau.sq
# 
# # linear predictor for being low on all ROB criteria
# # ~~~ report this somewhere?
# exp( sum(m$b.r[2:7]) )
# 
# # bm :)
# 
# ##### Consider a Hypothetical Study with Optimal Risks of Bias #####
# # design matrix of only the moderators
# Z = as.matrix( dm %>% select(qual.vars) )
# head(Z)
# 
# # confirm same ordering
# colnames(Z); m
# 
# # moderator coefficients
# # exclude intercept
# bhat = as.matrix( m$b.r[ 2:( length(qual.vars) + 1 ) ], ncol = 1 )
# 
# dm$linpredZ = Z %*% bhat
# 
# 
# ##### Try Shifting the yis Themselves to Use Existing Package and Sims #####
# dm$yi.shift = dm$logRR - dm$linpredZ  # shifted to have moderators set to 0
# ens.shift = MetaUtility::calib_ests(yi = dm$yi.shift,
#                                     sei = sqrt(dm$varlogRR) )
# 
