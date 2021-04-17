




# Compare tau^2 estimate from robumeta with same data, but with and without specifying clusters

library(robumeta)
library(metafor)
library(ICC)


setwd("~/Dropbox/Personal computer/Independent studies/2020/Meta-regression metrics (MRM)/Code (git)/Simulation study")
source("helper_MRM.R")

# simulate clustered data
d = sim_data2( k = 45, 
               m = 15,  # clusters
               b0 = 0, # intercept
               bc = 0, # effect of continuous moderator
               bb = 0, # effect of binary moderator 
               V = 0.3,
               Vzeta = 0.2,
               muN = 100, 
               minN = 50,
               sd.w = 1,
               true.effect.dist = "normal" )

# robumeta with cluster spec.
mod1 =  robu(formula = yi ~ 1,
             data = d,
             studynum = cluster, 
             var.eff.size = vyi,
             modelweights = "HIER",
             small = FALSE)

mod1$mod_info$tau.sq


# robumeta without cluster spec.
# equivalent to DL below when it's not set to 0
mod2 =  robu(formula = yi ~ 1,
             data = d,
             studynum = 1:nrow(d), 
             var.eff.size = vyi,
             modelweights = "HIER",
             small = FALSE)

mod2$mod_info$tau.sq


# DL
mod3 = rma.uni( yi = yi,
                vi = vyi,
                method = "DL", 
                data = d )

mod3$tau2


# REML
mod4 = rma.uni( yi = yi,
         vi = vyi,
         method = "REML", 
         data = d )

mod4$tau2




