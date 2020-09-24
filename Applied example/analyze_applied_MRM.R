
# code audited 2020-6-17

################################## READ IN DATA ################################## 

library(dplyr)
library(tidyverse)
library(testthat)
library(robumeta)
#library(MetaUtility)
library(boot)
library(metafor)
library(ICC)
library(ggplot2)

# use development version of prop_stronger because clustering addition
#  isn't yet on CRAN
setwd("~/Dropbox/Personal computer/Independent studies/MetaUtility R package/MetaUtility/R")
source("functions.R")

# should we redo the multi-hour bootsrapping for the pointwise CIs on the CDF plots?
bootstrap.plots.from.scratch = FALSE
# bm
boot.reps = 1000

prepped.data.dir = "~/Dropbox/Personal computer/Independent studies/2020/Meta-regression metrics (MRM)/Applied example/Prepped data"
code.dir = "~/Dropbox/Personal computer/Independent studies/2020/Meta-regression metrics (MRM)/Code (git)/Applied example"
overleaf.dir = "~/Dropbox/Apps/Overleaf/Moderators in meta-regression (MRM)"
results.dir = "~/Dropbox/Personal computer/Independent studies/2020/Meta-regression metrics (MRM)/Applied example/Results from R"

setwd(code.dir)
source("helper_applied_MRM.R")

# get the boot fns
setwd("~/Dropbox/Personal computer/Independent studies/2020/Meta-regression metrics (MRM)/Code (git)/Simulation study")
source("helper_MRM.R")
source("bootfuns.R")

setwd(prepped.data.dir)
dh = read.csv("hu_data_prepped.csv")
dm = read.csv("mathur_data_prepped.csv")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#                             EXAMPLE 1: MEMORY CONSOLIDATION            
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

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


( Phat = prop_stronger(q = q, 
                       tail = "above",
                       dat = dh,
                       R = boot.reps,
                       yi.name = "yi",
                       vi.name = "vi",
                       cluster.name = "StudyID") )

# sanity check
ens = calib_ests(dh$yi,
                 sqrt(dh$vi))
mean(ens)  # 0.23: lower than meta-analytic estimate, but still > q
quantile(ens, c(.25, .5, .75))  
mean(ens>q)  # less than 0.5 because of skewness
plot(density(ens))  # quick and dirty plot

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
dhNest = dh %>% group_nest(study)
expect_equal(nrow(dhNest), 87)

boot.res = boot( data = dhNest, 
                 parallel = "multicore",
                 R = boot.reps, 
                 statistic = function(original, indices) {
                    bNest = original[indices,]
                    b = bNest %>% unnest(data)
                    
                    get_phat_hu(dat = b,
                                     q = q,
                                     z = z,
                                     z0 = z0,
                                     return.meta = FALSE)
                 } )

# order of stats:
# Phat, Phat.ref, Phat - Phat.ref
( bootCIs = get_boot_CIs(boot.res, n.ests = 3) )
# point estimates:
stats[[2]]

################################## FIG 1: MARGINAL AND CONDITIONAL CALIBRATED ESTIMATES #################################

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
   


my_ggsave("hu_calib_plot.pdf",
          width = 8,
          height = 1)





################################## FIG 2: COMPLEMENTARY CDF #################################


##### Make Plotting Dataframe #####
q.vec = seq( -1, 1.25, 0.01 )
ql = as.list(q.vec)

z = c(1, 8)
z0 = c(0, 2)  # not actually using this, but just have it to pass to get_phat_hu


Phat.above.vec = lapply( ql,
                         FUN = function(.q) get_phat_hu(dat = dh,
                                                        q = .q,
                                                        z = z,
                                                        z0 = z0,
                                                        return.meta = FALSE)[1] )

res = data.frame( q = q.vec,
                  Est = unlist(Phat.above.vec) )


##### Selective Bootstrapping #####

# # look at just the values of q at which Phat jumps
# #  this will not exceed the number of point estimates in the meta-analysis
res.short = res[ diff(res$Est) != 0, ]


# bootstrap a CI for each entry in res.short
# takes ~2-3 hrs
if (bootstrap.plots.from.scratch == TRUE) {
   temp = res.short %>% rowwise() %>%
      do( phat_ci_hu(.dat = dh, .q = .$q) )
   
   
   temp$q = res.short$q
   temp$Est = 100*temp$Est
   
   # merge this with the full-length res dataframe, merging by Phat itself
   res = merge( res, temp, by.x = "q", by.y = "q")
   
   # # NOT USED?
   # # turn into percentage
   res$Est = 100*res$Est
   res$lo = 100*res$lo
   res$hi = 100*res$hi
   
   setwd(results.dir)
   write.csv(res, "hu_cdf_plot_dataframe.csv")
   
} else {
   setwd(results.dir)
   res = read.csv("hu_cdf_plot_dataframe.csv")
}



##### Make Plot #####
ggplot( data = res,
        aes( x = q,
             y = Est ) ) +
   theme_bw() +
   
   # # pooled point estimate
   # geom_vline( xintercept = exp(mu),
   #             lty = 2,
   #             color = "red" ) +
   
   # null
   geom_vline( xintercept = 0,
               lty = 2,
               color = "black" ) +
   
   scale_y_continuous(  breaks = seq(0, 100, 10) ) +

   scale_x_continuous(  breaks = seq(-1, 1.25, .25) ) +
   
   geom_line(lwd=1.2) +
   
   xlab("Threshold (SMD scale)") +
   ylab( paste( "Estimated percent of effects above threshold" ) ) +
   
   geom_ribbon( aes(ymin=lo, ymax=hi), alpha=0.15, fill = "black" ) 


my_ggsave(name = "hu_cdf_plot.pdf",
          width = 4,
          height = 4)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#                             EXAMPLE 2: MEAT CONSUMPTION            
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #




# # works but is very imprecise CI:
# .covars = c("qual.y.prox2",
#               #"low.miss",
#               "qual.exch2",
#               "qual.gen2",
#               "qual.sdb2",
#               "qual.prereg2",
#               "qual.public.data2"
#               )
# 
# # CI: 36%, 100%
# .covars = c("qual.exch2",
#               "qual.sdb2")
# 
# # CI: 13%, 100%
# .covars = c("qual.y.prox2",
#               "qual.exch2",
#               "qual.gen2",
#               "qual.sdb2")
# 
# # CI: 39%, 100%
# .covars = c("qual.exch2",
#               "qual.y.prox2")
# 
# # **95% (70%, 100%)
# .covars = c("qual.exch2")
# 
# # **91% (25%, 100%)
# .covars = c("qual.y.prox2",
#               #"low.miss",
#               "qual.exch2",
#               "qual.gen2",
#               "qual.sdb2"
#               #"qual.prereg2",
#               #"qual.public.data2"
# )
# 
# 
# ##### ***Social norms in exchangeable studies vs.
# #  studies at low risk on more characteristics (exch, gen, sdb, prox)
# # 93% (63%, 100%)
# .covars = c("qual.exch2",
#               "x.soc.norm" )
# 
# # 90% (11%, 100%)
# .covars = c("qual.y.prox2",
#               "qual.exch2",
#               "qual.gen2",
#               "qual.sdb2",
#               "x.soc.norm")



################################## BASIC STATS ##################################

# will get Phat for two sets of binary covariates in turn
# either just graphic contents ("x.suffer") or graphic contents plus 4 risk-of-bias indicators
covars = list( c("x.suffer"),
               
               c("qual.y.prox2",
                 "qual.exch2",
                 "qual.gen2",
                 "qual.sdb2",
                 "x.suffer") )

# #  studies at low risk on more characteristics (exch, gen, sdb, prox)
# # 70% (48%, 87%)
# .covars = c("x.suffer")
# 
# # 97% 
# .covars = c("qual.y.prox2",
#               "qual.exch2",
#               "qual.gen2",
#               "qual.sdb2",
#               "x.suffer")



################################## PHAT FOR EACH LEVEL OF COVARIATES #################################


for ( i in 1:length(covars) ){
   
   .covars = covars[[i]]
   
   # remove ones missing the quality variables
   dm = dm %>% drop_na(.covars)
   print( dim(dm) ) # 77 had no missing data
   # the 23 exclusions are all due to studies that didn't report amount of missing data)
   
   # proportion high on each quality variable
   print( colMeans( dm %>% select(.covars) ) )
   print( colSums( dm %>% select(.covars) ) )
   
   # sum of graphic and ROB metrics for each study (max 4 of 7)
   dm$qualSum = apply( dm %>% select(.covars), 1, sum )
   
   # studies with graphic content AND fulfilling all ROB criteria
   table(dm$qualSum == length(.covars))
   
   ##### Fit Meta-Regression #####
   ( phat = get_phat_mathur(.dat = dm,
                            .q = log(1.1),
                            .covars = .covars,
                            .return.meta = TRUE) )
   
   
   CI = phat_ci_mathur(.dat = dm,
                       .q = log(1.1),
                       .covars = .covars)
 
   row = data.frame(    bhatGraphic = exp(phat[[1]]$b.r[2]),
                        bhatGraphicLo = exp(phat[[1]]$reg_table$CI.L[2]),
                        bhatGraphicHi = exp(phat[[1]]$reg_table$CI.U[2]),
                        
                        Phat = 100*phat[[2]],
                        PhatLo = 100*CI[[4]][4],
                        PhatHi = 100*CI[[4]][5] )

   if ( i == 1 ) mathur.res = row else mathur.res = rbind(mathur.res, row)
}


setwd(results.dir)
write.csv(mathur.res, "mathur_phat_estimates.csv")



################################## FIG 3: COMPLEMENTARY CDF #################################


##### Make Plotting Dataframe #####
q.vec = seq( log(0.9), log(2), 0.01 )
ql = as.list(q.vec)


Phat.above.vec = lapply( ql,
                         FUN = function(.q) get_phat_mathur(.dat = dm,
                                                            .q = .q,
                                                             .covars = covars[[1]] ) )

res = data.frame( q = q.vec,
                  Est = unlist(Phat.above.vec) )


##### Selective Bootstrapping #####

# # look at just the values of q at which Phat jumps
# #  this will not exceed the number of point estimates in the meta-analysis
res.short = res[ diff(res$Est) != 0, ]


# bootstrap a CI for each entry in res.short
# takes ~2-3 hrs
if (bootstrap.plots.from.scratch == TRUE) {
   temp = res.short %>% rowwise() %>%
      do( phat_ci_mathur(.dat = dm, .covars = covars[[1]], .q = .$q) )
   
   
   temp$q = res.short$q
   temp$Est = 100*temp$Est
   
   # merge this with the full-length res dataframe, merging by Phat itself
   res = merge( res, temp, by.x = "q", by.y = "q")

   # # turn into percentage
   res$Est = 100*res$Est
   res$lo = 100*res$lo
   res$hi = 100*res$hi
   
   setwd(results.dir)
   write.csv(res, "mathur_cdf_plot_dataframe.csv")
   
} else {
   setwd(results.dir)
   res = read.csv("mathur_cdf_plot_dataframe.csv")
}




##### Make Plot #####
ggplot( data = res,
        aes( x = exp(q),
             y = Est ) ) +
   theme_bw() +
   
   # null
   geom_vline( xintercept = 1,
               lty = 2,
               color = "black" ) +
   
   scale_y_continuous( breaks = seq(0, 100, 10) ) +
   
   scale_x_continuous( limits = c(0.9, 2), breaks = seq(0.9, 2, .1) ) +
   
   geom_line(lwd=1.2) +
   
   xlab("Threshold (RR scale)") +
   ylab( paste( "Estimated percent of effects above threshold" ) ) +
   
   geom_ribbon( aes(ymin=lo, ymax=hi), alpha=0.15, fill = "black" ) 




my_ggsave(name = "mathur_cdf_plot.pdf",
          width = 8,
          height = 4)



