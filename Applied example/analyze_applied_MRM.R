
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
dm = read.csv("mathur_data_prepped.csv")


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







####### TRY A CDF PLOT:

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

# @temp only
#res.short = res.short[1:2,]

phat_ci = function(.dat, .q){
   #browser()
   datNest = .dat %>% group_nest(study)
   
   boot.res = boot( data = datNest, 
                    parallel = "multicore",
                    R = boot.reps, 
                    statistic = function(original, indices) {
                       bNest = original[indices,]
                       b = bNest %>% unnest(data)
                       
                       get_phat_hu(dat = b,
                                   q = .q,
                                   z = z,
                                   z0 = z0,
                                   return.meta = FALSE)[1]
                    } )
   
   # order of stats:
   # Phat, Phat.ref, Phat - Phat.ref
   bootCIs = get_boot_CIs(boot.res, n.ests = 1)[[1]]
   return( data.frame( lo = bootCIs[1], hi = bootCIs[2] ) )
}

# phat_ci(dh, 0)

# bootstrap a CI for each entry in res.short
# @RUN ME, BUT WILL TAKE HOURSS
temp = res.short %>% rowwise() %>%
   do( phat_ci(.dat = dh, .q = .$q) )


temp$q = res.short$q
temp$Est = 100*temp$Est

# merge this with the full-length res dataframe, merging by Phat itself
res = merge( res, temp, by.x = "q", by.y = "q")

# # NOT USED?
# # turn into percentage
res$Est = 100*res$Est
res$lo = 100*res$lo
res$hi = 100*res$hi

# setwd(results.dir)
# write.csv(res, "npphat_results.csv")


# remove last row because CI is NA
#res = res[ -nrow(res), ]

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
   geom_vline( xintercept = 1,
               lty = 2,
               color = "black" ) +
   
   # scale_y_continuous(  breaks = seq(0, 100, 10) ) +
   # # stop x-axis early because last CI is NA
   # scale_x_continuous(  breaks = seq(0.8, 2, .1) ) +
   
   geom_line(lwd=1.2) +
   
   xlab("Threshold (RR scale)") +
   ylab( paste( "Estimated percent of effects above threshold" ) ) +
   
   geom_ribbon( aes(ymin=res$lo, ymax=res$hi), alpha=0.15, fill = "black" ) 


setwd(results.dir)
ggsave( "npphat.pdf",
        width = 9,
        height = 6 )

setwd(overleaf.dir)
ggsave( "npphat.pdf",
        width = 9,
        height = 6 )





##### RESURRECTED FROM BEFORE:

################################## MATHUR ##################################

# # works but is very imprecise CI:
# qual.vars = c("qual.y.prox2",
#               #"low.miss",
#               "qual.exch2",
#               "qual.gen2",
#               "qual.sdb2",
#               "qual.prereg2",
#               "qual.public.data2"
#               )
# 
# # CI: 36%, 100%
# qual.vars = c("qual.exch2",
#               "qual.sdb2")
# 
# # CI: 13%, 100%
# qual.vars = c("qual.y.prox2",
#               "qual.exch2",
#               "qual.gen2",
#               "qual.sdb2")
# 
# # CI: 39%, 100%
# qual.vars = c("qual.exch2",
#               "qual.y.prox2")
# 
# # **95% (70%, 100%)
# qual.vars = c("qual.exch2")
# 
# # **91% (25%, 100%)
# qual.vars = c("qual.y.prox2",
#               #"low.miss",
#               "qual.exch2",
#               "qual.gen2",
#               "qual.sdb2"
#               #"qual.prereg2",
#               #"qual.public.data2"
# )


##### ***Social norms in exchangeable studies vs.
#  studies at low risk on more characteristics (exch, gen, sdb, prox)
# 93% (63%, 100%)
qual.vars = c("qual.exch2",
              "x.soc.norm" )

# 90% (11%, 100%)
qual.vars = c("qual.y.prox2",
              "qual.exch2",
              "qual.gen2",
              "qual.sdb2",
              "x.soc.norm")


#  studies at low risk on more characteristics (exch, gen, sdb, prox)
# 70% (48%, 87%)
qual.vars = c("x.suffer" )

# 97% 
qual.vars = c("qual.y.prox2",
              "qual.exch2",
              "qual.gen2",
              "qual.sdb2",
              "x.suffer")


##### Descriptives #####
setwd(prepped.data.dir)
dm = read.csv("mathur_data_prepped.csv")
# remove ones missing the quality variables
dm = dm %>% drop_na(qual.vars)
dim(dm)  # 77 had no missing data
# the 23 exclusions are all due to studies that didn't report amount of missing data)


# proportion high on each quality variable
colMeans( dm %>% select(qual.vars) )
colSums( dm %>% select(qual.vars) )


# sum of graphic and ROB metrics for each study (max 4 of 7)
dm$qualSum = apply( dm %>% select(qual.vars), 1, sum )

# studies with graphic content AND fulfilling all ROB criteria
table(dm$qualSum == length(qual.vars))

##### Fit Meta-Regression #####




( phat = get_phat_mathur(.dat = dm, .return.meta = TRUE) )


exp(phat[[1]]$b.r[2])
exp(phat[[1]]$reg_table$CI.L[2])
exp(phat[[1]]$reg_table$CI.U[2])

###### Sim study code

# get the boot fns
setwd("~/Dropbox/Personal computer/Independent studies/2020/Meta-regression metrics (MRM)/Code (git)/Simulation study")
source("helper_MRM.R")
source("bootfuns.R")

# nest by cluster in case we need to do cluster bootstrap
# now has one row per cluster
# works whether there is clustering or not
# because without clustering, the clusters are 1:nrow(data)

d = dm
d$cluster = d$authoryear

dNest = d %>% group_nest(cluster)

# @INCREASE LATER
boot.reps = 500

# this is just the resampling part, not the CI estimation
boot.res = my_boot( data = dNest, 
                    parallel = "multicore",
                    R = boot.reps, 
                    statistic = function(original, indices) {
                       

                          bNest = original[indices,]
                          b = bNest %>% unnest(data)
                       
                       tryCatch({
    
                          return( get_phat_mathur2(b) )
                          # 
                          # # return the stats of interest
                          # # order of stats has to match indices in CI tryCatch loops below
                          # # and in returned results because of bt.means and bt.sds
                          # c( as.numeric(b.stats["Phat"]),
                          #    as.numeric(b.stats["Phat.ref"]),
                          #    as.numeric(b.stats["Phat.diff"]),
                          #    as.numeric(b.stats["t2"]),
                          #    truncLogit( as.numeric(b.stats["Phat"]) ) # transformed Phat
                          # )
                       }, error = function(err){
                          
                          return( rep(NA, 1) )
                       })
                       
                    } )
boot.res


# boot diagnostics
bt.means = as.numeric( colMeans(boot.res$t, na.rm = TRUE) )
bt.sds = apply( boot.res$t, 2, function(x) sd(x, na.rm = TRUE) )

CI = boot.ci(boot.res, type = "bca", index = 1)
# put in nice vector format
( PhatBootCIs = c( CI[[4]][4], CI[[4]][5] ) )



