
# "**" = result reported in paper

################################## PRELIMINARIES ##################################

library(dplyr)
library(xtable)

prepped.data.dir = "~/Dropbox/Personal computer/Independent studies/2020/Meta-regression metrics (MRM)/Simulation study results"
results.dir = "~/Dropbox/Personal computer/Independent studies/2020/Meta-regression metrics (MRM)/Simulation study results"
#overleaf.dir = /Users/mmathur/Dropbox/Apps/Overleaf/Moderators\ in\ meta-regression/From\ R

setwd(prepped.data.dir)
agg = read.csv("*agg_dataset_as_analyzed.csv")


################################## I^2 PARAMETERIZATION OF HETEROGENEITY ##################################

I2 = function(t2, N) {
  t2 / (t2 + 4/N)
}


# for each possible sample size in our sims, convert all the heterogeneity 
#  parameters to I^2
round( I2( t2 = c(.01, .04, .25),
           N = 100), 2 )

round( I2( t2 = c(.01, .04, .25),
           N = 850), 2 )
# so the tau = 0.5 (t2 = 0.25) scenarios are approximately I^2 = 50%
# which is exactly Higgins' (2003) benchmark for "moderate" heterogeneity :)



################################## STATS AND TABLES FOR PAPER ##################################

##### Which Scen Params Predict Coverage And Bias? #####
# coverage of difference
mod = lm( CoverDiff ~ k + minN + true.effect.dist + V + TheoryP + TheoryDiff,
          data = agg )
summary(mod)
# **things that IMPROVE coverage: larger k, smaller V, larger TheoryP, smaller TheoryDiff
# **things that don't matter much: minN, true effect distribution

# and for bias
mod = lm( PhatAbsBias ~ k + minN + true.effect.dist + V + TheoryP + TheoryDiff,
          data = agg )
summary(mod)


##### All Scenarios #####

# just the summary stats
data.frame( agg %>%
  group_by(TRUE) %>%  # for some reason, summarise doesn't work without grouping...
  summarise( n.scens = n(),
             
             PhatAbsBias = mean(PhatAbsBias, na.rm = TRUE),
             MeanCoverPhat = mean(CoverPhat, na.rm = TRUE),
             MinCoverPhat = min(CoverPhat, na.rm = TRUE),
             
             PhatRefAbsBias = mean(PhatRefAbsBias, na.rm = TRUE),
             MeanCoverPhatRef = mean(CoverPhatRef, na.rm = TRUE),
             MinCoverPhatRef = min(CoverPhatRef, na.rm = TRUE),
             
             DiffAbsBias = mean(DiffAbsBias, na.rm = TRUE),
             MeanCoverDiff = mean(CoverDiff, na.rm = TRUE),
             MinCoverDiff = min(CoverDiff, na.rm = TRUE) ) )

# table for paper
t1 = data.frame( agg %>%
                   group_by(k, V) %>%  # for some reason, summarise doesn't work without grouping...
                   summarise( n.scens = n(),
                              
                              PhatAbsBias = mean(PhatAbsBias, na.rm = TRUE),
                              MeanCoverPhat = mean(CoverPhat, na.rm = TRUE),
                              MinCoverPhat = min(CoverPhat, na.rm = TRUE),
                              
                              PhatRefAbsBias = mean(PhatRefAbsBias, na.rm = TRUE),
                              MeanCoverPhatRef = mean(CoverPhatRef, na.rm = TRUE),
                              MinCoverPhatRef = min(CoverPhatRef, na.rm = TRUE),
                              
                              DiffAbsBias = mean(DiffAbsBias, na.rm = TRUE),
                              MeanCoverDiff = mean(CoverDiff, na.rm = TRUE),
                              MinCoverDiff = min(CoverDiff, na.rm = TRUE) ) )

t1 = round(t1, 2)
print( xtable(t1), include.rownames = FALSE )


##### k>=100, TheoryDiff > 0.05 #####

# just the summary stats
data.frame( agg %>%
  #filter( k >= 100 & TheoryDiff >.05) %>%
  filter( k >= 100 & TheoryDiff >.05) %>%
  group_by(TRUE) %>%  # for some reason, summarise doesn't work without grouping...
  summarise( n.scens = n(),
             
             PhatAbsBias = mean(PhatAbsBias, na.rm = TRUE),
             MeanCoverPhat = mean(CoverPhat, na.rm = TRUE),
             MinCoverPhat = min(CoverPhat, na.rm = TRUE),

             PhatRefAbsBias = mean(PhatRefAbsBias, na.rm = TRUE),
             MeanCoverPhatRef = mean(CoverPhatRef, na.rm = TRUE),
             MinCoverPhatRef = min(CoverPhatRef, na.rm = TRUE),

             DiffAbsBias = mean(DiffAbsBias, na.rm = TRUE),
             MeanCoverDiff = mean(CoverDiff, na.rm = TRUE),
             MinCoverDiff = min(CoverDiff, na.rm = TRUE) ) )

# table for paper
t2 = data.frame( agg %>%
              filter( k >= 100 & TheoryDiff >.05) %>%
              group_by(k, V) %>%  # for some reason, summarise doesn't work without grouping...
              summarise( n.scens = n(),
                         
                         PhatAbsBias = mean(PhatAbsBias, na.rm = TRUE),
                         MeanCoverPhat = mean(CoverPhat, na.rm = TRUE),
                         MinCoverPhat = min(CoverPhat, na.rm = TRUE),
                         
                         PhatRefAbsBias = mean(PhatRefAbsBias, na.rm = TRUE),
                         MeanCoverPhatRef = mean(CoverPhatRef, na.rm = TRUE),
                         MinCoverPhatRef = min(CoverPhatRef, na.rm = TRUE),
                         
                         DiffAbsBias = mean(DiffAbsBias, na.rm = TRUE),
                         MeanCoverDiff = mean(CoverDiff, na.rm = TRUE),
                         MinCoverDiff = min(CoverDiff, na.rm = TRUE) ) )
t2 = round(t2, 2)
print( xtable(t2), include.rownames = FALSE )


# 
# 
# #################### FROM NPPHAT: 
# # Note: "**" denotes key results reported in paper
# 
# # avoid saving old copies of objects
# #rm(list=ls())
# 
# library(dplyr)
# library(metRology)
# library(ggplot2)
# library(data.table)
# 
# # directories
# code.dir = "~/Dropbox/Personal computer/Independent studies/Nonparametric Phat (NPPhat)/Linked to OSF (NPPhat)/Code (on git)/Simulation study/Simulation code"
# data.dir =  "~/Dropbox/Personal computer/Independent studies/Nonparametric Phat (NPPhat)/Linked to OSF (NPPhat)/Simulation results/**2019-9-1 merged results in paper/Stitched data"
# results.dir = "~/Dropbox/Personal computer/Independent studies/Nonparametric Phat (NPPhat)/Linked to OSF (NPPhat)/Code (on git)/Simulation study/Simulation results/**2019-9-1 merged results in paper"
# # where to save the simulated data for density plots
# results.dist.dir = "~/Dropbox/Personal computer/Independent studies/Nonparametric Phat (NPPhat)/Linked to OSF (NPPhat)/Code (on git)/Simulation study/Simulated data to show distributions"
# 
# # helper fn
# setwd(code.dir)
# source("functions_analysis_RRR.R")
# source("functions_RRR.R")  # for sim_data
# 
# 
# # should we re-simulate data for the example density plots of the 4 distributions (slow)?
# simulate.dist.from.scratch = FALSE
# 
# # should the four datasets of simulation results be read in and merged from scratch (somewhat slow)?
# merge.from.scratch = FALSE
# 
# 
# #################### READ IN DATA ####################
# 
# # merge data from the four simulation datasets
# if (merge.from.scratch == TRUE) {
#   setwd(data.dir)
#   library(data.table)
#   s1 = fread("stitched_expo_normal.csv")
#   names(s1) = as.character(s1[1,])
#   s1 = as.data.frame(s1[-c(1), -1])
#   s1$sim_group = 1  # identify which "group" of simulations this belongs to
#   
#   s2 = fread("stitched_unif_t.csv")
#   #names(s2) = as.character(s2[1,])
#   s2 = as.data.frame(s2[-c(1), -1])
#   s2$sim_group = 2  # identify which "group" of simulations this belongs to
#   
#   s3 = fread("stitched_all4_ensemble.csv")
#   names(s3) = as.character(s3[2,])
#   s3 = as.data.frame(s3[-c(1:2), -1])
#   s3$sim_group = 3  # identify which "group" of simulations this belongs to
#   # fix stupid names situation
#   
#   s4 = fread("stitched_fill_missing.csv")
#   s4 = as.data.frame(s4[, -1])
#   s4$sim_group = 4  # identify which "group" of simulations this belongs to
#   
#   #s = rbind(s1, s2, s3, s4)
#   s = rbindlist(list(s1, s2, s3, s4), fill = TRUE)
#   
#   write.csv(s,
#             "overall_stitched.csv",
#             row.names = FALSE)
# } else {
#   # read in existing data
#   setwd(data.dir)
#   s = as.data.frame( fread("overall_stitched.csv") )
# }
# 
# 
# 
# #################### DATA CLEANING AND QUALITY CHECKS ####################
# 
# # remove erroneously-written rows with nonsense scenario names (e.g., "TheoryP")
# table( s$scen.name )
# s = s[ !is.na( as.numeric(s$scen.name) ), ]
# s = droplevels(s)
# 
# # rename the scenarios
# # note that some identical scenarios have different names
# #  this is an artifact from having run multiple sets of simulations
# s = s %>% group_by(k, minN, V, true.effect.dist, TheoryP) %>%  # these are all the variables defining unique scenarios
#   mutate(scen.name.2 = group_indices())
# 
# 
# # how many reps were run per scenario and Method?
# reps = s %>% group_by(scen.name.2, Method) %>%
#   summarise( n = n() ) %>%
#   arrange(n)
# #View(reps)
# # scenarios with too few reps for any method
# # these are due to bootstrapping failures
# length( unique( reps$scen.name.2[ reps$n < 500 ] ) )
# 
# 
# # proportion of times the ensemble bootstrap failed
# ( boot.failures = s %>% filter(sim_group == 3) %>%  # only look at reps from last group; otherwise they wouldn't have attempted bootstrap
#     group_by(k, TheoryP) %>%
#     filter(Method == "NP ensemble") %>%
#     summarise( boot.problem = sum(!is.na(Note)) / length(Note) ) )
# #View(boot.failures)
# 
# # ** overall mean boot failures (reported in Figure 1)
# # only use sim groups 3 and 4, where NP-ensemble actually was bootstrapped
# mean( is.na( s$Note[s$sim_group %in% c(3,4) & s$Method == "NP ensemble"] ) )
# 
# 
# # make sure variables are correct type
# s = as.data.frame(s)
# ( make.logical = names(s)[ grepl("Cover", names(s)) ] )
# ( make.numeric = names(s)[ !names(s) %in% c("Method",
#                                             "Note",
#                                             "tail",
#                                             "true.effect.dist") ] )
# # for logicals, first make them logical and then numeric
# s = s %>%
#   mutate_at(make.logical, as.logical) %>%
#   mutate_at(make.numeric, as.numeric) 
# 
# 
# # sanity check for data generation: TheoryP and TruthP should be close
# # keep close eye on tau^2 = 0.01 scenarios because we 
# #  reject samples with tau = 0 estimate
# # minimal bias in these scenarios :) 
# s %>% group_by(V, TheoryP) %>%
#   summarise( TruthP = mean(TruthP) )
# 
# # absolute bias
# s$phatAbsBias = abs(s$phatBias)
# 
# 
# 
# #################### AGGREGATE BY SCENARIO ####################
# 
# # for plotting joy
# s$plot.panel = paste( expression(tau^2), " = ", s$V, 
#                       "; q = ", s$q,
#                       "; true P = ", s$TheoryP,
#                       sep = "" )
# 
# # ** summarize results by method and scenario
# res.all = s %>%
#   filter(!is.na(Method)) %>%  # do not remove this -- behaves strangely if you do, despite use of na.rm below
#   group_by(scen.name.2, Method) %>%
#   group_by(k, V, minN, TheoryP, true.effect.dist, Method) %>%  # all variables that define unique scenarios
#   mutate(EmpVar = var(phat),
#          n.cover = sum( !is.na(Cover) ),
#          n.phat = sum( !is.na(phat) ),
#          n.rows = n(),
#          RMSE = sqrt( phatBias^2 + EmpVar ) ) %>%
#   summarise_if( is.numeric, function(x) mean(x, na.rm = TRUE) ) %>%
#   arrange(n.cover, n.phat) 
# 
# View(res.all)
# 
# setwd(results.dir)
# # don't overwrite
# # write.csv(res.all, 
# #           "res_all.csv",
# #           row.names = FALSE)
# 
# 
# #################### PLOTS ####################
# 
# # for plotting joy
# res.all$muN.pretty = paste( "E[N] = ", res.all$muN )
# res.all$TheoryP.pretty = paste( "True P = ",
#                                 format(round(res.all$TheoryP, 2), nsmall = 2) )
# 
# # also for plotting joy
# # name the methods differently for inference vs. point estimate plots
# library(car)
# res.all$Method.inf.pretty = recode( res.all$Method,
#                                     " 'Boot'='BCa-parametric';
#                          'NP ensemble'='BCa-calibrated';
#                          'NP sign test'='Sign test'" )
# res.all$Method.est.pretty = recode( res.all$Method,
#                                     "'NP ensemble'='Calibrated';
#                          'NP sign test'='Sign test max.'" )
# 
# 
# # need to have folder on Desktop called "Plots for each distribution"
# all_plots_one_dist(true.effect.dist = "expo",
#                    bias.max = round(max(res.all$phatBias) + 0.01, 2),
#                    bias.min = round( min(res.all$phatBias) - 0.01, 2),
#                    mse.max = max(res.all$RMSE, na.rm = TRUE) + 0.01,
#                    cover.min = .1,
#                    results.path = "~/Desktop/Plots for each distribution")
# 
# all_plots_one_dist(true.effect.dist = "normal",
#                    bias.max = round(max(res.all$phatBias) + 0.01, 2),
#                    bias.min = round( min(res.all$phatBias) - 0.01, 2),
#                    mse.max = max(res.all$RMSE, na.rm = TRUE) + 0.01,
#                    cover.min = .1,
#                    results.path = "~/Desktop/Plots for each distribution")
# 
# all_plots_one_dist(true.effect.dist = "unif2",
#                    bias.max = round(max(res.all$phatBias) + 0.01, 2),
#                    bias.min = round( min(res.all$phatBias) - 0.01, 2),
#                    mse.max = max(res.all$RMSE, na.rm = TRUE) + 0.01,
#                    cover.min = .1,
#                    results.path = "~/Desktop/Plots for each distribution")
# 
# all_plots_one_dist(true.effect.dist = "t.scaled",
#                    bias.max = round(max(res.all$phatBias) + 0.01, 2),
#                    bias.min = round( min(res.all$phatBias) - 0.01, 2),
#                    mse.max = max(res.all$RMSE, na.rm = TRUE) + 0.01,
#                    cover.min = .1,
#                    results.path = "~/Desktop/Plots for each distribution")
# 
# 
# #################### WINNING METHOD BY SCENARIO ####################
# 
# # point estimate winner
# # exclude boot since it's not a "real" point estimate
# est.winners = res.all %>% filter(Method != "Boot") %>%
#   group_by(scen.name, V, TheoryP, true.effect.dist) %>%
#   summarise( MSE.winner = Method[ which.min(RMSE) ],
#              Bias.winner = Method[ which.min(phatAbsBias) ] )
# View(est.winners)
# 
# # inference winner
# # winning at coverage is defined as having coverage as close to 95% as possible
# inf.winners = res.all %>% group_by(scen.name, TheoryP, true.effect.dist, V) %>%
#   summarise( Coverage.winner = Method[ which.min( abs(Cover-0.95) ) ],
#              Coverage.of.winner = Cover[ which.min( abs(Cover-0.95) ) ],
#              Width.winner = Method[ which.min(Width) ] )
# View(inf.winners)
# 
# 
# #################### ** TABLES FOR PAPER: HIGH-LEVEL SUMMARY OF INFERENCE AND POINT ESTIMATE RESULTS ####################
# 
# ##### Inference Summary #####
# inf.table = res.all %>%
#   filter(k>5, !is.na(Cover)) %>%
#   group_by(V, Method.inf.pretty) %>%
#   summarise( Mean.Cover = mean(Cover),
#              Min.Cover = min(Cover),
#              #q5Cover = quantile(Cover, 0.05, na.rm = TRUE),
#              #Cover.Above.90 = sum(Cover>.9)/length(Cover),
#              Width = mean(Width) )
# View(inf.table)
# library(xtable)
# print( xtable(inf.table), include.rownames = FALSE )
# 
# ##### Point Estimates Summary #####
# est.table = res.all %>%
#   filter(Method != "Boot") %>%
#   group_by(true.effect.dist, Method.est.pretty) %>%
#   summarise( Mean.RMSE = MetaUtility::format_stat( mean(RMSE), digits = 3 ),
#              Mean.Bias = MetaUtility::format_stat( mean(phatBias), digits = 3 ),
#              Mean.Abs.Bias = MetaUtility::format_stat( mean(phatAbsBias), digits = 3 ))
# View(est.table)
# 
# library(xtable)
# print( xtable(est.table), include.rownames = FALSE )
# 
# 
# #################### ** FLOWCHART STATS: SUMMARIZE INFERENCE RESULTS FOR FLOWCHART TERMINAL NODES ####################
# 
# ##### Inference #####
# 
# # baseline requirements:
# #  - k >= 10
# 
# # first use BCa if it converges:
# # min coverage: 91%
# nodeA = res.all %>%
#   filter(k > 5) %>%
#   filter(!is.na(Cover)) %>%
#   group_by(Method) %>%
#   summarise( Mean.Cover = mean(Cover),
#              Min.Cover = min(Cover),
#              Cover.Above.90 = sum(Cover>.9)/length(Cover),
#              Width = mean(Width), 
#              reps = n() )
# View(nodeA)
# 
# # if it doesn't converge, here are scenarios in which 
# #  you can use NP sign test instead:
# #  - normal or t-distributed effects
# #  - tau/mu >= 1 (i.e., V >= 0.25)
# # min coverage: 92%
# nodeB = res.all %>%
#   filter(k > 5) %>%
#   filter(V==.25)%>%
#   filter(true.effect.dist %in% c("normal", "t.scaled")) %>%
#   filter(!is.na(Cover)) %>%
#   group_by(Method) %>%
#   summarise( Mean.Cover = mean(Cover),
#              Min.Cover = min(Cover),
#              Cover.Above.90 = sum(Cover>.9)/length(Cover),
#              Width = mean(Width) )
# View(nodeB)
# 
# 
# 
# #################### PLOT EXAMPLE DATA FROM EACH DIST ####################
# 
# vec = c("normal", "expo", "unif2", "t.scaled")
# vec2 = c(0.25, 0.16, 0.09, 0.04, 0.01)
# 
# params = as.data.frame( expand.grid(vec, vec2) )
# names(params) = c("dist", "V")
# 
# if ( simulate.dist.from.scratch == TRUE ) {
#   sims = apply( params,
#                 MARGIN = 1, 
#                 FUN = function(row) {
#                   d = sim_data( k = 1000,
#                                 mu = 0.5,
#                                 V = as.numeric(row[["V"]]),
#                                 muN = 850,
#                                 minN = 800,
#                                 sd.w = 1,
#                                 true.effect.dist = row[["dist"]])
#                   d$true.effect.dist = row[["dist"]]
#                   d$V = as.numeric(row[["V"]])
#                   return(d)
#                 } )
#   
#   sims = do.call(rbind, sims)
#   setwd(results.dist.dir)
#   write.csv(sims, 
#             "simulated_density_data.csv",
#             row.names = FALSE)
# } else {
#   setwd(results.dist.dir)
#   sims = read.csv("simulated_density_data.csv")
#   
#   # we had simulated extra values of V, but then decided against actually simulating them
#   # remove those
#   sims = sims %>% filter( sims$V %in% c(0.01, 0.04, 0.25))
# }
# 
# # for plotting joy
# sims$dist.pretty = "Normal"
# sims$dist.pretty[ sims$true.effect.dist == "expo"] = "Exponential"
# sims$dist.pretty[ sims$true.effect.dist == "t.scaled"] = "t"
# sims$dist.pretty[ sims$true.effect.dist == "unif2"] = "Uniform mixture"
# 
# # for plotting joy
# sims$V.pretty = paste( expression(tau^2), " = ", sims$V,
#                        sep = "")
# 
# # make density plot
# ggplot( data = sims,
#         aes(x = Mi,
#             fill = dist.pretty)) +
#   #geom_histogram(bins=20) +
#   geom_vline(xintercept = 0.5, color = "gray") +
#   geom_density(adjust=.5) +
#   theme_bw() +
#   theme(axis.text.y = element_blank(),
#         axis.ticks.y = element_blank(),
#         # last two arguments remove gridlines
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank()) +
#   scale_x_continuous(breaks = seq(-0.5, 1.5, .5),
#                      limits = c(-0.5, 1.5)) +
#   facet_grid(dist.pretty ~ V.pretty) +
#   xlab( bquote(theta[i]) ) +
#   ylab("") +
#   labs(fill = "")
# # 11 x 8 works well
# 
# # # sanity check
# # # since t looks just like normal visually, confirm via Shapiro that 
# # #  it really is different
# # sims %>% filter(dist.pretty == "Normal") %>%
# #   group_by(V) %>% 
# #   summarise( shapiro.p = shapiro.test(Mi)$p.value )
# # # works :) 
# 
