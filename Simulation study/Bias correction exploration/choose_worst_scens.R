
# Choose scenarios with highest relative bias to include in the bias-correction
#  extension to simulation study.


setwd("~/Dropbox/Personal computer/Independent studies/2020/Meta-regression metrics (MRM)/Simulation study results/2020-9-23 for RSM_1")

agg = read.csv("*agg_dataset_as_analyzed.csv")

# @temp only
agg = agg %>% filter(sim.reps > 300)

summary(agg$PhatRelBias)

worst = agg %>% fi