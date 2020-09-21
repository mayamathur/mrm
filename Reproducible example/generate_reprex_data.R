
# Users do not need to run or change this. The data are already saved as a csv file. 

reprex.dir = "~/Dropbox/Personal computer/Independent studies/2020/Meta-regression metrics (MRM)/Code (git)/Reproducible example"
# borrow simulation study code for generating data
code.dir = "~/Dropbox/Personal computer/Independent studies/2020/Meta-regression metrics (MRM)/Code (git)/Simulation study"
setwd(code.dir)
source("helper_MRM.R")


library(dplyr)
library(ICC)

d = sim_data2(k = 100,
              m = 50,
              b0 = -1,
              bc = 0.5, 
              bb = 1, 
              V = 0.04,
              Vzeta = 0.02,
              muN = 100,
              minN = 50,
              sd.w = 1,
              true.effect.dist = "normal")


# remove the underlying parameters that would not be in observed data
d = d %>% select(cluster, yi, vyi, Zc, Zb)

# save data
setwd(reprex.dir)
write.csv(d, "reprex_fake_data.csv", row.names = FALSE)
