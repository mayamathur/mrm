
library(dplyr)
library(tidyverse)
library(testthat)

awr.data.dir = "~/Dropbox/Personal computer/Independent studies/2019/AWR (animal welfare review meat consumption)/Linked to OSF (AWR)/Data extraction"


prepped.data.dir = "~/Dropbox/Personal computer/Independent studies/2020/Meta-regression metrics (MRM)/Applied examples/Prepped data"
code.dir = "~/Dropbox/Personal computer/Independent studies/2020/Meta-regression metrics (MRM)/Code (git)/Applied examples"
  
setwd(code.dir)
source("helper_applied_MRM.R")


################################## BEDIOU ################################## 


################################## MATHUR ################################## 

setwd(awr.data.dir)
da = read.csv("prepped_data.csv")

# exclude self-selected within-subject studies, as in main analysis
da = da %>% filter( exclude.main == FALSE )
expect_equal( nrow(da), 100 )

qual.vars.raw = c("qual.y.prox",
                  "qual.missing",
                  "qual.exch",
                  "qual.gen",
                  "qual.sdb",
                  "qual.prereg",
                  "qual.public.data")

# recode quality variables as binary
da$qual.y.prox2 = binary_recode(da$qual.y.prox, "b.Self-reported"); table(da$qual.y.prox2, da$qual.y.prox)
da$low.miss = da$qual.missing < 15
da$qual.exch2 = binary_recode(da$qual.exch, "a.Low"); table(da$qual.exch2, da$qual.exch)
da$qual.sdb2 = binary_recode(da$qual.sdb, "a.Low")
da$qual.gen2 = binary_recode(da$qual.gen, "a.Low")
da$qual.prereg2 = binary_recode(da$qual.prereg, "Yes")
da$qual.public.data2 = binary_recode(da$qual.public.data, "Yes")

qual.vars = c("qual.y.prox2",
              "low.miss",
              "qual.exch2",
              "qual.gen2",
              "qual.sdb2",
              "qual.prereg2",
              "qual.public.data2")

# sanity check: check counts against Table 2 from AWR
apply( da %>% select(qual.vars), 2, function(x) sum(x, na.rm = TRUE) )


# remove ones missing the quality variables
da = da %>% drop_na(qual.vars.raw)
dim(da)  # 77 had no missing data
# the 23 exclusions are all due to studies that didn't report amount of missing data


setwd(prepped.data.dir)
write.csv(da, "mathur_data_prepped.csv")

################################## OTHER ONE DATA ################################## 




