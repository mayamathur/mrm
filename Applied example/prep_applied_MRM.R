
# code audited 2020-6-17

################################## PRELIMINARIES ################################## 

library(dplyr)
library(tidyverse)
library(testthat)
library(readxl)

raw.data.dir = "~/Dropbox/Personal computer/Independent studies/2020/Meta-regression metrics (MRM)/Applied example/Raw data"
prepped.data.dir = "~/Dropbox/Personal computer/Independent studies/2020/Meta-regression metrics (MRM)/Applied example/Prepped data"
code.dir = "~/Dropbox/Personal computer/Independent studies/2020/Meta-regression metrics (MRM)/Code (git)/Applied examples"
  
setwd(code.dir)
source("helper_applied_MRM.R")


################################## PREP DATA ################################## 

setwd(raw.data.dir)
dr = read_xlsx("EduIQ_master.xlsx")

# remove empty rows
dr = dr %>% filter( !is.na(`Author(s)`))
expect_equal(nrow(dr), 142)

# number of estimates and of papers
nrow(dr)
length(unique(dr$`Study ID`))  # this is NOT the variable they used for clustering
expect_equal( length(unique(dr$`Dataset ID`)), 42 ) # this IS

# effect modifier of interest
dr$age.fu = dr$`Average or Midpoint Age at Outcome test (derived)`
summary(dr$age.fu)

# rename variables
dr$yi = dr$`IQ points/year`
dr$vi = dr$`IQ points/year SE`^2
dr$study = dr$`Dataset ID`

# save prepped data
setwd(prepped.data.dir)
write.csv(dr, "ritchie_data_prepped.csv")
