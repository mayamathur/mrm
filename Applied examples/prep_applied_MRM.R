
################################## RITCHIE ################################## 


library(dplyr)
library(tidyverse)
library(testthat)
library(readxl)

awr.data.dir = "~/Dropbox/Personal computer/Independent studies/2019/AWR (animal welfare review meat consumption)/Linked to OSF (AWR)/Data extraction"
bediou.data.dir = "~/Dropbox/Personal computer/Independent studies/2020/Meta-regression metrics (MRM)/Applied examples/Bediou"
ritchie.data.dir = "~/Dropbox/Personal computer/Independent studies/2020/Meta-regression metrics (MRM)/Applied examples/Ritchie"

prepped.data.dir = "~/Dropbox/Personal computer/Independent studies/2020/Meta-regression metrics (MRM)/Applied examples/Prepped data"
code.dir = "~/Dropbox/Personal computer/Independent studies/2020/Meta-regression metrics (MRM)/Code (git)/Applied examples"
  
setwd(code.dir)
source("helper_applied_MRM.R")


################################## RITCHIE ################################## 

setwd(ritchie.data.dir)
dr = read_xlsx("EduIQ_master.xlsx")

dr = dr %>% filter( !is.na(`Author(s)`))
dim(dr)

dr$age.fu = dr$`Average or Midpoint Age at Outcome test (derived)`
summary(dr$age.fu)

dr$yi = dr$`IQ points/year`
dr$vi = dr$`IQ points/year SE`^2

dr$study = dr$`Study ID`

setwd(prepped.data.dir)
write.csv(dr, "ritchie_data_prepped.csv")
