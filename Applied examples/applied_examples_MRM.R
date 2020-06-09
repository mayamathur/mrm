
library(dplyr)

################################## AWR DATA ################################## 

setwd("~/Dropbox/Personal computer/Independent studies/2019/AWR (animal welfare review meat consumption)/Linked to OSF (AWR)/Data extraction")
da = read.csv("prepped_data.csv")

qual.vars = c("qual.y.prox",
                "qual.missing",
                "qual.exch",
                "qual.gen",
                "qual.sdb",
                "qual.prereg",
                "reproducible")

da$low.miss = da$qual.missing < 15

# remove ones missing these variables
da = da %>% drop_na(qual.vars)


# fit the meta-regression
library(robumeta)
m = robu( logRR ~ #randomized +  # too collinear with exch
            (qual.y.prox == "b.Self-reported") +
            low.miss +
            (qual.exch == "c.High") +
            (qual.sdb == "c.High") +
            (qual.gen == "c.High") +
            (qual.prereg == "Yes") +
            reproducible, 
          data = da, 
          studynum = authoryear,  # ~~~ clustering
          var.eff.size = varlogRR )

t2 = m$mod_info$tau.sq



##### Consider a Hypothetical Study with Optimal Risks of Bias #####
da$linpredZ = m$b.r[2] * (da$qual.y.prox == "b.Self-reported") +
  m$b.r[3] * da$low.miss +
  m$b.r[4] * (da$qual.exch == "c.High") +
  m$b.r[5] * (da$qual.sdb == "c.High") +
  m$b.r[6] * (da$qual.gen == "c.High") +
  m$b.r[7] * (da$qual.prereg == "Yes") +
  m$b.r[8] * da$reproducible


da$yi.shift = da$logRR - da$linpredZ  # shifted to have moderators set to 0
ens2 = calib_ests(yi = da$yi.shift,
                  sei = sqrt(da$varlogRR) )

# yes, agrees with the above! 
q = log(1.2)
# sum because all quality vars are coded such that 1 is good
q.shift = q - ( sum(m$b.r) - m$b.r[1] )  # remove the intercept
mean(ens2 > q.shift)

# Proportion of completely high-quality studies above RR = 1.2: 
# **82% [65%, 95%]
# main analysis in paper (page 19): 53% (95% CI: [37%, 65%])
MetaUtility::prop_stronger(q = q.shift,
                           tail = "above",
                           dat = da,
                           yi.name = "yi.shift",
                           vi.name = "varlogRR",
                           R = 1000) # ignore CI for now

# the linear predictor assumes that biases are essentially additive on the log-RR scale
#  i.e., multiplicative on the RR scale






