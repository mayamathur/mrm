
code.dir = "~/Dropbox/Personal computer/Independent studies/2020/Meta-regression metrics (MRM)/Code (git)/Simulation study"
setwd(code.dir)
source("helper_MRM.R")

d = sim_one_study(b0 = 0.5, # intercept
                  bc = 0.5, # effect of continuous moderator
                  bb = 1, # effect of binary moderator
                  V = .2, 
                  muN = 150,
                  minN = 100,
                  sd.w = 1,
                  true.effect.dist = "normal")


dm = sim_data( b0 = 0.5, # intercept
               bc = 0.5, # effect of continuous moderator
               bb = 1, # effect of binary moderator 
               k = 1000,
               V = .2,
               muN = 150, 
               minN = 100,
               sd.w = 1, 
               true.effect.dist = "normal")




yi = dm$yi
vyi = dm$vyi
zbi = dm$Zb
zci = dm$Zc

# sanity check for data generation
library(robumeta)
m = robu( yi ~ Zc + Zb, 
          data = dm, 
          studynum = 1:nrow(dm),
          var.eff.size = vyi )
bhat0 = m$b.r[1]
bhatc = m$b.r[2]
bhatb = m$b.r[3]
t2 = m$mod_info$tau.sq


library(MetaUtility)
# proportion stronger than q when Zc = 0, Zb = 1
q = 1.3
# ~~~ BE CAREFUL ABOUT DISTINCTION BETWEEN ZC, ZB AND ZCI, ZBI
zc = 0
zb = 1
q.shift = q - bhatc*zc - bhatb*zb
tail = "above"

# shifted calibrated estimates
shrinkage = sqrt( c(t2) / ( c(t2) + vyi ) )
# residual based on ACTUAL moderator values for a given observation
resid = yi - (bhat0 + bhatc*zci + bhatb*zbi)
# first part is linpred for chosen moderator values
ens = (bhat0 + bhatc*zc + bhatb*zb) + shrinkage * resid

# sanity check
var(ens); t2
hist(ens)

# 84%
mean( ens > q )


##### Try Shifting the yis Themselves to Use Existing Package and Sims #####
# is this the same? (idea for using the package directly)
dm$yi.shift = yi - (bhatc*zci + bhatb*zbi)  # shifted to have moderators set to 0
ens2 = calib_ests(yi = dm$yi.shift,
                  sei = sqrt(vyi) )

# yes, agrees with the above! 
mean(ens2 > q.shift)

# **64% [60%, 66%]
MetaUtility::prop_stronger(q = q.shift,
                           tail = "above",
                           dat = dm,
                           yi.name = "yi.shift",
                           vi.name = "vyi",
                           R = 1000) # ignore CI for now


# not quite because of DL estimation






##### Compare to Subgroup #####
# compare to subgroup of Zb = 1 (not controlling for Zc)
library(dplyr)
temp = dm %>% filter( Zb == 1 )

m2 = robu( yi ~ 1, 
           data = temp, 
           studynum = 1:nrow(temp),
           var.eff.size = vyi )

MetaUtility::prop_stronger(q = q,
                           tail = "above",
                           dat = temp,
                           yi.name = "yi",
                           vi.name = "vyi",
                           R = 1000) # ignore CI for now
# 61% [56%, 65%]



