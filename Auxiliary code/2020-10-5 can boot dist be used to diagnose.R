
# Here, before trying different contrasts in the covariates, I looked at diagnostics 
#  based on the bootstrap iterates.


##### Other boot diagnostics - skewness (doesn't work)

s$DiffLo[1:100]
s$DiffHi[1:100]

# # truncate bounds
# s$DiffLoTrunc = pmax(s$DiffLo, 0)
# table(s$DiffLoTrunc == s$DiffLo)  # original bound very often below 0
# 
# s$DiffHiTrunc = pmin(s$DiffHi, 1)
# table(s$DiffHiTrunc == s$DiffHi)  # never had to be truncated

summary(s$DiffHi - s$DiffBtMn)  # sometimes negative, which is a bad sign
summary(s$DiffBtMn - s$DiffLo)  # sometimes negative, which is a bad sign

s$skew = abs(s$DiffHi - s$DiffBtMn) / abs(s$DiffLo - s$DiffBtMn)

summary(s$skew)

s %>% filter( !is.na(skew) & skew > 50 ) %>%
  select(DiffLo, DiffBtMn, TheoryDiff, DiffHi, skew) %>%
  slice_head(n=20)


summary(s$DiffHiTrunc - s$DiffBtMn)

s$evil = (s$skew > 2) | (s$DiffHi - s$DiffBtMn < 0) | (s$DiffBtMn - s$DiffLo < 0)
s$evil = (s$skew > 2)
s$evil = (s$DiffHi - s$DiffBtMn < 0) | (s$DiffBtMn - s$DiffLo < 0)

table(s$evil)

agg11 = make_agg_data( s %>% filter(evil == FALSE & k >= 100) )

#agg11 = make_agg_data( s %>% filter(k >= 100) )

data.frame( my_summarise(agg11, description=""))





##### Other boot diagnostics - Phat2 as a diagnostic rather than a bias corrections

summary( abs(s$Phat2/s$Phat) )

s %>% filter(Phat2/Phat < 1.05) %>%
  select(Phat, Phat2, PhatBtMn, TheoryP) %>%
  slice_head(20)

agg11 = make_agg_data( s %>% filter( abs(Phat2/Phat) > 0.98 & abs(Phat2/Phat) < 1.02 ) )

#agg11 = make_agg_data( s %>% filter(k >= 100) )

selectVars = "Phat"
data.frame( my_summarise(agg11, description=""))

sdata.frame( my_summarise(agg, description=""))



##### Other filtering criteria - scenarios with okay bias in meta-regression estimators
# this one filtered at scen levels

agg11 = agg %>% filter(EstMeanRelBias < 0.02 &
                         k>=100 &
                         EstVarRelBias<.2)

data.frame( my_summarise(agg11, description=""))



##### look for the scenarios where it is okay
mean(s$CoverDiff>.93)  # only 12%

# bm
obsVars = c("k", "muN", "Phat", "PhatRef", "EstMean", "EstVar", "PhatBtFail",
            "true.effect.dist", "clustered")


obsVars = c("k", "muN",
            #"PhatBtFail", "bca.success",
            "true.effect.dist", "clustered")

