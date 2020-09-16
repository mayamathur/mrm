
# Use SAPB-E data to get realistic heterogeneity and clustering values.

library(testthat)
library(dplyr)

setwd("~/Dropbox/Personal computer/Independent studies/Sensitivity analysis for publication bias (SAPB)/Linked to OSF (SAPB)/Empirical benchmarks/Data collection/Prepped data for analysis")

b2 = read.csv("**b2_data_prepped_step2.csv")
f2 = read.csv("**f2_data_aggregated_step2.csv")

# exclude duplicated Metalab sensitivity estimates
unique( b2$meta.name[ grepl(pattern = "[.]sens", b2$meta.name) == TRUE ] )

##### Typical Clustering in SAPB-E #####
# average number of studies per cluster
t = b2 %>% group_by(meta.name) %>%
  filter( grepl(pattern = "[.]sens", meta.name) == FALSE ) %>%
  summarise( n.randomly.chosen = sum(randomly.chosen),
             n.total = n() )

# reproduce from SAPB-E
expect_equal( nrow(t), 63 )
expect_equal( median( t$n.total ), 80 )

# **typical number of studies per cluster: median 1.5; mean 2.7
median( t$n.total / t$n.randomly.chosen )
mean( t$n.total / t$n.randomly.chosen )


##### Typical Heterogeneity in SAPB-E #####
# fit robust model to get average heterogeneity estimates
fit_robu = function(dat) {
  
  # test only
  #dat = b2[ b2$meta.name == b2$meta.name[1], ]
  
  library(robumeta)
  
  tryCatch({
    ( meta = robu( EstF ~ 1, 
                   data = dat, 
                   studynum = as.factor(study.name),
                   var.eff.size = SE,
                   modelweights = "HIER",
                   small = TRUE) )
    
    est = meta$b.r
    t2 = as.numeric(meta$mod_info$tau.sq)
    mu.lo = meta$reg_table$CI.L
    mu.hi = meta$reg_table$CI.U
    mu.se = meta$reg_table$SE
    mu.pval = meta$reg_table$prob
    
    cat( paste( "\n", dat$meta.name[1], ": t2=", round( as.numeric(t2), 2 ), sep = "" ) )
    return(t2)
    
  }, error = function(err){
    t2 <<- NA
    return( t2 )
  })
  
  
}

temp = b2 %>% group_by(meta.name) %>%
  do( t2 = fit_robu(.) )

# fix messy format
temp2 = data.frame( meta.name = temp$meta.name,
                    t2 = unlist(temp$t2) )

# merge back into main dataset
f2 = merge( f2, temp2, all.x = TRUE, by = "meta.name" )

# exclude metalab sensitivity ones
f2 = f2 %>% filter( grepl(pattern = "[.]sens", meta.name) == FALSE )
expect_equal(63, nrow(f2))

# look at stats
# proportion with t2 > 0
table(!is.na(f2$t2))  # 3 are NA

mean( f2$t2 == 0, na.rm = TRUE  )  # **63% have no heterogeneity


# look just at SMD scale, as in MRM sims

# among all SMD metas
f2 %>% filter(Analysis.Scale == "smd" & !is.na(t2) ) %>%
  summarise( k = n(),
             t2.mn = mean(t2),
             t2.med = median(t2),
             t2.gt.0 = mean(t2 > 0),
             t2.lt.0.01 = mean(t2 < 0.01) )

# among only those with t2>0 (when it makes sense to apply our metrics)
# among all metas
f2 %>% filter(Analysis.Scale == "smd" & !is.na(t2) & t2 > 0 ) %>%
  summarise( k = n(),
             t2.mn = mean(t2),
             t2.med = median(t2),
             t2.lt.0.01 = mean(t2 < 0.01) )





