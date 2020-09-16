

library(boot)
library(foreach)
library(doParallel)
library(dplyr)


sim.reps = 500
boot.reps = 1000

# helper fn to check for CI coverage of truth
covers = function( truth, lo, hi ) {
  return( (lo <= truth) & (hi >= truth) )
}

# generate standard normal data with n=200
# we are going to estimate the mean
d = data.frame( ID = 1:200,
                y = rnorm(200) )

# run the simulation
# takes a couple minutes on my 8-core machine
rs = foreach( i = 1:sim.reps, .combine=rbind ) %dopar% {
  
  # Method A: standard use of internally generated indices
  tryCatch({
    boot.res1 = boot( data = d,
                      R = boot.reps,
                      parallel = "multicore",
                      statistic = function(original, indices){
                        # ***HERE IS THE KEY PART:
                        b = original[indices,]
                        mean(b$y)
                      } )
    
    CI1 = boot.ci(boot.res1, type = "bca")
    lo1 = CI1[[4]][4]
    hi1 = CI1[[4]][5]
    btSD1 = as.numeric( sd(boot.res1$t) )
    
  }, error = function(err){
    lo1 <<- NA
    hi1 <<- NA
    btSD1 <<- NA
    
  } )
 
  # Method B: using my own indices
  tryCatch({
  boot.res2 = boot( data = d,
                    R = boot.reps,
                    parallel = "multicore",
                    statistic = function(original, indices){
                      
                      myInd = sample(1:nrow(original),
                                     size = nrow(original),
                                     replace = TRUE)
                      # ***HERE IS THE KEY PART:
                      b = original[myInd,]
                      mean(b$y)
                      
                      # this makes things even worse:
                      # causes boot.res$t to be integers?? wtf?
                      # indices <<- myInd
                    } )
  
  CI2 = boot.ci(boot.res2, type = "bca")
  lo2 = CI2[[4]][4]
  hi2 = CI2[[4]][5]
  btSD2 = as.numeric( sd(boot.res2$t) )
  
  }, error = function(err){
    lo2 <<- NA
    hi2 <<- NA
    btSD2 <<- NA
  } )
  
  return( data.frame( method = c("a", "b"),
                      covers = c( covers( truth = 0, lo = lo1, hi = hi1 ),
                                  covers( truth = 0, lo = lo2, hi = hi2 ) ),
                      btSD = c( btSD1, btSD2 ) ) )
  
  
}


rs %>% group_by(method) %>%
  summarise( btFail = mean(is.na(covers)),
             covers = mean(covers, na.rm = TRUE),
             btSD = mean(btSD, na.rm = TRUE) )

# btSD should be close to the true SE, i.e., 1/sqrt(200) = 0.07071068

# results:
# method btFail covers   btSD
# <chr>   <dbl>  <dbl>  <dbl>
#   1 a       0      1     0.0718
#   2 b       0.002  0.655 0.0718




############ TRY VARIOUS WAYS TO FIX THE PROBLEM 


# helper fn to check for CI coverage of truth
covers = function( truth, lo, hi ) {
  return( (lo <= truth) & (hi >= truth) )
}

# generate standard normal data with n=200
# we are going to estimate the mean
d = data.frame( ID = 1:200,
                y = rnorm(200) )

# run the simulation
# takes a couple minutes on my 8-core machine
rs = foreach( i = 1:sim.reps, .combine=rbind ) %dopar% {
  
  # Method A: standard use of internally generated indices
  tryCatch({
    boot.res1 = boot( data = d,
                      R = boot.reps,
                      parallel = "multicore",
                      statistic = function(original, indices){
                        # ***HERE IS THE KEY PART:
                        b = original[indices,]
                        mean(b$y)
                      } )
    
    CI1 = boot.ci(boot.res1, type = "bca")
    lo1 = CI1[[4]][4]
    hi1 = CI1[[4]][5]
    btSD1 = as.numeric( sd(boot.res1$t) )
    
  }, error = function(err){
    lo1 <<- NA
    hi1 <<- NA
    btSD1 <<- NA
    
  } )
  
  # Method B: using my own indices
  tryCatch({
    boot.res2 = boot( data = d,
                      R = boot.reps,
                      parallel = "multicore",
                      statistic = function(original, indices){
                        
                        myInd = sample(1:nrow(original),
                                       size = nrow(original),
                                       replace = TRUE)
                        # ***HERE IS THE KEY PART:
                        b = original[myInd,]
                        mean(b$y)
                        
                        # this makes things even worse:
                        # causes boot.res$t to be integers?? wtf?
                        # indices <<- myInd
                      } )
    
    CI2 = boot.ci(boot.res2, type = "bca")
    lo2 = CI2[[4]][4]
    hi2 = CI2[[4]][5]
    btSD2 = as.numeric( sd(boot.res2$t) )
    
  }, error = function(err){
    lo2 <<- NA
    hi2 <<- NA
    btSD2 <<- NA
  } )
  
  return( data.frame( method = c("a", "b"),
                      covers = c( covers( truth = 0, lo = lo1, hi = hi1 ),
                                  covers( truth = 0, lo = lo2, hi = hi2 ) ),
                      btSD = c( btSD1, btSD2 ) ) )
  
  
}


rs %>% group_by(method) %>%
  summarise( btFail = mean(is.na(covers)),
             covers = mean(covers, na.rm = TRUE),
             btSD = mean(btSD, na.rm = TRUE) )

# btSD should be close to the true SE, i.e., 1/sqrt(200) = 0.07071068

# results:
# method btFail covers   btSD
# <chr>   <dbl>  <dbl>  <dbl>
#   1 a       0      1     0.0718
#   2 b       0.002  0.655 0.0718

