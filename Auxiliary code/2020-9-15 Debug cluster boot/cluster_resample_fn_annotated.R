

# guts of cluster.resample
data = d
cluster.name = "cluster"
size = length( unique(original$cluster) )
IDs <- unique(data[, cluster.name])



# IDs of clusters sampled with replacement
y <- sample(IDs, size, replace = TRUE)
# table of counts for how many clusters are to appear 1, 2, 3, ... times in dataset
z <- table(table(y))
# z[1] is the number of clusters chosen once, e.g., 31
# from ALL the clusters, choose 31 of them WITHOUT replacement
# so selectID represents the clusters that are to appear once in the ultimate resampled dataset
selectID <- sample(IDs, size = z[1], replace = FALSE)
# retain all observations whose cluster was in the selected IDs
newdata <- data[which(data[, cluster.name] %in% selectID),
]

# now go through the other counts (i) of how often a cluster could be assigned to appear
if (length(z) > 1) {
  # i: the number of times that a cluster could be assigned to appear
  for (i in 2:length(z)) {
    # IDs: the cluster IDs that were NOT assigned to be chosen once (i.e., still eligible for assignment)
    # if selectID was length 31 and there are 100 total clusters, this will be length 100-31
    IDs2 <- setdiff(IDs, selectID)
    # from the remaining eligible clusters, choose the appropriate count of them (z[i]) to appear twice (for i=2) in the resample
    selectID2 <- sample(IDs2, size = z[i], replace = FALSE)
    # update selectID to reflect that selectID2 are no longer eligible for the next i
    selectID <- c(selectID, selectID2)
    # rbind the corresponding data to the resample twice (for i=2)
    for (j in 1:i) {
      newdata <- rbind(newdata, data[which(data[, cluster.name] %in%
                                             selectID2), ])
    }
  }
  return(newdata)
}