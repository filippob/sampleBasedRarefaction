## R functions to extract incremental counts from sample table
## case of sample-based OTU rarefaction
## two implementations: 1) iterative function based on a while loop;  2) recursive function

##################################################
## iterative function
sampleBasedLoop <- function(tab) {
  
  steps <- c()
  
  while(nrow(tab)>1 & ncol(tab)>1) {
    
    indx <- list("ind"=which.max(colSums(tab>0)),"max"=max(colSums(tab>0)))
    steps <- c(steps,indx$max)
    tab <- tab[tab[,indx$ind]==0,]
  } 
  return(c(steps,1))
}


## recursive function to do the same loop as above
getIncrementalOtus <- function(tab) {
  
  indx <- list("ind"=which.max(colSums(tab>0)),"max"=max(colSums(tab>0)))
  steps <- indx$max
  
  tab <- tab[tab[,indx$ind]==0,]
  
  if(nrow(tab)>1) {
    
    steps <- c(steps,getIncrementalOtus(tab))
  } else {
    
    res <- ifelse(sum(steps)<nrow(tab),nrow(tab)-sum(steps),0)
    if(res==0) res <- NULL
    steps <- c(steps,res)
  }
  return(steps)
}
