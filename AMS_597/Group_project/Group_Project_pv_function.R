#'Extract p-values from 1 dataframe
#'
#'Take one dataframe to get the p-values for each biomarker
#'@param data one dataframe with more than 2 groups
#'@description calculate p-values from the dataframe
#'@examples data1 <- data.frame(group=sample(1:3,200,replace=TRUE),matrix(rnorm(p*200),ncol=100))
#'calculate_pvalues_1dataframe(data1) #get 100 p-values for this dataframe
#'@return the p-values for each biomarker in this dataframe
#'@export
calculate_pvalues_1dataframe <- function(data) {
  n=length(unique(data$group))
  if(n==2) {
    pv <- rep(NA,ncol(data)-1)
    for (i in 2:ncol(data)) {
      sp1 <- shapiro.test(data[data$group==1,i])$p.value
      sp2 <- shapiro.test(data[data$group==2,i])$p.value
      fp <- var.test(data[data$group==1,i],data[data$group==2,i])$p.value
      if(sp1>0.05&sp2>0.05&fp>0.05){
        pv[i-1] <- t.test(data[data$group==1,i],data[data$group==2,i],var.equal=TRUE)$p.value
      }else if(sp1>0.05&sp2>0.05&fp<=0.05){
        pv[i-1] <- t.test(data[data$group==1,i],data[data$group==2,i],var.equal=FALSE)$p.value
      }else{
        pv[i-1] <- wilcox.test(data[data$group==1,i],data[data$group==2,i])$p.value
      }
    }
  }else if(n==1){
    stop("the data frame should have at least 2 groups")
  }else{
    pv <- rep(NA,ncol(data)-1)
    for (i in 2:ncol(data)) {
      sp <- rep(NA,length(unique(data$group)))
      for (j in 1:length(unique(data$group))) {
        sp[j]=shapiro.test(data[data$group==j,i])$p.value
      }
      if(sp>0.05){
        pv[i-1] <- anova(lm(data[,i]~group,data = data))$"Pr(>F)"[1]
      }else{
        pv[i-1] <- kruskal.test(data[,i]~group,data = data)$p.value
      }
    }
  }
  return(pv)
}
#extract p-values from 2 dataframes
#'Extract p-values from 2 dataframes
#'
#'Take 2 dataframes to get the p-values for each biomarker
#'@param x dataframe with more than 2 groups
#'@param y dataframe with more than 2 groups and have the same number of columns(biomarkers) as x
#'@description calculate p-values from the 2 dataframes
#'@examples x <- data.frame(group=sample(1:3,200,replace=TRUE),matrix(rnorm(p*200),ncol=100))
#'y <- data.frame(group=sample(1:2,150,replace=TRUE),matrix(rnorm(p*150),ncol=100))
#'calculate_pvalues_2dataframes(x,y)
#'#get 2*100 p-values for the 2 dataframes in a matrix
#'@return the p-values for each biomarker in the 2 dataframes
#'@export
calculate_pvalues_2dataframes <- function(x,y){
  if (length(x)==length(y)){
  pv1 <- calculate_pvalues_1dataframe(x)
  pv2 <- calculate_pvalues_1dataframe(y)
  pv <- rbind(pv1,pv2)
  return(pv)
  }else{
    stop("the columns of each dataframe should be same")
  }
}
#extract p-values from 3 dataframes
#'Extract p-values from 3 dataframes
#'
#'Take 3 dataframes to get the p-values for each biomarker
#'@param x dataframe with more than 2 groups
#'@param y dataframe with more than 2 groups and have the same number of columns(biomarkers) as x
#'@param z dataframe with more than 2 groups and have the same number of columns(biomarkers) as x
#'@description calculate p-values from the 3 dataframes
#'@examples
#'calculate_pvalues_3dataframes(x,y,z)
#'#get 3*100 p-values for the 3 dataframes in a matrix
#'@return the p-values for each biomarker in the 3 dataframes
#'@export
calculate_pvalues_3dataframes <- function(x,y,z){
  if (length(x)==length(y)& length(x)==length(z)){
  pv1 <- calculate_pvalues_1dataframe(x)
  pv2 <- calculate_pvalues_1dataframe(y)
  pv3 <- calculate_pvalues_1dataframe(z)
  pv <- rbind(pv1,pv2,pv3)
  return(pv)
  }else{
    stop("the columns of each dataframe should be same")
  }
}
#extract p-values from 4 dataframes
#'Extract p-values from 4 dataframes
#'
#'Take 4 dataframes to get the p-values for each biomarker
#'@param x dataframe with more than 2 groups
#'@param y dataframe with more than 2 groups and have the same number of columns(biomarkers) as x
#'@param z dataframe with more than 2 groups and have the same number of columns(biomarkers) as x
#'@param m dataframe with more than 2 groups and have the same number of columns(biomarkers) as x
#'@description calculate p-values from the 4 dataframes
#'@examples
#'calculate_pvalues_4dataframes(x,y,z,m)
#'#get 4*100 p-values for the 4 dataframes in a matrix
#'@return the p-values for each biomarker in the 4 dataframes
#'@export
calculate_pvalues_4dataframes <- function(x,y,z,m){
  if (length(x)==length(y)& length(x)==length(z)& length(x)==length(m)){
  pv1 <- calculate_pvalues_1dataframe(x)
  pv2 <- calculate_pvalues_1dataframe(y)
  pv3 <- calculate_pvalues_1dataframe(z)
  pv4 <- calculate_pvalues_1dataframe(m)
  pv <- rbind(pv1,pv2,pv3,pv4)
  return(pv)
  }else{
    stop("the columns of each dataframe should be same")
  }
}
#extract p-values from 5 dataframes
#'Extract p-values from 5 dataframes
#'
#'Take 5 dataframes to get the p-values for each biomarker
#'@param x dataframe with more than 2 groups
#'@param y dataframe with more than 2 groups and have the same number of columns(biomarkers) as x
#'@param z dataframe with more than 2 groups and have the same number of columns(biomarkers) as x
#'@param m dataframe with more than 2 groups and have the same number of columns(biomarkers) as x
#'@param n dataframe with more than 2 groups and have the same number of columns(biomarkers) as x
#'@description calculate p-values from the 5 dataframes
#'@examples
#'calculate_pvalues_5dataframes(x,y,z,m,n)
#'#get 5*100 p-values for the 5 dataframes in a matrix
#'@return the p-values for each biomarker in the 5 dataframes
#'@export
calculate_pvalues_5dataframes <- function(x,y,z,m,n){
  if (length(x)==length(y)& length(x)==length(z)& length(x)==length(m)& length(x)==length(n)){
  pv1 <- calculate_pvalues_1dataframe(x)
  pv2 <- calculate_pvalues_1dataframe(y)
  pv3 <- calculate_pvalues_1dataframe(z)
  pv4 <- calculate_pvalues_1dataframe(m)
  pv5 <- calculate_pvalues_1dataframe(n)
  pv <- rbind(pv1,pv2,pv3,pv4,pv5)
  return(pv)
  }else{
    stop("the columns of each dataframe should be same")
  }
}

#Fisher
#'Implement Fisher method for pooling p-values from the previous calculate function
#'
#'Get the pooling p-values for each biomarker
#'@param pl a matrix contains the p-values for each biomarker in the dataframes
#'@description calculate the pooling p-values from input p-values
#'@examples
#'#pl=calculate_pvalues_5dataframes(x,y,z,m,n)
#'#pvfisher(pl)
#'#get the pooling p-values for the dataframes in a row
#'@return the pooling p-values for each biomarker
#'@export

pvfisher <- function(pl){
  poolingpv <- rep(NA,ncol(pl))
  for  (i in 1:ncol(pl)) {
    pfisher <- function(pl) {
      if (any(is.na(pl))) { res <- "There was an empty array of p-values"}
      else{
        t <- prod(pl)
        x <- -2*log(prod(pl))
        n <- 2*length(pl)
        res <- pchisq(x, n,low=F) }
      return(res)
    }
   poolingpv[i]=pfisher(pl[,i])
  }
  return(poolingpv)
}
#Stouffer
#'Implement Stouffer method for pooling p-values from the previous calculate function
#'
#'Get the pooling p-values for each biomarker
#'@param pl a matrix contains the p-values for each biomarker in the dataframes
#'@description calculate the pooling p-values from input p-values
#'@examples
#'#pl=calculate_pvalues_5dataframes(x,y,z,m,n)
#'#pvstouffer(pl)
#'#get the pooling p-values for the dataframes in a row
#'@return the pooling p-values for each biomarker
#'@export
pvstouffer <- function(pl){
  poolingpv <- rep(NA,ncol(pl))
  for  (i in 1:ncol(pl)) {
    pstouffer <- function(pl) {
      if (any(is.na(pl))) { res <- "There was an empty array of p-values"}
      else{
        t <- sum(qnorm(pl))
        n <- length(pl)
        x <- t/sqrt(n)
        res <- pnorm(x,0,1)
      }
      return(res)
    }
    poolingpv[i]=pstouffer(pl[,i])
  }
  return(poolingpv)
}

#maxP
#'Implement maxP method for pooling p-values from the previous calculate function
#'
#'Get the pooling p-values for each biomarker
#'@param pl a matrix contains the p-values for each biomarker in the dataframes
#'@description calculate the pooling p-values from input p-values
#'@examples
#'#pl=calculate_pvalues_5dataframes(x,y,z,m,n)
#'#maxPv(pl)
#'#get the pooling p-values for the dataframes in a row
#'@return the pooling p-values for each biomarker
#'@export
maxPv <- function(pl){
  poolingpv <- rep(NA,ncol(pl))
  for  (i in 1:ncol(pl)) {
    maxP <- function(p,b=1,alpha = 0.05){
      stopifnot(alpha > 0, alpha < 1)
      keep <- (p >= 0) & (p <= 1)
      pi <- p[keep]
      k <- length(pi)
      if ((b < 1)|(b > k)){
        b <- 1
        warning("Illegal r set to 1")
      }
      pi <- sort(pi)
      pr <- pi[k]
      res <- list(p = pbeta(pr, k, b), pr = pr, b = b,
                  critp = qbeta(alpha, k, b), alpha = alpha, validp = pi)
      return(res[[1]] )
    }
    poolingpv[i]=maxP(pl[,i])
  }
  return(poolingpv)
}
#minP
#'Implement minP method for pooling p-values from the previous calculate function
#'
#'Get the pooling p-values for each biomarker
#'@param pl a matrix contains the p-values for each biomarker in the dataframes
#'@description calculate the pooling p-values from input p-values
#'@examples
#'#pl=calculate_pvalues_5dataframes(x,y,z,m,n)
#'#minPv(pl)
#'#get the pooling p-values for the dataframes in a row
#'@return the pooling p-values for each biomarker
#'@export
minPv <- function(pl){
  poolingpv <- rep(NA,ncol(pl))
  for  (i in 1:ncol(pl)) {
    minP <- function(p,b=1,alpha = 0.05){
      stopifnot(alpha > 0, alpha < 1)
      keep <- (p >= 0) & (p <= 1)
      pi <- p[keep]
      k <- length(pi)
      pi <- sort(pi)
      pr <- pi[1]
      res <- list(p = pbeta(pr, 1, k), pr = pr, b = b,
                  critp = qbeta(alpha, 1, k), alpha = alpha, validp = pi)
      return(res[[1]] )
    }
    poolingpv[i]=minP(pl[,i])
  }
  return(poolingpv)
}
