
pv <- function(data) {
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

set.seed(123)
p <- 100
data1 <- data.frame(group=sample(1:3,200,replace=TRUE),
                    matrix(rnorm(p*200),ncol=p))
data2 <- data.frame(group=sample(1:2,150,replace=TRUE),
                    matrix(rnorm(p*150),ncol=p))
pd1 <- pv(data1)
pd2 <-pv(data2)



##############   Fisher method of pooling p-values
#typically applied to a collection of independent test statistics, 
#usually from separate studies having the same null hypothesis

#meta-analysis NULL HYPOTHESIS is that all of the separate null hypotheses are true
# ALTERNATIVE HYPOTHESIS is that at least one of the separate alternative hypotheses is true.
data <- data.frame("pd1"=pd1,"pd2"=pd2)

Fisher <- function(data, alpha = 0.05){
  ks_total <- c()
  i=1
  for (column in names(data)){
    kst = ks.test(data[[column]], "punif",0,1)
    ks_total[i] = kst$p.value
    i = i + 1
  }                # check if the every column(dataframe) is uniform distribution(check all the null hypotheses are true)
# The distribution of X2 is a chi-squared distribution for the following reason: under the null hypothesis for test i,the p-value pi follows a uniform distribution on the interval [0,1].
  
  #kst1 = ks.test(dp1, "punif")
  #kst2 = ks.test(dp2, "punif")
  if (sum(ks_total < alpha) > 0){
    stop("Null p-values are NOT all uniformly distributed")
  }else{
    logdata <- -2*log(data/2) 
    sum.col <- sum(colSums(logdata))
    chis <- pchisq(sum.col, length(data),low=F)   # smaller p-values, larger x2, which suggests that the null hypotheses are not true for every test.
    if (chis > alpha){
      cat("\n\tFisher's combined probability test\n\n","P-value =", chis,"\nSo we can reject the hypothesis that the null hypothesis is true in every study.")
    }else{
      cat("\n\tFisher's combined probability test\n\n", "P-value =", chis,"\nSo we can not reject the hypothesis. That means the null hypotheses are not true for every test.")
    }
  }
  
}
Fisher(data)



Stouffer <- function(data, alpha = 0.05){      # non-weighted Stouffer       # not sure if this's right
  ks_total <- c()
  i=1
  for (column in names(data)){
    kst = ks.test(data[[column]], "punif",0,1)
    ks_total[i] = kst$p.value
    i = i + 1
  }               
  Z <- c()
  for (i in 1:dim(data)[2]){
    Z[i]<- sum(qnorm(data[[column]]))
    i = i + 1
  }   # Z-scores
  
  Z.sum <- sum(Z)                     # the reciprocal of its squared standard error.
  if (sum(ks_total < alpha) > 0){
    stop("Null p-values are NOT all uniformly distributed")
  }else{
    Zs <- Z.sum / sqrt(length(data))
    norm <- pnorm(Zs, mean = 0,sd = 1)   # smaller p-values, larger x2, which suggests that the null hypotheses are not true for every test.
    if (norm > alpha){
      cat("\n\tStouffer's Method\n\n","P-value =", norm,"\nSo we can reject the hypothesis that the null hypothesis is true in every study.")
    }else{
      cat("\n\tStouffer's Method\n\n", "P-value =", norm,"\nSo we can not reject the hypothesis. That means the null hypotheses are not true for every test.")
    }
  }
  
}
Stouffer(data)