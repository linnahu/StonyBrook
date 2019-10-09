##### Question 1
#(a)
sample(c(1,2,3,4), 100, replace = T, prob = c(0.1, 0.2, 0.4, 0.3))

#(b)
multisample <- function(x, n){
  y <- runif(n)
  y[y <= 0.1] = x[1]
  y[y>0.1 & y <= 0.3] = x[2]
  y[y >0.3 & y <= 0.7] = x[3]
  y[y > 0.7 & y <= 1] =x[4]
  return(y)
}
example <- multisample(c(1,2,3,4), 100)



###### Question 2
x <- rexp(100, rate = 2)
plot(sort(x), (1:100)/100, type = "s", ylim = c(0,1), main = "Empirical Distribution Function")



##### Question 3
dataset1 <- read.table("http://www.ams.sunysb.edu/~pfkuan/Teaching/AMS597/Data/d_logret_6stocks.txt", header = T) 
#(a)
t.test(dataset1$AmerExp )
# P-value = 0.8516, so we cannot reject the null hypothesis.

#(b)
wilcox.test(dataset1$AmerExp)
# p-value = 0.3225, so we cannot reject the null htpothesis.

#(c)
var.test(dataset1$Pfizer, dataset1$AmerExp)
# p-value = 0.03896, Variance are not equal.
t.test(dataset1$Pfizer, dataset1$AmerExp)
# p-value = 0.318, so we cannot reject the null htpothesis.

#(d)
var.test(dataset1$Pfizer, dataset1$AmerExp)
# p-value = 0.03896, Variance are not equal.

#(e)
wilcox.test(dataset1$Pfizer, dataset1$AmerExp)
# p-value = 0.1662, we cannot reject the null hypothesis that  true location shift is equal to 0.



##### Question 4
my.t.test <- function(x, y = "default", alternative = "two.sides", mu = 0){
  if(!is.character(y) && length(y)!=1){ ## Two sample case
    n1 = length(x)
    n2 = length(y)
    v1 = sd(x)^2
    v2 = sd(y)^2
    if(var.test(x,y)$p.value <= 0.05){#unequal variance
      stat = (mean(x) - mean(y) - mu)/sqrt(v1/n1 + v2/n2)
      df = (v1/n1 + v2/n2)^2 / ((v1/n1)^2/(n1-1) + (v2/n2)^2/(n2-1))
    }
    else{#equal variance
      df = n1+n2-2
      sp_square = ((n1-1)*v1 + (n2-1)*v2)/df
      stat = (mean(x) - mean(y) -mu) / sqrt(sp_square*(1/n1 + 1/n2))
      }
  }
  else{#one sample
    if(y!="default"){
      if(length(y)==1 && is.numeric(y)){
        mu = y
      }
      else{
        if(is.numeric(alternative)){
          mu = alternative
        }
        alternative = y
      }
    }
    stat = (mean(x) - mu) / (sqrt(sd(x)^2/length(x)))
    df = length(x) - 1
  }
  if(alternative == "two.sided"){ #double side
    p.value = 2*pt(abs(stat), df, lower.tail = F)
  }
  else if(alternative == "greater"){#one side
    p.value = pt(stat, df, lower.tail = F)
  }
  else { #one side
    p.value = pt(stat, df, lower.tail = T)
  }
  return(c(stat = stat, df = df, p.value = p.value))
}

#Test the function
x = rnorm(30)
my.t.test(x, y = rnorm(20,mean=10,sd=1))
my.t.test(x, y, "two.sided")
my.t.test(x)

###Question 5
#(a)
# Let Residual = sum((Yi - Beta*Xi)^2), 
# Then minimize Residual, let d(Residual)/d(Beta) = 0,
# Then we get sum(Xi*(Yi-Beta*Xi))=0,
# So Beta = sum(Xi*Yi)/ sum(Xi^2).

#(b)
set.seed(123)
x <- rnorm(50)
y <- 2*x + rnorm(50)

Beta <- sum(x*y)/ sum(x^2)
plot(x, y, main = "scatterplot with the ???tted line")
abline(a = 0, b = Beta)

#(c)
fit <- lm(y~-1+x)
summary(fit)
