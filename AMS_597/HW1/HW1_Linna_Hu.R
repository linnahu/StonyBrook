###### Question_1
###### (a)
weight <- c(60, 72, 34, 56, 87, 80, 89, 95, 76, 28, 48, 59)
######(b)
mean(weight)    # 65.33333
mean(weight^2)   #4694.667
#####(c)
length(weight)   #12
#####(d) How many weights are larger than 55?
length(weight[weight > 55])   #9
#####(e) each weight is larger than 55 and smaller than 85
weight[weight > 55 & weight < 85]   ## 60£¬72£¬57£¬72


#####    Question_2
tmp <- matrix(rnorm(12) ,3 ,4 )       

#####  (a) Compute the sum of the ???rst and third column. 
sum(tmp[,1], tmp[,3])  #wrong, this is a number not a vector

tmp[,1] + tmp[,3]  # correct answer

#####  (b) Compute the product of the ???rst and third row. 
tmp[1,] * tmp[3,]    

#####(c) Show the dimension of the matrix. 
dim(tmp)    #3 4

#####(d) Use ¡®cat¡¯ function to output elements in the ???rst row that are larger than 0.5.
cat(tmp[1,][tmp[1,] > 0.5])


#####    Question_3
#How would you check whether two vectors are the same if they may contain missing (NA) values? 
#(Use of the identical function is considered cheating!)
x <- c(1, NA, 3)
y <- c(1, 2, 3)
length(x) == length(y) & all(is.na(x) == is.na(y)) & all(x[!is.na(x)] == y[!is.na(y)]) 


#####    Question_4
# If x is a factor with n levels and y is a length n vector, what happens if you compute y[x]?
n = 100
x <- factor(1:n, levels = 1:n)
y <- c(n:1)
y[x]   # y[x] will return y[100], y[99], .... , y[1]


#####    Question_5
mydna <- paste(sample(c('a', 't', 'c', 'g'), 1000, replace= T), collapse='')
# Write a function that takes a string as a input, counts the number of ¡°cg¡± in the input string, and replace 
#all ¡°cg¡± with ¡°XY¡±. Apply your function to mydna. (Do not use any special R packages.)
DNA <- function(string){
  x <- length(gregexpr('cg', string)[[1]])
  newstring <- chartr('cg', 'XY', string)
  print(n)
  print(newstring)
}

DNA(mydna)
# This is wrong
# chartr replace all the 'c' and 'g'. But we only need to replace 'cg',
# This is different.

##### Kuan's answer
f <- function(a){
  I = (nchar(a) - nchar(gsub("cg", "", a))) / nchar("cg")
  return(list("counts the number of cg" = I, "replace all cg with XY" = gsub("cg","XY", a)))
}
mydna <- paste(sample(c('a','t','c','g'),1000, replace = T), collapse = '')
f(mydna)
##### Question_6
#write a function 
#and output rows containing valid phone numbers from the ???le. 

pnumber <- function(){
  file <- file("http://www.ams.sunysb.edu/~pfkuan/Teaching/AMS597/Data/PhoneNumber.txt", open="r")
  lines <- readLines(file)
  pattern.1 = '\\d\\d\\d-\\d\\d\\d-\\d\\d\\d\\d'
  pattern.2 = '\\(\\d\\d\\d\\)\\s\\d\\d\\d-\\d\\d\\d\\d'
  pattern.3 = '\\d\\d\\d\\s\\d\\d\\d\\s\\d\\d\\d\\d'
  pattern.4 = '\\d\\d\\d\\.\\d\\d\\d\\.\\d\\d\\d\\d'
  for (i in 1:length(lines)){
    if (grepl(pattern.1,lines[i])) print(lines[i])
    if (grepl(pattern.2,lines[i])) print(lines[i])
    if (grepl(pattern.3,lines[i])) print(lines[i])
    if (grepl(pattern.4,lines[i])) print(lines[i])
  }
}

## Kuan's answer
mydat <- read.table('http://www.ams.sunysb.edu/~pfkuan/Teaching/AMS597/Data/PhoneNumber.txt',sep='\n')

subsetPhone <- function(dat){
  p1 <- "[0-9]{3}[:.:]|-|[[:space:]][0-9]{3}[:.:]|-|[[:space:]][0-9]{4}"
  p2 <- "([0-9]{3})[[:space:]][0-9]{3}-[0-9]{4}"
  
  myfunc <- function(x){
    tmp1 <- grepl(p1,x)
    tmp2 <- grepl(p2,x)
    return(max(tmp1,tmp2))
  }
  
  return(dat[apply(mydat,1,myfunc)==1,])
}

subsetPhone(mydat)


