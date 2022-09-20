myName="Lintong Li"
  
#1
v1 <- 1:20
v2 <- 20:1
v3 <-seq(1,20,by=2)
v4 <-rep(c(3,7,11),10)
v5 <-c(rep(c(3,7,11),10),3)
#2
tmp2 <- seq(3,6,by=0.1)
x1 <- exp(tmp2)*sin(tmp2)
#3
tmp3 <- 10:100
sum1 <- sum(tmp3^3+4*tmp3^2)
#4
str1 <- paste("label",1:30)
str2 <-paste("function",1:30,sep="")
#5
vs <- paste(c('1',"function",'NA',seq(1,5,2),0.125),collapse = ',')
#6
A <- matrix(1:9, nrow=3)
m1_ans <- A %*% A %*% A
#7
B <- matrix(c(12,-12,12), b=T, nc=3, nr=17)
m2_ans <- t(B)%*%B
#8
y <- c(7,-1,-3,5,17)
A <- matrix(0,nr=5,nc=5)
A <- abs(col(A)-row(A))+1
m3_ans <- solve(A,y)
#9
#(a)
func1 <- function(xv) {
  xv <- xv^(1:length(xv))
  return(xv)
}
xv <- seq(0,1,by=0.1)
func1_ans <- func1(xv)
#(b)
func2 <-function(xv){
  n <-length(xv)
  xv <- (xv^(1:n))/(1:n)
  return(xv)
}
xv <- seq(0,1,by=0.1)
func2_ans <- func2(xv)
#(c)
func3 <-function(x,n){
  n <-length(xv)
  xv <- 1+sum((xv^(1:n))/(1:n))
  return(xv)
}
xv <- seq(0,1,by=0.1)
func3_ans <- func3(xv)
#10
# F=(9/5)C + 32
cel_to_far <- function(tC){
  tF <- (9/5) * tC  +32
  return(tF)
}
#
far_to_cel <- function(tF){
  tc <- (tF-32) * (5/9)
  return(tc)
}

#11
func_odd <- function(x) x[ x %% 2 ==1]
odd_ans <- func_odd(1:2000)
#12
func_r <- function(r){
  s <- 1:r
  return(sum((s^0.5)/(11 + 3.5 * r^1.2)))
}
sum_ans <- sapply(10,func_r)

#13
modNumber <- function(x,y){
  if (x%%y==0)
    {return(x)}
  else
    {return((x%/%y+1)*y)}
}
#14 numberOfWheels
numberOfWheels <- function(x){return(switch(x,"unicycle"=1, "bike"=2, "car"=4,"truck"=4 ,"tricycle"=3,"motorcycle"=2))
}
#15
myFactorial <- function(n){
  if(n <=1){
    return(1)
  }else{
    return(n*myFactorial(n-1))
}
}
#16
myCustomFactorial <-function(x,y){
  factorial(y)/factorial(x-1)
}
#17
customRiverMean <- function(x)
{
  myv <- rivers
  Numofv <- 0
  Lenofv <- 0
  for (i in 1:length(myv))
  {
    if (myv[i] < x)
    {
      Numofv <- Numofv+1
      Lenofv <- Lenofv + myv[i]
    }
  
  }
  return(Lenofv/Numofv)
}
#18
data("ToothGrowth")
longTeeth =NULL
len=ToothGrowth$len
for(i in 1:length(ToothGrowth$len)){
  if (len[i] >=15){
    longTeeth = append(longTeeth,len[i])
  }
}
#19
data("mtcars")
ls <- apply(mtcars,2,mean)
averageHorsePower <- ls[["hp"]]
averageWeight <- ls[["wt"]]

