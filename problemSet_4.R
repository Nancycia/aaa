library(magrittr)
library(readr)
library(tidyr)
library(dplyr)
#0
myName = "Lintong Li"

#1
print_order <- function(x)
{
  y <- c()  
  max <- max(x)
  min <- min(x)
  for (i in 1:3)
  {
    if (x[i] == max){y[1] = x[i]}
    else if (x[i] == min){y[3] = x[i]}
    else{y[2] = x[i]}
  }
  return(y)
}

#2
print_string <- function(x)
{
  for(i in 1:x)
  {
    if (i %% 3 == 0 & i %% 5 !=0){print("Yes")}
    else if (i %% 5 == 0 & i %% 3 !=0){print("No")}
    else if (i %% 3 == 0 & i %% 5 ==0){print("Unknown")}
    else{print(i)}
  }
}

#3
calc_sum_of_factor <- function(x)
{ f <- c()
count <- 0
for(i in 1:x)
{
  if (x %% i == 0){
    count <- count+1
    f[count]=i
  }
}
return(sum(sapply(f,function(f) f ^ 2)))
}

#4
fake_intersect <- function(a,b){
  
  a <- a[!duplicated(a)]
  b <- b[!duplicated(b)]
  ab <- append(a,b)
  
  logic <- duplicated(ab)
  index <- which(logic == TRUE )
  
  ans <- ab[index]
  return(ans[!duplicated(ans)])
  
}

find_intersect <- function(a,b,c){
  
  d <- fake_intersect(a,b)
  e <- fake_intersect(d,c)
  
  return(e)
  
}

#5
factorial_base <- function(x)
{count <- 1
  for (i in 1:x){count <- count *i}
return(count)
}

#6.1
T <- function(n){
  return(n*(n+1)/2)
}
#6.2
perfect_sqr <- function(x){
  if (sqrt(x) == trunc(sqrt(x))){return("TRUE")}
      else{return("False")}
}
#6.3
num_tri_sqr <- function(x){
  num <- c()
  count <- 0
  for (i in 1:x){
    if(sqrt(i*(i+1)/2)==trunc(sqrt(i*(i+1)/2))){
      count <- count+1
      num[count]=i*(i+1)/2}
  }
    return(num)
}
q6_sum <- sum(num_tri_sqr(1500000))

#assign1
h1b_2022 <- read_csv("https://www.uscis.gov/sites/default/files/document/data/h1b_datahubexport-2022.csv")
#assign3
na_num <- sum(is.na(h1b_2022))
h1b_2022a <- h1b_2022 %>% drop_na()
h1b_2022a <- h1b_2022a[h1b_2022a$City != "-",]
h1b_2022a <- h1b_2022a[h1b_2022a$State!="-",]
#assign4
df_num <- aggregate(cbind(h1b_2022a$`Initial Approval`+h1b_2022a$`Initial Denial`,h1b_2022a$`Continuing Approval`+h1b_2022a$`Continuing Denial`,h1b_2022a$`Initial Approval`,h1b_2022a$`Initial Denial`),by=list(State=h1b_2022a$State),sum)
names(df_num) <- c("State","Init App","Conti App","Approve","Denial")
df_num <- as_tibble(df_num)
df_num
#assign5
app_num <- sum(df_num$Approve)
app_num
den_num <- sum(df_num$Denial)
den_num
#assign6
city_num <- as.data.frame(table(h1b_2022a$City))
names(city_num) <- c("City","Count")
city_num$City <- as.character(city_num$City)

#assign7
visa_num <- as.data.frame(table(h1b_2022a$NAICS))
names(visa_num) <- c("NAICS","Number")
visa_num$Percentage <- round(100*(visa_num$Number)/sum(visa_num$Number),3)
visa_num$NAICS <- as.numeric(levels(visa_num$NAICS)[visa_num$NAICS])

#extra
non_integer_factorial <- function(x){x*gamma(x)}
