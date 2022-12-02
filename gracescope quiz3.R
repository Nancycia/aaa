set.seed(223)
a <-runif(n=10,min=4,max=20)
b <-runif(n=10,min=-10,max=6)

C <-outer(a,b,"*")
D <-log(C)

typeof(C)
typeof(D)


library(lobstr)
col_sums_D <-colSums(D)
sum(col_sums_D, na.rm=TRUE)

row_sums_D <-rowSums(D)
sum(row_sums_D, na.rm=TRUE)

prod(row_sums_D,na.rm=TRUE)
prod(col_sums_D,na.rm=TRUE)

object.size(C)
object.size(D)
