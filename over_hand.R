set.seed(123)
library(purrr)

  #Over-hand Shuffle
  homeshuffle <- function(x) {
    i = rdunif(1,25,1)
    k = rdunif(1,52,27)
    temp = x[(i+1):k]
    x[(k-i+1): k] = x[1: i]
    x[1: (k-i)] = temp
    return(x)
  } 
 home.times = function(x,n) {
    y = x
    for(i in 1:n){
      y = homeshuffle(y)
    }
    return(y)
  }

#### Output sequence after the n many shuffle 
 set.seed(123)
 list.times.shuffle = c(5,20,50)
 par(mfrow = c(length(list.times.shuffle),1))
 for(p in list.times.shuffle){
   x = 1:52
   y = home.times(x,p)
   plot(y,type = "l",xlab = paste0("number of shuffle = ",p ))
   
 }
#### histogram for distribution of k^th position  AS WELL AS####
### Variation distance of kth position
set.seed(12)
k.list = c(1,26,52) # kth posiotn of the deck
n = 10000
a = 1:52
#list.times.shuffle = c(1,3,7)
variation.vector <- vector()

for(j in list.times.shuffle){
distri.shuffle = replicate(n, home.times(a,j)[k.list])
distri.theor = replicate(n, sample(a)[k.list])
par(mfrow=c(3,1))
for(i in 1:3){
  s = ""
  if(i == 1){
    s = paste0("Histogram for kth position after ",j, " number of Over-Hand shuffles.")
  }
  hist(distri.shuffle[i,],breaks = 26,xlab = paste0("The distribution of ",k.list[i]," position of the shuffled deck ")
      , main = s )
}

table.theor = tabulate(distri.theor[3,],nbins = 52)/n
table.shuffle = tabulate(distri.shuffle[3,],nbins = 52)/n
par(mfrow=c(1,1))
plot(ecdf(distri.theor[3,]),xlab= "",main = "ECDF of kth card - theoretic(black) vs shuffled(Red) ")
plot(ecdf(distri.shuffle[3,]),add=TRUE,col='RED')

variation.shuffle.distr = (sum(abs(table.theor-table.shuffle )))/2
variation.vector = c(variation.vector,variation.shuffle.distr)
}


###### Testing of rising sequence and fixed point #### 
set.seed(123)
countrs <- function(x) {
  c = 1
  i = 1
  while(i < 52) {
    if (which(x == i) > which(x == i+1)) {
            c = c+1}
    i = i + 1
  }
return(c)
}

countfp <- function(x) {
  c = 0
  i = 1
  while(i <= 52) {
    if (i == x[i]) {
      c = c + 1 }
      i = i + 1
  }
  return(c)
}


### for expected counts of fp and rs
set.seed(123)
n = 10000
a = 1:52
list.of.times = 1:30

countfp.list = vector()
for(j in list.of.times){
  output.j = replicate(n, countfp(home.times(a,j)))
  countfp.list= c(countfp.list,sum(output.j)/n)
}
plot(countfp.list,type = "both", xlab = "Number of Shuffles ", ylab = "Expected number of fixed points."
     ,main = "plot of fixed points in over-hand shuffle")
output.theoretical.fp = sum(replicate(n, countfp(sample(a))))/n

##Rising Sequence 
set.seed(123)
countrs.list = vector()
for(j in list.of.times){
  output.j = replicate(n, countrs(home.times(a,j)))
  countrs.list= c(countrs.list,sum(output.j)/n)
}
plot(countrs.list,type = "both", xlab = "Number of Shuffles ", ylab = "Expected number of rising sequence."
     ,main = "plot of rising sequence in over-hand shuffle")


output.theoretical.rs = sum(replicate(n, countrs(home.times(a,j))))/n

####Run test ####

library(randtests)
set.seed(12)
n = 10000
a = 1:52
list.times.shuffle = c(5,20,50)
M = matrix(NA, nrow = 4 , ncol = n ) 
M[1,] = replicate(n, runs.test(sample(a))$runs )

for(j in 1:3 ){
  M[(j+1),] = replicate(n,runs.test(home.times(a,
                        list.times.shuffle[j]))$runs)
}

plot(ecdf(M[1,]), col = "RED",main = "Emperical cdf of runs for over-hand",
       xlab = " Red(Theo),  Black(5),  Green(20),  Blue(50) ",
     )
plot(ecdf(M[2,]), add=TRUE)
plot(ecdf(M[3,]), col = "GREEN", add=TRUE)
plot(ecdf(M[4,]), col = "BLUE", add=TRUE)

###Difference Sign Test ####

set.seed(123)
n = 10000
a = 1:52
list.times.shuffle = c(5,20,50)
M = matrix(NA, nrow = 4 , ncol = n ) 


sum.diff.sign <- function(z){
  z2 = sum(as.numeric((sign(z[-1]-z[-52])+1)/2))
}
M[1,] = replicate(n, sum.diff.sign(sample(a)) )

for(j in 1:3 ){
  M[(j+1),] = replicate(n, sum.diff.sign(home.times(a,list.times.shuffle[j])) )
}

plot(ecdf(M[1,]), col = "RED",main = "Emperical cdf of diff sign for over-hand ",
       xlab = " Red(Theo),  Black(5),  Green(20),  Blue(50) ",
     )
plot(ecdf(M[2,]), col = "BLACK", add=TRUE)
plot(ecdf(M[3,]), col = "GREEN", add=TRUE)
plot(ecdf(M[4,]), col = "BLUE", add=TRUE)


#####Turning point####


set.seed(123)
n = 10000
a = 1:52
list.times.shuffle = c(5,20,50)
M = matrix(NA, nrow = 4 , ncol = n ) 

turning.point = function(z){
  z1 = (sign(z[-1]-z[-52])+1)/2
  tp = sum(abs(z1[-1]-z1[-51]))
  return(tp)
}


M[1,] = replicate(n, turning.point(sample(a)) )

for(j in 1:3 ){
  M[(j+1),] = replicate(n, turning.point(home.times(a,list.times.shuffle[j])) )
}

plot(ecdf(M[1,]), col = "RED",main = "Emperical cdf of turning for over-hand",
       xlab = " Red(Theo),  Black(5),  Green(20),  Blue(50) ",
     )
plot(ecdf(M[2,]), col = "BLACK", add=TRUE)
plot(ecdf(M[3,]), col = "GREEN", add=TRUE)
plot(ecdf(M[4,]), col = "BLUE", add=TRUE)

 
 
 
 
 ### Trash ####

#r <- as.factor(bin.string(x2))
#matrix(r, nrow = 4, ncol = 13)
#matrix(x2, nrow = 4, ncol = 13)
x <- seq(1, 52)
x1 <- riffle(x)
x2 <- riffle.times(x,7)
# randomness test
#bin.string <- function(x) {
  y = seq(0, 0, length.out = 52)
  r = 1
  while( r <= 52){
    if(x2[r] < median(x)){
      y[r] = 0
      r = r + 1
    }
    else {
      y[r] = 1
      r = r + 1
    }
  }
  return(y)

#r <- as.factor(bin.string(x2))
#matrix(r, nrow = 4, ncol = 13)
#matrix(x2, nrow = 4, ncol = 13)

 