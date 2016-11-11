getwd()
fn<-paste("coursera/","brUsers.csv",sep="")
##users <- read.csv("coursera/brUsers.csv",TRUE,',')
users<-read.csv(fn,TRUE,',')
users
head(users)
x <- c(1,NA,2)
names(x)=c("yes","na","no")
x[is.na(x)]
x[!is.na(x)]
x["yes"]
class(x)
head(users)
subset(users,age>65)
mylist <- list(
  fruits = c("banana","orange","apple"),
  mat = matrix(1:10,nrow=2),
  int = c(1,2,3)
)
mylist
mylist$fruits
URL <- "https://www.stanford.edu/~druau/pivot_table.csv"
pivot <- read.table(URL,sep=',',header = TRUE)
head(pivot)
users$btc <- round(users$btc,digits=3)
users
round(users$btc,digits=2)
test <- data.frame(users$lastName,users$age,users$income)
test
sqldf('SELECT * FROM users WHERE age>60 order by lastname')
mean(users$income)
 
i <- 200000
for (l in 1:10) {
if (users$income[l] > i) { print('lastname')
  print(users$lastName[l])
  print(users$income[l])
  
} else { print('first')  
  print(users$firstName[l])

}
}
# multi core
ncore = multicore::detectCores()

f <-c(NA)
f[1] <- f[2] <- 1
for (i in 3:50) {
  f[i] <- f[i-1] + f[i-2]
  
}
f 
fib <- function(n=20){
  if (n<3) {
    return(c(1,1))
  } else {
    fib <- c(NA)
    fib[1] <- fib[2] <- 1
    for (i in 3:n) {
       fib[i] = fib[i-1] + fib[i-2]
    }
    return(fib)
  }
}
fib(40)
fib(1)
install.packages("dplyr")
x1 <- c(1,1,1,1)
x2 <- c(2010,2011,2014,2015)
x3 <- c(2100,2200,2500,1800)
x4 <- c(3,4,2,2)
m  <- matrix (c(x1,x2,x3,x4),ncol=4)
m
dimnames(m) <- list(
                    c("1","2","3","4"),
                    c("x0","year","size","baths"))
m
class(m)
n1 <- c(7921,5184,8836,4761)
mean(n1)
range(n1)
X <- (n1[4] - mean(n1))/ (max(n1) - min(n1))
n1 ^ 2
n1
rand(1,10)
home <- data.frame(nm=c('sateesh','swathi','sirihaasa','shreya'),
                   gender=c('M','F','F','F'))
home
home <- data.frame(home,age=c(43,40,11,9))
users
plot(users$age,users$btc)
abline(lm(users$btc~users$age))
scoredata <- read.csv('ex2data1.txt',header=FALSE,sep=',')
names(scoredata) <- c("ex1","ex2",'Result')
scoredata
subset(scoredata,Result==0)
scoredata$Result==0
plot(scoredata$ex1,scoredata$ex2)
abline(lm(scoredata$ex2~scoredata$ex1))
lr <- glm(scoredata$Result~scoredata$ex1+scoredata$ex2,data=scoredata)
summary(lr)
houseprice <- read.csv('ex1data2.txt',header=FALSE,sep=',')
houseprice
names(houseprice) <- c('sqrft','br','price')
rm(houseprise)
test <- houseprice[(45:47),]
train <- houseprice[(1:44),]
lm_reg <- lm(houseprice$price~houseprice$sqrft+houseprice$br,data=train)
summary(lm_reg)
predicted <- predict(lm_reg,newdata = test)
predicted
ex2data1 <- read.csv('ex2data1.txt',header=FALSE,sep=',')
names(ex2data1) <-c('ex1','ex2','admit')
ex2data1
logisticreg<-glm(admit~ex1+ex2,data=ex2data1,family=binomial)
logisticreg
summary(logisticreg)
fnames <- c("001","002","003")
fnamei <- c(70,71,72)
formatC(fnamei,width=3,flag = "0")
fn1<-paste("specdata/",formatC(fnamei,width = 3,flag = "0"),".csv",sep="")
fn1
specdt <- list()
specdt[[1]]<-na.omit(read.csv(fn1[1],TRUE,','))
specdt[[2]]<-na.omit(read.csv(fn1[2],TRUE,','))
specdt[[3]]<-na.omit(read.csv(fn1[3],TRUE,','))
nobs <- sapply(specdt,nrow)
nobs
specdt[nobs < 200]
specdata <- do.call(rbind,specdt)
var<-"nitrate"
mean(specdata[[var]])
#specdata <- rbind(na.omit(specdata1),na.omit(specdata2))
#specdata
#specdata[complete.cases(specdata[,2:3]),]
#na.omit(specdata)
source("pollutantmean.R")
pollutantmean("specdata", "sulfate", 1:10)
pollutantmean("specdata", "nitrate", 23)
id <- numeric()
id
cor(specdata[,2],specdata[,3])
source("corr.R")
cr <- corr("specdata", 150)
head(cr)
 
cr <- corr("specdata", 2000)                
n <- length(cr)                
cr <- corr("specdata", 1000)                
cr <- sort(cr)
print(c(n, round(cr, 4)))