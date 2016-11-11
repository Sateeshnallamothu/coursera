corr <- function(directory, threshold=0){
  nm<-list.files(directory)
  fn1<-paste(directory,"/",nm,sep="")
  corr <- numeric()
  for (i in 1:length(fn1)) {
    specdata<-na.omit(read.csv(fn1[i],TRUE,','))
    if (nrow(specdata) > threshold) {
      corr[i]=cor(specdata[,2],specdata[,3])
    }
  }
  corr
}