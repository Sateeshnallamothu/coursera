pollutantmean <- function(directory, pollutant,id=1:332) {
  fn1<-paste(directory,"/",formatC(id,width = 3,flag = "0"),".csv",sep="")
  specdata <- list()
  for (i in 1:length(fn1)) {
      specdata[[i]]<-na.omit(read.csv(fn1[i],TRUE,','))
  }
  finaldata <- do.call(rbind,specdata)
  mn <-mean(finaldata[[pollutant]])
  mn
}