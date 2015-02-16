
#####second attempt
corr <- function(directory, threshold = 0) { 
  id <- 1:332
  complete <- function(directory, id = 1:332) {
    x <-vector(mode='numeric', length=0)
    for(i in id)  {
      data <- read.csv(paste('//Users/hcronin/Documents/datasciencecoursera-repo/', directory, "/", sprintf('%03d',i),'.csv', sep=""), header=TRUE)
      data1 <-data[complete.cases(data),]
      if (length(data1$ID)!=0) {data2 <- (aggregate(data1$sulfate, by=list(data1$ID), FUN=length))}  else {data2 <- data.frame(ID=min(data$ID), nobs=0)}
      colnames(data2) <- c("ID","nobs")
      data3 <- if (data2$nobs>threshold) {cor(data1[,2], data1[,3])}
      x <-c(x,data3)
    }
    return(x)
  }
  x <-complete(directory)

}