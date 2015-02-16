complete <- function(directory, id = 1:332) {
  x <-data.frame(ID=numeric(), nobs=numeric())
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files 
  for(i in id)  {
    data <- read.csv(paste('//Users/hcronin/Documents/datasciencecoursera-repo/', directory, "/", sprintf('%03d',i),'.csv', sep=""), header=TRUE)
    data1 <-data[complete.cases(data),]
    if (length(data1$ID)!=0) {data2 <- (aggregate(data1$sulfate, by=list(data1$ID), FUN=length))}  else {data2 <- data.frame(ID=min(data$ID), nobs=0)}
       colnames(data2) <- c("ID","nobs")
    x <- rbind(x, data2)
  }
  return(x)
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
}

