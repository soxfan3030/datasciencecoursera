setwd('//Users/hcronin/Documents/datasciencecoursera-repo/')

pollutantmean <- function(directory, pollutant, id = 1:332) {
  x <-vector(mode="numeric", length=0)
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files 
  for(i in id)  {
    data <- read.csv(paste('//Users/hcronin/Documents/datasciencecoursera-repo/',directory, "/", sprintf('%03d',i),'.csv', sep=""), header=TRUE)
   column_select <-c('Date', 'ID', pollutant)
   data1 <-subset(data, select=column_select)
   data2 <-data1[!is.na(data1[,3]),]
   data3 <-data2[,3] 
   x <- c(x, data3)
   #return(data3)
  }
  mean(x)
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
}
