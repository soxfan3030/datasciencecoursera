rankall <- function(outcome, num = "best") {
  ## Read outcome data
  data <- read.csv('outcome-of-care-measures.csv', colClasses = "character")
  ## Check that state and outcome are valid
  validoutcomes <- c('heart attack', 'heart failure', 'pneumonia')
  if(!(outcome %in% validoutcomes))  {stop('invalid outcome', call.=TRUE)}
  ## For each state, find the hospital of the given rank
  if (outcome=='heart attack')
  {data[,11] <-as.numeric(data[,11])
   data <- data[, c(2,7,11)]
   data <-data[order(data[,3]),]
   y <- data[!duplicated(data$State),2]
   data1 <-lapply(split(data, data$State), function(x) 
     ifelse(as.character(num)=='best', x[1,], ifelse(as.character(num)=='worst',
                                                  x[nrow(x),],
                                                x[as.numeric(num),])))
  ## Return a data frame with the hospital names and the
  data.frame <-data.frame(hospital=NULL, state=NULL)
  ## (abbreviated) state name
  for(i in y) {
    staterow <- as.data.frame(cbind(i, data1[[i]]))
    colnames(staterow) <- c("state", "hospital")
    data.frame <-rbind(data.frame, staterow)
  }} 
  
  if (outcome=='heart failure')
  {data[,17] <-as.numeric(data[,17])
   data <- data[, c(2,7,17)]
   data <-data[order(data[,3]),]
   y <- data[!duplicated(data$State),2]
   splitdata <-split(data, data$State)
   ordereddata <-lapply(split(splitdata, splitdata[,c(2,3)]), function(x) x[order(x$Hospital.Name), ])
   data1 <-lapply(split(data, data$State), function(x) ifelse(as.character(num)=='best', x[1,], ifelse(as.character(num)=='worst',
                                                                                                       x[nrow(x),],
                                                                                                       x[as.numeric(num),])))
   ## Return a data frame with the hospital names and the
   data.frame <-data.frame(hospital=NULL, state=NULL)
   ## (abbreviated) state name
   for(i in y) {
     staterow <- as.data.frame(cbind(i, data1[[i]]))
     colnames(staterow) <- c("state", "hospital")
     data.frame <-rbind(data.frame, staterow)
   }} 
  if (outcome=='pneumonia')
  {data[,23] <-as.numeric(data[,23])
   data <- data[, c(2,7,23)]
   data <-data[order(data[,3]),]
   y <- data[!duplicated(data$State),2]
   data1 <-lapply(split(data, data$State), function(x) ifelse(as.character(num)=='best', x[1,], ifelse(as.character(num)=='worst',
                                                                                                       x[nrow(x),],
                                                                                                       x[as.numeric(num),])))
   ## Return a data frame with the hospital names and the
   data.frame <-data.frame(hospital=NULL, state=NULL)
   ## (abbreviated) state name
   for(i in y) {
     staterow <- as.data.frame(cbind(i, data1[[i]]))
     colnames(staterow) <- c("state", "hospital")
     data.frame <-rbind(data.frame, staterow)
   }}
  data.frame$state <-as.character(data.frame$state)
  data.frame[order(data.frame[,'state']),]
  }