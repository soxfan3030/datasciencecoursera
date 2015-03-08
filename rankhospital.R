

rankhospital <- function(state, outcome, num) {
  ## Read outcome data
  data <- read.csv('outcome-of-care-measures.csv', colClasses = "character")
  ## Check that state and outcome are valid
  if(!(state %in% data$State)) {stop('invalid state', call.=TRUE)}
  validoutcomes <- c('heart attack', 'heart failure', 'pneumonia')
  if(!(outcome %in% validoutcomes))  {stop('invalid outcome', call.=TRUE)}
  ## Return hospital rank in that state with lowest 30-day death
  stateoutcome <- subset(data,State==state)
  if (outcome=='heart attack')
  {stateoutcome[,11] <-as.numeric(stateoutcome[,11])
   stateoutcome <- stateoutcome[, c(2,11)]
   stateoutcome1 <-stateoutcome[order(stateoutcome[,c(2)]), ]
   stateoutcome1 <-stateoutcome1[!is.na(stateoutcome1[,2]),]
   x <-stateoutcome1[,2]
   stateoutcome2 <-lapply(split(stateoutcome1, x), function(x) x[order(x$Hospital.Name), ])
   stateoutcome4 <-unsplit(stateoutcome2, x)
  }
  if (outcome=='heart failure')
  {stateoutcome[,17] <-as.numeric(stateoutcome[,17])
   stateoutcome <- stateoutcome[, c(2,17)]
   stateoutcome1 <-stateoutcome[order(stateoutcome[,c(2)]), ]
   stateoutcome1 <-stateoutcome1[!is.na(stateoutcome1[,2]),]
   x <-stateoutcome1[,2]
   stateoutcome2 <-lapply(split(stateoutcome1, x), function(x) x[order(x$Hospital.Name), ])
   stateoutcome4 <-unsplit(stateoutcome2, x)
  }
   if (outcome=='pneumonia')
  {stateoutcome[,23] <-as.numeric(stateoutcome[,23])
   stateoutcome <- stateoutcome[, c(2,23)]
   stateoutcome1 <-stateoutcome[order(stateoutcome[,c(2)]), ]
   stateoutcome1 <-stateoutcome1[!is.na(stateoutcome1[,2]),]
   x <-stateoutcome1[,2]
   stateoutcome2 <-lapply(split(stateoutcome1, x), function(x) x[order(x$Hospital.Name), ])
   stateoutcome4 <-unsplit(stateoutcome2, x)
  }
  
  ifelse(as.character(num)=='best', stateoutcome4[1,1], ifelse(as.character(num)=='worst',
    stateoutcome4[nrow(stateoutcome4),1],
  stateoutcome4[as.numeric(num),1]
  ))
}

#unsplit(lapply(split(stateoutcome1, stateoutcome1[,17]), order(stateoutcome1$Hospital.Name), stateoutcome1[,17])