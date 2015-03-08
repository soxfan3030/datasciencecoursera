

best <- function(state, outcome) {
  ## Read outcome data
  data <- read.csv('outcome-of-care-measures.csv', colClasses = "character")
  ## Check that state and outcome are valid
  if(!(state %in% data$State)) {stop('invalid state', call.=TRUE)}
  validoutcomes <- c('heart attack', 'heart failure', 'pneumonia')
  if(!(outcome %in% validoutcomes))  {stop('invalid outcome', call.=TRUE)}
  ## Return hospital name in that state with lowest 30-day death
  stateoutcome <- subset(data,State==state)
  if (outcome=='heart attack')
    {stateoutcome[,11] <-as.numeric(stateoutcome[,11])
  stateoutcome1 <-stateoutcome[order(stateoutcome[,11]), ]
}
if (outcome=='heart failure')
{stateoutcome[,17] <-as.numeric(stateoutcome[,17])
 stateoutcome1 <-stateoutcome[order(stateoutcome[,17]), ]
}
if (outcome=='pneumonia')
{stateoutcome[,23] <-as.numeric(stateoutcome[,23])
 stateoutcome1 <-stateoutcome[order(stateoutcome[,23]), ]
}
stateoutcome1[1,2]
}
