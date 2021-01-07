# Add event times (treatment and death) in the population, as long as each
# individual is at risk of death
refineTableAllEventTimes <- function(dataFr, eventState = c("death", "treat")){
  
  # All event times in the population (treatment or death) are relevant as long
  # as an individual is at risk of death
  eventTimesPopulation.DT <- dataFr[to.state %in% eventState, sort(to, index.return = T)]
  eventTimesPopulation <- eventTimesPopulation.DT$x
  toStatePopulation <-  dataFr[eventTimesPopulation.DT$ix, to.state]
  
  # Defining row number for row-wise evaluation in the next step
  dataFr[, rowNumber := .I]
  
  # When the subjects are at risk of death, they should have one row for each
  # event time (death or treatment) in the population. So we count the number of
  # events in the population that occur between the events for an individual
  dataFr[,numRep := as.numeric(sum(from < eventTimesPopulation & eventTimesPopulation < to)),by=rowNumber]
  
  # Expanding the data.table
  dataFr_expand <- dataFr[rep(1:.N,times=dataFr[,numRep] + 1)]
  
  # Preparing to set the population event times when the subject is at risk of death
  dataFr_expand[,putEventTimesPopulation := 
                  c(eventTimesPopulation[from[1] < eventTimesPopulation & eventTimesPopulation < to[1]], to[1]),
                by=rowNumber]
  dataFr_expand[,putToStatePopulation := 
                  c(toStatePopulation[from[1] < eventTimesPopulation & eventTimesPopulation < to[1]], to.state[1])
                ,by=rowNumber]
  
  # Label the event times that occur in the population (e.g. to.state = "treat.pop")
  dataFr_expand[putEventTimesPopulation != to, putToStatePopulation := paste0(putToStatePopulation, ".pop")]
  
  # Setting the population even times and their state
  dataFr_expand[, to := putEventTimesPopulation]
  dataFr_expand[, from := c(from[1], to[-.N]), by = id]
  dataFr_expand[, to.state := putToStatePopulation]
  dataFr_expand[, from.state := c(from.state[1], to.state[-.N]), by = id]
  
  # Deleting unnecessary columns 
  dataFr_expand[, c("rowNumber", "numRep", "putEventTimesPopulation", "putToStatePopulation") := NULL]
  
  return(dataFr_expand)
} 