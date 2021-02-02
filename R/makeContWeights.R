# dataFr: a data.table
makeContWeights <- function(faFit,cfaFit,dataFr,atRiskState,eventState,startTimeName,stopTimeName,startStatusName,endStatusName,idName,b,weightRange = c(0,10),dKRange = c(-10,10),willPlotWeights=T, thetaDesignXNonInvertible=NULL, max.time = NULL, thetaRange = NULL){
        
        if(class(faFit) != "aalen" | class(cfaFit) != "aalen")
          stop("The survival fits must be of type aalen.",call. = F)
        if(!requireNamespace("data.table",quietly = T))
          stop("The data.table package is needed for this function to work. Please install it.",call. = F)
  
        # Making new names for convenience
        namesMatch <- match(c(startStatusName,endStatusName,startTimeName,stopTimeName,idName),names(dataFr))
        saveNames <- names(dataFr)[namesMatch]
        names(dataFr)[namesMatch] <- c("from.state","to.state","from","to","id")
        
        # if(!is.null(max.time))
        #   dataFr <- dataFr[to <= max.time]

        # Add noise to tied times
        #dataFr <- addNoiseAtEventTimes(dataFr)
        
        # Data table to get predictions along
        wtFrame <- dataFr[from.state %in% atRiskState]
        # Note that we need to provide the event times at which predictions are
        # calculated. Otherwise, event times from the fitting procedure are used
        # (which could go beyond the intended follow-up when max.time != NULL )
        if(!is.null(max.time)) {
          predTimes <- wtFrame[to.state == eventState & to < max.time, to]
          predTimeIds <- wtFrame[to.state == eventState & to < max.time, id]
        }
        else {
          predTimes <- wtFrame[to.state == eventState, to]
          predTimeIds <- wtFrame[to.state == eventState, id]
        }
        
        pft <- predict(faFit,newdata=wtFrame, n.sim=0,se=F,resample.iid=0,
                       times = c(0, sort(predTimes)))
        cpft <- predict(cfaFit,newdata=wtFrame,n.sim=0,se=F,resample.iid=0,
                        times = c(0, sort(predTimes)))
        
        # Keep the times at which the design matrix from the fitting procedure
        # was non-invertible
        attr(pft, "timesDesignXNonInvertible") <- attr(faFit, "timesDesignXNonInvertible")
        attr(cpft, "timesDesignXNonInvertible") <- attr(cfaFit, "timesDesignXNonInvertible")
        
        ids <- dataFr[, unique(id)]
        eventIds <- predTimeIds #wtFrame[to.state %in% eventState, id]
        
        # Times we want to estimate the weights at
        eventTimes <- predTimes # wtFrame[to.state %in% eventState, to]
        sortedEventTimes <- sort(eventTimes)
        
        # Obtain estimated weights
        fPred<- pft;cfPred  <- cpft
        weightFrame <- weightPredict(pft,cpft,wtFrame,ids,eventTimes,eventIds,b,thetaDesignXNonInvertible, thetaRange)

        # Refining the data.frame for individuals at risk
        Table <- refineTable(dataFr,atRiskState,eventState)
        
        # Merge so that Table includes the weight estimates 
        Table <- merge(Table,weightFrame[, !c("dK")],by.x=c("id","from"), by.y = c("id", "to"),all.x=T)
        Table <- merge(Table,weightFrame[, .(id, to, dK)],by.x=c("id","to"), by.y = c("id", "to"),all.x=T)
        setcolorder(Table, c("id", "from", "to"))
        
        # Individuals weight constant after time of treatment 
        # (and the corresponding treatment process is set to NA)
        Table[isAtRiskForTreatment != 1,weights := weights[1],by=id]
        Table[isAtRiskForTreatment != 1,dK := NA_real_,by=id]
        
        # Remove unwanted columns
        Table <- subset(Table,select= !(names(Table) %in% c("rowNumber","numRep","putEventTimes","isAtRiskForTreatment","eventTime")))
        
        # Set weights to the last available value. This should apply to weights
        # after treatment time (this check is not implemented here!) This also
        # applies to weight estimates at times after max.time, which are the
        # times after which the design matrix in the treatment models (obs/hyp)
        # could be non-invertible (again, this check is also not implemented here!)
        Table[,weights:=naReplace(weights),by=id]

        if(is.null(thetaRange)){
          # Truncate weights that are outside a given range
          Table[weights < weightRange[[1]], weights := weightRange[[1]]]
          Table[weights > weightRange[[2]], weights := weightRange[[2]]]
          
          # Also truncate the increments of the treatment process (dK) 
          Table[dK < dKRange[[1]], dK := dKRange[[1]]]
          Table[dK > dKRange[[2]], dK := dKRange[[2]]]
        }
        
        # Optional plot of the weight trajectories
        if(willPlotWeights == T)
          plotContWeights(Table)
        
        # Switching names back
        namesMatch <- match(c("from.state","to.state","from","to","id"),names(Table))
        names(Table)[namesMatch] <- saveNames
        
        return(Table)
}
