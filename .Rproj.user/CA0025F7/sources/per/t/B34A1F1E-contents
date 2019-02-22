## function best() will take a state and illness outcome
## it returs the best performing hospital in that state


best <- function(state, outcome) {
        out <- read.csv("outcome-of-care-measures.csv") #read outcome data to a DF
        
        
        ##create vector of allowable states and outcomes
        allowstates <- unique(out[,7])
        allowout <- c("heart attack", "heart failure", "pneumonia")
        outlist <- list("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
        
        if (state %in% allowstates & outcome %in% allowout){
                pertout <- na.omit(data.frame(
                        "hosp" = out[,2], 
                        "State" = out[,7], 
                        "sorting" = as.numeric(paste(out[,outlist[[outcome]]]))))
                
                sortlist <- split(pertout, pertout$State)
                statedf <- sortlist[[state]]
                sorted <- statedf[order(statedf$sorting),]
                output <- sorted[1,1]
                
                
        } else {
                output <- "Input variables not found"
        }
        
       output
      
}