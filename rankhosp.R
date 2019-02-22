## function rankhosp() will take a state, illness and rank value and return 
## the nth ranked hospital for treating that illness in the selected state as 
## determined by 30-Day Mortality rate.

rankhosp <- function(state, outcome, num) {
        out <- read.csv("outcome-of-care-measures.csv") #read outcome data to a DF
        
        
        ##create vector of allowable states and outcomes
        allowstates <- unique(out[,7])
        allowout <- c("heart attack", "heart failure", "pneumonia")
        outlist <- list("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
        
        if (state %in% allowstates & outcome %in% allowout){#tests State and Outcome are allowable
                
                #sets a DF with only the pertinent info from full DF, omit NAs
                pertout <- na.omit(data.frame(
                        "hosp" = out[,2], 
                        "State" = out[,7], 
                        "sorting" = as.numeric(paste(out[,outlist[[outcome]]]))))
                
                #Sort DF by state.  ouputs a df of hospitals by state variable
                sortlist <- split(pertout, pertout$State)
                statedf <- sortlist[[state]]
                
                #return hospital related to the input number
                sorted <- statedf[order(statedf$sorting, statedf$hosp),]
                if (num == "best"){ n <- 1} else if (num == "worst") { n <- nrow(sorted) } else {n <- num}
                output <- as.character(sorted [n,1])
                
                
        } else {
                
                #Returns this if State or input value is wrong
                output <- "Input variables not found"
        }
        
        sorted
        
}