best <- function(state, outcome) {
        outcome_result <- NULL
        
        # Constants
        iHeart_attack = 11
        iHeart_failure = 17
        iPneumonia = 23
        iState = 7
        iName = 2
        outcomes = c("heart failure", "heart attack", "pneumonia")
        
        ## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv")
        
        oldw <- getOption("warn")
        options(warn = -1)
        data[, iHeart_attack] <- as.numeric(as.character(data[, iHeart_attack]))
        data[, iHeart_failure] <- as.numeric(as.character(data[, iHeart_failure]))
        data[, iPneumonia] <- as.numeric(as.character(data[, iPneumonia]))
        options(warn = oldw)
        
        ## Check that state and outcome are valid
        if(!(state %in% data[, iState])) {
                stop("invalid state")
        }
                
        if(!(is.element(outcome, outcomes))) {
                stop("invalid outcome")
        }
        
        outcome_state <- subset(data, data[, iState] == state)
        
        ## Return hospital name in that state with lowest 30-day death
        if(outcome == "heart attack") {
                min <- min(outcome_state[, iHeart_attack], na.rm = TRUE)
                outcome_result <- subset(outcome_state, outcome_state[, iHeart_attack] == min)
        }
        else if(outcome == "heart failure") {
                min <- min(outcome_state[, iHeart_failure], na.rm = TRUE)
                outcome_result <- subset(outcome_state, outcome_state[, iHeart_failure] == min)
        }
        else {
                min <- min(outcome_state[, iPneumonia], na.rm = TRUE)
                outcome_result <- subset(outcome_state, outcome_state[, iPneumonia] == min)
        }
        
        outcome_result <- as.vector(outcome_result[, iName])
        outcome_result <- sort(outcome_result)
        outcome_result[1]
}