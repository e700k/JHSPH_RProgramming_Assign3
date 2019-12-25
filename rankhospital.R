rankhospital <- function(state, outcome, num = "best") {
        outcome_result <- NULL
        outcome_ranked <- NULL
        
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
        
        ## Return hospital name in that state with the given rank 30-day death rate
        if(outcome == "heart attack") {
                outcome_state <- outcome_state[complete.cases(outcome_state[, iHeart_attack]), ]
                outcome_ranked <- outcome_state[order(outcome_state[, iHeart_attack], outcome_state[, iName]), ]
                outcome_result <- as.vector(outcome_ranked[, iName])
        }
        else if(outcome == "heart failure") {
                outcome_state <- outcome_state[complete.cases(outcome_state[, iHeart_failure]), ]
                outcome_ranked <- outcome_state[order(outcome_state[, iHeart_failure], outcome_state[, iName]), ]
                outcome_result <- as.vector(outcome_ranked[, iName])
        }
        else {
                outcome_state <- outcome_state[complete.cases(outcome_state[, iPneumonia]), ]
                outcome_ranked <- outcome_state[order(outcome_state[, iPneumonia], outcome_state[, iName]), ]
                outcome_result <- as.vector(outcome_ranked[, iName])
        }
        
        if(num == "best") {
                outcome_result[1]
        }
        else if(num == "worst") {
                outcome_result[length(outcome_result)]
        }
        else if(num > length(outcome_result) | num == 0) {
                NA
        }
        else {
                outcome_result[num]
        }
}