rankall <- function(outcome, num = "best") {
        ranked_data <- NULL
        state_col <- NULL
        name_col <- NULL
        
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
        if(!(is.element(outcome, outcomes))) {
                stop("invalid outcome")
        }
        
        index <- if(outcome == "heart attack") {
                iHeart_attack
        }
        else if(outcome == "heart failure") {
                iHeart_failure
        }
        else {
                iPneumonia
        }
        
        statefactor <- as.factor(data[, iState])
        splitdata <- split(data, statefactor)

        ranked_data <- lapply(splitdata, function(x) x[order(x[, index], x[, iName]), ])
        ranked_data <- lapply(ranked_data, function(x) x[complete.cases(x[, index]), ])
        
        if(num == "best") {
                name_col <- sapply(ranked_data, function(x) x[1, iName])
                state_col <- sapply(ranked_data, function(x) x[1, iState])
        }
        else if(num == "worst") {
                name_col <- sapply(ranked_data, function(x) x[nrow(x), iName])
                state_col <- sapply(ranked_data, function(x) x[nrow(x), iState])
        }
        else {
                name_col <- sapply(ranked_data, function(x) x[num, iName])
                state_col <- sapply(ranked_data, function(x) x[num, iState])
        }
        
        result <- data.frame(hospital = name_col, state = state_col)
}