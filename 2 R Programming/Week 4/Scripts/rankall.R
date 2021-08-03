rankall <- function(outcome, num = "best") {
        # Read outcome data
        
        df_outcome <- read.csv("Data/ProgAssignment3-data/outcome-of-care-measures.csv", stringsAsFactors = FALSE, na.strings = "Not Available")
        
        # Check that the outcome are valid
        
        possible_outcomes <- c("heart attack", "heart failure", "pneumonia")
        if (!(outcome %in% possible_outcomes)) stop("invalid outcome")
        
        # For each state, find the hospital of given rank
        
        states <- names(table(df_outcome[, 7]))
        n <- (length(states))
        
        if (outcome == "heart attack") {
                df_outcome <- df_outcome[, c(2, 7, 11)]
        } else if (outcome == "heart failure") {
                df_outcome <- df_outcome[, c(2, 7, 17)]
        } else if (outcome == "pneumonia") {
                df_outcome <- df_outcome[, c(2, 7, 23)]
        }
        
        answer <- character(0)
        
        for (i in 1 : n) {
                
                state_outcome <- split(df_outcome, df_outcome$State)[[states[i]]]
                
                if (num == "worst") {
                        state_outcome <- state_outcome[order(state_outcome[, 3], decreasing = TRUE), ]
                        answer <- c(answer, state_outcome[1, 1])
                } else if (num == "best") {
                        state_outcome <- state_outcome[order(state_outcome[, 3]), ]
                        answer <- c(answer, state_outcome[1, 1])
                } else {
                        state_outcome <- state_outcome[order(state_outcome[, 3]), ]
                        answer <- c(answer, state_outcome[num, 1])
                }
        }
        answer <- data.frame(cbind(answer, states))
        row.names(answer) <- states
        colnames(answer) <- c("hospital", "state")
        
        # Return data frame with the hospital names and the (abbreviated state name)
        
        return(answer)
        
        
        
}