
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
head(outcome)
ncol(outcome)
nrow(outcome)
names(outcome)
library(data.table)

outcome[, 11] <- as.numeric(outcome[, 11])

hist(outcome[, 11])


best <- function(state, outcome) {
    ##Read outcome data
    out_df <- data.table::fread('outcome-of-care-measures.csv')
    
    outcome <- tolower(outcome)
    
    chosen_state <- state
    
    ##Check that state and outcome are valid
    
    if (!chosen_state %in% unique(out_df[["State"]])) {
        stop('invalid state!')
    }
    
    if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
        stop('invalid outcome!')
    }
    
    setnames(out_df
             , tolower(sapply(colnames(out_df), gsub, pattern = "^Hospital 30-Day Death \\(Mortality\\) Rates from ", replacement = "" ))
    )
    
    out_df <- out_df[state == chosen_state]
    
    
    ##Ruturn hospital name in that state with  the lowest 30-day death rate
    
    col_indices <- grep(paste0("hospital name|state|^",outcome), colnames(out_df))
    
    out_df <- out_df[, .SD ,.SDcols = col_indices]
    
    out_df[, outcome] <- out_df[,  as.numeric(get(outcome))]
    
    out_df <- out_df[complete.cases(out_df),]
    
    out_df <- out_df[order(get(outcome), `hospital name`)]
    
    return(out_df[, "hospital name"][1])
    
}


rankhospital <- function(state, outcome, num = "best") {
    ##Read outcome data
    out_df <- data.table::fread('outcome-of-care-measures.csv')
    
    outcome <- tolower(outcome)
    
    chosen_state <- state
    
    ##Check that state and outcome are valid
    
    if (!chosen_state %in% unique(out_df[["State"]])) {
        stop('invalid state!')
    }
    
    if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
        stop('invalid outcome!')
    }
    
    setnames(out_df
             , tolower(sapply(colnames(out_df), gsub, pattern = "^Hospital 30-Day Death \\(Mortality\\) Rates from ", replacement = "" ))
    )
    
    out_df <- out_df[state == chosen_state]
    
    
    ##Ruturn hospital name in that state with the given rank 30-day death rate
    
    col_indices <- grep(paste0("hospital name|state|^",outcome), colnames(out_df))
    
    out_df <- out_df[, .SD ,.SDcols = col_indices]
    
    out_df[, outcome] <- out_df[,  as.numeric(get(outcome))]
    
    out_df <- out_df[complete.cases(out_df),]
    
    out_df <- out_df[order(get(outcome), `hospital name`)]
    
    out_df <- out_df[,  .(`hospital name` = `hospital name`, state = state, rate = get(outcome), Rank = .I)]
    
    if (num == "best"){
        return(out_df[1,`hospital name`])
    }
    
    if (num == "worst"){
        return(out_df[.N,`hospital name`])
    }
    
    return(out_df[num,`hospital name`])
    
    
    
}




rankall <- function(outcome, num = "best") {
    ##Read outcome data
    
    out_df <- data.table::fread('outcome-of-care-measures.csv')
    
    outcome <- tolower(outcome)
    
    
    ##Check that state and outcome are valid
    
    if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
        stop('invalid outcome')
    }
    
    setnames(out_df
             , tolower(sapply(colnames(out_df), gsub, pattern = "^Hospital 30-Day Death \\(Mortality\\) Rates from ", replacement = "" ))
    )
    
    col_indices <- grep(paste0("hospital name|state|^",outcome), colnames(out_df))
    
    
    
    ##For each state, find the hospital of the given rank
    
    setnames(out_df
             , tolower(sapply(colnames(out_df), gsub, pattern = "^Hospital 30-Day Death \\(Mortality\\) Rates from ", replacement = "" )))
    
    col_indices <- grep(paste0("hospital name|state|^",outcome), colnames(out_df))
    
    out_df <- out_df[, .SD ,.SDcols = col_indices]
    
    out_df[, outcome] <- out_df[,  as.numeric(get(outcome))]
    
    
    #Return a dataframe with the hospital names and the (abbreviated) state name
    
    if (num == "best"){
        return(out_df[order(state, get(outcome), `hospital name`)
                      , .(hospital = head(`hospital name`, 1))
                      , by = state])
    }
    
    if (num == "worst"){
        return(out_df[order(get(outcome), `hospital name`)
                      , .(hospital = tail(`hospital name`, 1))
                      , by = state])
    }
    
    return(out_df[order(state, get(outcome), `hospital name`)
                  , head(.SD,num)
                  , by = state, .SDcols = c("hospital name") ])
    
    
}

#Import library(data.table)
#Q1
best("SC", "heart attack")

#Q2
best("NY", "pneumonia")

#Q3
best("AK", "pneumonia")

#Q4
rankhospital("NC", "heart attack", "worst")

#Q5
rankhospital("WA", "heart attack", 7)

#Q6
rankhospital("TX", "pneumonia", 10)

#Q7
rankhospital("NY", "heart attack", 7)

#Q8
r <- rankall("heart attack", 4)
as.character(subset(r, state == "HI")$hospital)

#Q9
r <- rankall("pneumonia", "worst")
as.character(subset(r, state == "NJ")$hospital)

#Q10
r <- rankall("heart failure", 10)
as.character(subset(r, state == "NV")$hospital)
