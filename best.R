#head(outcome)
#outcome[, 11] <- as.numeric(outcome[, 11])
#hist(outcome[, 11])
best <- function( state, outcome) {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character");    
    
    ## Check that state and outcome are valid
    if (!(state %in% data[['State']])) {
        stop("invalid state");
    }
    noutcome <- match(outcome, c('heart attack', 'heart failure', 'pneumonia'), nomatch = 0 );
    if (noutcome == 0) {
        stop('invalid outcome');
    }    
    ## Return hospital name in that state with lowest 30-day death
    ## rate
    col_HA <- grep("Number.*Death.*Heart.*Attack", colnames(data),ignore.case = TRUE, perl = TRUE)
    col_HF <- grep("Number.*Death.*heart.*failure", colnames(data),ignore.case = TRUE, perl = TRUE)
    col_P  <- grep("Number.*Death.*pneumonia", colnames(data),ignore.case = TRUE, perl = TRUE)    
    col_num <- switch (noutcome,col_HA,col_HF,col_P);
    hospitals <- subset(data, State == state, select = c(Hospital.Name, col_num));
    hospitals_num <- cbind (Name = hospitals[,1], Value = as.numeric(hospitals[,2]));
    #hospitals_num
    min_val <- min(hospitals_num['Value'], na.rm = TRUE);
    min_val
#     best_hospitals <- subset( hospitals_num, 'Value' == min_val);
#     best_hospitals
}
best("TX","heart attack")