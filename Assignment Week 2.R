

pollutantmean <- function(directory, pollutant = "sulfate", id = 1:332 ){
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'pollutant' is a character vector of length 1 indicating
    ## the name of the pollutant for which we will calculate the
    ## mean; either "sulfate" or "nitrate".
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return the mean of the pollutant across all monitors list
    ## in the 'id' vector (ignoring NA values)
    
    # set working directory
    if(grep("specdata",directory) == 1){
        directory <- ("./specdata/")
    }
    # initialize a vector to hold the pollutant data
    mean_vector <- c()
    
    # find all files in the specdata folder
    all_files <- as.character(list.files(directory))
    file_paths <- paste(directory, all_files, sep = "")
    for(i in id){
        current_file <- read.csv(file_paths[i], header= T, sep = ",")
        head(current_file)
        pollutant
        na_removed <- current_file[!is.na(current_file[, pollutant]), pollutant]
        mean_vector <- c(mean_vector, na_removed)
    }
    result <- mean(mean_vector)
    return(round(result, 3))
    }
pollutantmean("specdata", "sulfate", 1:10) == 4.064

complete <- function(directory, id = 1:332){
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return a data frame of the form:
    ## id nobs
    ## 1  117
    ## 2  1041
    ## ...
    ## where 'id' is the monitor ID number and 'nobs' is the
    ## number of complete cases
    
    # set working directory
    if(grep("specdata", directory) == 1){
        directory <- ("./specdata/")
    }
    # get the length of id vector
    id_len <- length(id)
    complete_data <- rep(0, id_len)
   
     # find all files in the specdata folder
    all_files <- as.character(list.files(directory))
    file_paths <- paste(directory, all_files, sep = "")
    j <- 1
    for(i in id){
        current_file <- read.csv(file_paths[i], header=T, sep = ",")
        complete_data[j] <- sum(complete.cases(current_file))
        j <- j + 1
    }
    result <- data.frame(id = id, nobs = complete_data)
    return (result)
}

complete("specdata", 1)

complete("specdata", c(2, 4, 8, 10, 12))

corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    ## Return a numeric vector of correlations
    
    if(grep("specdata", directory) == 1) {
        directory <- ("./specdata/")
    }
    # get the complete table
    complete_table <- complete("specdata", 1:332)
    nobs <- complete_table$nobs
    # find the valid ids
    ids <- complete_table$id[nobs > threshold]
    # get the length of ids vector
    id_len <- length(ids)
    corr_vector <- rep(0, id_len)
    # find all files in the specdata folder
    all_files <- as.character( list.files(directory) )
    file_paths <- paste(directory, all_files, sep="")
    j <- 1
    for(i in ids) {
        current_file <- read.csv(file_paths[i], header=T, sep=",")
        corr_vector[j] <- cor(current_file$sulfate, current_file$nitrate, use="complete.obs")
        j <- j + 1
    }
    result <- corr_vector
    return(result)
}

cr <- corr("specdata", 150)
head(cr)


pollutantmean("specdata", "sulfate", 1:10)
pollutantmean("specdata", "nitrate", 70:72)
pollutantmean("specdata", "sulfate", 34)
pollutantmean("specdata", "nitrate")

cc <- complete("specdata", c(6, 10, 20, 34, 100, 200, 310))
print(cc$nobs)

cc <- complete("specdata", 54)
print(cc$nobs)

set.seed(42)
cc <- complete("specdata", 332:1)
use <- sample(332, 10)
print(cc[use, "nobs"])

cr <- corr("specdata")
cr <- sort(cr)                
set.seed(868)                
out <- round(cr[sample(length(cr), 5)], 4)
print(out)

cr <- corr("specdata", 129)                
cr <- sort(cr)                
n <- length(cr)                
set.seed(197)                
out <- c(n, round(cr[sample(n, 5)], 4))
print(out)

cr <- corr("specdata", 2000)                
n <- length(cr)                
cr <- corr("specdata", 1000)                
cr <- sort(cr)
print(c(n, round(cr, 4)))
