################# trying.R #############
# this code calculates mean mortality rates by hospital ownership types
# can set the parameter to 17 or 23 for other types of mortality
# 
# hospital 30-day mortality rate from
# colNum= 11: heart attack, 17: heart failure, 23: pneumonia
# can try other columns to compare, but never tested on columns other than 11,17,23
#

trying <- function(colNum = 11){
  data <- read.csv("final/hospital-data.csv", colClasses = "character")
  outcome <- read.csv("final/outcome-of-care-measures.csv", colClasses = "character")
  
  index <- numeric(dim(outcome)[1])
  for(i in 1:dim(outcome)[1]){
    out <- outcome[i,1]
    for(j in i:dim(data)[1]){
      dat <- data[j,1]
      if(out == dat){
        index[i] = j
        break
      }
    }
  }#this chunk of code connects hospitals in outcome.csv with those in hospital-data.csv, by looking at provider numbers
  
  ownership <- data[index,12] #contains data of ownership types, index matched with outcome.csv data
  
  for(i in colNum){
    
    splitted <- split(as.numeric(outcome[,i]),ownership) #splits the selected mortality rate according to ownership types
    tempResult <- sapply(splitted, mean, na.rm = TRUE) # calculate means for all types of ownerships
    #print(tempResult)
    size <- sapply(splitted, function(x){length(which(!is.na(x)))}) # calculate how many hospitals for each ownership types
    #print(size)
    asdf <- tempResult*size
    
    result <- numeric(3)
    result[1] <- sum(asdf[1:5]) / sum(size[1:5])
    result[2] <- sum(asdf[6]) / sum(size[6])
    result[3] <- sum(asdf[7:9]) / sum(size[7:9])
    # calculate the mean mortality rate into 3 categories. government, proprietary, non-profit
    
    print(paste("value of column",i,":"))
    print(result)
    print(" ")
    
    plot(result, xlab="government, proprietary, non-profit ownership type", ylab = paste("value of column",i))
  }
}
