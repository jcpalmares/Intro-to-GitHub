##1
unzip("rprog_data_specdata.zip", exdir = "specdata")
#change the working directory into "specdata" 
pollutantmean <- function(directory, pollutant, id = 1:332){ #create a function containing three variables
  means <- c() #set a variable that will hold the vector containing the desired data
  
  for(monitor in id){
    direction <- paste(getwd(), "/", directory, # print(direction)
"/", sprintf("%03d", monitor), ".csv", sep = "")
    monitor_id <- read.csv(direction) # print a variable that reads the csv file
    desired_pollutant <- monitor_id[pollutant] # print a variable that gets the desired pollutant
    means <- c(means, desired_pollutant[!is.na(desired_pollutant)]) # create a variable that holds the data but NAs 
  }
  mean(means) # return the mean of the pollutant across all monitors list
}
pollutantmean("specdata", "sulfate", 1:10)
pollutantmean("specdata", "nitrate", 70:72)
pollutantmean("specdata", "nitrate", 23)


##2
complete <- function(directory, id = 1:332){ #create a function that holds two variables
  results <- data.frame(id=numeric(0), nobs= numeric(0)) #set a variable that will hold desired result afterwards
  
  for(monitor in id){
  direction <- paste(getwd(), "/", directory, # print(direction)
             "/", sprintf("%03d", monitor), ".csv", sep = "")
  monitor_id <- read.csv(direction)   # print a variable that reads the csv file
  desired_pollutant <- monitor_id[(!is.na(monitor_id$nitrate)), ] # print a variable that access the data for nitrate excluding NAs
  desired_pollutant <- desired_pollutant[(!is.na(desired_pollutant$sulfate)), ] # print a variable that access the data for sulfate excluding NAs
  nobs <- nrow(desired_pollutant) # print a variable that access the row of the desired_pollutant 
  results <- rbind(results, data.frame(id = monitor, nobs=nobs)) # bind the accessed data
  }
  results # return the a data frame
}
complete("specdata", 1)
complete("specdata", c(2, 4, 8, 10, 12))
complete("specdata", 30:25)
complete("specdata", 3)


##3
?cor
corr <- function(directory, threshold = 0){ #create a function "corr" that takes the directory of the data files and a threshold 
  corr_results <- numeric(0)
  
  complete_cases <- complete(directory) #print complete_cases["id]
  complete_cases <- complete_cases[complete_cases$nobs>= threshold, ]#print(unlist(complete_cases["id]))
                                  #pint(complete_cases$id)
 if (nrow(complete_cases) > 0){
    for(monitor in complete_cases$id){
      direction <- paste(getwd(), "/", directory, 
             "/", sprintf("%03d", monitor), ".csv", sep = "") #print(direction)
      monitor_id <- read.csv(direction)# print(monitor_id) it reads the csv file
      desired_pollutant <- monitor_id[(!is.na(monitor_id$nitrate)), ]
      desired_pollutant <- desired_pollutant[(!is.na(desired_pollutant$sulfate)), ]
       nitrate_data <- desired_pollutant["nitrate"] #print(nitrate_data) accessing nitrate from desired_pollutant
       sulfate_data <- desired_pollutant["sulfate"] #print(sulfate_data) accessing sulfate from desired_pollutant
        corr_results <- c(corr_results, cor(nitrate_data, sulfate_data)) #compute the correlation
        }
     }
  corr_results #return a numeric vector of correlations
}
cr <- corr("specdata", 150)
head(cr); summary(cr)
cr <- corr("specdata", 400)
head(cr); summary(cr)
cr <- corr("specdata", 5000)
head(cr); summary(cr); length(cr)
cr <- corr("specdata")
head(cr); summary(cr); length(cr)


#4
unzip("rprog_data_ProgHospData.zip", exdir = "HospData") #unzip the file and change the working directory where the file was unzipped
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character") # print(outcome)
head(outcome) 
ncol(outcome)
nrow(outcome)
names(outcome)
Deaths <- outcome[ ,11] #print(Deaths)
outcome[ ,11] <- as.numeric(outcome[, 11]) # locate the desired column
hist(outcome[, 11]) # show the histogram for outcome
hist(Deaths) #show the histogram for Deaths
hist(Deaths, main = "Hospital 30-Day (Mortality) Rates from Heart Attack")











