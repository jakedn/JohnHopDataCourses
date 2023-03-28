pollutantmean <- function(directory, pollutant, id = 1:332){
  
  # create an empty data frame to store the combined data
  pollution_data <- data.frame()
  
  # loop through each CSV file and read it into the data frame
  for (i in id) {
    file = 
      paste(directory, "/", formatC(i, width = 3, flag = "0"), ".csv", sep = "")
    temp_data <- read.csv(file)
    pollution_data <- rbind(pollution_data, temp_data)
  }
  
  # print mean
  mean(pollution_data[[pollutant]][pollution_data$ID %in% id], na.rm = TRUE)
}


complete <- function(directory, id = 1:332){
  
  # create an empty data frame to store the combined data
  complete_reads <- data.frame()
  
  # loop through each CSV file and read it into the data frame
  for (i in id) {
    file = 
      paste(directory, "/", formatC(i, width = 3, flag = "0"), ".csv", sep = "")
    # read into dataframe
    temp_data <- read.csv(file)
    # omit nas
    complete_reads <- rbind(complete_reads, c(i, nrow(na.omit(temp_data))))
  }
  colnames(complete_reads) <- c("id", "nobs")
  complete_reads[complete_reads$id %in% id,]
}

corr <- function(directory, threshold = 0) {
  # create an empty data frame to store the combined data
  thresh_monitor_corrs <- c()
  
  # get a list of all CSV files in the directory
  csv_files <- list.files(directory, full.name = TRUE)
  
  # loop through each CSV file and read it into the data frame 
  # if its above thereshold
  count <- 1 # indexing starts from 1 NOT 0 !
  for (file in csv_files) {
    # read into dataframe
    temp_data <- read.csv(file)
    # omit nas
    if (nrow(na.omit(temp_data)) >= threshold) {
      temp_data <- na.omit(temp_data)
      thresh_monitor_corrs[count] <- 
        cor(temp_data$sulfate, temp_data$nitrate)
      count <- count + 1
    }
  }
  thresh_monitor_corrs
}
