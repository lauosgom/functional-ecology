library(tidyverse)

aggregate_data <- function(data_family, code, time) {
  # Convert into a dataframe
  data_family <- as.data.frame(data_family)
  
  # Extract the code and time you want to aggregate
  chrono_new <- data_family %>%
    filter(grepl(code, code) & time == time)
  
  # Remove the code and time from the original database
  data_family <- data_family %>%
    filter(!str_detect(code, code) | time != time)
  
  # Aggregate by taking the max value of the columns
  chrono_new <- as.data.frame(t(as.matrix(colMax(chrono_new))))
  
  # Paste the row at the end of the database
  data_family <- rbind(data_family, chrono_new)
  
  return(data_family)
}

dist <- function(x1, y1, x2, y2) {
    # Calculate the distance between two points
  return(sqrt(((x2-x1)^2)+((y2-y1)^2)))
}

data_family <- aggregate_data(data_family, "00013", "420")
data_family <- aggregate_data(data_family, "00089", "24")
data_family <- aggregate_data(data_family, "00089", "60")