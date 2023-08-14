#additional script to make faster the aggregations

# aggregation for code 13` time 420

#1. convert into a dataframe the matrix
data_family <- as.data.frame(data_family)

#2. extract the code and time you want to aggregate
chrono_new <- data_family %>% 
  filter(grepl("00013", code) & time == "420")

#3. remove the code and time from the original database
data_family <- data_family %>% 
  filter(!str_detect(code, "0013") | time != "420")

#4. aggregate by taking the max value of the columns
chrono_new<-as.data.frame(t(as.matrix(colMax(chrono_new))))

#5. paste the row at the end of the database
data_family<-rbind(data_family,chrono_new)

# aggregation for code 89 time 24

#1. convert into a dataframe the matrix
data_family<- as.data.frame(data_family)

#2. extract the code and time you want to aggregate
chrono_new <- data_family %>% 
  filter(grepl("00089", code) & time == "24")

#3. remove the code and time from the original database
data_family <- data_family %>% 
  filter(!str_detect(code, "0089") | time != "24")

#4. aggregate by taking the max value of the columns
chrono_new<-as.data.frame(t(as.matrix(colMax(chrono_new))))

#5. paste the row at the end of the database
data_family<-rbind(data_family,chrono_new)

# aggregation for code 89` time 60

#1. convert into a dataframe the matrix
data_family<- as.data.frame(data_family)

#2. extract the code and time you want to aggregate
chrono_new <- data_family %>% 
  filter(grepl("00089", code) & time == "60")

#3. remove the code and time from the original database
data_family <- data_family %>% 
  filter(!str_detect(code, "0089") | time != "60")

#4. aggregate by taking the max value of the columns
chrono_new<-as.data.frame(t(as.matrix(colMax(chrono_new))))

#5. paste the row at the end of the database
data_family<-rbind(data_family,chrono_new)

# aggregation for code 89` time 468

#1. convert into a dataframe the matrix
data_family<- as.data.frame(data_family)

#2. extract the code and time you want to aggregate
chrono_new <- data_family %>% 
  filter(grepl("00089", code) & time == "468")

#3. remove the code and time from the original database
data_family <- data_family %>% 
  filter(!str_detect(code, "0089") | time != "468")

#4. aggregate by taking the max value of the columns
chrono_new<-as.data.frame(t(as.matrix(colMax(chrono_new))))

#5. paste the row at the end of the database
data_family<-rbind(data_family,chrono_new)

# aggregation for code 89` time 0

#1. convert into a dataframe the matrix
data_family<- as.data.frame(data_family)

#2. extract the code and time you want to aggregate
chrono_new <- data_family %>% 
  filter(grepl("00089", code) & time == "0")

#3. remove the code and time from the original database
data_family <- data_family %>% 
  filter(!str_detect(code, "0089") | time != "0")

#4. aggregate by taking the max value of the columns
chrono_new<-as.data.frame(t(as.matrix(colMax(chrono_new))))

#5. paste the row at the end of the database
data_family<-rbind(data_family,chrono_new)

# aggrega site 13 time 0

#1. convert into a dataframe the matrix
data_family<- as.data.frame(data_family)

#2. extract the code and time you want to aggregate
chrono_new <- data_family %>% 
  filter(grepl("00013", code) & time == "0")

#3. remove the code and time from the original database
data_family <- data_family %>% 
  filter(!str_detect(code, "0013") | time != "0")

#4. aggregate by taking the max value of the columns
chrono_new<-as.data.frame(t(as.matrix(colMax(chrono_new))))

#5. paste the row at the end of the database
data_family<-rbind(data_family,chrono_new)

# aggregations site 14 forests  

#1. convert into a dataframe the matrix
data_family<- as.data.frame(data_family)

#2. extract the code and time you want to aggregate
chrono_new <- data_family %>% 
  filter(grepl("00014", code) & time == "0")

#3. remove the code and time from the original database
data_family <- data_family %>% 
  filter(!str_detect(code, "0014") | time != "0")

#4. aggregate by taking the max value of the columns
chrono_new<-as.data.frame(t(as.matrix(colMax(chrono_new))))

#5. paste the row at the end of the database
data_family<-rbind(data_family,chrono_new)

# aggregations site 14 time 600  

#1. convert into a dataframe the matrix
data_family<- as.data.frame(data_family)

#2. extract the code and time you want to aggregate
chrono_new <- data_family %>% 
  filter(grepl("00014", code) & time == "600")

#3. remove the code and time from the original database
data_family <- data_family %>% 
  filter(!str_detect(code, "0014") | time != "600")

#4. aggregate by taking the max value of the columns
chrono_new<-as.data.frame(t(as.matrix(colMax(chrono_new))))

#5. paste the row at the end of the database
data_family<-rbind(data_family,chrono_new)

# aggregation site 14 time 180

#1. convert into a dataframe the matrix
data_family<- as.data.frame(data_family)

#2. extract the code and time you want to aggregate
chrono_new <- data_family %>% 
  filter(grepl("00014", code) & time == "180")

#3. remove the code and time from the original database
data_family <- data_family %>% 
  filter(!str_detect(code, "00014") | time != "180")

#4. aggregate by taking the max value of the columns
chrono_new<-as.data.frame(t(as.matrix(colMax(chrono_new))))

#5. paste the row at the end of the database
data_family<-rbind(data_family,chrono_new)

# aggregations site 17 time 12  

#1. convert into a dataframe the matrix
data_family<- as.data.frame(data_family)

#2. extract the code and time you want to aggregate
chrono_new <- data_family %>% 
  filter(grepl("00017", code) & time == "12")

#3. remove the code and time from the original database
data_family <- data_family %>% 
  filter(!str_detect(code, "00017") | time != "12")

#4. aggregate by taking the max value of the columns
chrono_new<-as.data.frame(t(as.matrix(colMax(chrono_new))))

#5. paste the row at the end of the database
data_family<-rbind(data_family,chrono_new)

# aggregations site 17 time 126

#1. convert into a dataframe the matrix
data_family<- as.data.frame(data_family)

#2. extract the code and time you want to aggregate
chrono_new <- data_family %>% 
  filter(grepl("00017", code) & time == "126")

#3. remove the code and time from the original database
data_family <- data_family %>% 
  filter(!str_detect(code, "00017") | time != "126")

#4. aggregate by taking the max value of the columns
chrono_new<-as.data.frame(t(as.matrix(colMax(chrono_new))))

#5. paste the row at the end of the database
data_family<-rbind(data_family,chrono_new)

# aggregations site 17 time 240

#2. extract the code and time you want to aggregate
chrono_new <- data_family %>% 
  filter(grepl("00017", code) & time == "240")

#3. remove the code and time from the original database
data_family <- data_family %>% 
  filter(!str_detect(code, "00017") | time != "240")

#4. aggregate by taking the max value of the columns
chrono_new<-as.data.frame(t(as.matrix(colMax(chrono_new))))

#5. paste the row at the end of the database
data_family<-rbind(data_family,chrono_new)


#aggregations site 58 time 116

#2. extract the code and time you want to aggregate
chrono_new <- data_family %>% 
  filter(grepl("00058", code) & time == "116")

#3. remove the code and time from the original database
data_family <- data_family %>% 
  filter(!str_detect(code, "00058") | time != "116")

#4. aggregate by taking the max value of the columns
chrono_new<-as.data.frame(t(as.matrix(colMax(chrono_new))))

#5. paste the row at the end of the database
data_family<-rbind(data_family,chrono_new)

#aggregations site 58 time 504

#2. extract the code and time you want to aggregate
chrono_new <- data_family %>% 
  filter(grepl("00058", code) & time == "504")

#3. remove the code and time from the original database
data_family <- data_family %>% 
  filter(!str_detect(code, "00058") | time != "504")

#4. aggregate by taking the max value of the columns
chrono_new<-as.data.frame(t(as.matrix(colMax(chrono_new))))

#5. paste the row at the end of the database
data_family<-rbind(data_family,chrono_new)

#aggregations site 58 time 1488

#2. extract the code and time you want to aggregate
chrono_new <- data_family %>% 
  filter(grepl("00058", code) & time == "1488")

#3. remove the code and time from the original database
data_family <- data_family %>% 
  filter(!str_detect(code, "00058") | time != "1488")

#4. aggregate by taking the max value of the columns
chrono_new<-as.data.frame(t(as.matrix(colMax(chrono_new))))

#5. paste the row at the end of the database
data_family<-rbind(data_family,chrono_new)

#aggregations site 58 time 5160

#2. extract the code and time you want to aggregate
chrono_new <- data_family %>% 
  filter(grepl("00058", code) & time == "5160")

#3. remove the code and time from the original database
data_family <- data_family %>% 
  filter(!str_detect(code, "00058") | time != "5160")

#4. aggregate by taking the max value of the columns
chrono_new<-as.data.frame(t(as.matrix(colMax(chrono_new))))

#5. paste the row at the end of the database
data_family<-rbind(data_family,chrono_new)

#aggregations site 58 time forests

#2. extract the code and time you want to aggregate
chrono_new <- data_family %>% 
  filter(grepl("00058", code) & time == "0")

#3. remove the code and time from the original database
data_family <- data_family %>% 
  filter(!str_detect(code, "00058") | time != "0")

#4. aggregate by taking the max value of the columns
chrono_new<-as.data.frame(t(as.matrix(colMax(chrono_new))))

#5. paste the row at the end of the database
data_family<-rbind(data_family,chrono_new)

#aggregations site 36 time forests

#2. extract the code and time you want to aggregate
chrono_new <- data_family %>% 
  filter(grepl("00036", code) & time == "0")

#3. remove the code and time from the original database
data_family <- data_family %>% 
  filter(!str_detect(code, "00036") | time != "0")

#4. aggregate by taking the max value of the columns
chrono_new<-as.data.frame(t(as.matrix(colMax(chrono_new))))

#5. paste the row at the end of the database
data_family<-rbind(data_family,chrono_new)

#aggregations crono_code india-crono 04 forests

#2. extract the code and time you want to aggregate
chrono_new <- data_family %>% 
  filter(grepl("India - crono 04", crono_code) & time == "0")

#3. remove the code and time from the original database
data_family<-data_family[which(!(data_family$code %in% chrono_new$code)),]

#4. aggregate by taking the max value of the columns
chrono_new<-as.data.frame(t(as.matrix(colMax(chrono_new))))

#5. paste the row at the end of the database
data_family<-rbind(data_family,chrono_new)

#aggregations crono_code india-crono 03 forests

#2. extract the code and time you want to aggregate
chrono_new <- data_family %>% 
  filter(grepl("India - crono 03", crono_code) & time == "0")

#3. remove the code and time from the original database
data_family<-data_family[which(!(data_family$code %in% chrono_new$code)),]

#4. aggregate by taking the max value of the columns
chrono_new<-as.data.frame(t(as.matrix(colMax(chrono_new))))

#5. paste the row at the end of the database
data_family<-rbind(data_family,chrono_new)

#aggregations crono_code india-crono 02 forests

#2. extract the code and time you want to aggregate
chrono_new <- data_family %>% 
  filter(grepl("India - crono 02", crono_code) & time == "0")

#3. remove the code and time from the original database
data_family<-data_family[which(!(data_family$code %in% chrono_new$code)),]

#4. aggregate by taking the max value of the columns
chrono_new<-as.data.frame(t(as.matrix(colMax(chrono_new))))

#5. paste the row at the end of the database
data_family<-rbind(data_family,chrono_new)

#aggregations crono_code india-crono 01 forests

#2. extract the code and time you want to aggregate
chrono_new <- data_family %>% 
  filter(grepl("India - crono 01", crono_code) & time == "0")

#3. remove the code and time from the original database
data_family<-data_family[which(!(data_family$code %in% chrono_new$code)),]

#4. aggregate by taking the max value of the columns
chrono_new<-as.data.frame(t(as.matrix(colMax(chrono_new))))

#5. paste the row at the end of the database
data_family<-rbind(data_family,chrono_new)