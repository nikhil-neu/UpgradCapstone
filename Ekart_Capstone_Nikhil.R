library(dplyr)

consumerElectronicsData <- read.csv("ConsumerElectronics.csv", stringsAsFactors = FALSE, encoding = "UTF-8", na.strings = c("\\N", "NA","NaN","","#DIV/0!"))

# ***************************************************************************
#                   DATA CLEANING ----
# ***************************************************************************


head(consumerElectronicsData)
dim(consumerElectronicsData)
#checking for NA's in consumer Electronic Data
anyNA(consumerElectronicsData)

#checkng na % in each column
na_count <-sapply(consumerElectronicsData, function(y) sum(length(which(is.na(y))))/nrow(consumerElectronicsData)*100)

#removing deliverybdays and delivery c days , since they would be useful for ananlysis and also because 
# ~ 80% of data is NA

consumerElectronicsData <- consumerElectronicsData[,-c(9,10)]

str(consumerElectronicsData)
dim(consumerElectronicsData)
#we see negative values for cust_id and pincode, we can remove those two columns since they are not required for any analysis

consumerElectronicsData <- consumerElectronicsData[,-c(11,12)]

str(consumerElectronicsData)

dim(consumerElectronicsData)

# Converting the order_date to the "Date" format
consumerElectronicsData$order_date <- as.Date(consumerElectronicsData$order_date)
str(consumerElectronicsData)
dim(consumerElectronicsData)

# . . . .   Outlier Treatment ----
# Remove orders before July'15 and after June'16
consumerElectronicsData <- consumerElectronicsData[consumerElectronicsData$order_date>as.Date('2015-6-30'),]
consumerElectronicsData <- consumerElectronicsData[consumerElectronicsData$order_date<as.Date('2016-7-1'),]
dim(consumerElectronicsData)

# 609 records were outside the date range

duplicated(consumerElectronicsData)

# Filtering out the duplicate records based on the columns ["order_date", "order_id", "order_item_id" and "units"]
consumerElectronicsData <- consumerElectronicsData[!duplicated(consumerElectronicsData[c(1,2,5,6,8)]),]
dim(consumerElectronicsData)
#[1] 1541203      16
# Filtering out the rows/order where Product_MRP is '0'
consumerElectronicsData <- subset(consumerElectronicsData, product_mrp != 0)
dim(consumerElectronicsData)
#[1] 1536280      16

# Replacing the "gmv" with product_mrp*units wherever gmv=0 (assuming products were sold without any discount)
row_no <- which(consumerElectronicsData$gmv==0)

for(i in 1:length(row_no)){
  consumerElectronicsData$gmv[row_no[i]] <- (consumerElectronicsData$product_mrp[row_no[i]])*(consumerElectronicsData$units[row_no[i]])
}

temp <- consumerElectronicsData %>% filter(gmv > (product_mrp*units))
### Filtering out the records where "gmv" is greater than 'product_mrp*units' (as we can't charge more than MRP)
consumerElectronicsData <- subset(consumerElectronicsData, (product_mrp*units) >= gmv)

dim(consumerElectronicsData)
#[1] 1502019      16