
# Import libraries
# data import
library("readxl")

# data manipulation
library(dplyr)
library(tidyverse)

# visualizations
library(MASS)
library(ggplot2)
library(ggpubr)
library(gridExtra)
library(GGally)
library(ggjoy)
library(visdat) # missing values
options(scipen=100000)

# Derivation of proxies
library(RSQLite) 
library(sqldf)

# import dataset 
first <- read_excel('bigger online dataset.xlsx', sheet = 1)
second <- read_excel('bigger online dataset.xlsx', sheet = 2)

# check the first few rows
head(first)
head(second)

# check the structures of the sheets
str(first)
str(second)

# combine both sheets
data = rbind(first, second)

# inspect the full dataset
head(data)
dim(data)
summary(data)
str(data)

# check for missing data
sum(is.na(data))

# visualize where the missing data is
options(repr.plot.width=10, repr.plot.height=5)
vis_miss(data, warn_large_data = FALSE)

# find the number of missing data in each column
sapply(data, function(x) sum(is.na(x)))

# inspect rows with missing data
missing_rows <- data[rowSums(is.na(data)) > 0,]
head(missing_rows, 20)
tail(missing_rows, 20)
unique(missing_rows$Invoice)

# inspect rows with cancelled transactions
sum(str_detect(missing_rows$Invoice, 'C'))
sum(str_detect(data$Invoice, 'C'))

# delete NAs
data = data[complete.cases(data), ]

# reconfirm that there are no empty cells
sapply(data, function(x) sum(is.na(x)))
dim(data)

# delete cancelled transactions
data[str_detect(data$Invoice, 'C'), ]
data <- data[!str_detect(data$Invoice, 'C'), ]
dim(data)

# check if there are more negative quantities
count(data[data$Quantity < 0, ])

# check for duplicated rows 
sum(duplicated(data))
duplicates <- data[duplicated(data), ]
head(duplicates)

# inspect one duplicate
data[data$Description == 'VINTAGE SNAKES & LADDERS' & data$InvoiceDate == '2009-12-01 11:34:00' & 
       data$'Customer ID' == 16329, ]

# delete duplicates
data <- unique(data)
dim(data)
summary(data)

# Check each column

# 1. quantity. 
# check how many rows have quantity more than 100
head(data[data$Quantity > 100, ], 10)
count(data[data$Quantity > 100, ])
count(data[data$Quantity > 1000, ])
count(data[data$Quantity > 10000, ])

# remove rows with more than 10,000 in quantity
data <- subset(data, data$Quantity < 10000)
dim(data)
summary(data)

# Visualize the Quantity column
ggplot(data, aes(Quantity)) + 
  geom_histogram(bins = 100)

ggplot(data, aes(Quantity)) + 
  geom_density(adjust = 10)

# 2. Price
# check high values in price column
head(data[data$Price> 100, ], 20)
data[data$Price > 1000, ]
count(data[data$Price> 10000, ])
count(data[data$Price > 100, ])
count(data[data$Price > 1000, ])
count(data[data$Price > 5000, ])

# remove non-customer transactions
# Extracting rows with Stockcode == 'POST', 'M', 'ADJUST', or 'DOT'. These are 2,539 rows and they are discarded.
count(data[data$StockCode == 'POST' | data$StockCode == 'M' | data$StockCode == 'ADJUST' | data$StockCode == 'DOT' , ])
data <- data[!data$StockCode == 'POST' | data$StockCode == 'M' | data$StockCode == 'ADJUST' | data$StockCode == 'DOT' , ]

# delete prices above 1000
data <- subset(data, data$Price < 1000)
dim(data)
summary(data)

# visualize the price column
ggplot(data, aes(Price)) + 
  geom_histogram(bins = 500)

ggplot(data, aes(Price)) + 
  geom_density(adjust = 5)

# Re-confirm that there are no more cancelled transactions
sum(str_detect(data$Invoice, 'C'))
any(grep("C", data$Invoice))

# feature engineering
# From InvoiceDate, a Date and Time column were added.
data$Date <- format(as.Date(data$InvoiceDate, '%Y-%m-%d'), format = '%d/%m/%Y')
data$Time <- format(as.POSIXct(data$InvoiceDate, '%Y-%m-%d %H:%M:%S'), format = '%H:%M')
data$Date <- as.Date(data$Date, format = '%d/%m/%Y')
max(data$Date)

# change 'Customer ID' column to a string
colnames(data)[7] <- 'Customer_ID' # rename the column to Customer_ID
data$Customer_ID <- as.character(data$Customer_ID)

# insert totalprice column which is a product of Price and Quantity.
data$Total_Price <- data$Price * data$Quantity

# Add Year and Day of the week columns
data$Year <- format(as.Date(data$Date, format = '%Y-%m-%d'), '%Y')
data$Day_of_week <- weekdays(as.Date(data$Date))
str(data)
summary(data)

# Descriptive analysis
# 1. Country
country <- as.data.frame(table(data$Country))
colnames(country)[1] <- 'Country'
country

# visualize the countries
options(repr.plot.width=10, repr.plot.height=7)
ggplot(country, aes(x = reorder(Country, -Freq), y = Freq)) +
  geom_col() +
  xlab('Country') +
  ylab('Count') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle(paste('Count of Country'))

# top-10 countries
top_10_country <- country %>% 
  group_by(Country) %>% 
  arrange(desc(Freq))
top_10_country <- head(top_10_country, 10)
top_10_country

# plot as a bar plot
ggplot(top_10_country, aes(x = reorder(Country, -Freq), y = Freq)) +
  geom_col(fill = 'blue') +
  xlab('Country') +
  ylab('Count') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(text = element_text(size = 15)) +
  ggtitle(paste('Count of Country - Top 10'))

# 2. Year
Year <- as.data.frame(table(data$Year))
colnames(Year)[1] <- 'Year'
Year

# visualize
options(repr.plot.width=3, repr.plot.height=3)
ggplot(Year, aes(x = Year, y = Freq)) +
  geom_col(fill = 'blue') +
  xlab('Year') +
  ylab('Count') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle(paste('Year of purchase'))

# 3. unique customers
Customers <- as.data.frame(table(data$Customer_ID))
dim(Customers)

# 4. Products
Products <- as.data.frame(table(data$Description))
colnames(Products)[1] <- 'Name'

# top-20 products
top_20_products <- Products %>% 
  group_by(Name) %>% 
  arrange(desc(Freq))

top_20_products <- head(top_20_products, 20)
top_20_products

# visualize
options(repr.plot.width=15, repr.plot.height=8)
ggplot(top_20_products, aes(x = reorder(Name, -Freq), y = Freq)) +
  geom_col() +
  xlab('Name of product') +
  ylab('Count') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle(paste('Count of products'))

# other descriptive analysis
sum(data$Quantity)
sum(data$Total_Price)
length(unique(data$Description))
length(unique(data$StockCode))

# processed dataset
head(data)

### data pre-processing for machine learning
# find the latest date
max(data$Date)
head(data)

# write the processed data
write.csv(data, "C:\\Users\\anita\\Desktop\\assessment\\Project\\Project_files\\processed_data.csv")

### RFM derivation
# First, calculate how many days difference between each transaction and the anchor date 09/12/2011
data$days_since <- as.numeric(difftime(time1 = '2011-12-09', time2 = data$Date, units = 'days'))
summary(data)

# add the RFM columns
rfm_data <- sqldf("SELECT Customer_ID,
                    MAX(days_since) - MIN(days_since) AS 'Recency',
                    COUNT(DISTINCT Date) - 1 AS 'Frequency',
                    COUNT(DISTINCT Date) AS 'Count_of_transactions',
                    SUM(Total_Price) AS 'Monetary_value',
                    COUNT(DISTINCT StockCode) AS 'num_distinct_products',
                    MAX(days_since) AS 'Age'
                FROM data GROUP BY 1")

head(rfm_data)

# add average amount spent column
rfm_data$Average_amount_spent <- rfm_data$Monetary_value/rfm_data$Count_of_transactions

# derive customer satisfaction proxy
# 1. create a satisfaction index
rfm_data$SI <- 0.4*rfm_data$Recency/rfm_data$Count_of_transactions + 0.4*rfm_data$Frequency + 0.2*rfm_data$Average_amount_spent
summary(rfm_data)

# Step 1: create 5 bands of the satisfaction index
# option 1 (equal division), this approach was not effective and then discarded
rfm_data$s_band <- ifelse(rfm_data$SI <= 548, 1, 
                          ifelse(rfm_data$SI > 548 & rfm_data$SI <= 1094, 2,
                                 ifelse(rfm_data$SI > 1094 & rfm_data$SI <= 1641, 3,
                                        ifelse(rfm_data$SI >1641 & rfm_data$SI <= 2187, 4, 5))))
table(rfm_data$s_band)

# option 2 (using summary stat), effective
rfm_data$s_band <- ifelse(rfm_data$SI <= 55, 1, 
                          ifelse(rfm_data$SI > 55 & rfm_data$SI <= 83, 2,
                                 ifelse(rfm_data$SI > 83 & rfm_data$SI <= 119, 3, 4)))
table(rfm_data$s_band)

# visualize
ggplot(rfm_data, aes(s_band)) + geom_histogram() + ggtitle(paste('Satisfaction index'))

# Step 2: apply random normal distribution to introduce jitter
# create an empty column
rfm_data$CSI <- NA

# apply this band by band and save the result to CSI column
band1 <- rfm_data[rfm_data$s_band == 1, ]
band1$CSI <- rnorm(1479, 1, 0.5)

band2 <- rfm_data[rfm_data$s_band == 2, ]
band2$CSI <- rnorm(1426, 2, 0.5)

band3 <- rfm_data[rfm_data$s_band == 3, ]
band3$CSI <- rnorm(1511, 3, 0.5)

band4 <- rfm_data[rfm_data$s_band == 4, ]
band4$CSI <- rnorm(1458, 4, 0.5)

# combine the dataset
rfm_data2 <- rbind(band1, band2, band3, band4)

# round up the CSI values to whole numbers
rfm_data2$CSI <- round(rfm_data2$CSI, digits = 0)
hist(rfm_data2$CSI)
table(rfm_data2$CSI)

# create new bands to combine the -1 to 0 bands into a single band and also start the bands from 1
rfm_data2$CSI <- ifelse(rfm_data2$CSI == -1 | rfm_data2$CSI == 0, 1,
                        ifelse(rfm_data2$CSI == 1, 2,
                               ifelse(rfm_data2$CSI == 2, 3,
                                      ifelse(rfm_data2$CSI == 3, 4,
                                             ifelse(rfm_data2$CSI == 4, 5, 6)))))
# plot the CSI column
table(rfm_data2$CSI)
ggplot(rfm_data2, aes(CSI)) + geom_histogram() + ggtitle(paste('Customer Satisfaction index'))
summary(rfm_data2)

# drop SI and s_band columns
rfm_data2$SI <- NULL
rfm_data2$s_band <- NULL
head(rfm_data2)

# add CLV target column
# Step 1: Sort the data by Customer_ID and Date
orders <- data %>% arrange(Customer_ID, Date)

# Step 2: Group the data by CustomerID
orders_grouped <- orders %>% group_by(Customer_ID)

# Step 3: Get the date difference between consecutive purchases
orders_diff <- orders_grouped %>% 
  mutate(OrderDateDiff = difftime(Date, lag(Date), units = "days"))

# Step 4: replace NAs with 0. NAs were introduced for all dates where the customers made their first transaction
# replace NAs with 0
orders_diff$OrderDateDiff <- ifelse(orders_diff$OrderDateDiff == 'NA days', 0, orders_diff$OrderDateDiff)
orders_diff$OrderDateDiff <- ifelse(orders_diff$OrderDateDiff == 'NA', 0, orders_diff$OrderDateDiff)

#5. Get average number of days of inactivity
rfm_data3 <- sqldf("SELECT Customer_ID,
                    AVG(OrderDateDiff) AS 'Average_Inactive_days',
                    SUM(Total_Price) AS 'Monetary_value'
                FROM orders_diff GROUP BY 1")

rfm_data3$months_inactive <- rfm_data3$Average_Inactive_days/30
head(rfm_data3)

# Step 7: Create profit margin, penalty and CLV 
rfm_data3$G_Profit_margin <- rnorm(5874, 0.05, 0.02)*rfm_data3$Monetary_value
rfm_data3$Penalty <- rnorm(5874, 0.01, 0.005)*rfm_data3$Monetary_value
rfm_data3$Penalty_months <- rfm_data3$Penalty*rfm_data3$months_inactive
rfm_data3$CLV <- rfm_data3$G_Profit_margin - rfm_data3$Penalty_months

# Visualize the new columns
ggplot(rfm_data3, aes(CLV)) + geom_histogram(bins = 500) + ggtitle(paste('CLV Proxy'))
hist(rfm_data3$G_Profit_margin)
hist(rfm_data3$Penalty)
hist(rfm_data3$CLV)

# drop columns no longer needed
rfm_data3$G_Profit_margin <- NULL
rfm_data3$Penalty <- NULL
rfm_data3$Penalty_months <- NULL

head(rfm_data3)
head(rfm_data2)

# combine the 2 datasets 
rfm_data4 <- merge(rfm_data2, rfm_data3, by = "Customer_ID")
head(rfm_data4)

# drop duplicated columns not needed
rfm_data4$Monetary_value.y <- NULL
rfm_data4$Average_Inactive_days <- NULL
names(rfm_data4)[8] <- 'Avg_Amount'
names(rfm_data4)[5] <- 'Total_Amount'
names(rfm_data4)[6] <- 'DPR'

### BG/NBD/GG model assumptions
# reconfirm that all BG/NBD/GG assumptions are satisfied
# 1. Number of transactions by customers who are still alive follows a Poisson distribution
head(data)
bg_assumption <- sqldf("SELECT Date,
                              SUM(Quantity) AS 'Total_Quantity',
                              SUM(Total_Price) AS 'Total_Amount',
                              COUNT(Date) AS 'No_of_Transactions'
                        FROM data GROUP BY 1")
head(bg_assumption)

# visualize
ggplot(bg_assumption, aes(No_of_Transactions)) + geom_histogram(bins = 10) + 
  ggtitle(paste('Number of transactions by customers alive'))

# 2. Customers have different transaction rates and differences in transaction rates follow a gamma
# distribution
# step 1: get the number of days between each customer's first and last transaction
head(rfm_data2)
bg_assumption2 <- sqldf("SELECT Customer_ID,
                    MAX(Date) - MIN(Date) AS 'num_days',
                    COUNT(DISTINCT Date) AS 'Frequency'
                  FROM data GROUP BY 1")

# step 2: get the transaction rate = freq/num_days
bg_assumption2$TR <- bg_assumption2$num_days/bg_assumption2$Frequency

# step 3: get differences in TR: DTR
bg_assumption2$DTR <- bg_assumption2$TR - mean(bg_assumption2$TR)

# histogram of DTR
ggplot(bg_assumption2, aes(DTR)) + geom_density() +
  xlab('Differences in Transaction Rates') +
  ggtitle(paste("Differences in customers' transaction rates"))

# 3. The customer becomes inactive immediately after any transaction with probability p 
# hence the point at which the customer drops out is distributed across transactions according 
# to a geometric distribution.

# the best way to show this is to plot the count of transactions by bands
bg_assumption2$freq_bands <- ifelse(bg_assumption2$Frequency > 300, 'Above 300',
                                    ifelse(bg_assumption2$Frequency <= 300 & bg_assumption2$Frequency > 250, '250 - 300', 
                                           ifelse(bg_assumption2$Frequency <= 250 & bg_assumption2$Frequency > 200, '201 - 250',
                                                  ifelse(bg_assumption2$Frequency <= 200 & bg_assumption2$Frequency > 150, '151 - 200',
                                                         ifelse(bg_assumption2$Frequency <= 150 & bg_assumption2$Frequency > 100, '101 - 150',
                                                                ifelse(bg_assumption2$Frequency <= 100 & bg_assumption2$Frequency > 50, '51 - 100',
                                                                       ifelse(bg_assumption2$Frequency <= 50 & bg_assumption2$Frequency > 20, '21 - 50',
                                                                              ifelse(bg_assumption2$Frequency <= 20 & bg_assumption2$Frequency > 10, '11 - 20',
                                                                                     ifelse(bg_assumption2$Frequency <= 10 & bg_assumption2$Frequency > 5, '6 - 10',
                                                                                            ifelse(bg_assumption2$Frequency <= 5 & bg_assumption2$Frequency > 2, '2 - 5', '1'
                                                                                            ))))))))))

summary(bg_assumption2)
head(bg_assumption2, 20)
table(bg_assumption2$freq_bands)

