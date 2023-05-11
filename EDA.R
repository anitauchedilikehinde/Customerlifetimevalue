# import libraries

# data import
library("readxl")

# data manipulation
library(dplyr)
library(tidyverse)
library(lubridate)

# visualizations
library(MASS)
library(ggplot2)
library(ggpubr)
library(gridExtra)
library(GGally)
library(ggjoy)

library(visdat) # data visualization: missing values
options(scipen=100000)

# Derivation of proxies
library(RSQLite) 
library(sqldf)

head(data)
# rfm data 
head(rfm_data4)

# exploring correlation between numerical features
numcols = c('Recency', 'Frequency', 'Avg_Amount', 'Total_Amount',
            'DPR', 'Age', 'CSI', 'CLV')
options(repr.plot.width=10, repr.plot.height=10)
ggpairs(rfm_data4, columns = numcols, aes(alpha = 0.1), upper = list(continuous = "cor", combo = "box_no_facet"),
        lower = list(continuous = "cor", combo = "box", discrete = 'ratio'))

dataset_matrix <- as.matrix(rfm_data4[ , c('Recency', 'Frequency', 'Avg_Amount', 'Total_Amount',
                                           'DPR', 'Age', 'CSI', 'months_inactive', 'CLV')])
ggcorr(dataset_matrix, label = TRUE, label_size = 3, hjust = 0.75, 
       size = 3, layout.exp = 3)

# change CSI to factor to aid visualizations
rfm_data4$CSI <- as.factor(rfm_data4$CSI)

# descriptive analysis of numerical columns
# Recency 
ggplot(rfm_data4, aes(Recency)) +
  geom_histogram(color = 'blue') +
  xlab('Recency in days') +
  labs(title = 'Distribution of Recency of purchases')
# (ARCHIVE)
ggplot(rfm_data4, aes(Recency)) +
  geom_density(color = 'blue') +
  xlab('Recency in days') +
  labs(title = 'Distribution of Recency of purchases')

# Frequency
ggplot(rfm_data4, aes(Frequency)) +
  geom_histogram(color = 'blue', bins = 50) +
  xlab('Frequency') +
  labs(title = 'Distribution of Frequency of purchases')
# (archive)
ggplot(rfm_data4, aes(Frequency)) +
  geom_density(color = 'blue') +
  xlab('Frequency') +
  labs(title = 'Distribution of Frequency of purchases')

# Average amount spent 
# (archive)
ggplot(rfm_data4, aes(Avg_Amount)) +
  geom_density(color = 'blue') +
  xlab('Average purchase amount') +
  labs(title = 'Distribution of Average Purcahse Amount')
# (archive)
ggplot(rfm_data4, aes(Avg_Amount)) +
  geom_histogram(color = 'blue', bins = 500) +
  xlab('Average purchase amount') +
  labs(title = 'Distribution of Average purchase amount')

# Monetary value
ggplot(rfm_data4, aes(Total_Amount)) +
  geom_histogram(color = 'blue', bins = 500) +
  xlab('Total amount spent throughout lifetime') +
  labs(title = 'Distribution of Amount spent')
# (archive)
ggplot(rfm_data4, aes(Total_Amount)) +
  geom_density(color = 'blue') +
  xlab('Total amount spent throughout lifetime') +
  labs(title = 'Distribution of Amount spent')

# Number of distinct products
# (archive)
ggplot(rfm_data4, aes(DPR)) +
  geom_density(color = 'blue') +
  xlab('Number of distinct products purchased') +
  labs(title = 'Distribution of Number of Distinct Products')
# (archive)
ggplot(rfm_data4, aes(DPR)) +
  geom_histogram(color = 'blue', bins = 50) +
  xlab('Number of distinct products purchased') +
  labs(title = 'Distribution of Number of Distinct Products')

# Age
ggplot(rfm_data4, aes(Age)) +
  geom_histogram(color = 'blue', bins = 50) +
  xlab('Age in days') +
  labs(title = "Distribution of Customers' Age with the business")
# (archive)
ggplot(rfm_data4, aes(Age)) +
  geom_density(color = 'blue') +
  xlab('Age in days') +
  labs(title = "Distribution of Customers' Age with the business")

# CSI
ggplot(rfm_data4, aes(CSI)) +
  geom_bar(color = 'blue') +
  xlab('Customer Satisfaction Index') +
  labs(title = "Distribution of Customers Satisfaction Index (CSI)")

# CLV
ggplot(rfm_data4, aes(CLV)) +
  geom_histogram(color = 'blue', bins = 500) +
  xlab('Customer Lifetime Value') +
  labs(title = "Distribution of Customer Lifetime Value")
# (archive)
ggplot(rfm_data4, aes(CLV)) +
  geom_density(color = 'blue') +
  xlab('Customer Lifetime Value') +
  labs(title = "Distribution of Customer Lifetime Value")

# Bivariate analysis
ggplot(rfm_data4, aes(Recency, Frequency, color = CSI)) +
  geom_point(alpha = 0.7) +
  scale_color_brewer(palette = 'Spectral') +
  labs(title = "Recency vs Frequency by Customer Satisfaction") 

ggplot(rfm_data4, aes(Age, Avg_Amount, color = CSI, size = Avg_Amount)) +
  geom_point() +
  scale_color_brewer(palette = 'Spectral') +
  labs(title = "Average amount spent vs Customers' age by Customer satisfaction")

ggplot(rfm_data4, aes(y = Total_Amount, x = Age, color = CSI, size = Avg_Amount)) +
  geom_point(alpha = 0.8) +
  scale_color_brewer(palette = 'Spectral') +
  labs(title = "Total amount spent vs Customers' age by CSI & Average amount spent")

ggplot(rfm_data4, aes(Age, Frequency)) +
  geom_point(alpha = 0.3, color = 'blue') +
  scale_color_brewer(palette = 'Spectral') +
  labs(title = "Frequency vs Customers' age")

ggplot(rfm_data4, aes(y = DPR, x = CLV, color = CSI, size = CLV)) +
  geom_point(alpha = 0.7) +
  scale_color_brewer(palette = 'Spectral') +
  labs(title = "Number of distinct products vs CLV by Customer satisfaction") 


# (archive)
ggplot(rfm_data4, aes(y = Total_Amount, x = Age, color = CSI, size = Total_Amount)) +
 geom_point(alpha = 0.6) +
 scale_color_brewer(palette = 'Spectral') +
 labs(title = "Amount spent vs Customers' age by CSI")

ggplot(rfm_data4, aes(y = Age, x = CSI)) +
  geom_boxplot() +
  labs(title = "Customer Satisfaction vs Customers' age with the business")

ggplot(rfm_data4, aes(y = Recency, x = CSI)) +
  geom_boxplot() +
  labs(title = "Recency of purchases vs Customers satisfaction")

ggplot(rfm_data4, aes(y = DPR, x = CSI)) +
  geom_boxplot(alpha = 0.8) +
  scale_color_brewer(palette = 'Spectral') +
  labs(title = "Number of distinct products vs Customer satisfaction")

ggplot(rfm_data4, aes(y = DPR, x = Frequency, color = CSI)) +
  geom_point(alpha = 0.7) +
  scale_color_brewer(palette = 'Spectral') +
  labs(title = "Number of distinct products vs Frequency")
