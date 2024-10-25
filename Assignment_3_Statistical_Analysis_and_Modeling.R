
#################Section 1#####################################################################################

#Section 1 - Part A

covid_county_2020 <- read.csv("C:/Users/Jake Lorenzo/Desktop/Data Analytics/Assignment 3/us-counties-2020.csv")
covid_county_2021 <- read.csv("C:/Users/Jake Lorenzo/Desktop/Data Analytics/Assignment 3/us-counties-2021.csv")

#filter datasets to only cases and deaths columns
covid_cases_deaths_2020 <- covid_county_2020[, c("cases", "deaths")]
covid_cases_deaths_2021 <- covid_county_2021[, c("cases", "deaths")]

# Removing rows with NA values both columns
covid_cases_deaths_2020 <- na.omit(covid_cases_deaths_2020)
covid_cases_deaths_2021 <- na.omit(covid_cases_deaths_2021)

# Remove rows with '0' as a value in 2020 dataset
covid_cases_deaths_2020 <- covid_cases_deaths_2020[covid_cases_deaths_2020$cases != 0 & covid_cases_deaths_2020$deaths != 0, ]

# Remove rows with '0' as a value in 2021 dataset
covid_cases_deaths_2021 <- covid_cases_deaths_2021[covid_cases_deaths_2021$cases != 0 & covid_cases_deaths_2021$deaths != 0, ]

# Convert all columns in 2020 dataset to log base 10
covid_cases_deaths_2020_log <- log10(covid_cases_deaths_2020)

# Convert all columns in 2021 dataset to log base 10
covid_cases_deaths_2021_log <- log10(covid_cases_deaths_2021)

#boxplot comparing COVID-19 cases between 2020 and 2021
boxplot(covid_cases_deaths_2020_log$cases, covid_cases_deaths_2021_log$cases,
        names = c("2020 Cases", "2021 Cases"), 
        main = "Comparison of COVID-19 Cases (2020 vs 2021)",
        ylab = "Number of Cases", 
        col = c("lightblue", "lightgreen"))

#boxplot comparing COVID-19 deaths between 2020 and 2021
boxplot(covid_cases_deaths_2020_log$deaths, covid_cases_deaths_2021_log$deaths,
        names = c("2020 Deaths", "2021 Deaths"), 
        main = "Comparison of COVID-19 Deaths (2020 vs 2021)",
        ylab = "Number of Deaths", 
        col = c("lightblue", "lightgreen"))

#summary statistics for both datasets
summary(covid_cases_deaths_2020)
summary(covid_cases_deaths_2021)

#Section 1 - Part B
# Histogram for COVID-19 cases in 2020

hist(covid_cases_deaths_2020_log$cases, 
     main = "Distribution of COVID-19 Cases in 2020", 
     xlab = "Number of Cases", 
     col = "lightblue", 
     border = "black",
     breaks = 50,
     probability = TRUE)

# Overlay standard normal distribution curve
x_2020_cases <- seq(min(covid_cases_deaths_2020_log$cases), max(covid_cases_deaths_2020_log$cases), length = 100)
lines(x_2020_cases, dnorm(x_2020_cases, mean = mean(covid_cases_deaths_2020_log$cases), sd = sd(covid_cases_deaths_2020_log$cases)), col = "red", lwd = 2)


# Histogram for COVID-19 cases in 2021
hist(covid_cases_deaths_2021_log$cases, 
     main = "Distribution of COVID-19 Cases in 2021", 
     xlab = "Number of Cases", 
     col = "lightgreen", 
     border = "black",
     breaks = 50,
     probability = TRUE)

# Overlay standard normal distribution curve
x_2021_cases <- seq(min(covid_cases_deaths_2021_log$cases), max(covid_cases_deaths_2021_log$cases), length = 100)
lines(x_2021_cases, dnorm(x_2021_cases, mean = mean(covid_cases_deaths_2021_log$cases), sd = sd(covid_cases_deaths_2021_log$cases)), col = "red", lwd = 2)

# Histogram for COVID-19 deaths in 2020
hist(covid_cases_deaths_2020_log$deaths, 
     main = "Distribution of COVID-19 Deaths in 2020", 
     xlab = "Number of Deaths", 
     col = "lightblue", 
     border = "black",
     breaks = 50,
     probability = TRUE)

# Overlay standard normal distribution curve
x_2020_deaths <- seq(min(covid_cases_deaths_2020_log$deaths), max(covid_cases_deaths_2020_log$deaths), length = 100)

lines(x_2020_deaths, dnorm(x_2020_deaths, mean = mean(covid_cases_deaths_2020_log$deaths), sd = sd(covid_cases_deaths_2020_log$deaths)), col = "red", lwd = 2)

# Histogram for COVID-19 deaths in 2021
hist(covid_cases_deaths_2021_log$deaths, 
     main = "Distribution of COVID-19 Deaths in 2021", 
     xlab = "Number of Deaths", 
     col = "lightgreen", 
     border = "black",
     breaks = 50,
     probability = TRUE)

# Overlay standard normal distribution curve
x_2021_deaths <- seq(min(covid_cases_deaths_2021_log$deaths), max(covid_cases_deaths_2021_log$deaths), length = 100)

lines(x_2021_deaths, dnorm(x_2021_deaths, mean = mean(covid_cases_deaths_2021_log$deaths), sd = sd(covid_cases_deaths_2021_log$deaths)), col = "red", lwd = 2)

#Section 1 - Part C

# ECDF for COVID-19 cases in 2020
ecdf_2020_cases <- ecdf(covid_cases_deaths_2020_log$cases)
plot(ecdf_2020_cases, main = "ECDF of COVID-19 Cases in 2020", 
     xlab = "Number of Cases", 
     ylab = "ECDF", 
     col = "blue", 
     lwd = 2)

# ECDF for COVID-19 cases in 2021
ecdf_2021_cases <- ecdf(covid_cases_deaths_2021_log$cases)
plot(ecdf_2021_cases, main = "ECDF of COVID-19 Cases in 2021", 
     xlab = "Number of Cases", 
     ylab = "ECDF", 
     col = "green", 
     lwd = 2)

# ECDF for COVID-19 deaths in 2020
ecdf_2020_deaths <- ecdf(covid_cases_deaths_2020_log$deaths)
plot(ecdf_2020_deaths, main = "ECDF of COVID-19 Deaths in 2020", 
     xlab = "Number of Deaths", 
     ylab = "ECDF", 
     col = "blue", 
     lwd = 2)

# ECDF for COVID-19 deaths in 2021
ecdf_2021_deaths <- ecdf(covid_cases_deaths_2021_log$deaths)
plot(ecdf_2021_deaths, main = "ECDF of COVID-19 Deaths in 2021", 
     xlab = "Number of Deaths", 
     ylab = "ECDF", 
     col = "green", 
     lwd = 2)

#Plot the Q-Q Plot for both variables

# Q-Q plot comparing COVID-19 cases in 2020 vs 2021
qqplot(covid_cases_deaths_2020_log$cases, covid_cases_deaths_2021_log$cases,
       main = "Q-Q Plot: COVID-19 Cases (2020 vs 2021)",
       xlab = "2020 Cases Quantiles", 
       ylab = "2021 Cases Quantiles", 
       col = "blue")

#add reference line
abline(0, 1, col = "red", lwd = 2) 

# Q-Q plot comparing COVID-19 deaths in 2020 vs 2021
qqplot(covid_cases_deaths_2020_log$deaths, covid_cases_deaths_2021_log$deaths,
       main = "Q-Q Plot: COVID-19 Deaths (2020 vs 2021)",
       xlab = "2020 Deaths Quantiles", 
       ylab = "2021 Deaths Quantiles", 
       col = "blue")

#add reference line
abline(0, 1, col = "red", lwd = 2)

##############################################Section 2###################################################3

#Section 2 Filter the distributions you explored in Q1 by a number of states or counties. Repeat Q1b, Q1c and Q1d 

#read in datasets into different variables
covid_county_2020_new <- read.csv("C:/Users/Jake Lorenzo/Desktop/Data Analytics/Assignment 3/us-counties-2020.csv")
covid_county_2021_new <- read.csv("C:/Users/Jake Lorenzo/Desktop/Data Analytics/Assignment 3/us-counties-2021.csv")

#filter datasets down to state, cases, and deaths columns
covid_cases_deaths_2020_new <- covid_county_2020_new[, c("state","cases","deaths")]
covid_cases_deaths_2021_new <- covid_county_2021_new[, c("state","cases","deaths")]

# Removing rows with NA values in both datasets
covid_cases_deaths_2020_new <- na.omit(covid_cases_deaths_2020_new)
covid_cases_deaths_2021_new <- na.omit(covid_cases_deaths_2021_new)

# Remove rows with '0' values in 2020 dataset
covid_cases_deaths_2020_new <- covid_cases_deaths_2020_new[covid_cases_deaths_2020_new$cases != 0 & covid_cases_deaths_2020_new$deaths != 0, ]

# Remove rows with '0' values in 2021 dataset
covid_cases_deaths_2021_new <- covid_cases_deaths_2021_new[covid_cases_deaths_2021_new$cases != 0 & covid_cases_deaths_2021_new$deaths != 0, ]

install.packages("dplyr")
library(dplyr)

# Convert cases and deaths columns to log base 10 form (2020 dataset)
covid_cases_deaths_2020_new_log <- covid_cases_deaths_2020_new
covid_cases_deaths_2020_new_log[, 2:3] <- log10(covid_cases_deaths_2020_new[, 2:3])

# Convert cases and deaths columns to log base 10 form (2020 dataset)
covid_cases_deaths_2021_new_log <- covid_cases_deaths_2021_new
covid_cases_deaths_2021_new_log[, 2:3] <- log10(covid_cases_deaths_2021_new[, 2:3])

#filter 2020 dataframe by state
covid_2020_cases_deaths_filtered <- covid_cases_deaths_2020_new_log %>% filter(state %in% c("Washington", "California", "Florida", "Virginia", "Texas"))

#filter 2021 dataframes by state
covid_2021_cases_deaths_filtered <- covid_cases_deaths_2021_new_log %>% filter(state %in% c("Washington", "California", "Florida", "Virginia", "Texas"))

#Section 2 - Boxplots

#boxplot comparing cases between 2020 and 2021 filtered by state
boxplot(covid_2020_cases_deaths_filtered$cases, covid_2021_cases_deaths_filtered$cases,
        names = c("2020 Cases", "2021 Cases"), 
        main = "Comparison of COVID-19 Cases (2020 vs 2021)",
        ylab = "Number of Cases", 
        col = c("lightblue", "lightgreen"))

#boxplot comparing deaths between 2020 and 2021 filtered by state
boxplot(covid_2020_cases_deaths_filtered$deaths, covid_2021_cases_deaths_filtered$deaths,
        names = c("2020 Deaths", "2021 Deaths"), 
        main = "Comparison of COVID-19 Deaths (2020 vs 2021)",
        ylab = "Number of Deaths", 
        col = c("lightblue", "lightgreen"))

#summary statistics for both datasets
summary(covid_2020_cases_deaths_filtered)
summary(covid_2021_cases_deaths_filtered)

#Section 2 - Histograms
hist(covid_2020_cases_deaths_filtered$cases, 
     main = "Distribution of COVID-19 Cases in 2020", 
     xlab = "Number of Cases", 
     col = "lightblue", 
     border = "black",
     breaks = 50,
     probability = TRUE)


x_2020_cases <- seq(min(covid_2020_cases_deaths_filtered$cases), max(covid_2020_cases_deaths_filtered$cases), length = 100)

lines(x_2020_cases, dnorm(x_2020_cases, mean = mean(covid_2020_cases_deaths_filtered$cases), sd = sd(covid_2020_cases_deaths_filtered$cases)),  col = "red", lwd = 2)

# Histogram for COVID-19 cases in 2021
hist(covid_2021_cases_deaths_filtered$cases, 
     main = "Distribution of COVID-19 Cases in 2021", 
     xlab = "Number of Cases", 
     col = "lightgreen", 
     border = "black",
     breaks = 50,
     probability = TRUE)

x_2021_cases <- seq(min(covid_2021_cases_deaths_filtered$cases),  max(covid_2021_cases_deaths_filtered$cases), length = 100)

lines(x_2021_cases, dnorm(x_2021_cases, mean = mean(covid_2021_cases_deaths_filtered$cases), sd = sd(covid_2021_cases_deaths_filtered$cases)), col = "red", lwd = 2)

# Histogram for COVID-19 deaths in 2020
hist(covid_2020_cases_deaths_filtered$deaths, 
     main = "Distribution of COVID-19 Deaths in 2020", 
     xlab = "Number of Deaths", 
     col = "lightblue", 
     border = "black",
     breaks = 50,
     probability = TRUE)

# Overlay standard normal distribution curve
x_2020_deaths <- seq(min(covid_2020_cases_deaths_filtered$deaths),  max(covid_2020_cases_deaths_filtered$deaths), length = 100)

lines(x_2020_deaths, dnorm(x_2020_deaths, mean = mean(covid_2020_cases_deaths_filtered$deaths),  sd = sd(covid_2020_cases_deaths_filtered$deaths)), col = "red", lwd = 2)

# Histogram for COVID-19 deaths in 2021
hist(covid_2021_cases_deaths_filtered$deaths, 
     main = "Distribution of COVID-19 Deaths in 2021", 
     xlab = "Number of Deaths", 
     col = "lightgreen", 
     border = "black",
     breaks = 50,
     probability = TRUE)

# Overlay standard normal distribution curve
x_2021_deaths <- seq(min(covid_2021_cases_deaths_filtered$deaths), max(covid_2021_cases_deaths_filtered$deaths), length = 100)
lines(x_2021_deaths, dnorm(x_2021_deaths, mean = mean(covid_2021_cases_deaths_filtered$deaths), sd = sd(covid_2021_cases_deaths_filtered$deaths)), col = "red", lwd = 2)

#Section 2 - ECDF 

# ECDF for COVID-19 cases in 2020
ecdf_2020_cases_state <- ecdf(covid_2020_cases_deaths_filtered$cases)
plot(ecdf_2020_cases_state, main = "ECDF of COVID-19 Cases in 2020", 
     xlab = "Number of Cases", 
     ylab = "ECDF", 
     col = "blue", 
     lwd = 2)

# ECDF for COVID-19 cases in 2021
ecdf_2021_cases_state <- ecdf(covid_2021_cases_deaths_filtered$cases)
plot(ecdf_2021_cases_state, main = "ECDF of COVID-19 Cases in 2021", 
     xlab = "Number of Cases", 
     ylab = "ECDF", 
     col = "green", 
     lwd = 2)

# ECDF for COVID-19 deaths in 2020
ecdf_2020_deaths_state <- ecdf(covid_2020_cases_deaths_filtered$deaths)
plot(ecdf_2020_deaths_state, main = "ECDF of COVID-19 Deaths in 2020", 
     xlab = "Number of Deaths", 
     ylab = "ECDF", 
     col = "blue", 
     lwd = 2)

# ECDF for COVID-19 deaths in 2021
ecdf_2021_deaths <- ecdf(covid_2021_cases_deaths_filtered$deaths)
plot(ecdf_2021_deaths, main = "ECDF of COVID-19 Deaths in 2021", 
     xlab = "Number of Deaths", 
     ylab = "ECDF", 
     col = "green", 
     lwd = 2)

#Section 2 - Q-Q Plots

# Q-Q plot comparing cases in 2020 vs 2021
qqplot(covid_2020_cases_deaths_filtered$cases, covid_2021_cases_deaths_filtered$cases,
       main = "Q-Q Plot: COVID-19 Cases (2020 vs 2021)",
       xlab = "2020 Cases Quantiles", 
       ylab = "2021 Cases Quantiles", 
       col = "blue")

#add reference line
abline(0, 1, col = "red", lwd = 2) 

# Q-Q plot comparing deaths in 2020 vs 2021
qqplot(covid_2020_cases_deaths_filtered$deaths, covid_2021_cases_deaths_filtered$deaths,
       main = "Q-Q Plot: COVID-19 Deaths (2020 vs 2021)",
       xlab = "2020 Deaths Quantiles", 
       ylab = "2021 Deaths Quantiles", 
       col = "blue")

#add reference line
abline(0, 1, col = "red", lwd = 2)

#####################################Section 3#############################################

#Section 3 Part A - Linear Model

#read in ny housing data
ny_housing_data <- read.csv("C:/Users/Jake Lorenzo/Desktop/Data Analytics/Assignment 3/NY-House-Dataset.csv")

#filter down to four relevant columns
ny_housing_data <- ny_housing_data[, c("PRICE", "BEDS", "BATH", "PROPERTYSQFT")]

#remove rows with NA values
ny_housing_data <- na.omit(ny_housing_data)

#check data types of the dataset
str(ny_housing_data)

#summary statistics of dataset
summary(ny_housing_data)

##########Preprocessing data to remove outliers after running preliminary linear model

#removing the rows that have "0" as a value in the BATH column
ny_housing_data <- ny_housing_data[ny_housing_data$BATH != 0, ]

# display all rows where BATH > 6
ny_housing_data[ny_housing_data$BATH > 6, "BATH"]

# Remove rows where BATH > 6
ny_housing_data <- ny_housing_data[ny_housing_data$BATH <= 6, ]

# display all rows where BEDS > 5
ny_housing_data[ny_housing_data$BEDS > 5, "BEDS"]

# Remove rows where BEDS > 5
ny_housing_data <- ny_housing_data[ny_housing_data$BEDS <= 5, ]

# display all rows where PROPERTYSQFT > 9000
ny_housing_data[ny_housing_data$PROPERTYSQFT > 9000, "PROPERTYSQFT"]

# Remove rows where PROPERTYSQFT > 9000
ny_housing_data <- ny_housing_data[ny_housing_data$PROPERTYSQFT <= 9000, ]

# display all rows where PRICE > 15000000
ny_housing_data[ny_housing_data$PRICE > 15000000, "PRICE"]

# Remove rows where PRICE > 15000000
ny_housing_data <- ny_housing_data[ny_housing_data$PRICE <= 15000000, ]

#convert PRICE and PROPERTYSQFT columns to log base 10 version to clean up the data
ny_housing_data$PRICE <- log10(ny_housing_data$PRICE)  
ny_housing_data$PROPERTYSQFT <- log10(ny_housing_data$PROPERTYSQFT)

# Creating the linear model
ny_housing_lm <- lm(PRICE ~ BEDS + BATH + PROPERTYSQFT, data = ny_housing_data)

# Displaying a summary of the model
summary(ny_housing_lm)

# Plotting the linear model
plot(ny_housing_lm)

#summary statistics
summary(ny_housing_data)

#creating residuals which contains the values of all the residuals in the linear model
residuals <- residuals(ny_housing_lm)

# Calculate the standard deviation of the residuals
residual_sd <- sd(residuals)

# create a threshold of values within 2 standard deviation of the data center
threshold <- 2 * residual_sd

# Identifying outlier residuals that fall out of the 2 standard deviation threshold
outlier_residuals <- ny_housing_data[abs(residuals) > threshold, ]

# remove rows that contain values found in outlier residuals
ny_housing_data <- ny_housing_data[!rownames(ny_housing_data) %in% rownames(outlier_residuals), ]

# checking size of the new dataset to see amount of data lost
dim(ny_housing_data)  

# Refitting the linear model
ny_housing_lm <- lm(PRICE ~ BEDS + BATH + PROPERTYSQFT, data = ny_housing_data)

# Plotting the linear model
plot(ny_housing_lm)

# summary statistics
summary(ny_housing_lm)

# Plotting the scatterplot of PROPERTYSQFT vs PRICE
plot(ny_housing_data$PROPERTYSQFT, ny_housing_data$PRICE, 
     main = "Scatterplot of Price vs. Property Square Footage",
     xlab = "Property Square Footage", 
     ylab = "Price", 
     pch = 19, 
     col = "blue")

# adding line of best fit from the linear model
abline(lm(PRICE ~ PROPERTYSQFT, data = ny_housing_data), col = "red", lwd = 2)

#Section 3 Part B - Linear Model (Subset of Data - Price > 5,000,000)

#read in ny housing data
ny_housing_data_subset <- read.csv("C:/Users/Jake Lorenzo/Desktop/Data Analytics/Assignment 3/NY-House-Dataset.csv")

#filter down to four relevant columns
ny_housing_data_subset <- ny_housing_data_subset[, c("PRICE", "BEDS", "BATH", "PROPERTYSQFT")]

#remove rows with NA values
ny_housing_data_subset <- na.omit(ny_housing_data_subset)

#check data types of the dataset
str(ny_housing_data_subset)

#summary statistics of dataset
summary(ny_housing_data_subset)

# display all rows where PRICE > 5000000
ny_housing_data_subset[ny_housing_data_subset$PRICE > 5000000, "PRICE"]

# Remove rows where PRICE < 5000000
ny_housing_data_subset <- ny_housing_data_subset[ny_housing_data_subset$PRICE >= 5000000, ]

#convert PRICE and PROPERTYSQFT columns to log base 10 version to clean up the data
ny_housing_data_subset$PRICE <- log10(ny_housing_data_subset$PRICE)  
ny_housing_data_subset$PROPERTYSQFT <- log10(ny_housing_data_subset$PROPERTYSQFT)

# Creating the linear model
ny_housing_lm_subset <- lm(PRICE ~ BEDS + BATH + PROPERTYSQFT, data = ny_housing_data_subset)

# Displaying a summary of the model
summary(ny_housing_lm_subset)

# Plotting the linear model
plot(ny_housing_lm_subset)

#creating residuals which contains the values of all the residuals in the linear model subset
residuals <- residuals(ny_housing_lm_subset)

# Calculate the standard deviation of the residuals
residual_sd <- sd(residuals)

# create a threshold of values within 2 standard deviation of the data center
threshold <- 2 * residual_sd

# Identifying outlier residuals that fall out of the 2 standard deviation threshold
outlier_residuals <- ny_housing_data_subset[abs(residuals) > threshold, ]

# remove rows that contain values found in outlier residuals
ny_housing_data_subset <- ny_housing_data_subset[!rownames(ny_housing_data_subset) %in% rownames(outlier_residuals), ]

# Refitting the linear model
ny_housing_lm_subset <- lm(PRICE ~ BEDS + BATH + PROPERTYSQFT, data = ny_housing_data_subset)

# Plotting the linear model
plot(ny_housing_lm_subset)

# summary statistics
summary(ny_housing_lm_subset)

# Plotting the scatterplot of PROPERTYSQFT vs PRICE
plot(ny_housing_data_subset$PROPERTYSQFT, ny_housing_data_subset$PRICE, 
     main = "Scatterplot of Price vs. Property Square Footage",
     xlab = "Property Square Footage", 
     ylab = "Price", 
     pch = 19, 
     col = "blue")

# adding line of best fit from the linear model
abline(lm(PRICE ~ PROPERTYSQFT, data = ny_housing_data_subset), col = "red", lwd = 2)
