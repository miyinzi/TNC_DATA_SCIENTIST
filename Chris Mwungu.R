
# Author: Chris Miyinzi Mwungu 
# CHEF Foodscape Innovation Specialist Data Analysis Assignment 
# DATE: 7th June  2024 

# Clearing R Memory 
rm(list = ls())

#Importing required libraries
library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)

# Setting  directory 
setwd("C:/Users/cmiyinzi/OneDrive - CGIAR/Desktop/TNC JOB")

# 1. Data Loading & Inspection
data <- read.csv("data.csv", header = T)
head(data)
tail(data)
View(data)
str(data)
names(data)

###############################################################################

# 2. Descriptive Statistics for key variables (gender, marital status, education background, household size)
# Assuming gender1, married1, educ1 are variables representing the household head

# Frequency table for gender1 (HOUSEHOLD HEAD ONLY)
gender1_freq <- table(data$gender1)
print("Frequency table for gender:")
print(gender1_freq)

# Frequency table for maritalstatus1 (HOUSEHOLD HEAD ONLY)
maritalstatus1_freq <- table(data$married1)
print("Frequency table for maritalstatus:")
print(maritalstatus1_freq)

###########################################

# Summary statistics for Age, Education level and Household size. 
# I assume age1 and educ1 are for household head as I don't have enough time to understand the whole data
age1_stats <- data.frame(
  Variable = "Age of household head",
  Mean = mean(data$age1, na.rm = TRUE),
  SD = sd(data$age1, na.rm = TRUE),
  Variance = var(data$age1, na.rm = TRUE),
  Min = min(data$age1, na.rm = TRUE),
  Max = max(data$age1, na.rm = TRUE)
)

educ1_stats <- data.frame(
  Variable = "Education of household head",
  Mean = mean(data$educ1, na.rm = TRUE),
  SD = sd(data$educ1, na.rm = TRUE),
  Variance = var(data$educ1, na.rm = TRUE),
  Min = min(data$educ1, na.rm = TRUE),
  Max = max(data$educ1, na.rm = TRUE)
)

hhsize_stats <- data.frame(
  Variable = "Household Size",
  Mean = mean(data$hhsize, na.rm = TRUE),
  SD = sd(data$hhsize, na.rm = TRUE),
  Variance = var(data$hhsize, na.rm = TRUE),
  Min = min(data$hhsize, na.rm = TRUE),
  Max = max(data$hhsize, na.rm = TRUE)
)
result <- rbind(age1_stats, educ1_stats, hhsize_stats)
print(result) # Printing the results

############## Plotting histogram to see the distribution of age for household heads 
ggplot(data, aes(x = age1)) +
  geom_histogram(binwidth = 2, fill = "black") +
  labs(title = "Histogram of Age of the Household Head",
       x = "Age of the Household Head",
       y = "Frequency") +
  theme_minimal()

###############################################################################

# 3. Data Cleaning & Transformation 

# Clean gender
data <- data %>% 
  mutate(across(starts_with("gender"), ~ recode(., `1` = "Male", `2` = "Female")))

# Clean Married 
data <- data %>% 
  mutate(across(starts_with("married"), 
                ~ recode(., 
                         `1` = "Married", 
                         `2` = "Never Married", 
                         `3` = "Previously Married (currently divorced, separated, widowed)", 
                         `4` = "Not Applicable (child < 16 years)"
                )
  )
  )

#Clean Farm work 
data <- data %>% 
  mutate(across(starts_with("farmwork"), 
                ~ recode(., 
                         `1` = "Yes", 
                         `2` = "No"
                )
  )
  )

# Clean no Farm work 
data <- data %>% 
  mutate(across(starts_with("nfarmwork"), 
                ~ recode(., 
                         `1` = "Yes", 
                         `2` = "No"
                )
  )
  )


# Checking for missing values for variables 1 to 10.
subset_data <- data[, 1:10]
missing_values <- is.na(subset_data)
print(missing_values[1:10, ])


# Plotting boxplots for the age of the household head
p_age1 <- ggplot(data, aes(x = "", y = age1)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(title = "Boxplot of Age of the household head",
       y = "Age of the household age") +
  theme_minimal()
print(p_age1)


###############################################################################

# 4. Probability & Mathematical Modelling:Hypothetical Scenario 

set.seed(123)  # Set seed for reproducibility
sample_size <- 100
selected_indices <- sample(1:nrow(data), sample_size)

# Subset the data frame to include only the selected households
subset_data <- data[selected_indices, ]

# Identify households where at least one member is engaged in both farm and non-farm activities
farmwork_columns <- grep("^farmwork", names(subset_data), value = TRUE)
nfarmwork_columns <- grep("^nfarmwork", names(subset_data), value = TRUE)

subset_data$engaged_in_both <- rowSums(subset_data[farmwork_columns] == 1 & subset_data[nfarmwork_columns] == 1, na.rm = TRUE) > 0

# Calculate the probability
probability <- mean(subset_data$engaged_in_both, na.rm = TRUE)

# Model the expected number of households meeting this criterion in a sample of 100 randomly selected households
expected_households <- probability * sample_size

###############################################################################

# 5. Data visualization
# Bar chart - Marital Status 
data_filtered <- data[!is.na(data$married1), ]
# Create a bar plot of marital status categories
p <- ggplot(data_filtered, aes(x = married1)) +
  geom_bar(fill = "black", color = "black", width = 0.5) +
  labs(title = "Marital Status of the household head",
       x = "Marital Status of the household head",
       y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1, family = "serif"),
        panel.background = element_rect(fill = "white"))  
print(p)

###############################################################################

