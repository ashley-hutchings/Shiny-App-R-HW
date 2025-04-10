setwd("/Users/ashleyhutchings/Desktop/R Class/Shiny App/ShinyAppHW")

# Load necessary libraries
library(tidyverse)
library(ggplot2)
install.packages("skimr")
library(skimr)
install.packages("DataExplorer")
library(DataExplorer)


# Load the dataset
alz <- read.csv("alzheimers_disease_data.csv")

# View the structure and basic summary
str(alz)
summary(alz)
skim(alz)  # More detailed summary

# Check for missing values
colSums(is.na(alz))

# Explore distribution of Diagnosis
table(alz$Diagnosis)
ggplot(alz, aes(x = factor(Diagnosis))) +
  geom_bar(fill = "steelblue") +
  labs(title = "Distribution of Diagnosis", x = "Diagnosis", y = "Count")



# Bin DietQuality if you haven't already
alz$DietQualityBin <- cut(alz$DietQuality,
                               breaks = quantile(alz$DietQuality, probs = seq(0, 1, 0.2), na.rm = TRUE),
                               include.lowest = TRUE,
                               labels = c("Very Low", "Low", "Medium", "High", "Very High"))

# Grouped bar chart (side-by-side bars)
ggplot(alz, aes(x = DietQualityBin, fill = factor(Diagnosis))) +
  geom_bar(position = "dodge") +  # side-by-side bars
  labs(title = "Diagnosis Count by Diet Quality",
       x = "Diet Quality (Binned)",
       y = "Count",
       fill = "Diagnosis") +
  theme_minimal()

# Bin SleepQuality if you haven't already
alz$SleepQualityBin <- cut(alz$SleepQuality,
                          breaks = quantile(alz$SleepQuality, probs = seq(0, 1, 0.2), na.rm = TRUE),
                          include.lowest = TRUE,
                          labels = c("Very Low", "Low", "Medium", "High", "Very High"))

# Grouped bar chart (side-by-side bars)
ggplot(alz, aes(x = SleepQualityBin, fill = factor(Diagnosis))) +
  geom_bar(position = "dodge") +  # side-by-side bars
  labs(title = "Diagnosis Count by Diet Quality",
       x = "Diet Quality (Binned)",
       y = "Count",
       fill = "Diagnosis") +
  theme_minimal()


# Age distribution by Diagnosis
ggplot(alz, aes(x = Age, fill = factor(Diagnosis))) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 30) +
  labs(title = "Age Distribution by Diagnosis", x = "Age", fill = "Diagnosis")

# Correlation matrix for numeric variables
numeric_vars <- alz %>% select(where(is.numeric))
cor_matrix <- cor(numeric_vars)
corrplot::corrplot(cor_matrix, method = "color", type = "upper")


# Make sure Diagnosis is numeric (if it's categorical like 0/1, this is fine)
correlations <- cor(alz[sapply(alz, is.numeric)])
cor_with_diagnosis <- correlations[, "Diagnosis"]
sort(abs(cor_with_diagnosis), decreasing = TRUE)



