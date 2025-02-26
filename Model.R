# Importing libraries
library(tidyverse)




# DATA PREPROCESSING

oscar_df <- read.csv("Oscars Data 2025 - GCAK(oscars).csv")

# Select key variables starting with movie name up until Cin variable, then from PrNI-PrW, Gdr-SAF*, Age, Length, Days, WR

base_calibration_data <- oscar_df %>% 
  select(c(8:10, 12:25, 38:54, 70, 93, 94, 106)) %>% #Select relevant variables
  filter(Year < 2024 & Year > 2008) %>%  # Filter years 2009-2023
  mutate(Ch = factor(ifelse(Ch == 2, 0, Ch), levels = c(0, 1)))

cols_to_check <- c(4:8, 10:17, 24:34)

# Loop through the selected columns only
for (col in colnames(base_calibration_data)[cols_to_check]) {
  # Get unique values in the column, ignoring NA
  unique_vals <- unique(na.omit(base_calibration_data[[col]]))
  
  # Check if the column contains ONLY 0s and 1s (no other values)
  if (all(unique_vals %in% c(0, 1)) && length(unique_vals) == 2) {
    # Convert to a factor with levels 0 and 1
    base_calibration_data[[col]] <- factor(base_calibration_data[[col]], levels = c(0, 1))
  }
}


str(base_calibration_data)

# Define training sets

mov_cal <- base_calibration_data %>% 
  filter(PP==1) %>% 
  select(-2) # Movie calibration set

act_cal <- base_calibration_data %>% 
  filter(MM==1)

actr_cal <- base_calibration_data %>% 
  filter(FF==1)

dir_cal <- base_calibration_data %>% 
  filter(DD==1)


# Main model



model <- glm(Ch ~ . - Movie + factor(Year), data=mov_cal, family = binomial)
summary(model)
