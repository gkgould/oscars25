# Importing libraries
library(tidyverse)




# DATA PREPROCESSING

oscar_df <- read.csv("Oscardata.csv")

# Select key variables starting with movie name up until Cin variable, then from PrNI-PrW, Gdr-SAF*, Age, Length, Days, WR

base_calibration_data <- oscar_df %>% 
  select(c(8:10, 12:25, 38:54, 70, 93, 94, 106)) %>% #Select relevant variables
  filter(Year < 2024 & Year > 2008) %>%  # Filter years 2009-2023
  mutate(Ch = factor(ifelse(Ch == 2, 0, Ch), levels = c(0, 1)))

cols_to_check <- c(4:8, 10:17, 24:34) # Select columns for factorizing

# Factorize columns
for (col in colnames(base_calibration_data)[cols_to_check]) {
  # Get unique values in the column, ignoring NA
  unique_vals <- unique(na.omit(base_calibration_data[[col]]))
  
  # Check if the column contains ONLY 0s and 1s (no other values)
  if (all(unique_vals %in% c(0, 1)) && length(unique_vals) == 2) {
    # Convert to a factor with levels 0 and 1
    base_calibration_data[[col]] <- factor(base_calibration_data[[col]], levels = c(0, 1))
  }
}

base_calibration_data$Year <- as.factor(base_calibration_data$Year)

str(mov_cal)

# Define training sets

mov_cal <- base_calibration_data %>% 
  filter(PP==1) %>% 
  select(-c(2, 4:7, 35)) # Movie calibration set

act_cal <- base_calibration_data %>% 
  filter(MM==1) %>% 
  select(-c(2, 4:7, 35))

actr_cal <- base_calibration_data %>% 
  filter(FF==1)

dir_cal <- base_calibration_data %>% 
  filter(DD==1)

# Main model

mov_cal_df <- mov_cal %>%
  select(-c(1,2,"Gdr", "Gm2"))

act_cal_df <- act_cal %>% 
  select(-c(1,2,"Gdr", "Gm2"))

str(act_cal_df)

mov_model <- glm(Ch ~ ., data=mov_cal_df, family = binomial)
summary(mov_model)

act_model <- glm(Ch ~ ., data=act_cal_df, family = binomial)
summary(act_model)

model.null = glm(Ch ~ 1, data=mov_cal_df, family = binomial)
model.full = glm(Ch ~ ., data=mov_cal_df, family = binomial)
forward = step(model.full, scope = list(lower = model.null, upper = model.full),
               direction = "backward")
summary(forward)

mov_model_best <- glm(Ch ~ Nom + Dir + Aml + Afl + Ams + Scr + Cin + PrNl + PrNs + 
                        PrWs + PrN + Gmc + Gd + Gm1 + PGA + DGA + Length + Days + 
                        WR, data=mov_cal_df, family=binomial)

act_model_best <- glm(Ch ~ Nom + Dir + Aml + Afl + Ams + Scr + Cin + PrNl + PrNs + 
                        PrWs + PrN + Gmc + Gd + Gm1 + PGA + DGA + Length + Days + 
                        WR, data=act_cal_df, family=binomial)
summary(act_model_best)

summary(mov_model_best)
# Define prediction dataset

base_calibration_data <- oscar_df %>% 
  select(c(8:10, 12:25, 38:54, 70, 93, 94, 106)) %>% #Select relevant variables
  filter(Year==2024) %>%  # Filter year to 2024
  mutate(Ch = factor(ifelse(Ch == 2, 0, Ch), levels = c(0, 1)))

cols_to_check <- c(4:8, 10:17, 24:34) # Select columns for factorizing

# Factorize columns
for (col in colnames(base_calibration_data)[cols_to_check]) {
  # Get unique values in the column, ignoring NA
  unique_vals <- unique(na.omit(base_calibration_data[[col]]))
  
  # Check if the column contains ONLY 0s and 1s (no other values)
  if (all(unique_vals %in% c(0, 1)) && length(unique_vals) == 2) {
    # Convert to a factor with levels 0 and 1
    base_calibration_data[[col]] <- factor(base_calibration_data[[col]], levels = c(0, 1))
  }
}

base_calibration_data$Year <- as.factor(base_calibration_data$Year)

mov_pred <- base_calibration_data %>% 
  filter(PP==1) %>% 
  select(-c(2, 4:8, 35))
  
mov_pred_df <- mov_pred %>% 
  select(-c(1,2, "Gdr", "Gm2")) %>%
  mutate(Ams=as.numeric(Ams))

act_pred <- base_calibration_data %>% 
  filter(MM==1) %>% 
  select(-c(2, 4:8, 35))

act_pred_df <- act_pred %>% 
  select(-c(1,2, "Gdr", "Gm2")) %>%
  mutate(Ams=as.numeric(Ams))

str(mov_pred_df)
str(mov_cal_df)
predicted_probs <- predict(mov_model_best, mov_pred_df, type = "response")
predicted_probs

predicted_probs_act <- predict(act_model_best, act_pred_df, type = "response")
predicted_probs_act
