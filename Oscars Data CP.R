library(tidyverse)
library(dplyr)
library(MASS)
oscars <- read_csv("Oscars Data 2025 - GCAK(oscars) CP.csv")

for (col in colnames(oscars)) {
    unique_vals <- unique(na.omit(oscars[[col]])) # Get unique values in the column, ignoring NA
    # Check if the column contains ONLY 0s and 1s (no other values)
    if (all(unique_vals %in% c(0, 1)) && length(unique_vals) == 2) {
      # Convert to a factor with levels 0 and 1
      oscars[[col]] <- factor(oscars[[col]], levels = c(0, 1))
    }
  }

oscars <- oscars %>% dplyr::select(-c(1:6, "AD", "Comp", 26:36, 56:69, "PGPG13", "U", 71:92, 95:105, 107)) %>% 
  mutate(Year = as.factor(Year),
         Ch = factor(ifelse(Ch == 2, 0, Ch), levels = c(0, 1)),
         vibes = ifelse(is.na(vibes), 0, vibes))

# Create category subset data# Create category subset data
BP_df <- data.frame()
BD_df <- data.frame()
BMA_df <- data.frame()
BFA_df <- data.frame()

# Loop through each row to form four new sub-datasets
for (i in 1:nrow(oscars)) {
  if (oscars$PP[i] == 1) {
    BP_df <- rbind(BP_df, oscars[i, ])  # Add row to Best Picture dataframe
  }
  if (oscars$DD[i] == 1) {
    BD_df <- rbind(BD_df, oscars[i, ])  # Add row to Best Director dataframe
  }
  if (oscars$MM[i] == 1) {
    BMA_df <- rbind(BMA_df, oscars[i, ])  # Add row to Best Male Actor dataframe
  }
  if (oscars$FF[i] == 1) {
    BFA_df <- rbind(BFA_df, oscars[i, ])  # Add row to Best Female Actress dataframe
  }
}

# Remove PP, DD, MM, and FF columns from each new dataframe
BP_df <- BP_df[, !(names(BP_df) %in% c("PP", "DD", "MM", "FF", "Age", "Pic"))] # Also remove Age (irrelevant) and Pic (collinearity)
BD_df <- BD_df[, !(names(BD_df) %in% c("PP", "DD", "MM", "FF", "Dir"))] # Remove Dir (collinearity)
BMA_df <- BMA_df[, !(names(BMA_df) %in% c("PP", "DD", "MM", "FF", "Aml", "Gf2"))] # Remove Aml (Collinearity) and Gf2 (no correlation)
BFA_df <- BFA_df[, !(names(BFA_df) %in% c("PP", "DD", "MM", "FF", "Afl"))] # Remove Afl (Collinearity)

# Creating training DF
t.BP_df <- BP_df %>% filter(Year != 2024)
for (col in names(t.BP_df)) {
  # Check if the column is a factor
  if (is.factor(t.BP_df[[col]])) {
    # Convert factor to numeric
    t.BP_df[[col]] <- as.numeric(as.character(t.BP_df[[col]])) 
    # Check if there are exactly 2 unique values (excluding NA)
    if (length(unique(na.omit(t.BP_df[[col]]))) == 2) {
      t.BP_df[[col]] <- as.factor(t.BP_df[[col]])  # Convert back to factor
    }
  }
}
t.BP_df <- t.BP_df %>% mutate(Year = as.factor(Year)) %>% na.omit() # Keep Year as factor and omit NAs

t.BD_df <- BD_df %>% filter(Year != 2024)
for (col in names(t.BD_df)) {
  # Check if the column is a factor
  if (is.factor(t.BD_df[[col]])) {
    # Convert factor to numeric
    t.BD_df[[col]] <- as.numeric(as.character(t.BD_df[[col]])) 
    # Check if there are exactly 2 unique values (excluding NA)
    if (length(unique(na.omit(t.BD_df[[col]]))) == 2) {
      t.BD_df[[col]] <- as.factor(t.BD_df[[col]])  # Convert back to factor
    }
  }
}
t.BD_df <- t.BD_df %>% mutate(Year = as.factor(Year)) %>% na.omit()

t.BMA_df <- BMA_df %>% filter(Year != 2024)
for (col in names(t.BMA_df)) {
  # Check if the column is a factor
  if (is.factor(t.BMA_df[[col]])) {
    # Convert factor to numeric
    t.BMA_df[[col]] <- as.numeric(as.character(t.BMA_df[[col]])) 
    # Check if there are exactly 2 unique values (excluding NA)
    if (length(unique(na.omit(t.BMA_df[[col]]))) == 2) {
      t.BMA_df[[col]] <- as.factor(t.BMA_df[[col]])  # Convert back to factor
    }
  }
}
t.BMA_df <- t.BMA_df %>% mutate(Year = as.factor(Year)) %>% na.omit()

t.BFA_df <- BFA_df %>% filter(Year != 2024) 
for (col in names(t.BFA_df)) {
  # Check if the column is a factor
  if (is.factor(t.BFA_df[[col]])) {
    # Convert factor to numeric
    t.BFA_df[[col]] <- as.numeric(as.character(t.BFA_df[[col]])) 
    # Check if there are exactly 2 unique values (excluding NA)
    if (length(unique(na.omit(t.BFA_df[[col]]))) == 2) {
      t.BFA_df[[col]] <- as.factor(t.BFA_df[[col]])  # Convert back to factor
    }
  }
}
t.BFA_df <- t.BFA_df %>% mutate(Year = as.factor(Year)) %>% na.omit()

# Creating training models
BP.model.null <- glm(Ch ~ 1, data = t.BP_df, family = binomial) # Option to force in Total Noms, WR, Gdr, SAE
BP.model.full <- glm(Ch ~ ., data = t.BP_df, family = binomial)
AIC.t.BP_df <- step(BP.model.null, scope = list(lower = BP.model.null, upper = BP.model.full),
                    direction = "both")

BD.model.null <- glm(Ch ~ 1, data = t.BD_df, family = binomial) # Option to force in Nom, WR, Gd
BD.model.full <- glm(Ch ~ ., data = t.BD_df, family = binomial)
AIC.t.BD_df <- step(BD.model.null, scope = list(lower = BD.model.null, upper = BD.model.full),
                    direction = "both")

BMA.model.null <- glm(Ch ~ 1, data = t.BMA_df, family = binomial) # Option to force in Nom, WR, Gm1
BMA.model.full <- glm(Ch ~ ., data = t.BMA_df, family = binomial)
AIC.t.BMA_df <- step(BMA.model.null, scope = list(lower = BMA.model.null, upper = BMA.model.full),
                    direction = "both")

BFA.model.null <- glm(Ch ~ 1, data = t.BFA_df, family = binomial) # Option to force in Nom, WR, Gf1
BFA.model.full <- glm(Ch ~ ., data = t.BFA_df, family = binomial)
AIC.t.BFA_df <- step(BFA.model.null, scope = list(lower = BFA.model.null, upper = BFA.model.full),
                    direction = "both")

# Creating prediction DF
p.BP_df <- BP_df %>% filter(Year == 2024)
p.BD_df <- BD_df %>% filter(Year == 2024)
p.BMA_df <- BMA_df %>% filter(Year == 2024)
p.BFA_df <- BFA_df %>% filter(Year == 2024)


# Creating predictions

# Get predicted probabilities
p.BP_df$Prediction <- predict(AIC.t.BP_df, p.BP_df, type = "response")
p.BD_df$Prediction <- predict(AIC.t.BD_df, p.BD_df, type = "response")
p.BMA_df$Prediction <- predict(AIC.t.BMA_df, p.BMA_df, type = "response")
p.BFA_df$Prediction <- predict(AIC.t.BFA_df, p.BFA_df, type = "response")

# Get standard errors from the model
p.BP_se <- predict(AIC.t.BP_df, p.BP_df, type = "link", se.fit = TRUE)$se.fit
p.BD_se <- predict(AIC.t.BD_df, p.BD_df, type = "link", se.fit = TRUE)$se.fit
p.BMA_se <- predict(AIC.t.BMA_df, p.BMA_df, type = "link", se.fit = TRUE)$se.fit
p.BFA_se <- predict(AIC.t.BFA_df, p.BFA_df, type = "link", se.fit = TRUE)$se.fit

# Compute Wald p-values (two-tailed test)
p.BP_df$p_value <- 2 * (1 - pnorm(abs(p.BP_df$Prediction / p.BP_se)))
p.BD_df$p_value <- 2 * (1 - pnorm(abs(p.BD_df$Prediction / p.BD_se)))
p.BMA_df$p_value <- 2 * (1 - pnorm(abs(p.BMA_df$Prediction / p.BMA_se)))
p.BFA_df$p_value <- 2 * (1 - pnorm(abs(p.BFA_df$Prediction / p.BFA_se)))

# Normalize
p.BP_df$Prediction <- p.BP_df$Prediction / sum(p.BP_df$Prediction)
p.BD_df$Prediction <- p.BD_df$Prediction / sum(p.BD_df$Prediction)
p.BMA_df$Prediction <- p.BMA_df$Prediction / sum(p.BMA_df$Prediction)
p.BFA_df$Prediction <- p.BFA_df$Prediction / sum(p.BFA_df$Prediction)

# Select relevant columns
final_p.BP <- p.BP_df[, c("Name", "Prediction", "p_value")]
final_p.BD <- p.BD_df[, c("Movie", "Name", "Prediction", "p_value")]
final_p.BMA <- p.BMA_df[, c("Movie", "Name", "Prediction", "p_value")]
final_p.BFA <- p.BFA_df[, c("Movie", "Name", "Prediction", "p_value")]

# Convert probability to percentage format
final_p.BP$Prediction <- round(final_p.BP$Prediction * 100, 2)
final_p.BD$Prediction <- round(final_p.BD$Prediction * 100, 2)
final_p.BMA$Prediction <- round(final_p.BMA$Prediction * 100, 2)
final_p.BFA$Prediction <- round(final_p.BFA$Prediction * 100, 2)

# Arrange results in descending order
final_p.BP <- final_p.BP[order(-final_p.BP$Prediction), ]
final_p.BD <- final_p.BD[order(-final_p.BD$Prediction), ]
final_p.BMA <- final_p.BMA[order(-final_p.BMA$Prediction), ]
final_p.BFA <- final_p.BFA[order(-final_p.BFA$Prediction), ]

# Display final table
print(final_p.BP, row.names = FALSE)
print(final_p.BD, row.names = FALSE)
print(final_p.BMA, row.names = FALSE)
print(final_p.BFA, row.names = FALSE)

