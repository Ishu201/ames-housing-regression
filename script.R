## ALY6015 - Regression Diagnostics with R
## Module 1 - Assignment 1

# Reset environment
rm(list = ls())     # clears all objects
cat("\014")         # clears the console


# Install and load necessary packages
#install.packages("tidyverse")
#install.packages("car")
#install.packages("corrplot")
#install.packages("ggcorrplot")
#install.packages("leaps")

library(tidyverse)
library(car)
library(corrplot)
library(ggcorrplot)
library(leaps)

# ------------------------------- 1. DATASET LOADING ----------------------------------------------

ames_data <- read.csv("AmesHousing.csv")

# View the structure of the dataset
glimpse(ames_data)
head(ames_data)
names(ames_data)

# Clean column names by removing dots
names(ames_data) <- gsub("\\.", "", names(ames_data))

ames_data <- ames_data %>%
    select(-Order)  




# ---------------------------- 2. EDA ANALYSIS ---------------------------------------------------

# Identify variable types
numeric_vars <- ames_data %>% select_if(is.numeric)
categorical_vars <- ames_data %>% select_if(is.character)

# MISSING VALUES ANALYSIS
missing_summary <- data.frame(
    Variable = names(ames_data),
    Missing_Count = colSums(is.na(ames_data)),
    Missing_Percent = round(colSums(is.na(ames_data)) / nrow(ames_data) * 100, 2)
)
missing_summary <- missing_summary[missing_summary$Missing_Count > 0, ]
missing_summary <- missing_summary[order(-missing_summary$Missing_Count), ]



### NUMERIC VARIABLES ANALYSIS
# Summary statistics for ALL numeric variables
numeric_summary <- data.frame(
    Variable = names(numeric_vars),
    Mean = sapply(numeric_vars, function(x) round(mean(x, na.rm = TRUE), 2)),
    Median = sapply(numeric_vars, function(x) round(median(x, na.rm = TRUE), 2)),
    SD = sapply(numeric_vars, function(x) round(sd(x, na.rm = TRUE), 2)),
    Min = sapply(numeric_vars, function(x) round(min(x, na.rm = TRUE), 2)),
    Max = sapply(numeric_vars, function(x) round(max(x, na.rm = TRUE), 2)),
    Q1 = sapply(numeric_vars, function(x) round(quantile(x, 0.25, na.rm = TRUE), 2)),
    Q3 = sapply(numeric_vars, function(x) round(quantile(x, 0.75, na.rm = TRUE), 2))
)

# Create multi-panel plot for key continuous variables
key_vars <- c("SalePrice", "GrLivArea", "LotArea", "TotalBsmtSF", 
              "GarageArea", "YearBuilt", "OverallQual", "OverallCond")
key_vars <- key_vars[key_vars %in% names(ames_data)]

par(mfrow = c(3, 3), mar = c(3, 3, 2, 1))
for (var in key_vars) {
    if (var %in% names(ames_data)) {
        hist(ames_data[[var]], 
             main = paste("Distribution of", var),
             xlab = var,
             col = "steelblue",
             breaks = 30,
             cex.main = 0.9)
    }
}
par(mfrow = c(1, 1))

# Boxplots for key continuous variables to identify outliers
par(mfrow = c(1, 4))
for (var in key_vars) {
    if (var %in% names(ames_data)) {
        boxplot(ames_data[[var]], 
                main = var,
                col = "lightgreen",
                cex.main = 0.8)
    }
}
par(mfrow = c(1, 1))


### TARGET VARIABLE (SALEPRICE) DETAILED ANALYSIS
saleprice_stats_wide <- ames_data %>%
    summarise(
        Mean = round(mean(SalePrice, na.rm = TRUE), 2),
        Median = round(median(SalePrice, na.rm = TRUE), 2),
        Std_Dev = round(sd(SalePrice, na.rm = TRUE), 2),
        Min = round(min(SalePrice, na.rm = TRUE), 2),
        Max = round(max(SalePrice, na.rm = TRUE), 2),
        Range = round(Max - Min, 2),
        CV_Percent = round(sd(SalePrice, na.rm = TRUE) / mean(SalePrice, na.rm = TRUE) * 100, 2),
        Skewness = if (require(moments, quietly = TRUE)) round(skewness(SalePrice, na.rm = TRUE), 2) else NA,
        Kurtosis = if (require(moments, quietly = TRUE)) round(kurtosis(SalePrice, na.rm = TRUE), 2) else NA,
        Q1 = round(quantile(SalePrice, 0.25, na.rm = TRUE), 2),
        Q3 = round(quantile(SalePrice, 0.75, na.rm = TRUE), 2),
        IQR = round(Q3 - Q1, 2)
    )

# Prevent scientific notation
options(scipen = 999)

# SalePrice visualizations
par(mfrow = c(1, 2))

# Histogram
hist(ames_data$SalePrice, 
     main = "Distribution of Sale Prices",
     xlab = "Sale Price ($)",
     col = "lightblue",
     breaks = 50)

# Boxplot
boxplot(ames_data$SalePrice, 
        main = "Boxplot of Sale Prices",
        ylab = "Sale Price ($)",
        col = "lightgreen")

# Q-Q plot for normality
qqnorm(ames_data$SalePrice, main = "Q-Q Plot: SalePrice")
qqline(ames_data$SalePrice, col = "red")

# Density plot
plot(density(ames_data$SalePrice, na.rm = TRUE),
     main = "Density Plot: SalePrice",
     xlab = "Sale Price ($)",
     col = "darkblue",
     lwd = 2)

par(mfrow = c(1, 1))


### CATEGORICAL VARIABLES ANALYSIS

# Analyze key categorical variables
key_categorical <- c("MSZoning", "Neighborhood", "BldgType", "HouseStyle", 
                     "OverallQual", "OverallCond", "HeatingQC", "CentralAir")
key_categorical <- key_categorical[key_categorical %in% names(ames_data)]

for (var in key_categorical) {
    if (var %in% names(ames_data)) {
        cat("---", var, "---\n")
        freq_table <- table(ames_data[[var]], useNA = "ifany")
        freq_df <- data.frame(
            Category = names(freq_table),
            Count = as.numeric(freq_table),
            Percentage = round(as.numeric(freq_table) / sum(freq_table) * 100, 2)
        )
        freq_df <- freq_df[order(-freq_df$Count), ]
        print(freq_df, row.names = FALSE)
        cat("\n")
    }
}

# Visualize key categorical variables
# Select top categorical variables for visualization
viz_categorical <- c("BldgType", "HouseStyle","OverallQual", "OverallCond", "HeatingQC", "CentralAir")
viz_categorical <- viz_categorical[viz_categorical %in% names(ames_data)]

my_colors <- c("#4E79A7", "#F28E2B", "#E15759", "#76B7B2", "#59A14F", "#EDC948", "#B07AA1", "#FF9DA7", "#9C755F", "#BAB0AC")

par(mfrow = c(2, 3))
for (var in viz_categorical) {
    if (var %in% names(ames_data)) {
        freq <- table(ames_data[[var]])
        barplot(freq, 
                main = paste("Distribution of", var),
                col = my_colors[1:length(freq)],
                las = 2,
                cex.names = 0.7,
                cex.main = 0.9)
    }
}
par(mfrow = c(1, 1))


### RELATIONSHIP BETWEEN CATEGORICAL AND TARGET VARIABLE
# Boxplots showing SalePrice by categorical variables
analysis_categorical <- c("BldgType", "HouseStyle","OverallQual", "OverallCond", "HeatingQC", "CentralAir")
analysis_categorical <- analysis_categorical[analysis_categorical %in% names(ames_data)]

par(mfrow = c(1, 2))
for (var in analysis_categorical) {
    if (var %in% names(ames_data)) {
        boxplot(SalePrice ~ ames_data[[var]], 
                data = ames_data,
                main = paste("SalePrice by", var),
                xlab = var,
                ylab = "Sale Price ($)",
                col = "lightcoral",
                las = 2,
                cex.axis = 0.6,
                cex.main = 0.9)
    }
}
par(mfrow = c(1, 1))


# YEAR/TEMPORAL ANALYSIS
# Year Built distribution
if ("YearBuilt" %in% names(ames_data)) {
    par(mfrow = c(1, 2))
    hist(ames_data$YearBuilt,
         main = "Distribution of Year Built",
         xlab = "Year",
         col = "skyblue",
         breaks = 30)
    
    plot(ames_data$YearBuilt, ames_data$SalePrice,
         main = "SalePrice vs Year Built",
         xlab = "Year Built",
         ylab = "Sale Price ($)",
         col = alpha("blue", 0.3),
         pch = 19)
    par(mfrow = c(1, 1))
}

# Sale Year and Month analysis
if ("YrSold" %in% names(ames_data) && "MoSold" %in% names(ames_data)) {
    print(table(ames_data$YrSold))
    print(table(ames_data$MoSold))
    
    par(mfrow = c(1, 2))
    barplot(table(ames_data$YrSold),
            main = "Sales by Year",
            xlab = "Year",
            ylab = "Count",
            col = "coral")
    
    barplot(table(ames_data$MoSold),
            main = "Sales by Month",
            xlab = "Month",
            ylab = "Count",
            col = "lightgreen")
    par(mfrow = c(1, 1))
}




# ------------------------------- 3. IMPUTE MISSING VALUES ------------------------------------

# Check for missing value percentage
missing_summary <- data.frame(
    Variable = names(ames_data),
    Missing_Count = colSums(is.na(ames_data)),
    Missing_Percent = round(colSums(is.na(ames_data)) / nrow(ames_data) * 100, 2)
)
missing_summary <- missing_summary[missing_summary$Missing_Count > 0, ]

# Create a copy of the dataset for cleaning
ames_clean <- ames_data

# Create Age variable instead of using YearBuilt
if ("YearBuilt" %in% names(ames_clean) && "YrSold" %in% names(ames_clean)) {
    ames_clean$HouseAge <- ames_clean$YrSold - ames_clean$YearBuilt
}

# Remove Variables with more than 30% missing values
ames_clean <- ames_clean %>%
    select(-Alley, -FireplaceQu, -PoolQC, -Fence, -MiscFeature)

# Impute missing values for numeric variables with their mean
numeric_columns <- names(ames_clean)[sapply(ames_clean, is.numeric)]

for (col in numeric_columns) {
    if (sum(is.na(ames_clean[[col]])) > 0) {
        mean_val <- mean(ames_clean[[col]], na.rm = TRUE)
        ames_clean[[col]][is.na(ames_clean[[col]])] <- mean_val
        print(col)
    }
}
colSums(is.na(ames_clean))
numeric_vars <- ames_clean %>% select_if(is.numeric)




# -------------------------------- 4. CORRELATION MATRIX ---------------------------------------

# Remove identifiers
numeric_vars <- numeric_vars %>% select(-PID)

# Calculate correlation matrix
cor_matrix <- cor(numeric_vars, use = "complete.obs")

# Display correlations with SalePrice (sorted)
saleprice_cor <- cor_matrix[, "SalePrice"]
saleprice_cor_sorted <- sort(abs(saleprice_cor), decreasing = TRUE)

print(saleprice_cor_sorted)




# ---------------------------------- 5. CORRELATION MATRIX VISUALIZATION --------------------------
# Correlation plot
corrplot(
    cor_matrix,
    method = "circle",
    type = "upper",
    tl.col = "black",
    tl.srt = 45,
    tl.cex = 0.5,   
    title = "Correlation Matrix of Key Numeric Variables",
    mar = c(0, 0, 1, 0)
)

# Focused correlation plot for key variables
key_numeric_vars <- numeric_vars %>%
    select(SalePrice, GrLivArea, TotalBsmtSF, GarageArea, YearBuilt, OverallQual, OverallCond, LotArea, OpenPorchSF)

cor_matrix_key <- cor(key_numeric_vars, use = "complete.obs")
corrplot(
    cor_matrix_key,
    method = "circle",
    type = "upper",
    tl.col = "black",
    tl.srt = 45,
    tl.cex = 0.8,   
    title = "Correlation Matrix of Selected Key Numeric Variables",
    mar = c(0, 0, 1, 0)
)



# ---------------------------------- 6. SCATTER PLOTS FOR KEY RELATIONSHIPS ----------------------
# Identify variables with different correlation strengths with SalePrice
saleprice_cor_df <- data.frame(
    Variable = names(saleprice_cor),
    Correlation = saleprice_cor
)
saleprice_cor_df <- saleprice_cor_df[saleprice_cor_df$Variable != "SalePrice", ]
saleprice_cor_df <- saleprice_cor_df[order(abs(saleprice_cor_df$Correlation), decreasing = TRUE), ]

# Highest correlation
highest_cor_var <- saleprice_cor_df$Variable[1]
highest_cor_val <- saleprice_cor_df$Correlation[1]

# Lowest correlation (closest to zero, but not zero)
lowest_cor_var <- saleprice_cor_df$Variable[nrow(saleprice_cor_df)]
lowest_cor_val <- saleprice_cor_df$Correlation[nrow(saleprice_cor_df)]

# Correlation closest to 0.5
closest_to_05 <- saleprice_cor_df[which.min(abs(abs(saleprice_cor_df$Correlation) - 0.5)), ]
mid_cor_var <- closest_to_05$Variable
mid_cor_val <- closest_to_05$Correlation


# Scatter plots for these variables
ggplot(numeric_vars, aes(x = .data[[highest_cor_var]], y = SalePrice)) +
    geom_point(color = "blue", alpha = 0.5) +
    geom_smooth(method = "lm", se = FALSE, color = "#A32424") +
    labs(title = paste("SalePrice vs", highest_cor_var, "(Highest Correlation:", round(highest_cor_val, 2), ")"),
         x = highest_cor_var, y = "Sale Price ($)") +
    theme_minimal()

ggplot(numeric_vars, aes(x = .data[[lowest_cor_var]], y = SalePrice)) +
    geom_point(color = "#00832D", alpha = 0.5) +
    geom_smooth(method = "lm", se = FALSE, color = "#A32424") +
    labs(title = paste("SalePrice vs", lowest_cor_var, "(Lowest Correlation:", round(lowest_cor_val, 2), ")"),
         x = lowest_cor_var, y = "Sale Price ($)") +
    theme_minimal()

ggplot(numeric_vars, aes(x = .data[[mid_cor_var]], y = SalePrice)) +
    geom_point(color = "purple", alpha = 0.5) +
    geom_smooth(method = "lm", se = FALSE, color = "#A32424") +
    labs(title = paste("SalePrice vs", mid_cor_var, "(Correlation ~0.5:", round(mid_cor_val, 2), ")"),
         x = mid_cor_var, y = "Sale Price ($)") +
    theme_minimal()




# -------------------------------- 7. FIT A REGRESSION MODEL -------------------------------------------

# Continuous variables with good correlations Based on correlation analysis
# 1. OverallQual (overall quality rating)
# 2. GrLivArea  (above grade living area)
# 3. GarageArea (garage area)
# 4. HouseAge (age of house - calculated from YrSold - YearBuilt)


# Model 1 : Fit the multiple linear regression model
model1 <- lm(SalePrice ~ OverallQual + GrLivArea + GarageArea + HouseAge, 
             data = ames_clean)

# Display model summary
summary(model1)

# Extract coefficients
coef_model1 <- coef(model1)
print(coef_model1)

cat("SalePrice =", round(coef_model1[1], 2), "+", 
    round(coef_model1[2], 2), "* OverallQual +",
    round(coef_model1[3], 2), "* GrLivArea +",
    round(coef_model1[4], 2), "* GarageArea +",
    round(coef_model1[5], 2), "* HouseAge\n")


# ------------ 8.Report Model in Equation Form and Interpret Coefficients -----------------

cat("\nCoefficient Interpretations:\n")

cat("- OverallQual:", round(coef_model1[2], 2), "\n")
cat("  For each 1-unit increase in overall quality rating (on a 1-10 scale), \n")
cat("  sale price increases by $", round(coef_model1[2], 2), ", holding other variables constant\n\n")

cat("- GrLivArea:", round(coef_model1[3], 2), "\n")
cat("  For each additional square foot of living area, sale price increases by $", 
    round(coef_model1[3], 2), ", holding other variables constant\n\n")

cat("- GarageArea:", round(coef_model1[4], 2), "\n")
cat("  For each additional square foot of garage area, sale price increases by $", 
    round(coef_model1[4], 2), ", holding other variables constant\n\n")

cat("- HouseAge:", round(coef_model1[5], 2), "\n")
if (coef_model1[5] < 0) {
    cat("  For each additional year of age, sale price DECREASES by $", 
        abs(round(coef_model1[5], 2)), ", holding other variables constant\n")
    cat("  This negative coefficient makes sense: older houses typically sell for less\n")
} else {
    cat("  For each additional year of age, sale price increases by $", 
        round(coef_model1[5], 2), ", holding other variables constant\n")
    cat("  Note: This unexpected positive coefficient may indicate confounding or that\n")
    cat("  older houses in this dataset have been renovated/maintained exceptionally well\n")
}




# -------------------------------- 9. PLOT REGRESSION MODEL ---------------------------------
# Standard regression diagnostics
par(mfrow = c(2, 2))
# Residuals vs Fitted plot
plot(model1,
     which = 1,
     pch = 21,            
     bg = "#33ccff",    
     col = "#004466",       
     cex = 0.8)

# Q-Q plot
plot(model1,
     which = 2,
     pch = 21,            
     bg = "#33ccff",    
     col = "#004466",  
     cex = 0.8)

# Scale-Location plot
plot(model1,
     which = 3,
     pch = 21,            
     bg = "#33ccff",    
     col = "#004466",  
     cex = 0.8)

# Residuals vs Leverage plot 
plot(model1,
     which = 5,
     pch = 21,            
     bg = "#33ccff",    
     col = "#004466",   
     cex = 0.8)

par(mfrow = c(1, 1))


# Extract fitted values and residuals
ames_clean$Predicted_SalePrice <- fitted(model1)
ames_clean$Residuals <- residuals(model1)


# Observed vs Predicted plot
ggplot(ames_clean, aes(x = Predicted_SalePrice, y = SalePrice)) +
    geom_point(
        shape = 21,
        fill = "#70dbdb",
        color = "#0a2929",
        alpha = 0.5
    ) +
    geom_abline(slope = 1, intercept = 0, color = "#002699", linetype = "dashed",linewidth=0.8) +
    labs(title = "Observed vs Predicted SalePrice",
         x = "Predicted SalePrice ($)",
         y = "Observed SalePrice ($)") +
    theme_minimal()





# ---------------------------- 10. CHECK FOR MULTICOLLINEARITY Check -------------------------
# Calculate VIF 
vif_values <- vif(model1)
print(vif_values)

# Check if multicollinearity exists
if (any(vif_values > 10)) {
    cat("SEVERE MULTICOLLINEARITY DETECTED!\n")
    cat("Variables with VIF > 10:", names(vif_values[vif_values > 10]), "\n")
} else if (any(vif_values > 5)) {
    cat("MODERATE MULTICOLLINEARITY DETECTED\n")
    cat("Variables with VIF > 5:", names(vif_values[vif_values > 5]), "\n")
} else {
    cat("No significant multicollinearity detected. All VIF values < 5.\n")
}



# ---------------------------- 11. CHECK FOR OUTLIERS -------------------------------------------

# Standardized residuals
std_residuals <- rstandard(model1)

# Identify outliers
outliers_index <- which(abs(std_residuals) > 3)

if (length(outliers_index) > 0) {
    cat("Outliers based on standardized residuals (|z| > 3):\n")
    print(ames_clean[outliers_index, c("SalePrice", "OverallQual", "GrLivArea", "GarageArea", "YearBuilt")])
}

# Cook's Distance: influential points
cooks_d <- cooks.distance(model1)
influential <- which(cooks_d > 4 / nrow(ames_clean))

cat("\nNumber of influential points (Cook's D > 4/n):", length(influential), "\n")
if (length(influential) > 0) print(head(influential, 10))

# Leverage: high leverage points
lev <- hatvalues(model1)
high_lev <- which(lev > 2 * mean(lev))

cat("\nNumber of high leverage points (h > 2*mean(h)): ", length(high_lev), "\n")

# Combined quick plot of Cook's distance
plot(cooks_d, type = "h", col = "steelblue",
     main = "Cook's Distance",
     xlab = "Observation Index",
     ylab = "Cook's Distance")
abline(h = 4 / nrow(ames_clean), col = "red", lty = 2)




# ------------------ 12.CORRECT ISSUES AND IMPROVE MODEL  --------------------------------

# Remove extreme outliers 
# Based on the documentation, remove houses > 4000 sq ft
ames_clean2 <- ames_clean[ames_clean$GrLivArea <= 4000, ]
cat("Removed", nrow(ames_clean) - nrow(ames_clean2), "observations with GrLivArea > 4000 sq ft\n")

# Remove observations with high Cook's distance
if (length(influential) > 0 && length(influential) < 100) {
    ames_clean2 <- ames_clean2[-influential[influential %in% rownames(ames_clean2)], ]
    cat("Removed", length(influential), "influential observations\n")
}

# Model 2: Refit the model
model2 <- lm(SalePrice ~ OverallQual + GrLivArea + GarageArea + HouseAge, 
             data = ames_clean2)

#Model 2 Summary (After Corrections)
summary(model2)

# Compare models
model_comparison <- data.frame(
    Metric = c(
        "R-squared",
        "Adjusted R-squared",
        "Residual Std Error",
        "Observations"
    ),
    Model_1 = c(
        summary(model1)$r.squared,
        summary(model1)$adj.r.squared,
        summary(model1)$sigma,
        nobs(model1)
    ),
    Model_2 = c(
        summary(model2)$r.squared,
        summary(model2)$adj.r.squared,
        summary(model2)$sigma,
        nobs(model2)
    )
)

model_comparison$Change <- c(
    paste0(
        round(
            (model_comparison$Model_2[1:3] - model_comparison$Model_1[1:3]) /
                model_comparison$Model_1[1:3] * 100,
            2
        ),
        "%"
    ),
    model_comparison$Model_2[4] - model_comparison$Model_1[4]
)


print(model_comparison)

if (summary(model2)$adj.r.squared > summary(model1)$adj.r.squared) {
    cat("Model 2 shows an improvement, with a higher adjusted R-squared than Model 1.\n")
} else {
    cat("Model 2 does not improve upon Model 1; adjusted R-squared is similar or lower.\n")
}


# Diagnostic plots for improved model2
par(mfrow = c(2, 2))
# Residuals vs Fitted plot
plot(model2,
     which = 1,
     pch = 21,            
     bg = "lightgreen",    
     col = "darkgreen",       
     cex = 0.8,
     lwd = 2)

# Q-Q plot
plot(model2,
     which = 2,
     pch = 21,            
     bg = "lightgreen",    
     col = "darkgreen",  
     cex = 0.8)

# Scale-Location plot
plot(model2,
     which = 3,
     pch = 21,            
     bg = "lightgreen",    
     col = "darkgreen",  
     cex = 0.8,
     lwd = 2)

# Residuals vs Leverage plot 
plot(model2,
     which = 5,
     pch = 21,            
     bg = "lightgreen",    
     col = "darkgreen",  
     cex = 0.8,
     lwd = 2)

par(mfrow = c(1, 1))




# ---------------------------- 13. SUBSETS REGRESSION ----------------------------------------------

# Prepare data for all subsets regression
# Select potential predictors 
predictors <- c("OverallQual", "GrLivArea", "GarageArea", "HouseAge",
                "TotalBsmtSF", "X1stFlrSF", "FullBath", "TotRmsAbvGrd",
                "Fireplaces", "LotArea")

# Ensure all predictors exist in dataset
predictors <- predictors[predictors %in% names(ames_clean2)]
print(predictors)

# Create formula
formula_str <- paste("SalePrice ~", paste(predictors, collapse = " + "))

# Run all subsets regression
regfit_full <- regsubsets(as.formula(formula_str), 
                          data = ames_clean2,
                          nvmax = length(predictors))

# Summary
reg_summary <- summary(regfit_full)

# Find best model based on different criteria
best_cp <- which.min(reg_summary$cp)
best_bic <- which.min(reg_summary$bic)
best_adjr2 <- which.max(reg_summary$adjr2)

cat("\nBest model by Cp:", best_cp, "variables\n")
cat("Best model by BIC:", best_bic, "variables\n")
cat("Best model by Adjusted R²:", best_adjr2, "variables\n")

# Plot selection criteria
par(mfrow = c(2, 2))
plot(reg_summary$cp, xlab = "Number of Variables", ylab = "Cp", type = "b")
points(best_cp, reg_summary$cp[best_cp], col = "red", cex = 2, pch = 20)

plot(reg_summary$bic, xlab = "Number of Variables", ylab = "BIC", type = "b")
points(best_bic, reg_summary$bic[best_bic], col = "red", cex = 2, pch = 20)

plot(reg_summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted R²", type = "b")
points(best_adjr2, reg_summary$adjr2[best_adjr2], col = "red", cex = 2, pch = 20)

par(mfrow = c(1, 1))

# Get coefficients of best model (using BIC as criterion)
best_model_vars <- names(coef(regfit_full, best_bic))[-1]  
cat("\nBest Model Variables (by BIC):\n")
print(best_model_vars)

# Model 3 : Fit the preferred model
formula_best <- paste("SalePrice ~", paste(best_model_vars, collapse = " + "))
model_preferred <- lm(as.formula(formula_best), data = ames_clean2)

cat("\nPreferred Model Summary:\n")
summary(model_preferred)

# Report equation
coef_preferred <- coef(model_preferred)
cat("\n=== PREFERRED MODEL EQUATION ===\n")
equation_parts <- paste(names(coef_preferred)[-1], "*", round(coef_preferred[-1], 2))
cat("SalePrice =", round(coef_preferred[1], 2), "+", paste(equation_parts, collapse = " + "), "\n")



# ---------------------------- 14. COMPARE THE MODELS ----------------------------------------------

# Model Comparison
cat("\nModel 2 (Improved Model 1):\n","Variables:", paste(names(coef(model2))[-1], collapse = ", "), "\n","R-squared:", summary(model2)$r.squared, "\n","Adj R-squared:", summary(model2)$adj.r.squared, "\n","AIC:", AIC(model2), "\n","BIC:", BIC(model2), "\n","RSE:", summary(model2)$sigma, "\n\n")
cat("Model 3 Preferred Model (All Subsets):\n","Variables:", paste(names(coef(model_preferred))[-1], collapse = ", "), "\n","R-squared:", summary(model_preferred)$r.squared, "\n","Adj R-squared:", summary(model_preferred)$adj.r.squared, "\n","AIC:", AIC(model_preferred), "\n","BIC:", BIC(model_preferred), "\n","RSE:", summary(model_preferred)$sigma, "\n\n")

# Detailed Model Comparison Table
final_comparison <- data.frame(
    Metric = c("Variables", "R-squared", "Adj R-squared", "AIC", "BIC", "RSE"),
    Model2 = c(
        paste(names(coef(model2))[-1], collapse = ", "),
        round(summary(model2)$r.squared, 4),
        round(summary(model2)$adj.r.squared, 4),
        round(AIC(model2), 2),
        round(BIC(model2), 2),
        round(summary(model2)$sigma, 2)
    ),
    Preferred_Model = c(
        paste(names(coef(model_preferred))[-1], collapse = ", "),
        round(summary(model_preferred)$r.squared, 4),
        round(summary(model_preferred)$adj.r.squared, 4),
        round(AIC(model_preferred), 2),
        round(BIC(model_preferred), 2),
        round(summary(model_preferred)$sigma, 2)
    )
)
print(final_comparison)

# Model Preference Justification
if (summary(model_preferred)$adj.r.squared > summary(model2)$adj.r.squared) {
    cat("PREFERRED: All Subsets Model\n","Reason: Higher Adjusted R-squared and better model selection criteria\n")
} else {
    cat("PREFERRED: Model 2\n","Reason: Similar performance with fewer variables (parsimony principle)\n")
}


# ============================================================================
# FINAL DIAGNOSTIC PLOTS FOR PREFERRED MODEL
# ============================================================================

par(mfrow = c(2, 2))
# Residuals vs Fitted plot
plot(model_preferred,
     which = 1,
     pch = 21,            
     bg = "#b3cccc",    
     col = "#476b6b",       
     cex = 0.8,
     lwd = 2)

# Q-Q plot
plot(model_preferred,
     which = 2,
     pch = 21,            
     bg = "#b3cccc",    
     col = "#476b6b",   
     cex = 0.8)

# Scale-Location plot
plot(model_preferred,
     which = 3,
     pch = 21,            
     bg = "#b3cccc",    
     col = "#476b6b",  
     cex = 0.8,
     lwd = 2)

# Residuals vs Leverage plot 
plot(model_preferred,
     which = 5,
     pch = 21,            
     bg = "#b3cccc",    
     col = "#476b6b",   
     cex = 0.8,
     lwd = 2)

par(mfrow = c(1, 1))


# --------------------------- END OF SCRIPT ----------------------------------------------




