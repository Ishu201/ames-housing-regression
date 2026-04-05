# Ames Housing Price Prediction — Regression Diagnostics and Model Evaluation

**ALY 6015 | Module 01 | Northeastern University**

An end-to-end multiple linear regression analysis on the Ames Housing dataset to predict residential property sale prices. The project covers exploratory data analysis, diagnostic evaluation, outlier handling, and automated model selection using R.

---

## Overview

The [Ames Housing dataset](https://www.kaggle.com/datasets/prevek18/ames-housing-dataset) contains 2,930 residential property sales in Ames, Iowa from 2006 to 2010 across 82 variables. The goal of this analysis is to develop, evaluate, and refine multiple linear regression models that accurately predict housing sale prices based on property characteristics.

---

## Project Structure
ames-housing-regression/
│
├── script.R                          # Full R analysis script
├── AmesHousing.csv                   # Dataset
├── AmesHousingDataDocumentation.txt  # Dataset documentation
├── Project_report.pdf                # Full written report
└── plots/                            # Generated visualizations

---

## Methodology

### 1. Exploratory Data Analysis
- Examined distributions of key numeric variables including `SalePrice`, `GrLivArea`, `LotArea`, `TotalBsmtSF`, `GarageArea`, `YearBuilt`, `OverallQual`, and `OverallCond`
- Identified right-skewed distributions and outliers through histograms, boxplots, Q-Q plots, and density plots
- Median sale price: ~$160,000 | Mean sale price: ~$180,921

### 2. Correlation Analysis
- Built a correlation matrix across all numeric variables
- Strongest correlations with `SalePrice`:
  - `OverallQual` → r = 0.80
  - `GrLivArea` → r = 0.70
  - `GarageArea` → r = 0.64
  - `TotalBsmtSF` → r = 0.63
  - `YearBuilt` → r = 0.56
- Weakest: `BsmtFinSF2` → r = 0.01 (excluded from models)

### 3. Initial Regression Model (Model 1)
Built with 4 continuous predictors: `OverallQual`, `GrLivArea`, `GarageArea`, `HouseAge`
SalePrice = -62,480.58 + 23,743.79 × OverallQual + 55.81 × GrLivArea + 60.77 × GarageArea − 380.84 × HouseAge

| Metric | Value |
|--------|-------|
| R-squared | 0.7703 |
| Adjusted R-squared | 0.7699 |
| Residual Std Error | $38,316 |

### 4. Diagnostic Evaluation
- **Residuals vs Fitted** — mild heteroscedasticity at higher fitted values
- **Q-Q Plot** — approximate normality in center; heavy right tail
- **Scale-Location** — slight upward trend confirming mild heteroscedasticity
- **Residuals vs Leverage** — observations 1499, 2181, 2182 flagged as highly influential (Cook's distance > 0.5)
- **VIF Analysis** — all predictors scored below 5, confirming no multicollinearity

### 5. Improved Model (Model 2)
- Removed properties with `GrLivArea > 4,000 sq ft` per dataset documentation
- Re-fitted the same 4-predictor model on the cleaned dataset

| Metric | Model 1 | Model 2 | Change |
|--------|---------|---------|--------|
| R-squared | 0.7703 | 0.7928 | +2.93% |
| Adjusted R-squared | 0.7699 | 0.7926 | +2.94% |
| Residual Std Error | $38,316 | $35,777 | −6.63% |
| Observations | 2,930 | 2,925 | −5 |

### 6. All Subsets Regression
Used the `leaps` package to evaluate all possible predictor combinations across 10 candidate variables. Model selected based on BIC criterion.

**Preferred model predictors:** `OverallQual`, `GrLivArea`, `GarageArea`, `HouseAge`, `TotalBsmtSF`, `FullBath`, `Fireplaces`, `LotArea`

| Metric | Model 2 | Preferred Model |
|--------|---------|-----------------|
| R-squared | 0.7928 | 0.8369 |
| Adjusted R-squared | 0.7926 | 0.8364 |
| AIC | 69,645.51 | 68,957.86 |
| BIC | 69,681.40 | 69,029.63 |
| RSE | $35,777 | $31,777 |

---

## Key Findings

- **Overall quality** is the strongest single predictor of sale price — each 1-unit increase in rating corresponds to ~$23,744 in added value
- **Living area** adds ~$55.81 per additional square foot
- **House age** has a negative effect — each additional year reduces price by ~$380.84
- Removing 5 extreme outliers (properties > 4,000 sq ft) improved prediction accuracy by 6.63%
- The preferred 8-predictor model achieves **Adjusted R² = 0.84**, explaining 84% of variance in sale prices

---

## Tools and Packages

| Tool | Purpose |
|------|---------|
| R | Core analysis language |
| `tidyverse` | Data manipulation and visualization |
| `car` | VIF multicollinearity assessment |
| `corrplot` | Correlation matrix visualization |
| `leaps` | All subsets regression |
| `ggplot2` | Scatter plots and diagnostics |

---

## How to Run

1. Clone this repository
2. Open `week1.Rproj` in RStudio
3. Place `AmesHousing.csv` in the project root directory
4. Run `script.R` from top to bottom
```r
# Install required packages if needed
install.packages(c("tidyverse", "car", "corrplot", "ggcorrplot", "leaps"))
```

---

## References

- De Cock, D. (2011). Ames, Iowa: Alternative to the Boston housing data as an end of semester regression project. *Journal of Statistics Education, 19*(3). https://doi.org/10.1080/10691898.2011.11889627
- James, G., Witten, D., Hastie, T., & Tibshirani, R. (2021). *An Introduction to Statistical Learning with Applications in R* (2nd ed.). Springer.
- Fox, J., & Weisberg, S. (2019). *An R Companion to Applied Regression* (3rd ed.). Sage Publications.

---

*Isuri Nawodya Herath S W M M G | NUID: 002021794 | Northeastern University*
