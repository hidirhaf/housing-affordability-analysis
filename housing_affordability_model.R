# ============================================================
# Housing Affordability in England — Panel Data Regression
# MATH3092 Final Year Project
# ============================================================


# ============================================================
# Section 0 — Setup
# ============================================================

# install.packages(c("readxl","dplyr","tidyr","readr","stringr",
#                    "glmnet","randomForest","car","plm","lmtest",
#                    "sandwich","fixest"))

library(readxl)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(glmnet)
library(randomForest)
library(car)
library(plm)
library(lmtest)
library(sandwich)
library(fixest)
library(ggplot2)

# --- File paths ---
AFF_FILE          <- "aff1ratioofhousepricetoworkplacebasedearnings.xlsx"
AFF_SHEET         <- "1c"
AFF_SKIP          <- 1
UNEMP_FILE        <- "modelled-unemployment-line-chart-data.csv"
UNEMP_SKIP        <- 7
NetHousing_file   <- "additions-to-the-housing-stock-line-chart-data (1).csv"
MortgageRate_File <- "datadownload.xlsx"
Population_File   <- "population-count-line-chart-data (1).csv"
Inflation_file    <- "series-140326.csv"
GVA_file          <- "gross-value-added-per-hour-worked-line-chart-data (1).csv"

# --- Global parameters ---
YEAR_START    <- 1997
YEAR_END      <- 2024
EST_START     <- 2004
EXCLUDE_AREAS <- c("England", "Wales", "England and Wales")
SPLIT_YEAR    <- 2018  # temporal train/test boundary


# ============================================================
# Section 1 — Affordability ratio: import + reshape wide -> long
# ============================================================

aff_raw <- read_excel(AFF_FILE, sheet = AFF_SHEET, skip = AFF_SKIP)

year_cols <- as.character(YEAR_START:YEAR_END)
stopifnot(length(setdiff(year_cols, names(aff_raw))) == 0)

aff_long <- aff_raw %>%
  pivot_longer(
    cols      = all_of(year_cols),
    names_to  = "Year",
    values_to = "AffordabilityRatio"
  ) %>%
  mutate(
    Year = as.integer(Year),
    Name = as.factor(Name)
  )

# ============================================================
# Section 2 — Unemployment rate: import + clean
# ============================================================

unemp_raw <- read.csv(UNEMP_FILE, skip = UNEMP_SKIP)[, -1]

value_col <- grep("^Value", names(unemp_raw), value = TRUE)[1]
stopifnot(!is.na(value_col))

unemp_clean <- unemp_raw %>%
  transmute(
    Name             = Area.name,
    Year             = Time.period,
    UnemploymentRate = .data[[value_col]]
  )


# ============================================================
# Section 3 — Net housing additions: import + clean
# ============================================================

nethousing_raw <- read.csv(NetHousing_file, skip = 7)[, -1]

housing_val_col <- grep("^Value", names(nethousing_raw), value = TRUE)[1]
stopifnot(!is.na(housing_val_col))

nethousing_clean <- nethousing_raw %>%
  transmute(
    Name       = Area.name,
    Year       = as.integer(str_extract(Time.period, "\\d{4}")),
    NetHousing = as.numeric(.data[[housing_val_col]])
  ) %>%
  filter(!is.na(Year))


# ============================================================
# Section 4 — Mortgage rate: import + clean (monthly -> annual)
# ============================================================

mortgagerate_raw <- read_excel(
  MortgageRate_File,
  skip      = 4,
  col_types = c("text", "numeric", "numeric", "numeric")
)

# Parse dates — handles both dd/mm/yyyy strings and Excel serial numbers
x           <- trimws(mortgagerate_raw$date)
date_parsed <- as.Date(x, format = "%d/%m/%Y")
is_serial   <- grepl("^\\d+$", x)
date_parsed[is_serial] <- as.Date(as.numeric(x[is_serial]), origin = "1899-12-30")

mortgagerate_raw$date <- date_parsed
mortgagerate_raw      <- mortgagerate_raw[!is.na(mortgagerate_raw$date), ]
mortgagerate_raw$Year <- as.integer(format(mortgagerate_raw$date, "%Y"))

mortgage_yearly <- aggregate(
  `Mortgages of which fixed rate` ~ Year,
  data  = mortgagerate_raw,
  FUN   = mean,
  na.rm = TRUE
) %>%
  rename(MortgageRate = `Mortgages of which fixed rate`)


# ============================================================
# Section 5 — Population: import + compute annual growth rate
# ============================================================

population_raw <- read.csv(Population_File, skip = 7)[, -1]
population_raw <- population_raw[order(population_raw$Area.name,
                                       population_raw$Time.period), ]

population_clean <- population_raw %>%
  group_by(Area.name) %>%
  mutate(
    PopGrowth = (Value..people. - dplyr::lag(Value..people.)) /
      dplyr::lag(Value..people.) * 100
  ) %>%
  ungroup() %>%
  transmute(
    Name      = Area.name,
    Year      = Time.period,
    PopGrowth = PopGrowth
  )

# ============================================================
# Section 6 — CPIH inflation: import + clean
# ============================================================

inflation_clean <- read_csv(Inflation_file, skip = 8, col_names = FALSE) %>%
  rename(Year = X1, Inflation = X2) %>%
  slice(1:37) %>%
  mutate(Year = as.numeric(Year))


# ============================================================
# Section 7 — GVA per hour: import + clean
# ============================================================

GVA_clean <- read.csv(GVA_file, skip = 7)[, -1] %>%
  transmute(
    Name = Area.name,
    Year = Time.period,
    GVA  = Value....
  )




# ============================================================
# Section 8 — Merge all datasets + build estimation sample
# ============================================================

df_model <- aff_long %>%
  filter(!Name %in% EXCLUDE_AREAS, Year >= EST_START) %>%
  left_join(unemp_clean,      by = c("Name", "Year")) %>%
  left_join(nethousing_clean, by = c("Name", "Year")) %>%
  left_join(mortgage_yearly,  by = "Year") %>%
  left_join(population_clean, by = c("Name", "Year")) %>%
  left_join(inflation_clean,  by = "Year") %>%
  left_join(GVA_clean,        by = c("Name", "Year"))
 

# Drop rows with any missing predictor — yields balanced panel
df_est <- df_model %>%
  filter(
    !is.na(UnemploymentRate),
    !is.na(NetHousing),
    !is.na(MortgageRate),
    !is.na(PopGrowth),
    !is.na(Inflation),
    !is.na(GVA)
  )

cat("Estimation sample:", nrow(df_est), "obs |",
    "Years:", range(df_est$Year)[1], "to", range(df_est$Year)[2],
    "| Regions:", length(unique(df_est$Name)), "\n")

# Add event dummies BEFORE declaring pdata.frame (required for plm access)
df_est$crisis_2008 <- ifelse(df_est$Year >= 2008 & df_est$Year <= 2012, 1, 0)
df_est$covid       <- ifelse(df_est$Year >= 2020, 1, 0)

# Declare panel structure
df_panel <- pdata.frame(df_est, index = c("Name", "Year"))

# ============================================================
# Section  — Descriptive Statistics
# ============================================================

desc_vars <- df_est %>% 
  filter(Year >= 2004 & Year <= 2022)%>%
  select(AffordabilityRatio, UnemploymentRate, NetHousing, 
       MortgageRate, PopGrowth, Inflation, GVA) %>%
  summarise(across(everything(), list(
    Mean   = ~mean(., na.rm = TRUE),
    SD     = ~sd(., na.rm = TRUE),
    Min    = ~min(., na.rm = TRUE),
    Median = ~median(., na.rm = TRUE),
    Max    = ~max(., na.rm = TRUE)
  )))

str(desc_vars)
# Reshape into a clean table
desc_table <- data.frame(
  Variable = c("AffordabilityRatio", "UnemploymentRate", "NetHousing",
               "MortgageRate", "PopGrowth", "Inflation", "GVA"),
  Mean   = sapply(select(df_est, AffordabilityRatio, UnemploymentRate, NetHousing, MortgageRate, PopGrowth, Inflation, GVA), mean,   na.rm = TRUE),
  SD     = sapply(select(df_est, AffordabilityRatio, UnemploymentRate, NetHousing, MortgageRate, PopGrowth, Inflation, GVA), sd,     na.rm = TRUE),
  Min    = sapply(select(df_est, AffordabilityRatio, UnemploymentRate, NetHousing, MortgageRate, PopGrowth, Inflation, GVA), min,    na.rm = TRUE),
  Median = sapply(select(df_est, AffordabilityRatio, UnemploymentRate, NetHousing, MortgageRate, PopGrowth, Inflation, GVA), median, na.rm = TRUE),
  Max    = sapply(select(df_est, AffordabilityRatio, UnemploymentRate, NetHousing, MortgageRate, PopGrowth, Inflation, GVA), max,    na.rm = TRUE)
)

print(desc_table, digits = 2)

# ============================================================
# Section  — Affordability ratio trend
# ============================================================

df_est %>%
  filter(Year >= 2004 & Year <= 2022) %>%
  ggplot(aes(x = Year, y = AffordabilityRatio, color = Name)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.5) +
  labs(
    title  = "Affordability Ratio by Region, 2004–2022",
    x      = "Year",
    y      = "Affordability Ratio",
    color  = "Region"
  ) +
  scale_x_continuous(breaks = seq(2004, 2022, by = 2)) +
  theme_minimal() +
  theme(legend.position = "bottom")
ggsave("region_trend.png", width = 10, height = 5, dpi = 300)


# ============================================================
# Section 9 — Baseline models (Equations 1–3)
# ============================================================

# Eq. 1 — Time trend only
m_time <- lm(AffordabilityRatio ~ Year, data = df_est)
summary(m_time)

# Eq. 2 — Add unemployment rate
m_unemp <- lm(AffordabilityRatio ~ Year + UnemploymentRate, data = df_est)
summary(m_unemp)

# Eq. 3 — Add region fixed effects (LSDV)
m_fe_lsdv <- lm(AffordabilityRatio ~ Year + UnemploymentRate + factor(Name),
                data = df_est)
summary(m_fe_lsdv)


# ============================================================
# Section 10 — Full FE model via plm()
#
# Notes:
#   - Year dropped: GVIF = 58.7, near-perfectly collinear with GVA
#   - crisis_2008 and covid dropped: absorbed by time variation in GVA
#   - PopGrowth retained: estimable after within-demeaning (not in lm())
# ============================================================

fe_full <- plm(
  log(AffordabilityRatio) ~ UnemploymentRate + NetHousing +
    MortgageRate + PopGrowth + Inflation + GVA,
  data   = df_panel,
  model  = "within",
  effect = "individual"
)

summary(fe_full)


# ============================================================
# Section 11 — Panel diagnostic tests
# ============================================================

# Pooled OLS — needed as baseline for F-test and Breusch-Pagan
pool_model <- plm(
  log(AffordabilityRatio) ~ UnemploymentRate + NetHousing +
    MortgageRate + PopGrowth + Inflation + GVA,
  data  = df_panel,
  model = "pooling"
)

# Random effects — needed for Hausman test
re_model <- plm(
  log(AffordabilityRatio) ~ UnemploymentRate + NetHousing +
    MortgageRate + PopGrowth + Inflation + GVA,
  data   = df_panel,
  model  = "random"
)

# 1. F-test: FE vs Pooled OLS
#    H0: no individual effects → significant p favours FE
cat("\n--- F-test: FE vs Pooled OLS ---\n")
print(pFtest(fe_full, pool_model))

# 2. Hausman test: FE vs RE
#    H0: RE consistent → significant p favours FE
#    Note: low power with N=9; FE justified on theoretical grounds
#    regardless (unobserved regional characteristics plausibly
#    correlated with GVA and unemployment)
cat("\n--- Hausman Test: FE vs RE ---\n")
print(phtest(fe_full, re_model))

# 3. Robust Hausman test (robust to heteroskedasticity + serial corr.)
cat("\n--- Robust Hausman Test ---\n")
print(phtest(fe_full, re_model,
             vcov = function(x) vcovHC(x, type = "HC1", cluster = "group")))

# 4. Breusch-Pagan LM: RE vs Pooled OLS
#    H0: no panel effects → significant p confirms panel structure needed
cat("\n--- Breusch-Pagan LM Test ---\n")
print(plmtest(pool_model, type = "bp"))

# 5. Breusch-Godfrey: serial correlation
#    H0: no serial correlation → significant p → use robust SEs
cat("\n--- Serial Correlation (Breusch-Godfrey) ---\n")
print(pbgtest(fe_full))

# 6. Pesaran CD: cross-sectional dependence
#    H0: no cross-sectional dependence → significant p → use DK SEs
cat("\n--- Cross-Sectional Dependence (Pesaran CD) ---\n")
print(pcdtest(fe_full, test = "cd"))


# ============================================================
# Section 12 — Multicollinearity diagnostics (VIF)
#
# Note: VIF requires lm() — car::vif() does not accept plm objects.
# PopGrowth excluded from all lm() VIF checks: perfectly collinear
# with factor(Name) + Year in the LSDV design matrix (rank deficient).
# It is estimable in plm() after within-demeaning removes the source
# of collinearity.
# ============================================================

# Check 1: full model including Year 
vif_model1 <- lm(
  log(AffordabilityRatio) ~ Year + UnemploymentRate + NetHousing +
    MortgageRate + PopGrowth + Inflation + GVA + factor(Name),
  data = df_est
)
cat("\n--- VIF: full model (with Year) ---\n")
print(vif(vif_model1))

# Check 2: drop Year 
vif_model2 <- lm(
  log(AffordabilityRatio) ~ UnemploymentRate + NetHousing +
    MortgageRate + PopGrowth + Inflation + GVA + factor(Name),
  data = df_est
)
cat("\n--- VIF: after dropping Year ---\n")
print(vif(vif_model2))

# Check 3: within-demeaned variables — shows VIF as seen by FE estimator
# This is the most relevant check: collinearity in demeaned space
# directly affects precision of plm() estimates
df_demeaned <- df_est %>%
  group_by(Name) %>%
  mutate(across(
    c(UnemploymentRate, NetHousing, MortgageRate, PopGrowth, Inflation, GVA),
    ~ . - mean(.)
  )) %>%
  ungroup()
vif_model3 <- lm(
  log(AffordabilityRatio) ~ UnemploymentRate + NetHousing +
    MortgageRate + PopGrowth + GVA + Inflation,
  data = df_demeaned
)
cat("\n--- VIF: within-demeaned (as seen by FE estimator) ---\n")
print(vif(vif_model3))


cor(df_demeaned$MortgageRate, df_demeaned$GVA)


# ============================================================
# Section 13 — Backward elimination
#
# Strategy: drop predictors one at a time in order of decreasing
# p-value; retain if adjusted R² materially falls or Wald test
# rejects the restriction.
# ============================================================

# Step 1: drop UnemploymentRate (largest p-value in fe_full)
fe_step1 <- plm(
  log(AffordabilityRatio) ~ NetHousing + MortgageRate +
    PopGrowth + Inflation + GVA,
  data   = df_panel,
  model  = "within",
  effect = "individual"
)
summary(fe_step1)

# Step 2: drop Inflation if still insignificant
fe_step2 <- plm(
  log(AffordabilityRatio) ~ NetHousing + MortgageRate + PopGrowth + GVA ,
  data   = df_panel,
  model  = "within",
  effect = "individual"
)
summary(fe_step2)

# Adjusted R² at each step
cat("\n--- Adj. R² across backward elimination steps ---\n")
cat("Full model: ", round(summary(fe_full)$r.squared["adjrsq"],  4), "\n")
cat("Step 1:     ", round(summary(fe_step1)$r.squared["adjrsq"], 4), "\n")
cat("Step 2:     ", round(summary(fe_step2)$r.squared["adjrsq"], 4), "\n")

# Formal Wald test: full model vs step 2 (restricted)
# waldtest() preferred over lrtest() for plm objects
cat("\n--- Wald test: full vs step 2 ---\n")
print(waldtest(fe_full, fe_step2))


# ============================================================
# Section 14 — Final FE model via fixest
#
# fixest::feols() preferred over plm() for final reporting:
# - Driscoll-Kraay SEs built in (handles serial corr. + cross-sect. dep.)
# - etable() produces clean publication-quality regression tables
# ============================================================

# Set panel structure once — applies to all subsequent feols() calls
setFixest_estimation(panel.id = ~ Name + Year)

fe_final <- feols(
  log(AffordabilityRatio) ~ MortgageRate + PopGrowth + NetHousing + GVA | Name,
  data = df_est,
  vcov = "DK"
)
summary(fe_final)


fe_crisis <- feols(
  log(AffordabilityRatio) ~ MortgageRate + PopGrowth + 
    NetHousing + GVA + crisis_2008 + covid | Name,
  data = df_est,
  vcov = "DK"
)
summary(fe_crisis)

# --- Residual diagnostics ---
# Fit equivalent lm() for diagnostic plots (feols has no plot method)
fe_lm_equiv <- lm(
  log(AffordabilityRatio) ~ MortgageRate + PopGrowth +
    NetHousing + GVA + factor(Name),
  data = df_est
)

# Save 2x2 diagnostic plot for report
png("residual_diagnostics.png", width = 2400, height = 2000, res = 300)
par(mfrow = c(2, 2))
plot(fe_lm_equiv)
par(mfrow = c(1, 1))
dev.off()


# Cook's distance — identify influential observations
cooksd <- cooks.distance(fe_lm_equiv)
threshold <- 4 / nrow(df_est)

plot(cooksd,
     main = "Cook's Distance — Final FE Model",
     ylab = "Cook's Distance",
     xlab = "Observation index")
abline(h = threshold, col = "red", lty = 2)
text(
  which(cooksd > threshold),
  cooksd[cooksd > threshold],
  labels = df_est$Name[cooksd > threshold],
  pos    = 4,
  cex    = 0.7
)

# --- Regression table: all specifications side by side ---
etable(
  feols(log(AffordabilityRatio) ~ Year | Name,
        data = df_est, vcov = "DK"),
  feols(log(AffordabilityRatio) ~ UnemploymentRate + NetHousing +
          MortgageRate + PopGrowth + Inflation + GVA | Name,  
        data = df_est, vcov = "DK"),
  fe_final,
  headers = c("(1) Time trend", "(2) Full FE", "(3) Final FE")
)

fe_final$collin.var
# ============================================================
# Section 15 — Time-based train/test split
#
# Temporal split used throughout to avoid data leakage.
# Random splitting is invalid for panel time-series: future
# observations would appear in the training set.
# Train: 2004-2018 | Test: 2019-2022
# ============================================================

train <- df_est %>% filter(Year <= SPLIT_YEAR)
test  <- df_est %>% filter(Year >  SPLIT_YEAR)

cat("Train:", nrow(train), "obs | Years:", range(train$Year), "\n")
cat("Test: ", nrow(test),  "obs | Years:", range(test$Year),  "\n")

# OLS baseline 
model_train <- lm(
  log(AffordabilityRatio) ~ factor(Name) +
    NetHousing + MortgageRate + PopGrowth + GVA,
  data = train
)

# Confirm no rank deficiency before proceeding
X_check <- model.matrix(
  log(AffordabilityRatio) ~ factor(Name) +
    NetHousing + MortgageRate + PopGrowth + GVA,
  data = train
)
cat("Rank check — rank:", qr(X_check)$rank,
    "| columns:", ncol(X_check), "(should match)\n")

# Define response vectors — used across all learning methods below
y_train <- log(train$AffordabilityRatio)
y_test  <- log(test$AffordabilityRatio)

# OLS predictions and RMSE
train_pred <- predict(model_train, newdata = train)
test_pred  <- predict(model_train, newdata = test)
train_rmse <- sqrt(mean((y_train - train_pred)^2))
test_rmse  <- sqrt(mean((y_test  - test_pred)^2))

cat("OLS Train RMSE:", round(train_rmse, 4), "\n")
cat("OLS Test RMSE: ", round(test_rmse,  4), "\n")
print(results, row.names = FALSE)

# ============================================================
# Section 16 — Design matrices for shrinkage methods
#
# Year used as continuous (not factor) to avoid unseen year
# levels in the test set causing column dimension mismatch.
# ============================================================

x_train <- model.matrix(
  log(AffordabilityRatio) ~ Year + UnemploymentRate + factor(Name) +
    NetHousing + MortgageRate + PopGrowth + Inflation + GVA,
  data = train
)[, -1]

x_test <- model.matrix(
  log(AffordabilityRatio) ~ Year + UnemploymentRate + factor(Name) +
    NetHousing + MortgageRate + PopGrowth + Inflation + GVA,
  data = test
)[, -1]

cat("x_train cols:", ncol(x_train),
    "| x_test cols:", ncol(x_test), "(must match)\n")

dim(x_train)
dim(x_test)
colnames(x_train)
colnames(x_test)
all(colnames(x_train) == colnames(x_test))
# ============================================================
# Section 17 — Ridge Regression (alpha = 0)
#
# Adds L2 penalty: beta_ridge = argmin ||y - Xb||^2 + lambda||b||^2
# Closed-form solution: (X'X + lambdaI)^{-1} X'y
# All coefficients shrink toward zero but never exactly zero.
# ============================================================

set.seed(123)
ridge_cv      <- cv.glmnet(x_train, y_train, alpha = 0)
ridge_cv_rmse <- sqrt(min(ridge_cv$cvm))

ridge_pred_min <- predict(ridge_cv, s = "lambda.min", newx = x_test)
ridge_pred_1se <- predict(ridge_cv, s = "lambda.1se", newx = x_test)
ridge_rmse_min <- sqrt(mean((y_test - ridge_pred_min)^2))
ridge_rmse_1se <- sqrt(mean((y_test - ridge_pred_1se)^2))

cat("\nRidge lambda.min:", round(ridge_cv$lambda.min, 6),
    "| lambda.1se:", round(ridge_cv$lambda.1se, 6), "\n")
cat("Ridge CV RMSE:          ", round(ridge_cv_rmse,  4), "\n")
cat("Ridge Test RMSE (min):  ", round(ridge_rmse_min, 4), "\n")
cat("Ridge Test RMSE (1se):  ", round(ridge_rmse_1se, 4), "\n")

# Ridge plots
png("ridge_cv.png", width = 3200, height = 2000, res = 300)
par(mfrow = c(1, 2))
plot(ridge_cv, main = "Ridge: CV Error")
plot(ridge_cv$glmnet.fit, xvar = "lambda", main = "Ridge: Coefficient Paths")
abline(v = log(ridge_cv$lambda.min), lty = 2, col = "red")
abline(v = log(ridge_cv$lambda.1se), lty = 2, col = "blue")
legend("topright", legend = c("lambda.min","lambda.1se"),
       lty = 2, col = c("red","blue"), cex = 0.8)
par(mfrow = c(1, 1))
dev.off()

# ============================================================
# Section 18 — LASSO (alpha = 1)
#
# Adds L1 penalty: beta_lasso = argmin ||y - Xb||^2 + lambda||b||_1
# L1 geometry (cross-polytope corners on axes) → exact zeros → variable
# selection. This is the key distinction from Ridge (L2 sphere).
# ============================================================

set.seed(123)
lasso_cv      <- cv.glmnet(x_train, y_train, alpha = 1)
lasso_cv_rmse <- sqrt(min(lasso_cv$cvm))

lasso_pred_min <- predict(lasso_cv, s = "lambda.min", newx = x_test)
lasso_pred_1se <- predict(lasso_cv, s = "lambda.1se", newx = x_test)
lasso_rmse_min <- sqrt(mean((y_test - lasso_pred_min)^2))
lasso_rmse_1se <- sqrt(mean((y_test - lasso_pred_1se)^2))

cat("\nLASSO lambda.min:", round(lasso_cv$lambda.min, 6),
    "| lambda.1se:", round(lasso_cv$lambda.1se, 6), "\n")
cat("LASSO CV RMSE:          ", round(lasso_cv_rmse,  4), "\n")
cat("LASSO Test RMSE (min):  ", round(lasso_rmse_min, 4), "\n")
cat("LASSO Test RMSE (1se):  ", round(lasso_rmse_1se, 4), "\n")
cat("Vars selected (lambda.min):",
    sum(coef(lasso_cv, s = "lambda.min")[-1] != 0), "\n")
cat("Vars selected (lambda.1se):",
    sum(coef(lasso_cv, s = "lambda.1se")[-1] != 0), "\n")

# Non-zero coefficients at lambda.min
cat("\n--- LASSO selected variables (lambda.min) ---\n")
lasso_coefs_min <- coef(lasso_cv, s = "lambda.min")
print(lasso_coefs_min[lasso_coefs_min[, 1] != 0, , drop = FALSE])

# LASSO plots
png("lasso_cv.png", width = 3200, height = 2000, res = 300)
par(mfrow = c(1, 2))
plot(lasso_cv, main = "LASSO: CV Error")
plot(lasso_cv$glmnet.fit, xvar = "lambda", main = "LASSO: Coefficient Paths")
abline(v = log(lasso_cv$lambda.min), lty = 2, col = "red")
abline(v = log(lasso_cv$lambda.1se), lty = 2, col = "blue")
legend("topright", legend = c("lambda.min","lambda.1se"),
       lty = 2, col = c("red","blue"), cex = 0.8)
par(mfrow = c(1, 1))
dev.off()

# ============================================================
# Section 19 — Elastic Net (0 < alpha < 1)
#
# Combines L1 and L2 penalties:
#   argmin (1/n)||y-Xb||^2 + lambda[(1-alpha)/2 ||b||^2 + alpha||b||_1]
# alpha = 0 -> Ridge; alpha = 1 -> LASSO
# Alpha tuned over grid [0, 1] by minimising test RMSE.
# ============================================================

# Step 1: tune alpha
set.seed(123)
alphas         <- seq(0, 1, by = 0.1)
enet_rmse_grid <- sapply(alphas, function(a) {
  cv   <- cv.glmnet(x_train, y_train, alpha = a)
  pred <- predict(cv, s = "lambda.min", newx = x_test)
  sqrt(mean((y_test - pred)^2))
})

best_alpha <- alphas[which.min(enet_rmse_grid)]
cat("\nElastic Net alpha grid:\n")
print(data.frame(Alpha = alphas, Test_RMSE = round(enet_rmse_grid, 4)))
cat("Best alpha:", best_alpha, "\n")

plot(alphas, enet_rmse_grid,
     type = "b", pch = 16,
     xlab = "Alpha (0 = Ridge, 1 = LASSO)",
     ylab = "Test RMSE",
     main = "Elastic Net — alpha tuning")
abline(v = best_alpha, col = "red", lty = 2)
text(best_alpha, max(enet_rmse_grid),
     paste0("best alpha = ", best_alpha), pos = 4, cex = 0.8)

# Step 2: fit final Elastic Net at best alpha
set.seed(123)
enet_cv       <- cv.glmnet(x_train, y_train, alpha = best_alpha)
enet_cv_rmse  <- sqrt(min(enet_cv$cvm))

enet_pred_min <- predict(enet_cv, s = "lambda.min", newx = x_test)
enet_pred_1se <- predict(enet_cv, s = "lambda.1se", newx = x_test)
enet_rmse_min <- sqrt(mean((y_test - enet_pred_min)^2))
enet_rmse_1se <- sqrt(mean((y_test - enet_pred_1se)^2))

cat("\nElastic Net (alpha =", best_alpha, ")\n")
cat("CV RMSE:                ", round(enet_cv_rmse,  4), "\n")
cat("Test RMSE (lambda.min): ", round(enet_rmse_min, 4), "\n")
cat("Test RMSE (lambda.1se): ", round(enet_rmse_1se, 4), "\n")
cat("Vars selected (lambda.min):",
    sum(coef(enet_cv, s = "lambda.min")[-1] != 0), "\n")

plot(enet_cv$glmnet.fit, xvar = "lambda",
     main = paste0("Elastic Net (alpha = ", best_alpha,
                   ") — coefficient paths"))
abline(v = log(enet_cv$lambda.min), lty = 2, col = "red")


# ============================================================
# Section 21 — Random Forest
# ============================================================

# Tune mtry over a grid (Name converted to numeric for tuneRF)
set.seed(123)
tuneRF(
  x = data.frame(
    Year             = train$Year,
    UnemploymentRate = train$UnemploymentRate,
    Name             = as.numeric(as.factor(train$Name)),
    NetHousing       = train$NetHousing,
    MortgageRate     = train$MortgageRate,
    PopGrowth        = train$PopGrowth,
    Inflation        = train$Inflation,
    GVA              = train$GVA
  ),
  y          = log(train$AffordabilityRatio),
  ntreeTry   = 500,
  stepFactor = 1.5,
  trace      = FALSE,
  plot       = TRUE
)

# Fit Random Forest
set.seed(123)
rf_model <- randomForest(
  log(AffordabilityRatio) ~ Year + UnemploymentRate + Name +
    NetHousing + MortgageRate + PopGrowth + Inflation + GVA,
  data  = train,
  ntree = 500
)

plot(rf_model, main = "Random Forest — OOB error vs number of trees")

rf_pred <- predict(rf_model, newdata = test)
rf_rmse <- sqrt(mean((y_test - rf_pred)^2))
cat("Random Forest Test RMSE:", round(rf_rmse, 4), "\n")

varImpPlot(rf_model, main = "Random Forest — variable importance (%IncMSE)")


# ============================================================
# Section 22 — Final model comparison table
# ============================================================

results <- data.frame(
  Model = c(
    "OLS (baseline)",
    "Ridge (lambda.min)",
    "Ridge (lambda.1se)",
    "LASSO (lambda.min)",
    "LASSO (lambda.1se)",
    paste0("Elastic Net alpha=", best_alpha, " (lambda.min)"),
    paste0("Elastic Net alpha=", best_alpha, " (lambda.1se)"),
    "Random Forest"
  ),
  CV_RMSE = c(
    NA,
    round(ridge_cv_rmse, 4), NA,
    round(lasso_cv_rmse, 4), NA,
    round(enet_cv_rmse,  4), NA,
    NA
  ),
  Test_RMSE = round(c(
    test_rmse,
    ridge_rmse_min, ridge_rmse_1se,
    lasso_rmse_min, lasso_rmse_1se,
    enet_rmse_min,  enet_rmse_1se,
    rf_rmse
  ), 4)
)

results <- results[order(results$Test_RMSE), ]
cat("\n========================================\n")
cat("  Final Model Comparison — Test RMSE\n")
cat("========================================\n")
print(results, row.names = FALSE)


# ============================================================
# Appendix — Descriptive diagnostics
# Select lines manually to run — skipped on source()
# ============================================================
if (FALSE) {

  # Data coverage per variable
  cat("Affordability ratio:", range(aff_long$Year), "\n")
  cat("Unemployment:       ", range(unemp_clean$Year), "\n")
  cat("Net housing:        ", range(nethousing_clean$Year), "\n")
  cat("Mortgage rate:      ", range(mortgage_yearly$Year), "\n")
  cat("Population:         ", range(population_clean$Year), "\n")
  cat("Inflation:          ", range(inflation_clean$Year), "\n")
  cat("GVA:                ", range(GVA_clean$Year), "\n")

  # Estimation sample summary
  cat("\nEstimation sample:\n")
  cat("Year range:", range(df_est$Year), "\n")
  cat("Obs:       ", nrow(df_est), "\n")
  cat("Regions:   ", length(unique(df_est$Name)), "\n")
  print(table(df_est$Name, df_est$Year))

  # Variable summaries
  summary(df_est[, c("AffordabilityRatio", "UnemploymentRate",
                     "NetHousing", "MortgageRate",
                     "PopGrowth", "Inflation", "GVA")])

  # Regional affordability 2004 vs 2022
  df_est %>% filter(Year == 2004) %>%
    select(Name, AffordabilityRatio) %>%
    arrange(AffordabilityRatio) %>% print()

  df_est %>% filter(Year == 2022) %>%
    select(Name, AffordabilityRatio) %>%
    arrange(AffordabilityRatio) %>% print()

  # National average trend
  df_est %>%
    group_by(Year) %>%
    summarise(
      Mean = round(mean(AffordabilityRatio), 2),
      Min  = round(min(AffordabilityRatio),  2),
      Max  = round(max(AffordabilityRatio),  2)
    ) %>% print(n = 19)

} # end if(FALSE)
