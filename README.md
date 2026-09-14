# housing-affordability-analysis
# Housing Affordability Analysis

This project investigates regional housing affordability in England using
statistical modelling and machine-learning techniques in R.

## Project overview

Housing affordability is measured using the ratio of median house prices to
workplace-based earnings across nine English regions. The analysis examines
how economic and housing-market variables are associated with affordability
and compares alternative models based on predictive performance.

## Methods

- Data cleaning and transformation
- Exploratory data analysis
- Fixed-effects regression
- Ridge regression and LASSO
- Cross-validation
- Out-of-sample model evaluation

## Key result

The fixed-effects regression model produced the strongest predictive
performance, outperforming the regularised models on the test dataset.

## Tools

- R
- `dplyr`
- `tidyr`
- `glmnet`
- `plm`
- `sandwich`
- `randomForest`

## Repository contents

- `housing_affordability_model.R` — complete data preparation, modelling and
  evaluation workflow

## Data

The analysis uses publicly available regional housing and economic data from
UK government and official statistical sources. Raw datasets are not included
in this repository.

## Project report

The complete methodology, results and discussion are available in the
[final project report](housing_affordability_report.pdf).

## Author

Hidir Azlan Shah  
Mathematics with Actuarial Science graduate
