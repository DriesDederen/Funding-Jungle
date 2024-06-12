# README - Navigating the Funding Jungle: Predictive Analytics for Funding Rounds in European SaaS Startups

This repository contains the code and data used for the thesis "Navigating the Funding Jungle: Predictive Analytics for Funding Rounds in European SaaS Startups." The research aims to analyze and predict the success factors for securing funding rounds in European SaaS startups using machine learning models.

## Repository structure

- `dealroom_data.xlsx`: The original dataset containing raw data from Dealroom.
- `final_cleaned_data.RData`: The cleaned dataset ready for analysis.
- `data_cleaning.R`: Script for cleaning and preprocessing the raw data.
- `EDA.R`: Script for exploratory data analysis.
- `boosting_funding_rounds.R`: Script for predicting the number of funding rounds using a boosting model.
- `random_forest_funding_rounds.R`: Script for predicting the number of funding rounds using a random forest model.
- `boosting_target_invest.R`: Script for predicting the likelihood of securing future funding rounds using a boosting model.

## Getting started

To follow the steps described in the thesis, run the files in the following order:
1. `data_cleaning.R`
2. `EDA.R`
3. `boosting_funding_rounds.R`
4. `random_forest_funding_rounds.R`
5. `boosting_target_invest.R`

Note: before running the XGBoost and Random Forest code, you should uncomment the correct train/test-split coding lines. In order to specific values for the tuning parameters, you should also comment out the tuning grid and choose specific values. 