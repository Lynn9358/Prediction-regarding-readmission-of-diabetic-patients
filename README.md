
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Prediction-regarding-readmission-of-diabetic-patients

This is a project to predict the possibility of
readmission for diabetic patients based on their demographic and
physical features of patients, medications, number of certain medical
record and diagnoses.



## Repository Details

**R**  
- `dataprocessing.R` Read and clean the raw data, recode for diagnoses
and medical. Code used for descriptive analysis.  
- `predictionmodels.R` Code for spliting data and predictive model,
including logistic regression, naive bayesian, decision tree and random
forest.  
- `resultsandplots.R` Code for model comparison using ROC, AUC and
confusion matrix, showinig results for the four prediction methods.

**Report** 
- `Report.Rmd` Report in .Rmd template

## Attribution

Dataset in this project was from Kaggle [Diabetic Patients’ Re-admission
Prediction](https://www.kaggle.com/datasets/saurabhtayal/diabetic-patients-readmission-prediction/code),
attributing to Saurabh Tayal.
