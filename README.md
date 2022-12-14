
# Prediction of Readmission for Diabetic Patients

This is a project to predict the possibility of
readmission for diabetic patients based on their demographic and
physical features of patients, medications, number of certain medical
record and diagnoses.

## Motivation
Predicts the possibility of readimission in diabetic patients, providing reasonal advice and reference for doctors to design treatment, as well as providing patients' families and patients with an general remainder of their condition.

## Method
- `Prodiction Methods` 

| [Logistic Regression](https://en.wikipedia.org/wiki/Logistic_regression) | [Naive Bayes](https://en.wikipedia.org/wiki/Naive_Bayes_classifier) | [Decision Tree](https://en.wikipedia.org/wiki/Decision_tree) | [Random Forest](https://en.wikipedia.org/wiki/Random_forest) |
|---------------------|-------------|---------------|---------------|

- `Evaluation Methods` 

| [ROC&AUC](https://en.wikipedia.org/wiki/Receiver_operating_characteristic) | [Confusion Matrix](https://en.wikipedia.org/wiki/Confusion_matrix) |
|---------|------------------|


## Repository Details

**R code**  
- `dataprocessing.R` Read and clean the raw data, recode for diagnoses
and medical. Code used for descriptive analysis.  
- `predictionmodels.R` Code for spliting data and predictive model,
including logistic regression, naive bayesian, decision tree and random
forest.  
- `resultsandplots.R` Code for model comparison using ROC, AUC and
confusion matrix, showinig results for the four prediction methods.

**Report** 
- `Report.Rmd` Report in .Rmd template
- `Report.pdf` Report in .pdf template

## Contribution
- `Anran Yao` Responsible for the prediction model building and comparison and the result of report.
- `Mingrui Li` Responsible for the data discriptive analysis and the preprocessing section in report.
- `Wenjing Li` Responsible for maintain and tracking repository and data clearning and preprocessing.

## Attribution

Dataset in this project was from Kaggle [Diabetic Patients’ Re-admission
Prediction](https://www.kaggle.com/datasets/saurabhtayal/diabetic-patients-readmission-prediction/code),
attributing to Saurabh Tayal.
