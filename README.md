# Shleep Consulting Project

This contains files for the consulting project for Shleep.

Clean_data is a Jupyter Notebook that cleans and prepares the data from the raw data tables provided by the startup.

Exploratory_analysis is a Jupyter Notebook that shows exploratory plots and preliminary analyses of the pre-assessment, user retention, and sleep tracker data.

lme_model is a R script that fits a linear mixed-effects model to the data, measuring the association between sleep tracker usage and hours of sleep.

RandomForest_predict_engagement is a Jupyter Notebook that uses behavior in the first 2 weeks and pre-assessment answers to predict engagement based on activity 6 weeks out. I used a random forest classifier with hyperparameters tuned using randomized search cross validation. Also tried multinomial logistic regression, naive Bayes, decision tree, k-nearest neighbors, and adaboost out of the box in sci-kit learn.