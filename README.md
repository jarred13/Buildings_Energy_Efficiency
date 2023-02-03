# Buildings Energy Efficiency

# Overview
This project uses machine learning to predict the energy efficiency of buildings based off of the building's features. This notebook is hosted on Kaggle and can be found here: https://www.kaggle.com/code/jarredpriester/predicting-a-building-s-energy-efficiency

# Purpose of this Project
In the 21st Century we will need to be more efficient with our energy use. The purpose of this project is to use machine learning to make predictions of building efficiency. The idea is that we can use these predictions to help with designing more efficent buildings.

# What Did I Learn
Random Forest is a good algorithm for this problem. It's flexibility is able to capture the important features and had the lowest RMSEof the models created. I learned that it is much easier to predict the heating load of a building compared to predicting the cooling load.

# Dataset Used
This dataset we will be using is from the University of California, Irvine Machine Learning Repository. The dataset was created by Angeliki Xifara (Civil/Structural Engineer) and was processed by Athanasios Tsanas (Oxford Centre for Industrial and Applied Mathematics, University of Oxford, UK). They performed energy analysis using 12 different building shapes simulated in Ecotect. The buildings differ with respect to the glazing area, the glazing area distribution, and the orientation, amongst other parameters. They simulate various settings as functions of the afore-mentioned characteristics to obtain 768 building shapes. The dataset comprises 768 samples and 8 features, aiming to predict two real valued responses. It can also be used as a multi-class classification problem if the response is rounded to the nearest integer.

# Files Used
ENB2012_data.csv - dataset  
Building Energy Predictions.R - R script  
predicting-a-building-s-energy-efficiency.r - Kaggle R Notebook  
predicting_building_energy_efficiency.Rmd - R markdown  
predicting_building_energy_efficiency.pdf - PDF 
