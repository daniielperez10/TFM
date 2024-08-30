This repository is part of my Master's thesis for the Master's in Computational Social Sciences at Universidad Carlos III.

My thesis aims to evaluate the impact of the Madrid Central policy innovatively by predicting the counterfactual using Machine Learning models.

The code is organized into the following folders:

1. DATAFRAME CREATION: To execute the script "1.Dataframe Creation.rmd" correctly, you need to download the files located in the folder named "RAW DATA."
The output of this process is a DataFrame saved as a file named "df_combined_complete," which contains pollution data from each station, both within the Municipality of Madrid and in peripheral areas of the Community of Madrid. This DataFrame also includes information about these stations, such as their location, type, and other details, along with weather data. This output can be directly used to execute the subsequent scripts if you wish to skip the DataFrame creation step. It is available in the folder "ATT ESTIMATIONS."

2. PARALLEL TRENDS: To execute the script "PARALLEL TRENDS CODE.Rmd" correctly, you need to download the files located in the folder named "PARALLEL TRENDS DATA." Additionally, an HTML file with the graph "PARALLEL-TRENDS-GRAPH.html" is available in the folder.

3. ML MODELS: To execute "ML MODELS.Rmd" correctly, you need to download the files located in the folder named "ML MODELS DATA." This step produces two outputs that are used in step 4 for estimating the effects of the Madrid Central policy:
A DataFrame named "predictions_xgboost_POSTPOLICY_CITYCENTER_CV," which contains the counterfactual predictions. This means that using the model trained with pre-Madrid Central data, predictions are generated for the post-Madrid Central period (i.e., what would have happened in the absence of the policy).
Another DataFrame named "train_data_20," which contains the cross-validated residuals from the training model. These residuals are used in step 4 to calculate the standard errors of the estimates.

4. ATT ESTIMATIONS: To execute the script "ATT AND C ATT.Rmd" correctly, you need to download the files located in the folder named "DATA FOR ATT AND CONDITIONAL ATT ESTIMATIONS." This folder contains three files: "df_combined_complete.csv" (the output from DATAFRAME CREATION in step 1), "predictions_xgboost_POSTPOLICY_CITYCENTER_CV" (the output from ML MODELS in step 3), and "train_data_20" (a DataFrame containing the cross-validated residuals from the training model run also in step 3). By running the specified Rmd file, you can obtain the treatment effects on the treated for Madrid Central using both the traditional Differences-in-Differences method and the alternative approach proposed in the thesis, which constructs a predictive counterfactual using Machine Learning models.

Additionally, an HTML file "ATT-AND-C ATT.html" is available in the folder.
