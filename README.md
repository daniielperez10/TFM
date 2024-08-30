This repository is part of my Master's thesis for the Master's in Computational Social Sciences at Universidad Carlos III.

My thesis aims to evaluate the impact of the Madrid Central policy in an innovative way by predicting the counterfactual using Machine Learning models.

The code is structured as follows:

1. DATAFRAME CREATION: To execute the script "1.Dataframe Creation.rmd" correctly, you need to download files located in the folder named: "RAW DATA"
   
The output of this DataFrame is a file called "df_combined_complete," which contains pollution data from each station, both within the Municipality of Madrid and from peripheral areas of the Community of Madrid, information related to these stations such as their location, type, and other details, as well as weather data.
This output can be directly used to execute the following scripts if you wish to skip the DataFrame creation step and is available in the folder "ATT ESTIMATIONS"

2. Parallel Trends: To excute the script "PARALLEL TRENDS CODE.Rmd" correctly, you need to download files located in the folder named: "PARALLEL TRENDS DATA"
In addition, an HTML with the graph “PARALLEL-TRENDS-GRAPH.html” is available in the folder.




4. ATT ESTIMATIONS: To execute the script "ATT AND C ATT.Rmd" correctly, you need to download files located in the folder named: "DATA FOR ATT AND CONDITIONAL ATT ESTIMATIONS," where two files are available: "df_combined_complete.csv," which is the output from DATAFRAME CREATION in step 1, and "predictions_xgboost_POSTPOLICY_CITYCENTER_CV," which is the output from ML MODELS in step 3. Thus, by running the aforementioned Rmd file, one can obtain the treatment effects on the treated for Madrid Central using the traditional Differences-in-Differences method and the alternative approach proposed in the thesis, constructing a predictive counterfactual with Machine Learning models. 

In addition, HTML "ATT-AND-C ATT.html" is available in the folder.
