This repository is part of my Master's thesis for the Master's in Computational Social Sciences at Universidad Carlos III.

My thesis aims to evaluate the impact of the Madrid Central policy in an innovative way by predicting the counterfactual using Machine Learning models.

The code is structured as follows:

1. DATAFRAME CREATION: To execute the script "1.Dataframe Creation.rmd" correctly, you need to download files located in the folder named: "RAW DATA"
   
The output of this DataFrame is a file called "df_combined_complete," which contains pollution data from each station, both within the Municipality of Madrid and from peripheral areas of the Community of Madrid, information related to these stations such as their location, type, and other details, as well as weather data.
This output can be directly used to execute the following scripts if you wish to skip the DataFrame creation step and is available in the folder "ATT ESTIMATIONS"

2. Parallel Trends: To excute the script "PARALLEL TRENDS CODE.Rmd" correctly, you need to download files located in the folder named: "PARALLEL TRENDS DATA"
In addition, an HTML with the graph “PARALLEL-TRENDS-GRAPH.html” is available in the folder.




4. ATT ESTIMATIONS: To excute the script "ATT AND C ATT.Rmd" correctly, you need to download files located in the folder named:"DATA FOR ATT AND CONDITIONAL ATT ESTIMATIONS", donde se encuentran dos archivos disponibles "df_combined_complete.csv" que es el output de DATAFRAME CREATION en el paso 1, y "predictions_xgboost_POSTPOLICY_CITYCENTER_CV" que es el output del ML MODELS en el paso 3. Así, ejecutando el Rmd anteriormente mencionado se pueden obtener los efectos del tratamiento sobre los tratados de Madrid Central por el método tradicional de Differences-in-Differences y por el enfoque alternativo propuesto en la tesis construyendo un contrafactual predictivo con modelos de Machine Learning
In addition, HTML "ATT-AND-C ATT.html” is available in the folder.
