# Predict Liver Disease WebApp 

<img src="https://github.com/santigberruga/Predict-Liver-Disease/blob/main/WebApp/www/favicon.svg" alt="App Icon" style="height:60px"> 

Predict Liver Disease WebApp is a prediction project for patients with liver disease using Machine Learning algorithms. To make the predictions, we use a classification model created with a **single-hidden-layer artificial neural network algorithm** and trained with the [ILPD (Indian Liver Patient Dataset)](https://archive.ics.uci.edu/ml/datasets/ILPD+(Indian+Liver+Patient+Dataset)) data set obtained from the [UCI machine learning repository](https://archive.ics.uci.edu/ml/index.php).


## How to try it

You can access and try the web application from this [link](https://gonzalezberrugasantiago.shinyapps.io/Predict_Liver_Disease/)! 🧪🩺


## Summary

Liver diseases have increased considerably in recent years due to changes in lifestyle habits and are one of the leading causes of mortality worldwide. However, the diagnosis of liver diseases remains complex, expensive and most of the times late. 😨

This work seeks an automatic classification model that allows for an early and simple diagnosis of liver patients. To this end, models are generated using the ILPD liver patient dataset and the machine learning algorithms K-nearest neighbour (KNN), Naive Bayes (NB), Decision tree (DT), Random Forest (RF), Support Vector Machines (SVM), Artificial Neural Network (ANN) and Logistic Regression (LR).

To determine the best model, the metrics accuracy, false negatives, false positives, error rate, kappa statistic, sensitivity, specificity, precision, recall and F1-score were used. Based on this, the ANN and RF models showed better results than the other models for the prediction of liver patients, with an accuracy of 75.1% and 74.6% and precision of 76.8% and 75.3%. 

Therefore, this work has demonstrated that it is possible to diagnose liver patients using automatic classification models trained with simple clinical variables, without having to use invasive methods on the patients. 

Furthermore, the ANN model has been implemented in a web application, generating a unique tool with great potential to support healthcare professionals during the diagnosis of liver diseases, allowing an early diagnosis without the need of intrusive techniques! 🤗


## Programming languages

To create the machine learning models, the statistical software *R* version 4.1.1 has been used together with the integrated development environment (IDE) *RStudio* version 2021.09.0 + 351, allowing to work comfortably with the R programming language. 

The main R packages used to create machine learning models are *dplyr* for dataset management, *ggplot2* for graph creation, *recipes* for data preprocessing, and *caret* for creating machine learning models.

On the other hand, the main packages used for the creation of the web application are *shiny* and *shinydashboard* for the creation of the web application, *ggplot2* and *plotly* to generate plots and *shinyjs*, *shinyFeedback*, *shinyalert* and *waiter* to add different functionalities to the application.


## Screenshots

### Initial view of the app

We observe the initial view of the application, configured by default to make a unique prediction and upload the data through a *csv* file.

![view app](https://user-images.githubusercontent.com/96242441/146653458-43b85b3c-d0f2-4f26-9632-79f571424a08.png)


### Manual form

We look at the form fields to enter the data manually and make a unique prediction.

![manual form fields](https://user-images.githubusercontent.com/96242441/146653497-342f4e4e-9091-4ebe-a24c-b6cc6b43b466.png)


### Feedback to users

We offer the option of downloading the application guide and different csv file templates. 👌😉

![popup help](https://user-images.githubusercontent.com/96242441/146653526-d6aa15ac-62ec-43d9-a36f-e174413514a0.png)

We also carry out different validations of the data provided by the users and create informative messages that help users to understand what is happening in the application.

![form validations](https://user-images.githubusercontent.com/96242441/146653514-10a0755f-35fb-4afd-9188-1ffcfcb0df37.png)

![popup_fail](https://user-images.githubusercontent.com/96242441/146653520-5333caa1-b0a4-4a07-8e5f-26ba7a2d80ff.png)

![popup_success](https://user-images.githubusercontent.com/96242441/146653524-e296dc7c-e38b-4a72-af0d-0a2a42744eea.png)


Finally, we look at the results of the predictions made with the application:

* **Results of Unique Prediction**

![pred_unique_full](https://user-images.githubusercontent.com/96242441/146653559-829ba001-adeb-4282-bf35-5e53a4345ffa.png)

* **Results of Multiple Prediction**

![pred_multi_full](https://user-images.githubusercontent.com/96242441/146653565-3513483e-310b-4f07-88e6-e05bc2a14114.png)


