# Customer-Churn-Analysis-Telecom Sector #
Analysing customer behaviour, deriving patterns and insights from the provided dataset and thereby predicting customer churn rate in the Telecom sector.

PROJECT OBJECTIVE :

The main objective of this predictive modelling project is to predict the churn rate of the customers in Telecom sector. The project simulates one such case of customer churn where we work on a data of postpaid customers with a contract. Predictive models are built using the following Machine Learning algorithms, and the same is been compared for the best performance among them. 

•	Logistic Regression 
•	K-Nearest Neighbor 
•	Naïve Bayes 

BASIC DATA SUMMARY : 

•	The dataset contains both categorical and continuous variables.        
•	The variable names are “Churn”, “AccountWeeks”, “DataPlan”, “DataUsage”, “ContractRenewal”, “CustServCalls”, ”DayMins”, “DayCalls”, “MonthlyCharge”, “OverageFee”, “RoamMins”.  
•	The 5 point summary is calculated for all the variables. The measures of Central Tendency and measures of Dispersion are also examined.  
•	Few variables are not of appropriate datatype. So we correct them as and when necessary according to the assumptions of the models to be built. 

EXPLORATORY DATA ANALYSIS : 

UNIVARIATE ANALYSIS of each variable is done to understand its distribution, nature and presence of outliers.

BIVARIATE ANALYSIS : The bivariate analysis of the target variable “Churn” with all the other predictor variables are done. 
Inferences are drawn for highest churn rate with each one of the predictors. 
•	When the customer’s account of active connection was between 92 – 102 weeks, churn rate was more. 
•	42.4 % customers churned when they did not renew the contract and 11.5 % customers churned even after renewing the contract. 
•	16.7 % customers churned who had no data plan and 8.7 % customers churned who had a data plan. 
•	When there was 0 GHZ data usage, 17.8 % customers churned and when the data usage was between 0.11 – 0.26 GHZ, 14.2 % customers churned. 
•	63.6 % customers churned when there was 6 calls to the customer service. 
•	47.6 % customers churned when the average day time call minutes of the customers was within  250 – 351 mins. 
•	19.2 % customers churned when the average day time calls was between 118 – 127. 
•	40.4 % customers churned when their average monthly charges were between 63.1 % - 70.1 %. 
•	When the overage fee was between the range 13.30 % - 18.19 %, 
21.3 % customers churned. 
•	When the average roaming minutes was between 13.8 % - 20.0 %, 
22.6 % customers had churned. 

MISSING VALUE IDENTIFICATION : There are no missing values in the provided dataset. Hence no missing value treatments is carried out.  

OUTLIER IDENTIFICATION : Except for the categorical variable ‘Data Plan’, the outliers are present among all the other categorical and continuous variables. Categorical variables have 2 levels at the minimum and 10 levels at the maximum (CustServCalls - 0 to 9). Not all outliers are extreme values, so we are not ignoring them on our further analysis. 

MULTICOLLINEARITY CHECK : 
•	The variables DataPlan and DataUsage are highly correlated. 
•	The variable MonthlyCharge is correlated with DataPlan and DataUsage on significant level. 
•	The variable DayMins is moderately correlated with MonthlyCharge. 
Since the variables are of both categorical and continuous type, multicollinearity cannot be treated using dimensionality reduction techniques such as Principal Component Analysis. PCA would be highly feasible for continuous variables. Hence we can use Forward Selection , Backward Selection   or Stepwise Selection of significant variables and dropping the insignificant ones using any linear models. 

CORRELATION TREATMENT : 
Backward Selection of significant variables are done by building a Linear Probability Model and the same is been crosschecked with Subset Selection Algorithm. 
The only criterion of selecting the significant variables is increase in the Adjusted R2 value and decrease in the Residual Standard Error of the corresponding model. Other parameters such as CP and BIC are also taken into account when selecting the best model with the significant variables. 
Now the significant explanatory variables chosen for predicting Churn are: 

•	ContractRenewal 

•	DataUsage 

•	CustServCalls 

•	DayMins 

•	OverageFee 

•	RoamMins 

Their corresponding ‘vif’ values are also checked which shows no sign of correlation between them. 

BALANCING THE DATASET : 
The provided dataset is highly imbalanced with the proportion of customers not churned being more than the customers who have churned. Hence building predictive models on an imbalanced data will reduce the reliability and performance of the models.
In order to build a robust model, the given dataset is balanced using SMOTE technique by under sampling the majority class and over sampling the minority class. 

Now the dataset is highly balanced with equal proportions of customer churned and not churned. The balanced dataset is further split into Train and Test sets in the ratio 70:30, where MACHINE LEARNING models are built on.
Train set and is been validated using the Test set. We use one hot encoding for the variable ‘CustServCalls’ which has 10 levels in it. We also choose only the significant levels among them for our analysis.

LOGISTIC REGRESSION - INTERPRETATION : 

•	Unlike Linear Regression, Logistic Regression does not depend on its coefficients for the model performance. 
•	The coefficients simply explains the predictor variables and its linear relation with the logs odd ratio of the predictand. The formula used to predict the probabilities of the target variable is,         
exp (coefficients (log model)) / 1 + exp (coefficients (log model)) .

Logistic Regression’s model performance is only measured through the High values of Concordance or the Confusion Matrix.  (Refer below for the performance metrics comparison table)

K NEAREST NEIGHBOUR - INTERPRETATION : 

•	KNN is a distance based algorithm (Euclidean), different variables with different scales are normalized before applying the algorithm.
•	Using 10 repeats cross – validation the optimum value of ‘k’ is chosen as ‘7’ which is believed to provide a good accuracy level. 
•	The optimum value of ‘k’ is crucial for building a high performance model. 
The graphs we used more or less converge on optimum value of k as 7.

NAIVE BAYES - INTERPRETATION : 

Naïve Bayes algorithm works based on Bayes Theorem for calculating probabilities and conditional probabilities. 
•	It has a strong assumption of independence among the predictor variables. Multicollinearity shouldn’t be a problem between the predictors, which is already been taken care of.
•	The other important assumption of Naïve Bayes is that, all the continuous predictors should be normally distributed which is checked using appropriate graphs.
•	From the graphs we could clearly figure out that the predictors DataUsage and RoamMins are not normally distributed whereas DayMins and OverageFee are more or less nearing to the bell curve. 
•	In addition to the visual analysis, we also perform Statistical test methods such as Shapiro - Wilk’s Normality test and Kolmogorov – Smirnov test on continuous predictors to check its normality. Refer source code. 
•	All the visuals and test proves to be identical depicting the variables DataUsage and RoamMins as not bell curves. 
•	Hence we try to transform the variables using BoxCox transformation or Yeo Johnson’s method. 
•	Though the variables are not highly normal after the transformation, may be to some extent it can now comply with Naïve Bayes assumption. 

CONFUSION MATRIX FOR ALL THE BUILT MODELS:
![image](https://user-images.githubusercontent.com/81927278/184962585-ff0940b9-1de1-47a3-8267-abb7c37efafe.png)

•	The Accuracy of KNN is more when compared to the LOGIT model and Naïve Bayes classifier. The models have done a decent job of segregating the data points. Lesser the false predictions, higher the accuracy of the model. 
•	Sensitivity is the actual positive data points identified by the model as positive. KNN leads for the Sensitivity score, followed by LOGIT model and Naïve Bayes. 
•	The Specificity value of the KNN is the most. It is followed by Naïve Bayes in a small difference and then comes the LOGIT model. It is the actual negative data points identified by the model as negative. 
•	Precision is the positive data points identified by the model which are really positive. Again KNN has best Precision value. Then comes the Naïve Bayes and finally the LOGIT model.  
•	The F1 score is also highest for KNN, followed by Naïve Bayes and then LOGIT model. It is the harmonic mean of Sensitivity and Precision. 
•	Type - 1 error of KNN is less when compared to the other two models. There is a very small degree of difference between the Type 1 errors of KNN and Naïve Bayes (only 1 data point). 
•	Type - 2 error of KNN is the least followed by LOGIT model and then Naïve Bayes. 
•	Even the Detection Rates and Balanced Accuracy of KNN is highest followed by Naïve Bayes and LOGIT model. 
Detection Rate = rate of accurately predicting the positive class as the proportion of total. 
Balanced Accuracy =   (Sensitivity + Specificity) / 2

OTHER PERFORMANCE MEASURES : 
Few other performance measures are also calculated for all the three models such as Area under the Curve (AUC), Kolmogorov – Smirnov Goodness of Fit Test (K.S. statistic) and Gini Coefficient. 
![image](https://user-images.githubusercontent.com/81927278/184963201-0f61a064-db07-449b-8d06-749cc19dc7f4.png)

REMARKS ON MODEL VALIDATION : 

•	With respect to all the performance metric measures discussed above, KNN proves to be the best model with high performance measure. 

•	Since our objective is to determine the Churn rate which is the positive class (1) for all the built models, our basic attention would be towards the metrics which measures the positive class such as Sensitivity, Precision, Detection rate and Balanced accuracy. 

•	Keeping that into account, both Logistic Regression and Naïve Bayes gives a relatively secondary performance when compared to KNN.

•	With reference to other measures such as AUC, K.S. statistic and Gini coefficient, once again KNN has great values followed by Logistic Regression and Naïve Bayes Classifier. 

•	On summarizing all the above given performance metric values, we could strongly infer that KNN is the best optimum model. Both Naïve Bayes and Logistic Regression more or less gives a relatively secondary performance. 

BUSINESS INSIGHTS & RECOMMENDATIONS : 

•	The models are built on the balanced dataset, hence the results are not biased. 

•	With reference to the predictor variables we can infer that 
Customer who renew their contract, having a good data plan, best Customer service, low Overage fee, higher outgoing calls, an uninterrupted network and high roaming minutes at the minimum billing is less likely to churn. 

•	The above predictive modelling techniques can be used to identify customers who are more likely to churn and approaching them with suitable offers. 

•	We can further persuade them to renew their contract by providing additional data packs, extension of  fixed minimum calls, valuable services like giving them a family plan on their current bill, improvised signal strength etc., 

•	The biggest challenge of the Telecom Industry is to provide a good stable connection to the customer without incurring losses for themselves. But at the same time we should admit that it is less expensive to retain existing customers than to acquire new ones. 

[CHURN ANALYSIS-1.pdf](https://github.com/Shalini-Krishnan/Customer-Churn-Analysis-Telecom-/files/9353622/CHURN.ANALYSIS-1.pdf)

