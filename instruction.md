<br>  
This app lets you visualize OLS regression with one or two variables as well as an interaction. It should help teaching what regression, and controlling for covariates, does.
<br>  
For example: Let's explore the relationship of political interest and reading newspapers.
* Regress political interest on whether a someone reads newspapers. You will see that reading newspapers predicts higher levels of political interest. 
* Now add education as a second independent variable. The predictive effect of reading newspapers diminishes. The 3D scatterplot and the regression hyperplane illustrate why: Those reading newspapers also tend to have higher levels of education. Thus, some of the difference between newspaper readers and non-readers can be explained by education. 
* Now select the "Interaction" box to check whether the predictive effect of readings newspapers differs across levels of education. The regression plane suggest suggests that reading a newspaper is more strongly related to political interest for those with lower education (but the interaction is small and statistically insignificant).
<br>  
The data used is the British Election Study 2017 that can be found at https://www.britishelectionstudy.com/data-object/2017-face-to-face/. It has been reduced to include the following variables only: 
* age in years
* ideology: between 0 = left and 10 = right
* education_level: between 1 = "No qualifications" and 6 = "Postgraduate level"
* canvassed: whether or not respondent was canvassed during election campaign
* read_newspapers: whether or not respondent reads newspapers
* political_trust: between 0 = "no trust" and 10 = "great deal of trust"
* political_interest: between 1 = "Not at all interested" and 4 = "Very interested"
