library (tidyverse)
telco = read.csv('telco_dataset.csv')
nrow(telco)
view(telco)
colnames(telco)
# [1] "customerID"       "gender"           "SeniorCitizen"    "Partner"         
# [5] "Dependents"       "tenure"           "PhoneService"     "MultipleLines"   
# [9] "InternetService"  "OnlineSecurity"   "OnlineBackup"     "DeviceProtection"
# [13] "TechSupport"      "StreamingTV"      "StreamingMovies"  "Contract"        
# [17] "PaperlessBilling" "PaymentMethod"    "MonthlyCharges"   "TotalCharges"    
# [21] "Churn" 

colSums(is.na(telco)) #checking missing values -- results: Total charges = 11
# customerID           gender    SeniorCitizen          Partner       Dependents           tenure     PhoneService 
#          0                0                0                0                0                0                0 
# MultipleLines  InternetService   OnlineSecurity     OnlineBackup DeviceProtection      TechSupport      StreamingTV 
#          0                0                0                0                0                0                0 
# StreamingMovies         Contract PaperlessBilling    PaymentMethod   MonthlyCharges     TotalCharges            Churn 
#          0                0                0                0                0               11                0 


#MODEL 1: Demographics (gender + SeniorCitizen + Partner + Dependents) 

model1 = lm(tenure ~ gender + SeniorCitizen + Partner + Dependents, data = telco)
summary (model1)

# Coefficients:
#                Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)    23.2038     0.4887  47.482   <2e-16 ***
#   genderMale      0.2926     0.5416   0.540    0.589    
#   SeniorCitizen   0.4933     0.7578   0.651    0.515    
#   PartnerYes     18.9513     0.6128  30.928   <2e-16 ***
#   DependentsYes  -0.7158     0.6838  -1.047    0.295    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 22.72 on 7038 degrees of freedom
# Multiple R-squared:  0.1444,	Adjusted R-squared:  0.144 
# F-statistic: 297.1 on 4 and 7038 DF,  p-value: < 2.2e-16

library (ggfortify)
autoplot (model1)

#MODEL 2: Services (PhoneService + MultipleLines + InternetService + OnlineSecurity +
# OnlineBackup + DeviceProtection + TechSupport + StreamingTV + StreamingMovies )

model2 = lm(tenure ~ PhoneService + MultipleLines + InternetService + OnlineSecurity +
            OnlineBackup + DeviceProtection + TechSupport + StreamingTV + StreamingMovies, data = telco)
summary(model2)

# Coefficients: (7 not defined because of singularities)
#                                       Estimate Std. Error t value Pr(>|t|)    
#    (Intercept)                           9.2834     0.8290  11.198  < 2e-16 ***
#    PhoneServiceYes                      -5.5192     0.8854  -6.233 4.83e-10 ***
#    MultipleLinesNo phone service             NA         NA      NA       NA    
#    MultipleLinesYes                     12.8535     0.5212  24.663  < 2e-16 ***
#    InternetServiceFiber optic            0.1176     0.6213   0.189     0.85    
#    InternetServiceNo                    23.9024     0.7888  30.303  < 2e-16 ***
#    OnlineSecurityNo internet service         NA         NA      NA       NA    
#    OnlineSecurityYes                    11.7470     0.5707  20.582  < 2e-16 ***
#    OnlineBackupNo internet service           NA         NA      NA       NA    
#    OnlineBackupYes                      12.0803     0.5395  22.390  < 2e-16 ***
#    DeviceProtectionNo internet service       NA         NA      NA       NA    
#    DeviceProtectionYes                   9.8976     0.5639  17.552  < 2e-16 ***
#    TechSupportNo internet service            NA         NA      NA       NA    
#    TechSupportYes                        8.6943     0.5827  14.920  < 2e-16 ***
#    StreamingTVNo internet service            NA         NA      NA       NA    
#    StreamingTVYes                        4.9404     0.5869   8.417  < 2e-16 ***
#    StreamingMoviesNo internet service        NA         NA      NA       NA    
#    StreamingMoviesYes                    5.3117     0.5877   9.038  < 2e-16 ***
#  ---
#    Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 18.98 on 7032 degrees of freedom
# Multiple R-squared:  0.4033,	Adjusted R-squared:  0.4025 
# F-statistic: 475.3 on 10 and 7032 DF,  p-value: < 2.2e-16

autoplot(model2)


#MODEL 3: Accounts ( Contract + PaperlessBilling + PaymentMethod + MonthlyCharges )

model3 = lm(tenure ~ Contract + PaperlessBilling + PaymentMethod + MonthlyCharges, 
            data = telco)
summary (model3)

# Coefficients:
#                                    Estimate Std. Error t value Pr(>|t|)    
#  (Intercept)                            9.598102   0.689268  13.925  < 2e-16 ***
#  ContractOne year                      22.843194   0.506753  45.078  < 2e-16 ***
#  ContractTwo year                      37.586345   0.505930  74.292  < 2e-16 ***
#  PaperlessBillingYes                    1.128318   0.423822   2.662  0.00778 ** 
#  PaymentMethodCredit card (automatic)  -1.056396   0.580044  -1.821  0.06861 .  
#  PaymentMethodElectronic check         -7.043115   0.556848 -12.648  < 2e-16 ***
#  PaymentMethodMailed check            -10.990175   0.600348 -18.306  < 2e-16 ***
#  MonthlyCharges                         0.206764   0.007254  28.505  < 2e-16 ***
#  ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 16.06 on 7035 degrees of freedom
# Multiple R-squared:  0.5731,	Adjusted R-squared:  0.5726 
# F-statistic:  1349 on 7 and 7035 DF,  p-value: < 2.2e-16

autoplot (model3)

# Model 4: All variables excluding customerID, TotalCharges and Churn
#          and also 7 N/A singularities/duplicates as they are redundant.

telcoexcluded = telco %>%
  select (-customerID, -TotalCharges, -Churn,
          -MultipleLines, -OnlineSecurity, -OnlineBackup,
          -DeviceProtection, -TechSupport, -StreamingTV, -StreamingMovies) #exclude these

model4 = lm (tenure ~ ., data = telcoexcluded)
summary (model4)

# Coefficients:
#                                      Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                           -0.71574    1.05410  -0.679   0.4972    
# genderMale                             0.60741    0.36271   1.675   0.0940 .  
# SeniorCitizen                          2.62712    0.52495   5.005 5.74e-07 ***
# PartnerYes                             7.60938    0.42888  17.742  < 2e-16 ***
# DependentsYes                         -2.11605    0.46437  -4.557 5.28e-06 ***
# PhoneServiceYes                      -11.90792    0.80265 -14.836  < 2e-16 ***
# InternetServiceFiber optic           -10.05762    0.73245 -13.731  < 2e-16 ***
# InternetServiceNo                     17.04376    1.00982  16.878  < 2e-16 ***
# ContractOne year                      18.59394    0.51653  35.998  < 2e-16 ***
# ContractTwo year                      30.73086    0.56671  54.227  < 2e-16 ***
# PaperlessBillingYes                    1.01957    0.40351   2.527   0.0115 *  
# PaymentMethodCredit card (automatic)  -0.88216    0.54990  -1.604   0.1087    
# PaymentMethodElectronic check         -6.31563    0.53099 -11.894  < 2e-16 ***
# PaymentMethodMailed check             -9.99162    0.57242 -17.455  < 2e-16 ***
# MonthlyCharges                         0.51737    0.01898  27.252  < 2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 15.21 on 7028 degrees of freedom
# Multiple R-squared:  0.6171,	Adjusted R-squared:  0.6163 
# F-statistic:   809 on 14 and 7028 DF,  p-value: < 2.2e-16

autoplot (model4)

# Model 5: Ols_step_backwards_p --> remove insignificant predictors

library (olsrr)
model5 = lm(tenure ~., data = telcoexcluded)
ols_step_backward_p (model5)
summary (model5)

#[1] "No variables have been removed from the model."  

autoplot (model5)


#Model 6: ols_step_best_subset --> the best/top 4 predictors

model6 = lm(tenure ~., data = telcoexcluded)
ols_step_best_subset (model6)
summary (model6)

# Top 4: Partner, Contract, Payment Method & Monthly Charges

autoplot (model6)


#Model 7: Final model with top four predictors

model7 = lm(tenure ~ Partner + Contract + PaymentMethod +MonthlyCharges, data = telcoexcluded)
summary (model7)
autoplot (model7)

# Coefficients:
#                                     Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                            7.770963   0.669685  11.604   <2e-16 ***
# PartnerYes                             7.363281   0.395477  18.619   <2e-16 ***
# ContractOne year                      21.252741   0.499344  42.561   <2e-16 ***
# ContractTwo year                      34.897256   0.508098  68.682   <2e-16 ***
# PaymentMethodCredit card (automatic)  -0.807079   0.566678  -1.424    0.154    
# PaymentMethodElectronic check         -6.558807   0.543834 -12.060   <2e-16 ***
# PaymentMethodMailed check            -10.346328   0.586759 -17.633   <2e-16 ***
# MonthlyCharges                         0.199886   0.006819  29.315   <2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 15.68 on 7035 degrees of freedom
# Multiple R-squared:  0.5927,	Adjusted R-squared:  0.5923 
# F-statistic:  1463 on 7 and 7035 DF,  p-value: < 2.2e-16

