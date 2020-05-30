# Measures of forecasting accuracy for all models

MSE_LG <- round(mean((LG_Filt$f-y)^2),3) # mean square error (MSE)
MAE_LG <- round(mean(abs(LG_Filt$f-y)),3) # mean abs. error (MAE)
MAPE_LG <- round(mean(abs(LG_Filt$f-y)/y),3) # mean abs. percentage error (MAPE)

MSE_LG_2 <- round(mean((LG_Filt_2$f-y)^2),3) # mean square error (MSE)
MAE_LG_2 <- round(mean(abs(LG_Filt_2$f-y)),3) # mean abs. error (MAE)
MAPE_LG_2 <- round(mean(abs(LG_Filt_2$f-y)/y),3) # mean abs. percentage error (MAPE)

MSE_LG_3 <- round(mean((LG_Filt_3$f-y)^2),3) # mean square error (MSE)
MAE_LG_3 <- round(mean(abs(LG_Filt_3$f-y)),3) # mean abs. error (MAE)
MAPE_LG_3 <- round(mean(abs(LG_Filt_3$f-y)/y),3) # mean abs. percentage error (MAPE)

MSE_BSM <- round(mean(BSM$residuals),3) # mean square error (MSE)
MAE_BSM <- round(mean(abs(BSM$residuals)),3) # mean abs. error (MAE)
MAPE_BSM <- round(mean(abs(BSM$residuals)/y),3) # mean abs. percentage error (MAPE)

MSE_DR <- round(mean((DR_Filt$f-y)^2),3) # mean square error (MSE)
MAE_DR <- round(mean(abs(DR_Filt$f-y)),3) # mean abs. error (MAE)
MAPE_DR <- round(mean(abs(DR_Filt$f-y)/y),3) # mean abs. percentage error (MAPE)

MSE_SUR <- round(mean((SUR_Filt$f-y)^2),3) # mean square error (MSE)
MAE_SUR <- round(mean(abs(SUR_Filt$f-y)),3) # mean abs. error (MAE)
MAPE_SUR <- round(mean(abs(SUR_Filt$f-y)/y),3) # mean abs. percentage error (MAPE)

MSE_SUTSE <- round(mean((SUTSE_Filt$f-y)^2),3) # mean square error (MSE)
MAE_SUTSE <- round(mean(abs(SUTSE_Filt$f-y)),3) # mean abs. error (MAE)
MAPE_SUTSE <- round(mean(abs(SUTSE_Filt$f-y)/y),3) # mean abs. percentage error (MAPE)