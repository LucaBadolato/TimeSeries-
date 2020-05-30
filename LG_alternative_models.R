##############################################################################

## Linear growth model: Model 2 (assume constant nominal speed in the dynamics)

# Build the model
build_LG_2 <- function(param){dlmModPoly(order=2, dV=exp(param[1]), dW=c(exp(param[2]), 0),m0=c(y[1],0))}
fit_LG_2 <- dlmMLE(y, parm=rep(1,2), build_LG_2, hessian=TRUE) # convergence achieved
#unlist(build_LG_2(fit_LG_2$par)[c("V","W")])

# Calculate standard errors of the MLE using delta method
estVarLog_LG_2 <- solve(fit_LG_2$hessian)
estVar_LG_2 <- diag(exp(fit_LG_2$par)) %*% estVarLog_LG_2 %*% + diag(exp(fit_LG_2$par))
SE_LG_2 <- sqrt(diag(estVar_LG_2))

# Put MLE in the model
LG_2 <- build_LG_2(fit_LG_2$par)

# Kalman filter
LG_Filt_2 <- dlmFilter(y, LG_2)

# Remove the first item (m_0) in the vector of filtered states
filt_est_2 = dropFirst(LG_Filt_2$m)

# One step ahead forecast
y_filt_2 <- LG_Filt_2$f

##############################################################################

## Linear growth model: Model 3 (integrated random walk model)

# Build the model
build_LG_3 <- function(param){dlmModPoly(order=2, dV=exp(param[1]), dW=c(0,exp(param[2])), m0=c(y[1],0))}
fit_LG_3 <- dlmMLE(y, parm=rep(1,2), build_LG_3, hessian=TRUE) # convergence achieved
#unlist(build_LG_3(fit_LG_3$par)[c("V","W")])

# Calculate standard errors of the MLE using delta method
estVarLog_LG_3 <- solve(fit_LG_3$hessian)
estVar_LG_3 <- diag(exp(fit_LG_3$par)) %*% estVarLog_LG_3 %*% + diag(exp(fit_LG_3$par))
SE_LG_3 <- sqrt(diag(estVar_LG_3))


# Put MLE in the model
LG_3 <- build_LG_3(fit_LG_3$par)

# Kalman filter
LG_Filt_3 <- dlmFilter(y, LG_3)

# Remove the first item (m_0) in the vector of filtered states
filt_est_3 = dropFirst(LG_Filt_3$m)

# One step ahead forecast
y_filt_3 <- LG_Filt_3$f