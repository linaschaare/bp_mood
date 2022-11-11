# E-statistic inspection for unmeasured confounding bias
-----------------------------------------------------------
  
### Install and load Evalue package
#install.packages("EValue")
library(EValue)


# select variables
dat <- dplyr::select(ukb_lbl, f.eid, depr_c.0, wb.0, mean_sbp.0, high_bp.0, htn_meds_count.0,
                     f.21003.0.0, f.31.0.0, angina.0, heartattack.0, f.2443.0.0, depr_l.0, 
                     f.21001.0.0, mean_hr.0)
dat$f.31.0.0 <- as.numeric(dat$f.31.0.0)
dat$f.2443.0.0 <- as.numeric(dat$f.2443.0.0)

# define predictors and covariates
predictors <- c("mean_sbp.0", "high_bp.0", "htn_meds_count.0")
covs <- c("f.21003.0.0", "f.31.0.0", "angina.0", "heartattack.0", "f.2443.0.0", "depr_l.0",
          "f.21001.0.0", "mean_hr.0")

### depressive mood
outcome <- "depr_c.0"
dat_scaled <- data.frame(scale(na.omit(dplyr::select(dat, depr_c.0, mean_sbp.0, high_bp.0,
                                                     htn_meds_count.0, f.21003.0.0, f.31.0.0, 
                                                     angina.0, heartattack.0, f.2443.0.0, depr_l.0,
                                                     f.21001.0.0, mean_hr.0))))


mdl <- as.formula(paste(paste(outcome), paste(paste(predictors, collapse = " + "), 
                                              paste(covs, collapse = " + "), sep = " + "), 
                        sep = " ~ "))
mdl_fit <- lm(mdl, data=dat_scaled)
summary(mdl_fit)

# compute E-value for SBP
# use residual SD to avoid conservatism
evalues.OLS(est = abs(mdl_fit$coefficients[2]),
            se = summary(mdl_fit)$coefficients['mean_sbp.0', 'Std. Error'],
            sd = summary(mdl_fit)$sigma)

# compute E-value for HTN
# use residual SD to avoid conservatism
evalues.OLS(est = abs(mdl_fit$coefficients[3]),
            se = summary(mdl_fit)$coefficients['high_bp.0', 'Std. Error'],
            sd = summary(mdl_fit)$sigma)

# compute E-value for lifetime depression as reference for magnitude of effects
evalues.OLS(est = abs(mdl_fit$coefficients[10]),
            se = summary(mdl_fit)$coefficients['depr_l.0', 'Std. Error'],
            sd = summary(mdl_fit)$sigma)

### well-being
outcome <- "wb.0"
dat_scaled <- data.frame(scale(na.omit(dplyr::select(dat, wb.0, mean_sbp.0, high_bp.0,
                                                     htn_meds_count.0, f.21003.0.0, f.31.0.0, 
                                                     angina.0, heartattack.0, f.2443.0.0, depr_l.0,
                                                     f.21001.0.0, mean_hr.0))))


mdl <- as.formula(paste(paste(outcome), paste(paste(predictors, collapse = " + "), 
                                              paste(covs, collapse = " + "), sep = " + "), 
                        sep = " ~ "))
mdl_fit <- lm(mdl, data=dat_scaled)
summary(mdl_fit)

# compute E-value for SBP
# use residual SD to avoid conservatism
evalues.OLS(est = abs(mdl_fit$coefficients[2]),
            se = summary(mdl_fit)$coefficients['mean_sbp.0', 'Std. Error'],
            sd = summary(mdl_fit)$sigma)

# compute E-value for HTN
# use residual SD to avoid conservatism
evalues.OLS(est = abs(mdl_fit$coefficients[3]),
            se = summary(mdl_fit)$coefficients['high_bp.0', 'Std. Error'],
            sd = summary(mdl_fit)$sigma)
