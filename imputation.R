# Missing value inspection for selection and survival bias
-----------------------------------------------------------
  
### Install and load MICE package
#install.packages("mice")
library(mice)


### Visualise missing value pattern
dat <- dplyr::select(ukb_lbl, depr_c.0, wb.0, mean_sbp.0, high_bp.0,
                     htn_meds_count.0, f.21003.0.0, f.31.0.0, 
                     angina.0, heartattack.0, f.2443.0.0, depr_l.0,
                     f.21001.0.0, mean_hr.0)

md.pattern(dat)

### Impute data
dat <- dplyr::select(ukb_lbl, depr_c.0, mean_sbp.0, high_bp.0,
                     htn_meds_count.0, f.21003.0.0, f.31.0.0, 
                     angina.0, heartattack.0, f.2443.0.0, depr_l.0,
                     f.21001.0.0, mean_hr.0)

imputed_dat <- mice(dat, m=20, method = "pmm", seed = 161)
summary(imputed_dat)
imputed_dat$imp


# define predictors and covariates
predictors <- c("mean_sbp.0", "high_bp.0", "htn_meds_count.0")
covs <- c("f.21003.0.0", "f.31.0.0", "angina.0", "heartattack.0", "f.2443.0.0", "depr_l.0",
          "f.21001.0.0", "mean_hr.0")

## depressive mood
outcome <- "depr_c.0"

mdl <- as.formula(paste(paste(outcome), paste(paste(predictors, collapse = " + "), 
                                              paste(covs, collapse = " + "), sep = " + "), 
                        sep = " ~ "))
fit <- imputed_dat %>%
  mice::complete("all") %>%
  map(lm, formula = mdl) %>%
  pool()
summary(fit)


## well-being
dat <- dplyr::select(ukb_lbl, wb.0, mean_sbp.0, high_bp.0,
                     htn_meds_count.0, f.21003.0.0, f.31.0.0, 
                     angina.0, heartattack.0, f.2443.0.0, depr_l.0,
                     f.21001.0.0, mean_hr.0)

imputed_dat_wb <- mice(dat, m=20, method = "pmm", seed = 161)
summary(imputed_dat_wb)
imputed_dat_wb$imp

outcome <- "wb.0"

mdl <- as.formula(paste(paste(outcome), paste(paste(predictors, collapse = " + "), 
                                              paste(covs, collapse = " + "), sep = " + "), 
                        sep = " ~ "))

fit_wb <- imputed_dat_wb %>%
  mice::complete("all") %>%
  map(lm, formula = mdl) %>%
  pool()
summary(fit_wb)




### Assess selection bias
dat$missing_data <- ifelse(rowSums(is.na(dat)) > 0, "Missing data", "No missing data")

table1(~ f.21003.0.0 + f.31.0.0 + mean_sbp.0 + as.factor(high_bp.0) | missing_data, 
       data=subset(dat))

# t.test(dat$mean_sbp.0 ~ factor(dat$missing_data))
# t.test(dat$f.21003.0.0 ~ factor(dat$missing_data))

# na.1 <- dat[rowSums(is.na(dat)) > 0, ] # sample with NA in any row
# na.0 <- dat[rowSums(is.na(dat)) == 0, ] # sample without NA

### Predict missing data from variables
# prepare data
dat$missing_data_num <- ifelse(rowSums(is.na(dat)) > 0, 1, 0)
dat$f.31.0.0 <- as.numeric(dat$f.31.0.0)
dat$f.2443.0.0 <- as.numeric(dat$f.2443.0.0)

outcome <- "missing_data_num"
predictors <- c("mean_sbp.0", "high_bp.0", "htn_meds_count.0")
covs <- c("f.21003.0.0", "f.31.0.0", "f.2443.0.0", "f.21001.0.0", "mean_hr.0")
dat_scaled <- data.frame(scale(na.omit(dplyr::select(dat, missing_data_num, mean_sbp.0, high_bp.0,
                                                     htn_meds_count.0, f.21003.0.0, f.31.0.0, 
                                                     f.2443.0.0,
                                                     f.21001.0.0, mean_hr.0))))

# with predictors
mdl <- as.formula(paste(paste(outcome), paste(paste(predictors, collapse = " + "), 
                                              paste(covs, collapse = " + "), sep = " + "), 
                        sep = " ~ "))
mdl_missing <- lm(mdl, data=dat)
summary(mdl_missing)

# plot and nicer summary
summ(mdl_missing, confint = TRUE, digits = 3)

# forest plots
cfs = c("Systolic blood pressure" = "mean_sbp.0",
        "Diagnosed hypertension" = "high_bp.0",
        "No. antihypertensive medication" = "htn_meds_count.0",
        "Age" = "f.21003.0.0",
        "Gender" = "f.31.0.0",
        "Diabetes" = "f.2443.0.0",
        "BMI" = "f.21001.0.0",
        "Heart rate" = "mean_hr.0")

plot_summs(mdl_missing,
           coefs = cfs, model.names = c("Missing data"),
           legend.title = "Outcome",
           colors = "Qual1") + labs(x="Standardized Beta") + xlim(c(-0.25, 0.25)) +
  theme(text = element_text(size=16),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        legend.position = "top"
  )
ggsave("man/FIG/forest_plot_missing.png", width = 14, height = 5, device='png', dpi=600)
