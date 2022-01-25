## sensivity analysis - predicting specific organ domain morbidity
sensitivity_organdomain_data <- complete(mids_data, "long", include = TRUE) %>%
  mutate(Infectious_overall = rep(export_data$Infectious.overall, 11),
         Gastrointestinal_overall = rep(export_data$Gastrointestinal.overall,11),
         Diabetes.control = relevel(factor(Diabetes.control), ref = "No diabetes"),
         S02PatientsASAGrade.combined = relevel(factor(S02PatientsASAGrade.combined), ref = "I"),
         Rockwood.combined = relevel(factor(Rockwood.combined), ref = "1")) %>%
  as.mids()

## refit StepAIC models to domain outcome
# fit full model on dataset

# define model
POMS_gastro_full_model <- Gastrointestinal_overall ~ 
  S01Gender + 
  rcs(S01AgeYears.centred, 3) + 
  S02PatientsASAGrade.combined + 
  SORT_severity.combined + 
  ModeSurgeryCombined + 
  S02UrgencyOfSurgery + 
  rcs(S01BMI.centred, 4) + 
  rcs(S02SerumSodium.centred, 4) + 
  rcs(S02SerumPotassium.centred, 3) + 
  rcs(log.Urea.centred, 4) + 
  rcs(log.Creatinine.centred, 4) + 
  rcs(S02SerumAlbumin.centred, 4) + 
  rcs(S02WhiteCellCount.centred, 4) + 
  rcs(S02Haemoglobin.centred, 3) + 
  rcs(S02PulseRate.centred, 4) + 
  rcs(S02SystolicBP.centred, 3) + 
  rcs(S02SpO2.centred, 3) + 
  S02RespiratoryHistoryFindings.combined + 
  S02CardiacHistoryFindings.combined + 
  S02NYHAHeartFailureClassification.combined + 
  S02ECGFindings + 
  S02PatientHadRespiratoryInfectionInTheLastMonth + 
  S02PatientHasHistoryOfCerebrovascularDisease.combined + 
  S02PatientHasDementia + 
  CancerDiagnosis + 
  S03HowManyOperationsInPast30Days.combined + 
  Diabetes.control + 
  Rockwood.combined + 
  S02PatientsSmokingHistory + 
  S02PatientHasLiverDisease.combined + 
  IMD_quintile_adjusted

sens_POMSdomain.fit1 <- glm(POMS_gastro_full_model, complete(sensitivity_organdomain_data,1), family = "binomial")
sens_POMSdomain.fit2 <- glm(POMS_gastro_full_model, complete(sensitivity_organdomain_data,2), family = "binomial")
sens_POMSdomain.fit3 <- glm(POMS_gastro_full_model, complete(sensitivity_organdomain_data,3), family = "binomial")
sens_POMSdomain.fit4 <- glm(POMS_gastro_full_model, complete(sensitivity_organdomain_data,4), family = "binomial")
sens_POMSdomain.fit5 <- glm(POMS_gastro_full_model, complete(sensitivity_organdomain_data,5), family = "binomial")
sens_POMSdomain.fit6 <- glm(POMS_gastro_full_model, complete(sensitivity_organdomain_data,6), family = "binomial")
sens_POMSdomain.fit7 <- glm(POMS_gastro_full_model, complete(sensitivity_organdomain_data,7), family = "binomial")
sens_POMSdomain.fit8 <- glm(POMS_gastro_full_model, complete(sensitivity_organdomain_data,8), family = "binomial")
sens_POMSdomain.fit9 <- glm(POMS_gastro_full_model, complete(sensitivity_organdomain_data,9), family = "binomial")
sens_POMSdomain.fit10 <- glm(POMS_gastro_full_model, complete(sensitivity_organdomain_data,10), family = "binomial")

# Perform bootstrap backwards stepwise selection across multiply imputed datasets

#gastro_sens_stepAIC_1 <- boot.stepAIC(sens_POMSdomain.fit1, complete(sensitivity_organdomain_data,1), B = 500, alpha = 0.05, direction = "backward", 
#                            k = 2, seed = 10062021)
#save(gastro_sens_stepAIC_1, file = "gastro_sens_stepAIC_1.RData")
#rm(gastro_sens_stepAIC_1)

#####====================================================================

#gastro_sens_stepAIC_2 <- boot.stepAIC(sens_POMSdomain.fit2, complete(sensitivity_organdomain_data,2), B = 500, alpha = 0.05, direction = "backward", 
#                                      k = 2, seed = 11062021)
#save(gastro_sens_stepAIC_2, file = "gastro_sens_stepAIC_2.RData")
#rm(gastro_sens_stepAIC_2)

######====================================================================
#
#gastro_sens_stepAIC_3 <- boot.stepAIC(sens_POMSdomain.fit3, complete(sensitivity_organdomain_data,3), B = 500, alpha = 0.05, direction = "backward", 
#                                      k = 2, seed = 12062021)
#save(gastro_sens_stepAIC_3, file = "gastro_sens_stepAIC_3.RData")
#rm(gastro_sens_stepAIC_3)
#
######====================================================================
#
#gastro_sens_stepAIC_4 <- boot.stepAIC(sens_POMSdomain.fit4, complete(sensitivity_organdomain_data,4), B = 500, alpha = 0.05, direction = "backward", 
#                                      k = 2, seed = 13062021)
#save(gastro_sens_stepAIC_4, file = "gastro_sens_stepAIC_4.RData")
#rm(gastro_sens_stepAIC_4)
#
######====================================================================
#
#gastro_sens_stepAIC_5 <- boot.stepAIC(sens_POMSdomain.fit5, complete(sensitivity_organdomain_data,5), B = 500, alpha = 0.05, direction = "backward", 
#                                      k = 2, seed = 14062021)
#save(gastro_sens_stepAIC_5, file = "gastro_sens_stepAIC_5.RData")
#rm(gastro_sens_stepAIC_5)
#
######====================================================================
#
#gastro_sens_stepAIC_6 <- boot.stepAIC(sens_POMSdomain.fit6, complete(sensitivity_organdomain_data,6), B = 500, alpha = 0.05, direction = "backward", 
#                                      k = 2, seed = 15062021)
#save(gastro_sens_stepAIC_6, file = "gastro_sens_stepAIC_6.RData")
#rm(gastro_sens_stepAIC_6)
#
######====================================================================
#
#gastro_sens_stepAIC_7 <- boot.stepAIC(sens_POMSdomain.fit7, complete(sensitivity_organdomain_data,7), B = 500, alpha = 0.05, direction = "backward", 
#                                      k = 2, seed = 16062021)
#save(gastro_sens_stepAIC_7, file = "gastro_sens_stepAIC_7.RData")
#rm(gastro_sens_stepAIC_7)
#
######====================================================================
#
#gastro_sens_stepAIC_8 <- boot.stepAIC(sens_POMSdomain.fit8, complete(sensitivity_organdomain_data,8), B = 500, alpha = 0.05, direction = "backward", 
#                                      k = 2, seed = 17062021)
#save(gastro_sens_stepAIC_8, file = "gastro_sens_stepAIC_8.RData")
#rm(gastro_sens_stepAIC_8)
#
######====================================================================
#
#gastro_sens_stepAIC_9 <- boot.stepAIC(sens_POMSdomain.fit9, complete(sensitivity_organdomain_data,9), B = 500, alpha = 0.05, direction = "backward", 
#                                      k = 2, seed = 18062021)
#save(gastro_sens_stepAIC_9, file = "gastro_sens_stepAIC_9.RData")
#rm(gastro_sens_stepAIC_9)
#
######====================================================================
#
#gastro_sens_stepAIC_10 <- boot.stepAIC(sens_POMSdomain.fit10, complete(sensitivity_organdomain_data,10), B = 500, alpha = 0.05, direction = "backward", 
#                                      k = 2, seed = 19062021)
#save(gastro_sens_stepAIC_10, file = "gastro_sens_stepAIC_10.RData")
#rm(gastro_sens_stepAIC_10)
#
######====================================================================

# load bootStepAIC objects
load("StepAIC/Sensitivity/gastro/gastro_sens_stepAIC_1.RData")
load("StepAIC/Sensitivity/gastro/gastro_sens_stepAIC_2.RData")

gastro_variable.frequency <- full_join(tibble::rownames_to_column(as.data.frame(gastro_sens_stepAIC_1$Covariates), "Variable"),
                                tibble::rownames_to_column(as.data.frame(gastro_sens_stepAIC_2$Covariates), "Variable"), 
                                by = "Variable") %>%
  rename(., 
         imp1 = `(%).x`, 
         imp2 = `(%).y`)

# remove objects to free up RAM
rm(gastro_sens_stepAIC_1, gastro_sens_stepAIC_2)

# load bootStepAIC objects
load("StepAIC/Sensitivity/gastro/gastro_sens_stepAIC_3.RData")
load("StepAIC/Sensitivity/gastro/gastro_sens_stepAIC_4.RData")

gastro_variable.frequency <- full_join(gastro_variable.frequency,
                                tibble::rownames_to_column(as.data.frame(gastro_sens_stepAIC_3$Covariates), "Variable"),
                                by = "Variable") %>%
  full_join(tibble::rownames_to_column(as.data.frame(gastro_sens_stepAIC_4$Covariates), "Variable"),
            by = "Variable") %>%
  rename(., 
         imp3 = `(%).x`, 
         imp4 = `(%).y`)

# remove objects to free up RAM
rm(gastro_sens_stepAIC_3, gastro_sens_stepAIC_4)

# load bootStepAIC objects
load("StepAIC/Sensitivity/gastro/gastro_sens_stepAIC_5.RData")
load("StepAIC/Sensitivity/gastro/gastro_sens_stepAIC_6.RData")

gastro_variable.frequency <- full_join(gastro_variable.frequency,
                                tibble::rownames_to_column(as.data.frame(gastro_sens_stepAIC_5$Covariates), "Variable"),
                                by = "Variable") %>%
  full_join(tibble::rownames_to_column(as.data.frame(gastro_sens_stepAIC_6$Covariates), "Variable"),
            by = "Variable") %>%
  rename(., 
         imp5 = `(%).x`, 
         imp6 = `(%).y`)

# remove objects to free up RAM
rm(gastro_sens_stepAIC_5, gastro_sens_stepAIC_6)

# load bootStepAIC objects
load("StepAIC/Sensitivity/gastro/gastro_sens_stepAIC_7.RData")
load("StepAIC/Sensitivity/gastro/gastro_sens_stepAIC_8.RData")

gastro_variable.frequency <- full_join(gastro_variable.frequency,
                                tibble::rownames_to_column(as.data.frame(gastro_sens_stepAIC_7$Covariates), "Variable"),
                                by = "Variable") %>%
  full_join(tibble::rownames_to_column(as.data.frame(gastro_sens_stepAIC_8$Covariates), "Variable"),
            by = "Variable") %>%
  rename(., 
         imp7 = `(%).x`, 
         imp8 = `(%).y`)

# remove objects to free up RAM
rm(gastro_sens_stepAIC_7, gastro_sens_stepAIC_8)

# load bootStepAIC objects
load("StepAIC/Sensitivity/gastro/gastro_sens_stepAIC_9.RData")
load("StepAIC/Sensitivity/gastro/gastro_sens_stepAIC_10.RData")

gastro_variable.frequency <- full_join(gastro_variable.frequency,
                                tibble::rownames_to_column(as.data.frame(gastro_sens_stepAIC_9$Covariates), "Variable"),
                                by = "Variable") %>%
  full_join(tibble::rownames_to_column(as.data.frame(gastro_sens_stepAIC_10$Covariates), "Variable"),
            by = "Variable") %>%
  rename(., 
         imp9 = `(%).x`, 
         imp10 = `(%).y`)

# remove objects to free up RAM
rm(gastro_sens_stepAIC_9, gastro_sens_stepAIC_10)

#summarise results of each multiply imputation bootstrap model fits:
gastro_variable.frequency.sum <- gastro_variable.frequency %>%
  pivot_longer(., names_to = "imputation", cols = starts_with("imp")) %>% 
  group_by(Variable) %>%
  summarise(mean_frequency = mean(value),
            sd_frequency = sd(value),
            median_frequency = median(value),
            IQR.25 = quantile(value, 0.25),
            IQR.75 = quantile(value, 0.75)) %>%
  arrange(desc(mean_frequency))

#show top 20 rows of variable.frequency.sum
head(gastro_variable.frequency.sum, 20)


### 
## define model
gastrointestinal.model <- Gastrointestinal_overall ~ 
  ModeSurgeryCombined + 
  SORT_severity.combined +
  Rockwood.combined +
  S01Gender +
  S02PatientsASAGrade.combined +
  Diabetes.control +
  rcs(S02SerumSodium.centred, 4)

## fit to gastrointestinal POMS as outcomes variable
gastrointestinal.model.fit <- with(sensitivity_organdomain_data, 
                                   glm(Gastrointestinal_overall ~ 
                                         ModeSurgeryCombined + 
                                         SORT_severity.combined +
                                         Rockwood.combined +
                                         S01Gender +
                                         S02PatientsASAGrade.combined +
                                         Diabetes.control +
                                         rcs(S02SerumSodium.centred, 4), 
                                       family = "binomial"))
pool(gastrointestinal.model.fit)
summary(pool(gastrointestinal.model.fit))

summary(pool(gastrointestinal.model.fit), conf.int = TRUE, exponentiate = TRUE)[,c(1,2,7,8)]

lrm.gastrointestinal.model <- fit.mult.impute(formula = gastrointestinal.model, xtrans = sensitivity_organdomain_data, fitter = lrm,
                                              x=TRUE, y=TRUE)

perf.gastro_poms <- pool_performance(data=complete(sensitivity_organdomain_data, action = "long", include = FALSE), nimp=10,
                                 impvar=".imp", 
                                 Outcome = "Gastrointestinal_overall", predictors = c("ModeSurgeryCombined",
                                                                                      "SORT_severity.combined",
                                                                                      "Rockwood.combined",
                                                                                      "S01Gender",
                                                                                      "S02PatientsASAGrade.combined",
                                                                                      "Diabetes.control",
                                                                                      "rcs(S02SerumSodium.centred, 4)"), 
                                 cal.plot=TRUE, plot.indiv=TRUE, groups_cal = 15)


# set data distribution
dd <- datadist(mice::complete(sensitivity_organdomain_data, "long", include = FALSE))
options(datadist="dd")
## calcuate odds ratios for change in continuous variables
row_names_POMSgastro <- c("S02SerumSodium.centred", "Odds_ratio_sodium",
                           "ModeSurgeryCombined - Opn:Lap","Odds_ratio",
                           "ModeSurgeryCombined - Rob:Lap","Odds_ratio",
                           "SORT_severity.combined - Xma:Min/Int/Maj","Odds_ratio",
                           "SORT_severity.combined - Com:Min/Int/Maj","Odds_ratio",
                           "Rockwood.combined - 2:1","Odds_ratio",
                           "Rockwood.combined - 3:1","Odds_ratio",
                           "Rockwood.combined - 4:1","Odds_ratio",
                           "Rockwood.combined - 5:1","Odds_ratio",
                           "Rockwood.combined - 6-9:1","Odds_ratio",
                           "S01Gender - M:F","Odds_ratio",
                           "S02PatientsASAGrade.combined - II:I","Odds_ratio",
                           "S02PatientsASAGrade.combined - III:I","Odds_ratio",
                           "S02PatientsASAGrade.combined - IV/V:I","Odds_ratio",
                           "Diabetes.control - Diabetes, good control:No diabetes","Odds_ratio",
                           "Diabetes.control - Diabetes, poor control:No diabetes","Odds_ratio")


OR.sodium.gastro <- function(na.adj, model, rowNames) { 
  row_names <- rowNames
  OR <- as_tibble(summary(model, 
                          ModeSurgeryCombined = "Lap",
                          Rockwood.combined = "1",
                          S01Gender = "F",
                          SORT_severity.combined = "Min/Int/Maj",
                          S02PatientsASAGrade.combined = "I",
                          S02SerumSodium.centred = c(na.reference - mids.centred$S02SerumSodium.mean,
                                                     na.adj - mids.centred$S02SerumSodium.mean),
                          Diabetes.control = "No diabetes")) %>%
    mutate(variable = row_names,
           Na = na.adj) %>%
    select(variable, Na, Effect, `Lower 0.95`, `Upper 0.95`) %>%
    filter(variable == "Odds_ratio_sodium")
  
  return(OR)
  
}

na.reference = 140

OddsRatio.na.gastro <- rbind(OR.sodium.gastro(120,lrm.gastrointestinal.model.2, row_names_POMSgastro),
                             OR.sodium.gastro(130,lrm.gastrointestinal.model.2, row_names_POMSgastro), 
                             OR.sodium.gastro(140,lrm.gastrointestinal.model.2, row_names_POMSgastro),
                             OR.sodium.gastro(150,lrm.gastrointestinal.model.2, row_names_POMSgastro),
                             OR.sodium.gastro(160,lrm.gastrointestinal.model.2, row_names_POMSgastro))
