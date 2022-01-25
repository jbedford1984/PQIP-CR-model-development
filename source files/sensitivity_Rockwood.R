## Perform sensitivity analysis to assess impact of missing data on Rockwood Clinical Frailty scale coefficients
## Variable was added in January 2019 to dataset

data_impute_sensitivity_rockwood <- export_data %>% 
  filter(S01DateofSurgery >= lubridate::ymd("20190101")) %>%
  select(POMS.overall, POMS.major, ClavienGradeIIabove,
         S01Gender, S01AgeYears, S02PatientsASAGrade.combined, CancerDiagnosis,
         SORT_severity.combined, ModeSurgeryCombined, S02UrgencyOfSurgery, S01BMI,
         S02SerumSodium, S02SerumPotassium, S02SerumUrea, S02SerumCreatinine, 
         S02WhiteCellCount, S02SerumAlbumin,
         S02Haemoglobin, S02PulseRate, S02SystolicBP, S02SpO2, 
         S02RespiratoryHistoryFindings.combined, S02CardiacHistoryFindings.combined,
         S02NYHAHeartFailureClassification.combined, S02ECGFindings, 
         S02PatientHadRespiratoryInfectionInTheLastMonth,
         S02PatientHasHistoryOfCerebrovascularDisease.combined, S02PatientHasDementia, S03HowManyOperationsInPast30Days.combined, Diabetes, S02HbA1c, S02PatientsSmokingHistory,
         S02PatientHasLiverDisease.combined,
         S14LookAfterPersonalToiletHygieneUnaided,S14AbleToBreatheEasily,
         S14WorkOrUndertakeUsualHomeActivities, 
         EQ5D01Mobility, EQ5D02Selfcare, EQ5D03UsualActivities,
         S15StandingForLongPeriodsSuchAs30Minutes,S15TakingCareOfHouseholdResponsibilities,
         S15HowMuchProblemJoiningCommunityActivitiesInTheSameWayAsAnyoneElseCan,
         S15WalkingALongDistanceSuchAsAKilometreOrEquivalent, S15WashingYourWholeBody,
         S15GettingDressed, Rockwood.combined, SiteName, S01PostcodeOut,
         IMD_quintile_adjusted) %>%
  mutate(log.Urea = log(S02SerumUrea),
         log.Creatinine = log(S02SerumCreatinine),
         IMD_quintile_adjusted = recode(IMD_quintile_adjusted, 
                                        `1` = "5 - least deprived",
                                        `2` = "4",
                                        `3` = "3",
                                        `4` = "2",
                                        `5` = "1 - most deprived"),
         IMD_quintile_adjusted = relevel(IMD_quintile_adjusted, ref = "5 - least deprived"),
         S15StandingForLongPeriodsSuchAs30Minutes = factor(S15StandingForLongPeriodsSuchAs30Minutes),
         S15TakingCareOfHouseholdResponsibilities = factor(S15TakingCareOfHouseholdResponsibilities),
         S15HowMuchProblemJoiningCommunityActivitiesInTheSameWayAsAnyoneElseCan = factor(S15HowMuchProblemJoiningCommunityActivitiesInTheSameWayAsAnyoneElseCan),
         S15WalkingALongDistanceSuchAsAKilometreOrEquivalent = factor(S15WalkingALongDistanceSuchAsAKilometreOrEquivalent),
         S15WashingYourWholeBody = factor(S15WashingYourWholeBody),
         S15GettingDressed = factor(S15GettingDressed)) %>%
  select(c(-S02SerumUrea, -S02SerumCreatinine))

#mids_sensitivity_rockwood <- parlmice(data_impute_sensitivity_rockwood, m=10, cluster.seed = 26052021, ncore = 5)
#save(mids_sensitivity_rockwood, file = "mids_sensitivity_rockwood.RData")
load("mids_sensitivity_rockwood.RData")

mids_data_sensitivity_rockwood <- complete(mids_sensitivity_rockwood, action = "long", include = TRUE) %>%
  mutate(Diabetes.control = ifelse(Diabetes == "N", "No diabetes",
                                   ifelse(Diabetes == "Y" & S02HbA1c <= 8.5, "Diabetes, good control", 
                                          ifelse(Diabetes == "Y" & S02HbA1c >8.5, "Diabetes, poor control",NA)))) %>%
  select(c(-S14LookAfterPersonalToiletHygieneUnaided,S14AbleToBreatheEasily,
           -S14WorkOrUndertakeUsualHomeActivities,
           -EQ5D01Mobility, 
           -EQ5D02Selfcare, 
           -EQ5D03UsualActivities,
           -S15StandingForLongPeriodsSuchAs30Minutes,
           -S15TakingCareOfHouseholdResponsibilities,
           -S15HowMuchProblemJoiningCommunityActivitiesInTheSameWayAsAnyoneElseCan,
           -S15WalkingALongDistanceSuchAsAKilometreOrEquivalent, 
           -S15WashingYourWholeBody,
           -S15GettingDressed,
           -S01PostcodeOut)) %>%
  as.mids(.)

mids.centred_sensitivity_rockwood<- complete(mids_data_sensitivity_rockwood, action = "long", include = FALSE) %>%
  summarise(S01AgeYears.mean = round(mean(S01AgeYears),0),
            S01BMI.mean = round(mean(S01BMI),0),
            S02SerumSodium.mean = round(mean(S02SerumSodium),0),
            S02SerumPotassium.mean = round(mean(S02SerumPotassium),1),
            log.Urea.mean = round(mean(log.Urea),1),
            log.Creatinine.mean = round(mean(log.Creatinine),1),
            S02SerumAlbumin.mean = round(mean(S02SerumAlbumin),0),
            S02WhiteCellCount.mean = round(mean(S02WhiteCellCount),1),
            S02Haemoglobin.mean = round(mean(S02Haemoglobin),1),
            S02PulseRate.mean = round(mean(S02PulseRate),0),
            S02SystolicBP.mean = round(mean(S02SystolicBP),0),
            S02SpO2.mean = round(mean(S02SpO2),0))

mids_data_sensitivity_rockwood <- complete(mids_data_sensitivity_rockwood, action = "long", include = TRUE) %>%
  mutate(S02PatientsSmokingHistory = relevel(S02PatientsSmokingHistory, ref = "NS")) %>%
  mutate(S01AgeYears.centred = S01AgeYears - mids.centred_sensitivity_rockwood$S01AgeYears.mean,
         S01BMI.centred  = S01BMI - mids.centred_sensitivity_rockwood$S01BMI.mean,
         S02SerumSodium.centred  = S02SerumSodium - mids.centred_sensitivity_rockwood$S02SerumSodium.mean,
         S02SerumPotassium.centred  = S02SerumPotassium - mids.centred_sensitivity_rockwood$S02SerumPotassium.mean,
         log.Urea.centred  = log.Urea - mids.centred_sensitivity_rockwood$log.Urea.mean,
         log.Creatinine.centred  = log.Creatinine - mids.centred_sensitivity_rockwood$log.Creatinine.mean,
         S02SerumAlbumin.centred  = S02SerumAlbumin - mids.centred_sensitivity_rockwood$S02SerumAlbumin.mean,
         S02WhiteCellCount.centred  = S02WhiteCellCount - mids.centred_sensitivity_rockwood$S02WhiteCellCount.mean,
         S02Haemoglobin.centred  = S02Haemoglobin - mids.centred_sensitivity_rockwood$S02Haemoglobin.mean,
         S02PulseRate.centred  = S02PulseRate - mids.centred_sensitivity_rockwood$S02PulseRate.mean,
         S02SystolicBP.centred  = S02SystolicBP - mids.centred_sensitivity_rockwood$S02SystolicBP.mean,
         S02SpO2.centred  = S02SpO2 - mids.centred_sensitivity_rockwood$S02SpO2.mean) %>%
  as.mids(.) 



#fit full model on dataset
model.fit1.rockwood <- glm(full.model, complete(mids_data_sensitivity_rockwood,1), family = "binomial")
model.fit2.rockwood <- glm(full.model, complete(mids_data_sensitivity_rockwood,2), family = "binomial")
model.fit3.rockwood <- glm(full.model, complete(mids_data_sensitivity_rockwood,3), family = "binomial")
model.fit4.rockwood <- glm(full.model, complete(mids_data_sensitivity_rockwood,4), family = "binomial")
model.fit5.rockwood <- glm(full.model, complete(mids_data_sensitivity_rockwood,5), family = "binomial")
model.fit6.rockwood <- glm(full.model, complete(mids_data_sensitivity_rockwood,6), family = "binomial")
model.fit7.rockwood <- glm(full.model, complete(mids_data_sensitivity_rockwood,7), family = "binomial")
model.fit8.rockwood <- glm(full.model, complete(mids_data_sensitivity_rockwood,8), family = "binomial")
model.fit9.rockwood <- glm(full.model, complete(mids_data_sensitivity_rockwood,9), family = "binomial")
model.fit10.rockwood <- glm(full.model, complete(mids_data_sensitivity_rockwood,10), family = "binomial")

# Perform bootstrap backwards stepwise selection across multiply imputed datasets
stepAIC.MI1.sensitivity.rockwood <- boot.stepAIC(model.fit1.rockwood, complete(mids_data_sensitivity_rockwood,1), B = 500, alpha = 0.05, direction = "backward", 
                                                 k = 2, seed = 10062021)
save(stepAIC.MI1.sensitivity.rockwood, file = "stepAIC_1_rockwood.RData")
rm(stepAIC.MI1.sensitivity.rockwood)

stepAIC.MI2.sensitivity.rockwood <- boot.stepAIC(model.fit2.rockwood, complete(mids_data_sensitivity_rockwood,2), B = 500, alpha = 0.05, direction = "backward", 
                                                 k = 2, verbose = TRUE, seed = 11062021)

save(stepAIC.MI2.sensitivity.rockwood, file = "stepAIC_2_rockwood.RData")
rm(stepAIC.MI2.sensitivity.rockwood)

stepAIC.MI3.sensitivity.rockwood <- boot.stepAIC(model.fit3.rockwood, complete(mids_data_sensitivity_rockwood,3), B = 500, alpha = 0.05, direction = "backward", 
                                                 k = 2, verbose = TRUE, seed = 12062021)
save(stepAIC.MI3.sensitivity.rockwood, file = "stepAIC_3_rockwood.RData")
rm(stepAIC.MI3.sensitivity.rockwood)

stepAIC.MI4.sensitivity.rockwood <- boot.stepAIC(model.fit4.rockwood, complete(mids_data_sensitivity_rockwood,4), B = 500, alpha = 0.05, direction = "backward", 
                                                 k = 2, verbose = TRUE, seed = 13062021)
save(stepAIC.MI4.sensitivity.rockwood, file = "stepAIC_4_rockwood.RData")
rm(stepAIC.MI4.sensitivity.rockwood)

stepAIC.MI5.sensitivity.rockwood <- boot.stepAIC(model.fit5.rockwood, complete(mids_data_sensitivity_rockwood,5), B = 500, alpha = 0.05, direction = "backward", 
                                                 k = 2, verbose = TRUE, seed = 14062021)
save(stepAIC.MI5.sensitivity.rockwood, file = "stepAIC_5_rockwood.RData")
rm(stepAIC.MI5.sensitivity.rockwood)

stepAIC.MI6.sensitivity.rockwood <- boot.stepAIC(model.fit6.rockwood, complete(mids_data_sensitivity_rockwood,6), B = 500, alpha = 0.05, direction = "backward",              k = 2, verbose = TRUE, seed = 15062021)
save(stepAIC.MI6.sensitivity.rockwood, file = "stepAIC_6_rockwood.RData")
rm(stepAIC.MI6.sensitivity.rockwood)

stepAIC.MI7.sensitivity.rockwood <- boot.stepAIC(model.fit7.rockwood, complete(mids_data_sensitivity_rockwood,7), B = 500, alpha = 0.05, direction = "backward", 
                                                 k = 2, verbose = TRUE, seed = 16062021)
save(stepAIC.MI7.sensitivity.rockwood, file = "stepAIC_7_rockwood.RData")
rm(stepAIC.MI7.sensitivity.rockwood)

stepAIC.MI8.sensitivity.rockwood <- boot.stepAIC(model.fit8.rockwood, complete(mids_data_sensitivity_rockwood,8), B = 500, alpha = 0.05, direction = "backward", 
                                                 k = 2, verbose = TRUE, seed = 17062021)
save(stepAIC.MI8.sensitivity.rockwood, file = "stepAIC_8_rockwood.RData")
rm(stepAIC.MI8.sensitivity.rockwood)

stepAIC.MI9.sensitivity.rockwood <- boot.stepAIC(model.fit9.rockwood, complete(mids_data_sensitivity_rockwood,9), B = 500, alpha = 0.05, direction = "backward", 
                                                 k = 2, verbose = TRUE, seed = 18062021)
save(stepAIC.MI9.sensitivity.rockwood, file = "stepAIC_9_rockwood.RData")
rm(stepAIC.MI9.sensitivity.rockwood)

stepAIC.MI10.sensitivity.rockwood <- boot.stepAIC(model.fit10.rockwood, complete(mids_data_sensitivity_rockwood,10), B = 500, alpha = 0.05, direction = "backward", 
                                                  k = 2, verbose = TRUE, seed = 19062021)

# Save multiple objects
save(stepAIC.MI10.sensitivity.rockwood, file = "stepAIC_10_rockwood.RData")
rm(stepAIC.MI10.sensitivity.rockwood)


# load bootStepAIC objects
load("StepAIC/Sensitivity/rockwood_preimputation/rcs_3_4/stepAIC_1_rockwood_rcs3_4.RData")
load("StepAIC/Sensitivity/rockwood_preimputation/rcs_3_4/stepAIC_2_rockwood_rcs3_4.RData")

variable.frequency.sensitivity.rockwood <- full_join(tibble::rownames_to_column(as.data.frame(stepAIC.MI1.sensitivity.rockwood$Covariates), "Variable"),
                                                     tibble::rownames_to_column(as.data.frame(stepAIC.MI2.sensitivity.rockwood$Covariates), "Variable"), 
                                                     by = "Variable") %>%
  rename(., 
         imp1 = `(%).x`, 
         imp2 = `(%).y`)

# remove objects to free up RAM
rm(stepAIC.MI1.sensitivity.rockwood, stepAIC.MI2.sensitivity.rockwood)

# load bootStepAIC objects
load("StepAIC/Sensitivity/rockwood_preimputation/rcs_3_4/stepAIC_3_rockwood_rcs3_4.RData")
load("StepAIC/Sensitivity/rockwood_preimputation/rcs_3_4/stepAIC_4_rockwood_rcs3_4.RData")

variable.frequency.sensitivity.rockwood <- full_join(variable.frequency.sensitivity.rockwood,
                                                     tibble::rownames_to_column(as.data.frame(stepAIC.MI3.sensitivity.rockwood$Covariates), "Variable"),
                                                     by = "Variable") %>%
  full_join(tibble::rownames_to_column(as.data.frame(stepAIC.MI4.sensitivity.rockwood$Covariates), "Variable"),
            by = "Variable") %>%
  rename(., 
         imp3 = `(%).x`, 
         imp4 = `(%).y`)

# remove objects to free up RAM
rm(stepAIC.MI3.sensitivity.rockwood, stepAIC.MI4.sensitivity.rockwood)

# load bootStepAIC objects
load("StepAIC/Sensitivity/rockwood_preimputation/rcs_3_4/stepAIC_5_rockwood_rcs3_4.RData")
load("StepAIC/Sensitivity/rockwood_preimputation/rcs_3_4/stepAIC_6_rockwood_rcs3_4.RData")

variable.frequency.sensitivity.rockwood <- full_join(variable.frequency.sensitivity.rockwood,
                                                     tibble::rownames_to_column(as.data.frame(stepAIC.MI5.sensitivity.rockwood$Covariates), "Variable"),
                                                     by = "Variable") %>%
  full_join(tibble::rownames_to_column(as.data.frame(stepAIC.MI6.sensitivity.rockwood$Covariates), "Variable"),
            by = "Variable") %>%
  rename(., 
         imp5 = `(%).x`, 
         imp6 = `(%).y`)

# remove objects to free up RAM
rm(stepAIC.MI5.sensitivity.rockwood, stepAIC.MI6.sensitivity.rockwood)

# load bootStepAIC objects
load("StepAIC/Sensitivity/rockwood_preimputation/rcs_3_4/stepAIC_7_rockwood_rcs3_4.RData")
load("StepAIC/Sensitivity/rockwood_preimputation/rcs_3_4/stepAIC_8_rockwood_rcs3_4.RData")

variable.frequency.sensitivity.rockwood <- full_join(variable.frequency.sensitivity.rockwood,
                                                     tibble::rownames_to_column(as.data.frame(stepAIC.MI7.sensitivity.rockwood$Covariates), "Variable"),
                                                     by = "Variable") %>%
  full_join(tibble::rownames_to_column(as.data.frame(stepAIC.MI8.sensitivity.rockwood$Covariates), "Variable"),
            by = "Variable") %>%
  rename(., 
         imp7 = `(%).x`, 
         imp8 = `(%).y`)

# remove objects to free up RAM
rm(stepAIC.MI7.sensitivity.rockwood, stepAIC.MI8.sensitivity.rockwood)

# load bootStepAIC objects
load("StepAIC/Sensitivity/rockwood_preimputation/rcs_3_4/stepAIC_9_rockwood_rcs3_4.RData")
load("StepAIC/Sensitivity/rockwood_preimputation/rcs_3_4/stepAIC_10_rockwood_rcs3_4.RData")

variable.frequency.sensitivity.rockwood <- full_join(variable.frequency.sensitivity.rockwood,
                                                     tibble::rownames_to_column(as.data.frame(stepAIC.MI9.sensitivity.rockwood$Covariates), "Variable"),
                                                     by = "Variable") %>%
  full_join(tibble::rownames_to_column(as.data.frame(stepAIC.MI10.sensitivity.rockwood$Covariates), "Variable"),
            by = "Variable") %>%
  rename(., 
         imp9 = `(%).x`, 
         imp10 = `(%).y`)

# remove objects to free up RAM
rm(stepAIC.MI9.sensitivity.rockwood, stepAIC.MI10.sensitivity.rockwood)

#summarise results of each multiply imputation bootstrap model fits:
variable.frequency.sum.rockwood <- variable.frequency.sensitivity.rockwood %>%
  pivot_longer(., names_to = "imputation", cols = starts_with("imp")) %>% 
  group_by(Variable) %>%
  summarise(mean_frequency = mean(value),
            sd_frequency = sd(value),
            median_frequency = median(value),
            IQR.25 = quantile(value, 0.25),
            IQR.75 = quantile(value, 0.75)) %>%
  arrange(desc(mean_frequency))

#show top 20 rows of variable.frequency.sum
head(variable.frequency.sum.rockwood, 20)

####
lrm.mult.impute.sensitivity.rockwood <- fit.mult.impute(formula = final.model, 
                                                        xtrans = mids_data_sensitivity_rockwood, 
                                                        fitter = lrm,
                                                        x=TRUE, y=TRUE)

nrow(complete(mids_data_sensitivity_rockwood,0))

final.model.fit.rockwood <- with(mids_data_sensitivity_rockwood, 
                                 glm(POMS.overall ~
                                       ModeSurgeryCombined +
                                       Rockwood.combined + 
                                       S01Gender +
                                       SORT_severity.combined + 
                                       S02PatientsASAGrade.combined + 
                                       rcs(S01BMI.centred,4) + 
                                       rcs(S02SerumSodium.centred, 4) + 
                                       IMD_quintile_adjusted + 
                                       rcs(log.Creatinine.centred, 4) + 
                                       rcs(S02WhiteCellCount.centred, 4) + 
                                       rcs(log.Urea.centred, 4) + 
                                       rcs(S01AgeYears.centred, 3), 
                                     family = "binomial"))
est1.rockwood <- pool(final.model.fit.rockwood)
summary(est1.rockwood)

summary(est1.rockwood, conf.int = TRUE, exponentiate = TRUE)[,c(1,2,7,8)]


(nrow(complete(mids_data_sensitivity_rockwood,0) %>%
        filter(is.na(Rockwood.combined))) / nrow(complete(mids_data_sensitivity_rockwood,0)))*100


missing_data_summary.rockwood <- complete(mids_data_sensitivity_rockwood,0) %>%
  summarise(Age = sum(is.na(S01AgeYears))/nrow(.),
            Sex = sum(is.na(S01Gender))/nrow(.),
            ASA = sum(is.na(S02PatientsASAGrade.combined))/nrow(.),
            Cancer = sum(is.na(CancerDiagnosis))/nrow(.),
            SORT = sum(is.na(SORT_severity.combined))/nrow(.),
            Mode = sum(is.na(ModeSurgeryCombined))/nrow(.),
            Urgency = sum(is.na(S02UrgencyOfSurgery))/nrow(.),
            BMI = sum(is.na(S01BMI))/nrow(.),
            Na = sum(is.na(S02SerumSodium))/nrow(.),
            K = sum(is.na(S02SerumPotassium))/nrow(.),
            WCC = sum(is.na(S02WhiteCellCount))/nrow(.),
            Albumin = sum(is.na(S02SerumAlbumin))/nrow(.),
            Hb = sum(is.na(S02Haemoglobin))/nrow(.),
            Pulse = sum(is.na(S02PulseRate))/nrow(.),
            BP = sum(is.na(S02SystolicBP))/nrow(.),
            SpO2 = sum(is.na(S02SpO2))/nrow(.),
            RespHx = sum(is.na(S02RespiratoryHistoryFindings.combined))/nrow(.),
            CardioHx = sum(is.na(S02CardiacHistoryFindings.combined))/nrow(.),
            NYHA = sum(is.na(S02NYHAHeartFailureClassification.combined))/nrow(.),
            ECG = sum(is.na(S02ECGFindings))/nrow(.),
            RespInf = sum(is.na(S02PatientHadRespiratoryInfectionInTheLastMonth))/nrow(.),
            CVA = sum(is.na(S02PatientHasHistoryOfCerebrovascularDisease.combined))/nrow(.),
            Dementia = sum(is.na(S02PatientHasDementia))/nrow(.),
            Ops30 = sum(is.na(S03HowManyOperationsInPast30Days.combined))/nrow(.),
            Diabetes = sum(Diabetes == "Y" & is.na(S02HbA1c))/nrow(.),
            Smoking = sum(is.na(S02PatientsSmokingHistory))/nrow(.),
            Liver = sum(is.na(S02PatientHasLiverDisease.combined))/nrow(.),
            Rockwood = sum(is.na(Rockwood.combined))/nrow(.),
            IMD = sum(is.na(IMD_quintile_adjusted))/nrow(.),
            Urea = sum(is.na(log.Urea))/nrow(.),
            Creat = sum(is.na(log.Creatinine))/nrow(.)) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "percent_missing") %>%
  mutate(n_missing = percent_missing*5241,
         percent_missing = round(percent_missing*100,2))

summary_table_rockwood <- summary_variable_POMS_factor(mids_data_sensitivity_rockwood, Rockwood.combined)
summary_table_rockwood