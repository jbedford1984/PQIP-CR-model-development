# Check calibration of POMS model for alternative outcomes
calibration_data_sensitivity_POMSmajor <- mice::complete(mids_data, "long", include = TRUE) %>% 
  as_tibble() %>%
  mutate(POMS.logit = -3.3081876 +
           0.71919942*(ModeSurgeryCombined=="Opn") +
           0.34618954*(ModeSurgeryCombined=="Rob") +
           0.2551226*(Rockwood.combined=="2") +
           0.51657126*(Rockwood.combined=="3") +
           0.70509523*(Rockwood.combined=="4") +
           0.8082908*(Rockwood.combined=="5") +
           0.90823978*(Rockwood.combined=="6-9") +
           0.30969644*(S01Gender=="M") +
           0.17454359*(SORT_severity.combined=="Xma") +
           0.56964698*(SORT_severity.combined=="Com") +
           0.15952372*(S02PatientsASAGrade.combined=="II") +
           0.42110205*(S02PatientsASAGrade.combined=="III") +
           0.44385062*(S02PatientsASAGrade.combined=="IV/V") +
           -0.022455417* S01BMI.centred +
           0.00058549124*pmax(S01BMI.centred+7.99,0)^3 +
           -0.0017445337*pmax(S01BMI.centred+2.92,0)^3 +
           0.0013330039*pmax(S01BMI.centred-0.9,0)^3 +
           -0.00017396144*pmax(S01BMI.centred-9.2875,0)^3 +
           -0.029228382* S02SerumSodium.centred +
           -0.00012250672*pmax(S02SerumSodium.centred+6,0)^3 +
           0.0034580052*pmax(S02SerumSodium.centred+1,0)^3 +
           -0.0053549862*pmax(S02SerumSodium.centred-1,0)^3 +
           0.0020194878*pmax(S02SerumSodium.centred-4,0)^3 +
           0.030910885*(IMD_quintile_adjusted=="4") +
           -0.014178209*(IMD_quintile_adjusted=="3") +
           0.17637341*(IMD_quintile_adjusted=="2") +
           0.15955264*(IMD_quintile_adjusted=="1 - most deprived") +
           -0.78754659* log.Creatinine.centred +
           4.3784501*pmax(log.Creatinine.centred+0.34875628,0)^3 +
           -14.909756*pmax(log.Creatinine.centred+0.065893495,0)^3 +
           12.759045*pmax(log.Creatinine.centred-0.11884061,0)^3 +
           -2.2277398*pmax(log.Creatinine.centred-0.43619845,0)^3 +
           -0.010351246* S02WhiteCellCount.centred +
           0.0021637774*pmax(S02WhiteCellCount.centred+3.2,0)^3 +
           -0.0048087225*pmax(S02WhiteCellCount.centred+1.2,0)^3 +
           0.0026861286*pmax(S02WhiteCellCount.centred-0.5,0)^3 +
           -0.000041183419*pmax(S02WhiteCellCount.centred-4.6,0)^3 +
           -0.5855539* log.Urea.centred +
           0.73580682*pmax(log.Urea.centred+0.53528926,0)^3 +
           -2.4752866*pmax(log.Urea.centred+0.073943697,0)^3 +
           1.8187039*pmax(log.Urea.centred-0.14046617,0)^3 +
           -0.079224044*pmax(log.Urea.centred-0.56332303,0)^3 +
           0.0017750254* S01AgeYears.centred +
           0.0000026814324*pmax(S01AgeYears.centred+20,0)^3 +
           -0.0000074255052*pmax(S01AgeYears.centred-3,0)^3 +
           0.0000047440728*pmax(S01AgeYears.centred-16,0)^3,
         POMS.pred = inv.logit(POMS.logit))%>%
  select(.imp, .id, POMS.major, ClavienGradeIIabove,  
         POMS.logit, POMS.pred, POMS.major_Wound, POMS.major_Infection, POMS.major_Cardio,
         POMS.major_Resp, POMS.major_Renal, POMS.major_Pain) %>%
  as.mids()


##=============================================================
## POMS major
##=============================================================

model_POMS.major <- POMS.major ~ POMS.logit


## Fit model to derive linear shrinkage factors:
lrm.mult.impute.POMSmajor <- fit.mult.impute(formula = POMS.major ~ 
                                                         POMS.logit,
                                                       xtrans = calibration_data_sensitivity_POMSmajor, fitter = lrm,
                                                       x=TRUE, y=TRUE)

POMSmajor.fit <- with(calibration_data_sensitivity_POMSmajor, 
                      glm(POMS.major ~ POMS.logit, 
                                       family = "binomial"))

# model estimates on log odds scale
pool(POMSmajor.fit)


perf.POMSmajor <- pool_performance(data=complete(calibration_data_sensitivity_POMSmajor, action = "long", include = FALSE), 
                                             nimp=10,
                                             impvar=".imp", 
                                             Outcome = "POMS.major", predictors = c("POMS.logit"), 
                                             cal.plot=TRUE, plot.indiv=TRUE, groups_cal = 15)

##Validation
POMS_major_validate <- extract_fits_validate(calibration_data_sensitivity_POMSmajor, M=10, model_POMS.major, seed = 01062022)

POMS_major_data <- validation_calculations(validation_data = POMS_major_validate, 
                                                original_mids_object = calibration_data_sensitivity_POMSmajor, 
                                                model = model_POMS.major) 


# calculate mean model performance estimates
mean(POMS_major_data$optimism_corrected_c_stat)
mean(POMS_major_data$slope_index_corrected)
mean(POMS_major_data$intercept_optimism)

# create datatable of model discrimination across imputed datasets, prior to pooling using the pool_auc function
c_stat_data_POMSmajor <- POMS_major_data %>%
  group_by(.imp) %>%
  dplyr::summarise(mean_opt_corr_C = mean(optimism_corrected_c_stat),
                   variance_corrected_Cstat = mean(Var(optimism_corrected_c_stat)),
                   SD_Cstat = sqrt(variance_corrected_Cstat))

optimism_corrected_C_stat_CI95_POMSmajor <- pool_auc(est_auc = c_stat_data_POMSmajor$mean_opt_corr_C, 
                                           est_se = c_stat_data_POMSmajor$SD_Cstat, nimp = 10, log_auc = TRUE)

optimism_corrected_C_stat_CI95_POMSmajor

## Calculate slope and intercept confidence intervals
slope_data_POMSmajor <- POMS_major_data %>%
  dplyr::group_by(.imp) %>%
  dplyr::summarise(mean_slope_index_corrected = mean(slope_index_corrected),
                   var.within = sd(slope_index_corrected)/sqrt(1000),
                   variance = Var(slope_index_corrected),
                   SE = sd(slope_index_corrected)/sqrt(1000),
                   SD = sd(slope_index_corrected),
                   mean_intercept = mean(intercept_index_corrected),
                   variance.intercept = Var(intercept_index_corrected))


## optimism corrected slope
slope.var_within_POMSmajor <- 1/10 * sum(slope_data_POMSmajor$variance)

slope.var_between_POMSmajor <- (1/(10-1))*sum((slope_data_POMSmajor$mean_slope_index_corrected - mean(slope_data_POMSmajor$mean_slope_index_corrected))^2)

slope.var_total_POMSmajor <- slope.var_within_POMSmajor + (slope.var_between_POMSmajor*(1+1/10))

slope.se_pooled_POMSmajor <- sqrt(slope.var_total_POMSmajor)

slope.pooled_95_conf_int_POMSmajor <- c(mean(slope_data_POMSmajor$mean_slope_index_corrected)-1.96*slope.se_pooled_POMSmajor,
                              mean(slope_data_POMSmajor$mean_slope_index_corrected),
                              mean(slope_data_POMSmajor$mean_slope_index_corrected)+1.96*slope.se_pooled_POMSmajor)
slope.pooled_95_conf_int_POMSmajor
## optimism corrected intercept
intercept.var_within_POMSmajor <- 1/10 * sum(slope_data_POMSmajor$variance.intercept)

intercept.var_between_POMSmajor <- (1/(10-1))*sum((slope_data_POMSmajor$mean_intercept - mean(slope_data_POMSmajor$mean_intercept))^2)

intercept.var_total_POMSmajor <- intercept.var_within_POMSmajor + (intercept.var_between_POMSmajor*(1+1/10))

intercept.se_pooled_POMSmajor <- sqrt(intercept.var_total_POMSmajor)

intercept.pooled_95_conf_int_POMSmajor <- c(mean(slope_data_POMSmajor$mean_intercept)-1.96*intercept.se_pooled_POMSmajor,
                                  mean(slope_data_POMSmajor$mean_intercept),
                                  mean(slope_data_POMSmajor$mean_intercept)+1.96*intercept.se_pooled_POMSmajor)
intercept.pooled_95_conf_int_POMSmajor            

##=============================================================
## Clavien-Dindo Grade II and above
##=============================================================

model_ClavienII <- ClavienGradeIIabove ~ POMS.logit


## Fit model to derive linear shrinkage factors:
lrm.mult.impute.ClavienII <- fit.mult.impute(formula = ClavienGradeIIabove ~ 
                                               POMS.logit,
                                             xtrans = calibration_data_sensitivity_POMSmajor, fitter = lrm,
                                             x=TRUE, y=TRUE)

perf.ClavienII <- pool_performance(data=complete(calibration_data_sensitivity_POMSmajor, action = "long", include = FALSE), 
                                   nimp=10,
                                   impvar=".imp", 
                                   Outcome = "ClavienGradeIIabove", predictors = c("POMS.logit"), 
                                   cal.plot=TRUE, plot.indiv=TRUE, groups_cal = 15)


##Validation
ClavienII_validate <- extract_fits_validate(calibration_data_sensitivity_POMSmajor, M=10, model_ClavienII, seed = 05062022)

ClavienII_data <- validation_calculations(validation_data = ClavienII_validate, 
                                           original_mids_object = calibration_data_sensitivity_POMSmajor, 
                                           model = model_ClavienII) 


# calculate mean model performance estimates
mean(ClavienII_data$optimism_corrected_c_stat)
mean(ClavienII_data$slope_index_corrected)
mean(ClavienII_data$intercept_optimism)


# create datatable of model discrimination across imputed datasets, prior to pooling using the pool_auc function
c_stat_data_ClavienII <- ClavienII_data %>%
  group_by(.imp) %>%
  dplyr::summarise(mean_opt_corr_C = mean(optimism_corrected_c_stat),
                   variance_corrected_Cstat = mean(Var(optimism_corrected_c_stat)),
                   SD_Cstat = sqrt(variance_corrected_Cstat))

optimism_corrected_C_stat_CI95_ClavienII <- pool_auc(est_auc = c_stat_data_ClavienII$mean_opt_corr_C, 
                                                     est_se = c_stat_data_ClavienII$SD_Cstat, nimp = 10, log_auc = TRUE)

optimism_corrected_C_stat_CI95_ClavienII

## Calculate slope and intercept confidence intervals
slope_data_ClavienII <- ClavienII_data %>%
  dplyr::group_by(.imp) %>%
  dplyr::summarise(mean_slope_index_corrected = mean(slope_index_corrected),
                   var.within = sd(slope_index_corrected)/sqrt(1000),
                   variance = Var(slope_index_corrected),
                   SE = sd(slope_index_corrected)/sqrt(1000),
                   SD = sd(slope_index_corrected),
                   mean_intercept = mean(intercept_index_corrected),
                   variance.intercept = Var(intercept_index_corrected))


## optimism corrected slope
slope.var_within_ClavienII <- 1/10 * sum(slope_data_ClavienII$variance)

slope.var_between_ClavienII <- (1/(10-1))*sum((slope_data_ClavienII$mean_slope_index_corrected - mean(slope_data_ClavienII$mean_slope_index_corrected))^2)

slope.var_total_ClavienII <- slope.var_within_ClavienII + (slope.var_between_ClavienII*(1+1/10))

slope.se_pooled_ClavienII <- sqrt(slope.var_total_ClavienII)

slope.pooled_95_conf_int_ClavienII <- c(mean(slope_data_ClavienII$mean_slope_index_corrected)-1.96*slope.se_pooled_ClavienII,
                                        mean(slope_data_ClavienII$mean_slope_index_corrected),
                                        mean(slope_data_ClavienII$mean_slope_index_corrected)+1.96*slope.se_pooled_ClavienII)
slope.pooled_95_conf_int_ClavienII
## optimism corrected intercept
intercept.var_within_ClavienII <- 1/10 * sum(slope_data_ClavienII$variance.intercept)

intercept.var_between_ClavienII <- (1/(10-1))*sum((slope_data_ClavienII$mean_intercept - mean(slope_data_ClavienII$mean_intercept))^2)

intercept.var_total_ClavienII <- intercept.var_within_ClavienII + (intercept.var_between_ClavienII*(1+1/10))

intercept.se_pooled_ClavienII <- sqrt(intercept.var_total_ClavienII)

intercept.pooled_95_conf_int_ClavienII <- c(mean(slope_data_ClavienII$mean_intercept)-1.96*intercept.se_pooled_ClavienII,
                                            mean(slope_data_ClavienII$mean_intercept),
                                            mean(slope_data_ClavienII$mean_intercept)+1.96*intercept.se_pooled_ClavienII)



##==============================
## Create calibration plots
##==============================

calibration_data_POMSmajor <- complete(calibration_data_sensitivity_POMSmajor, "long", include = FALSE) %>% 
  mutate(Major_pred = inv.logit(lrm.mult.impute.POMSmajor$coefficients[1]+
                                      lrm.mult.impute.POMSmajor$coefficients[2]*POMS.logit),
           ClavienII_pred = inv.logit(lrm.mult.impute.ClavienII$coefficients[1]+
                                        lrm.mult.impute.ClavienII$coefficients[2]*POMS.logit)) %>%
  pivot_longer(cols = c(Major_pred, ClavienII_pred), names_to = "outcome_predicted", values_to = "pred") %>%
  select(outcome_predicted, pred, POMS.major, ClavienGradeIIabove) %>%
  group_by(outcome_predicted) %>%
  mutate(binPred=ntile(pred,10),
         Outcome_Major=ifelse(POMS.major==TRUE,1,0),
         Outcome_ClavienII=ifelse(ClavienGradeIIabove==TRUE,1,0),
         Freq=1) %>%
  group_by(outcome_predicted, binPred) %>% 
  summarise(Observed_Major=(sum(Outcome_Major, na.rm = TRUE)/sum(Freq, na.rm = TRUE))*100,
            Observed_Clavien=(sum(Outcome_ClavienII, na.rm = TRUE)/sum(Freq, na.rm = TRUE))*100,
            Predicted=mean(pred, na.rm=TRUE)*100,
            n=sum(Freq, na.rm = TRUE)) %>%
  mutate(outcome_predicted = dplyr::recode(outcome_predicted,
                               `Major_pred` = "POMSmajor",
                               `ClavienII_pred` = "Clavien-Dindo Grade II and above"),
         Observed = ifelse(outcome_predicted=="POMSmajor", Observed_Major,
                           ifelse(outcome_predicted=="Clavien-Dindo Grade II and above", Observed_Clavien, NA))) %>%
  select(outcome_predicted, binPred, Predicted, Observed)


## ==================================================
## ggplot calibration object
## ==================================================
fig_calibration_POMSmajor <- ggplot(calibration_data_POMSmajor %>%
                                      filter(outcome_predicted == "Clavien-Dindo Grade II and above"), 
                                    aes(x=Predicted, y=Observed,
                                        shape = outcome_predicted, colour = outcome_predicted)) +
  geom_abline(intercept = 0, slope = 1, colour="grey") + 
  geom_point(size=1, stroke=1) +
  theme_thesis() +
  scale_colour_manual(values = cbPalette) +
  theme(strip.background = element_rect(colour="white", fill="white")) +
  scale_y_continuous(limits = c(0,50), breaks = seq(0,50, by=10)) +
  scale_x_continuous(limits = c(0,50), breaks = seq(0,50, by=10)) +
  labs(x="Predicted risk (%)", y="Observed risk (%)") +
  facet_wrap(~outcome_predicted) +
  theme(legend.position = "none")

#ggsave(filename = "./figures/Appendix S8 - Calibration_plot_POMSmajor_sensitivity.eps", plot = fig_calibration_POMSmajor, dpi = 400, width = 100, height = 100, units = "mm", device = cairo_ps)


