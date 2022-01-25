## Sensitivity analysis - POMSmajor and Clavien-Dindo grade II and above complications
## Fit the predictor variables from the model to alternative outcome measures

## Firstly POMSmajor using lrm package
POMS.major.model <- POMS.major ~ 
  ModeSurgeryCombined + 
  Rockwood.combined + 
  S01Gender + 
  SORT_severity.combined + 
  S02PatientsASAGrade.combined + 
  rcs(S01BMI.centred, 4) +
  rcs(S02SerumSodium.centred, 4) + 
  IMD_quintile_adjusted + 
  rcs(log.Creatinine.centred, 4) +
  rcs(S02WhiteCellCount.centred, 4) +
  rcs(log.Urea.centred, 4) + 
  rcs(S01AgeYears.centred, 3)

lrm.mult.impute.POMSmajor <- fit.mult.impute(formula = POMS.major ~ 
                                               ModeSurgeryCombined + 
                                               Rockwood.combined + 
                                               S01Gender + 
                                               SORT_severity.combined + 
                                               S02PatientsASAGrade.combined + 
                                               rcs(S01BMI.centred, 4) +
                                               rcs(S02SerumSodium.centred, 4) + 
                                               IMD_quintile_adjusted + 
                                               rcs(log.Creatinine.centred, 4) +
                                               rcs(S02WhiteCellCount.centred, 4) +
                                               rcs(log.Urea.centred, 4) + 
                                               rcs(S01AgeYears.centred, 3), 
                                             xtrans = mids_data, fitter = lrm,
                                             x=TRUE, y=TRUE)

## Function to calculate predictions outside of R environment
Function(lrm.mult.impute.POMSmajor)

## Fit model using glm and MICE packages
POMSmajor.model.fit <- with(mids_data, glm(POMS.major ~ 
                                             ModeSurgeryCombined + 
                                             Rockwood.combined + 
                                             S01Gender + 
                                             SORT_severity.combined + 
                                             S02PatientsASAGrade.combined + 
                                             rcs(S01BMI.centred, 4) +
                                             rcs(S02SerumSodium.centred, 4) + 
                                             IMD_quintile_adjusted + 
                                             rcs(log.Creatinine.centred, 4) +
                                             rcs(S02WhiteCellCount.centred, 4) +
                                             rcs(log.Urea.centred, 4) + 
                                             rcs(S01AgeYears.centred, 3), 
                                           family = "binomial"))
est2 <- pool(POMSmajor.model.fit)

## show model estimates on log odds scale
summary(est2)

## show model estimates on odds ratio scale
summary(est2, conf.int = TRUE, exponentiate = TRUE)[,c(1,2,7,8)]

### Fit model to Clavien-Dindo grade II and above complications as outcome measure
Clavien.model <- ClavienGradeIIabove ~ 
  ModeSurgeryCombined + 
  Rockwood.combined + 
  S01Gender + 
  SORT_severity.combined + 
  S02PatientsASAGrade.combined + 
  rcs(S01BMI.centred, 4) +
  rcs(S02SerumSodium.centred, 4) + 
  IMD_quintile_adjusted + 
  rcs(log.Creatinine.centred, 4) +
  rcs(S02WhiteCellCount.centred, 4) +
  rcs(log.Urea.centred, 4) + 
  rcs(S01AgeYears.centred, 3)

lrm.mult.impute.Clavien <- fit.mult.impute(formula = ClavienGradeIIabove ~ 
                                             ModeSurgeryCombined + 
                                             Rockwood.combined + 
                                             S01Gender + 
                                             SORT_severity.combined + 
                                             S02PatientsASAGrade.combined + 
                                             rcs(S01BMI.centred, 4) +
                                             rcs(S02SerumSodium.centred, 4) + 
                                             IMD_quintile_adjusted + 
                                             rcs(log.Creatinine.centred, 4) +
                                             rcs(S02WhiteCellCount.centred, 4) +
                                             rcs(log.Urea.centred, 4) + 
                                             rcs(S01AgeYears.centred, 3), 
                                           xtrans = mids_data, fitter = lrm,
                                           x=TRUE, y=TRUE)

## take estimates outside R environment
Function(lrm.mult.impute.Clavien)

Clavien.model <- ClavienGradeIIabove ~ 
  ModeSurgeryCombined + 
  Rockwood.combined + 
  S01Gender + 
  SORT_severity.combined + 
  S02PatientsASAGrade.combined + 
  rcs(S01BMI.centred, 4) +
  rcs(S02SerumSodium.centred, 4) + 
  IMD_quintile_adjusted + 
  rcs(log.Creatinine.centred, 4) +
  rcs(S02WhiteCellCount.centred, 4) +
  rcs(log.Urea.centred, 4) + 
  rcs(S01AgeYears.centred, 3)

ClavienGradeIIabove.model.fit <- with(mids_data, glm(ClavienGradeIIabove ~ 
                                                       ModeSurgeryCombined + 
                                                       Rockwood.combined + 
                                                       S01Gender + 
                                                       SORT_severity.combined + 
                                                       S02PatientsASAGrade.combined + 
                                                       rcs(S01BMI.centred, 4) +
                                                       rcs(S02SerumSodium.centred, 4) + 
                                                       IMD_quintile_adjusted + 
                                                       rcs(log.Creatinine.centred, 4) +
                                                       rcs(S02WhiteCellCount.centred, 4) +
                                                       rcs(log.Urea.centred, 4) + 
                                                       rcs(S01AgeYears.centred, 3), 
                                                     family = "binomial"))
est3 <- pool(ClavienGradeIIabove.model.fit)

# show model estimates on log odds scale
summary(est3)

# show model estimates on odds ratio scale
summary(est3, conf.int = TRUE, exponentiate = TRUE)[,c(1,2,7,8)]



calibration_data_sensitivity <- mice::complete(mids_data, "long", include = FALSE) %>% 
  as_tibble() %>%
  mutate(clavien_prediction = inv.logit(-2.8490558 +
                                          0.45960702*(ModeSurgeryCombined=="Opn") +
                                          0.17283286*(ModeSurgeryCombined=="Rob") +
                                          0.18247727*(Rockwood.combined=="2") +
                                          0.44784172*(Rockwood.combined=="3") +
                                          0.7918986*(Rockwood.combined=="4") +
                                          0.57065036*(Rockwood.combined=="5") +
                                          0.72995879*(Rockwood.combined=="6-9") +
                                          0.32440918*(S01Gender=="M") +
                                          -0.12727578*(SORT_severity.combined=="Xma") +
                                          0.089324645*(SORT_severity.combined=="Com") +
                                          0.36045328*(S02PatientsASAGrade.combined=="II") +
                                          0.62561942*(S02PatientsASAGrade.combined=="III") +
                                          0.64023966*(S02PatientsASAGrade.combined=="IV/V") +
                                          -0.015631438* S01BMI.centred +
                                          0.00031642432*pmax(S01BMI.centred+7.99,0)^3 +
                                          -0.00092449074*pmax(S01BMI.centred+2.92,0)^3 +
                                          0.00069373467*pmax(S01BMI.centred-0.9,0)^3 +
                                          -8.566825e-05*pmax(S01BMI.centred-9.2875,0)^3 +
                                          -0.02818156* S02SerumSodium.centred +
                                          6.2554182e-05*pmax(S02SerumSodium.centred+6,0)^3 +
                                          0.0011732161*pmax(S02SerumSodium.centred+1,0)^3 +
                                          -0.0021638741*pmax(S02SerumSodium.centred-1,0)^3 +
                                          0.00092810384*pmax(S02SerumSodium.centred-4,0)^3 +
                                          0.052543343*(IMD_quintile_adjusted=="4") +
                                          0.066162269*(IMD_quintile_adjusted=="3") +
                                          0.11322111*(IMD_quintile_adjusted=="2") +
                                          0.10583954*(IMD_quintile_adjusted=="1 - most deprived") +
                                          -0.99809113* log.Creatinine.centred +
                                          5.4601315*pmax(log.Creatinine.centred+0.34875628,0)^3 +
                                          -20.629646*pmax(log.Creatinine.centred+0.065893495,0)^3 +
                                          19.13305*pmax(log.Creatinine.centred-0.11884061,0)^3 +
                                          -3.9635345*pmax(log.Creatinine.centred-0.43619845,0)^3 +
                                          -0.011981103* S02WhiteCellCount.centred +
                                          0.001379623*pmax(S02WhiteCellCount.centred+3.2,0)^3 +
                                          -0.0023082206*pmax(S02WhiteCellCount.centred+1.2,0)^3 +
                                          0.00064063909*pmax(S02WhiteCellCount.centred-0.5,0)^3 +
                                          0.00028795853*pmax(S02WhiteCellCount.centred-4.6,0)^3 +
                                          -0.37212535* log.Urea.centred +
                                          0.51004179*pmax(log.Urea.centred+0.53528926,0)^3 +
                                          -0.8399657*pmax(log.Urea.centred+0.073943697,0)^3 +
                                          -0.059254068*pmax(log.Urea.centred-0.14046617,0)^3 +
                                          0.38917798*pmax(log.Urea.centred-0.56332303,0)^3 +
                                          0.000113404* S01AgeYears.centred +
                                          2.4052003e-07*pmax(S01AgeYears.centred+20,0)^3 +
                                          -6.6605548e-07*pmax(S01AgeYears.centred-3,0)^3 +
                                          4.2553544e-07*pmax(S01AgeYears.centred-16,0)^3),
         POMSmajor_prediction = inv.logit(-4.2994397 +
                                            0.61773061*(ModeSurgeryCombined=="Opn") +
                                            0.029793576*(ModeSurgeryCombined=="Rob") +
                                            0.19407778*(Rockwood.combined=="2") +
                                            0.42718289*(Rockwood.combined=="3") +
                                            0.54645037*(Rockwood.combined=="4") +
                                            0.71773693*(Rockwood.combined=="5") +
                                            0.73311089*(Rockwood.combined=="6-9") +
                                            0.33627242*(S01Gender=="M") +
                                            0.37246669*(SORT_severity.combined=="Xma") +
                                            0.69201956*(SORT_severity.combined=="Com") +
                                            0.33718954*(S02PatientsASAGrade.combined=="II") +
                                            0.57861083*(S02PatientsASAGrade.combined=="III") +
                                            0.6116451*(S02PatientsASAGrade.combined=="IV/V") +
                                            -0.027736782* S01BMI.centred +
                                            0.00079703455*pmax(S01BMI.centred+7.99,0)^3 +
                                            -0.0023582441*pmax(S01BMI.centred+2.92,0)^3 +
                                            0.0017904621*pmax(S01BMI.centred-0.9,0)^3 +
                                            -0.0002292525*pmax(S01BMI.centred-9.2875,0)^3 +
                                            -0.036158868* S02SerumSodium.centred +
                                            -4.1576076e-05*pmax(S02SerumSodium.centred+6,0)^3 +
                                            0.0035447103*pmax(S02SerumSodium.centred+1,0)^3 +
                                            -0.0057692635*pmax(S02SerumSodium.centred-1,0)^3 +
                                            0.0022661293*pmax(S02SerumSodium.centred-4,0)^3 +
                                            0.052970801*(IMD_quintile_adjusted=="4") +
                                            -0.043597376*(IMD_quintile_adjusted=="3") +
                                            0.16304008*(IMD_quintile_adjusted=="2") +
                                            0.24707409*(IMD_quintile_adjusted=="1 - most deprived") +
                                            -0.74603311* log.Creatinine.centred +
                                            4.0930978*pmax(log.Creatinine.centred+0.34875628,0)^3 +
                                            -13.81271*pmax(log.Creatinine.centred+0.065893495,0)^3 +
                                            11.729201*pmax(log.Creatinine.centred-0.11884061,0)^3 +
                                            -2.0095887*pmax(log.Creatinine.centred-0.43619845,0)^3 +
                                            -0.01162863* S02WhiteCellCount.centred +
                                            0.0027328168*pmax(S02WhiteCellCount.centred+3.2,0)^3 +
                                            -0.0059801418*pmax(S02WhiteCellCount.centred+1.2,0)^3 +
                                            0.0032606953*pmax(S02WhiteCellCount.centred-0.5,0)^3 +
                                            -1.3370414e-05*pmax(S02WhiteCellCount.centred-4.6,0)^3 +
                                            -0.6904649* log.Urea.centred +
                                            1.2661205*pmax(log.Urea.centred+0.53528926,0)^3 +
                                            -5.0402701*pmax(log.Urea.centred+0.073943697,0)^3 +
                                            4.3064714*pmax(log.Urea.centred-0.14046617,0)^3 +
                                            -0.53232171*pmax(log.Urea.centred-0.56332303,0)^3 +
                                            0.0029197574* S01AgeYears.centred +
                                            1.9243542e-06*pmax(S01AgeYears.centred+20,0)^3 +
                                            -5.3289808e-06*pmax(S01AgeYears.centred-3,0)^3 +
                                            3.4046266e-06*pmax(S01AgeYears.centred-16,0)^3)) %>%
  select(POMS.major, ClavienGradeIIabove, clavien_prediction, POMSmajor_prediction) %>%
  pivot_longer(!c(POMS.major,ClavienGradeIIabove), names_to = "model", values_to = "pred") %>%
  group_by(model) %>%
  mutate(binPred=ntile(pred,10),
         Outcome.POMS=ifelse(POMS.major==TRUE,1,0),
         Outcome.Clavien=ifelse(ClavienGradeIIabove==TRUE,1,0)) %>%
  group_by(model, binPred) %>% 
  summarise(Observed.POMS=sum(Outcome.POMS)/n(),
            Predicted.POMS=mean(pred),
            Predicted.SD.POMS = sd(pred),
            n=n(),
            Observed.Clavien=sum(Outcome.Clavien)/n(),
            Predicted.Clavien=mean(pred),
            Predicted.SD.Clavien = sd(pred))

calibration_data_sens1 <- calibration_data_sensitivity %>% 
  filter(model == "clavien_prediction") %>%
  select(model, binPred, n, 
         Observed.Clavien, Predicted.Clavien,
         Predicted.SD.Clavien)
calibration_data_sens2 <- calibration_data_sensitivity %>% 
  filter(model == "POMSmajor_prediction") %>%
  select(model, binPred, n, 
         Observed.POMS, Predicted.POMS,
         Predicted.SD.POMS)

names(calibration_data_sens1) <- c("model", "binPred", "n", "Observed", "Predicted", 
                                   "Predicted.SD")
names(calibration_data_sens2) <- c("model", "binPred", "n", "Observed", "Predicted", 
                                   "Predicted.SD")


calibration_data_sensitivity <- base::rbind(calibration_data_sens1, calibration_data_sens2) %>%
  as_tibble(.) %>%
  mutate(model = dplyr::recode(model, 
                               `clavien_prediction` = "Clavien-Dindo grade II and above model",
                               `POMSmajor_prediction` = "POMSmajor model"),
         model = ordered(model, levels = c("POMSmajor model", 
                                           "Clavien-Dindo grade II and above model")),
         Observed = Observed*100,
         Predicted = Predicted*100)
## ==================================================
## ggplot calibration object
## ==================================================
fig_calibration_sensitivity <- ggplot(calibration_data_sensitivity, aes(x=Predicted, y=Observed, shape = model, colour = model)) +
  geom_abline(intercept = 0, slope = 1, colour="grey") + 
  geom_point(size=1, stroke=1) +
  theme_thesis() +
  scale_colour_manual(values = cbPalette) +
  theme(strip.background = element_rect(colour="white", fill="white")) +
  scale_y_continuous(limits = c(0,60), breaks = seq(0,60, by=10)) +
  scale_x_continuous(limits = c(0,60), breaks = seq(0,60, by=10)) +
  labs(x="Predicted risk (%)", y="Observed risk (%)") +
  facet_wrap(~model) +
  theme(legend.position = "none")

#ggsave(filename = "./figures/Calibration_plot_sensitivity_rcs3_4.eps", plot = fig_calibration_sensitivity, dpi = 400, width = 130, height = 75, units = "mm", device = cairo_ps)

perf.Clavien <- pool_performance(data=complete(mids_data, action = "long", include = FALSE), nimp=10,
                                 impvar=".imp", 
                                 Outcome = "ClavienGradeIIabove", predictors = c("ModeSurgeryCombined",
                                                                                 "Rockwood.combined",
                                                                                 "S01Gender",
                                                                                 "SORT_severity.combined",
                                                                                 "S02PatientsASAGrade.combined",
                                                                                 "rcs(S01BMI.centred, 4)",
                                                                                 "rcs(S02SerumSodium.centred, 4)",
                                                                                 "rcs(S01AgeYears.centred, 3)",
                                                                                 "IMD_quintile_adjusted",
                                                                                 "rcs(log.Creatinine.centred, 4)",
                                                                                 "rcs(S02WhiteCellCount.centred, 4)",
                                                                                 "rcs(log.Urea.centred, 4)"), 
                                 cal.plot=TRUE, plot.indiv=TRUE, groups_cal = 15)

perf.POMSmajor <- pool_performance(data=complete(mids_data, action = "long", include = FALSE), nimp=10,
                                   impvar=".imp", 
                                   Outcome = "POMS.major", predictors = c("ModeSurgeryCombined",
                                                                          "Rockwood.combined",
                                                                          "S01Gender",
                                                                          "SORT_severity.combined",
                                                                          "S02PatientsASAGrade.combined",
                                                                          "rcs(S01BMI.centred, 4)",
                                                                          "rcs(S02SerumSodium.centred, 4)",
                                                                          "rcs(S01AgeYears.centred, 3)",
                                                                          "IMD_quintile_adjusted",
                                                                          "rcs(log.Creatinine.centred, 4)",
                                                                          "rcs(S02WhiteCellCount.centred, 4)",
                                                                          "rcs(log.Urea.centred, 4)"), 
                                   cal.plot=TRUE, plot.indiv=TRUE, groups_cal = 15)

## perform internal validation of POMSmajor and Clavien-Dindo model performance to obtain optimism corrected C-statistic

## internal validation of POMSmajor model performance
#validation.POMS <- extract_fits_validate(mids.object=mids_data, M=10, model=POMS.major.model, seed=13122021)
#save(validation.POMS, file = "internal_validation_POMSmajor.RData")
load("internal_validation_POMSmajor.RData")

## internal validation Clavien-Dindo 
#validation.Clavien <- extract_fits_validate(mids.object=mids_data, M=10, model=Clavien.model, seed=14122021)
#save(validation.Clavien, file = "internal_validation_Clavien.RData")
load("internal_validation_Clavien.RData")

## calculate internal validation measures for POMSmajor model performance
validation_data_POMSmajor<- rbind.data.frame(tibble(validation.POMS$validation_m1$mids_validate),
                                             tibble(validation.POMS$validation_m2$mids_validate),
                                             tibble(validation.POMS$validation_m3$mids_validate),
                                             tibble(validation.POMS$validation_m4$mids_validate),
                                             tibble(validation.POMS$validation_m5$mids_validate),
                                             tibble(validation.POMS$validation_m6$mids_validate),
                                             tibble(validation.POMS$validation_m7$mids_validate),
                                             tibble(validation.POMS$validation_m8$mids_validate),
                                             tibble(validation.POMS$validation_m9$mids_validate),
                                             tibble(validation.POMS$validation_m10$mids_validate)) %>%
  mutate(c_statistic_training = 0.5*(Dxy_training+1),
         c_statistic_test = 0.5*(Dxy_test+1),
         c_statistic_optimism = c_statistic_training-c_statistic_test,
         slope_optimism = Slope_training - Slope_test,
         slope_index_corrected = 1-slope_optimism,
         intercept_optimism = Intercept_training - Intercept_test,
         intercept_index_corrected = 0-intercept_optimism,
         Brier_optimism = B_training - B_test,
         R2_optimism = R2_training - R2_test)

imp.c_stat <- list()
imp.R2 <- list()
imp.Brier <- list()

for (i in 1:10) {
  model.fit <- lrm(POMS.major.model, data=complete(mids_data,i))
  name <- paste0('imp.',i)
  imp.c_stat[name] <- model.fit$stats[6]
  imp.R2[name] <- model.fit$stats[10]
  imp.Brier[name] <- model.fit$stats[11]
}

imp.c_stat <- cbind.data.frame(tibble(matrix(unlist(imp.c_stat))),
                               tibble(matrix(unlist(imp.R2))),
                               tibble(matrix(unlist(imp.Brier)))) %>%
  mutate(.imp = 1:10) %>%
  dplyr::rename(.,
                original_c_statistic = `matrix(unlist(imp.c_stat))`,
                original_Brier = `matrix(unlist(imp.Brier))`,
                original_R2 = `matrix(unlist(imp.R2))`)

validation_data_POMSmajor <- left_join(validation_data_POMSmajor, imp.c_stat, by = ".imp") %>%
  mutate(original_c_stat = c_statistic_training,
         optimism_corrected_c_stat = original_c_statistic-c_statistic_optimism) %>%
  group_by(.imp) %>%
  mutate(mean_original_c_stat = mean(original_c_stat),
         mean_c_stat_imp = mean(optimism_corrected_c_stat)) %>%
  ungroup() %>%
  mutate(original_c_stat_SE = original_c_stat - mean_original_c_stat,
         original_c_stat_SE.sq = (original_c_stat - mean_original_c_stat)^2,
         c_stat_SE.sq = (optimism_corrected_c_stat - mean_c_stat_imp)^2,
         c_stat_SE = optimism_corrected_c_stat - mean_c_stat_imp)

mean(validation_data_POMSmajor$optimism_corrected_c_stat)
mean(validation_data_POMSmajor$slope_index_corrected)
mean(validation_data_POMSmajor$intercept_optimism)



slope_data_POMSmajor <- validation_data_POMSmajor %>%
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


perf.POMSmajor <- pool_performance(data=complete(mids_data, action = "long", include = FALSE), nimp=10,
                                   impvar=".imp", 
                                   Outcome = "POMS.major", predictors = c("ModeSurgeryCombined",
                                                                          "Rockwood.combined",
                                                                          "S01Gender",
                                                                          "SORT_severity.combined",
                                                                          "S02PatientsASAGrade.combined",
                                                                          "rcs(S01BMI.centred, 4)",
                                                                          "rcs(S02SerumSodium.centred, 4)",
                                                                          "rcs(S01AgeYears.centred, 3)",
                                                                          "IMD_quintile_adjusted",
                                                                          "rcs(log.Creatinine.centred, 4)",
                                                                          "rcs(S02WhiteCellCount.centred, 4)",
                                                                          "rcs(log.Urea.centred, 4)"), 
                                   cal.plot=TRUE, plot.indiv=TRUE, groups_cal = 15)

c_stat_data_POMSmajor <- validation_data_POMSmajor %>%
  group_by(.imp) %>%
  dplyr::summarise(mean_opt_corr_C = mean(optimism_corrected_c_stat),
                   variance_corrected_Cstat = mean(Var(optimism_corrected_c_stat)),
                   SD_Cstat = sqrt(variance_corrected_Cstat))

optimism_corrected_C_stat_CI95_POMSmajor <- pool_auc(est_auc = c_stat_data_POMSmajor$mean_opt_corr_C, 
                                                     est_se = c_stat_data_POMSmajor$SD_Cstat, nimp = 10, log_auc = TRUE)


## calculate internal validation measures for Clavien-Dindo model performance
validation_data_Clavien<- rbind.data.frame(tibble(validation.Clavien$validation_m1$mids_validate),
                                           tibble(validation.Clavien$validation_m2$mids_validate),
                                           tibble(validation.Clavien$validation_m3$mids_validate),
                                           tibble(validation.Clavien$validation_m4$mids_validate),
                                           tibble(validation.Clavien$validation_m5$mids_validate),
                                           tibble(validation.Clavien$validation_m6$mids_validate),
                                           tibble(validation.Clavien$validation_m7$mids_validate),
                                           tibble(validation.Clavien$validation_m8$mids_validate),
                                           tibble(validation.Clavien$validation_m9$mids_validate),
                                           tibble(validation.Clavien$validation_m10$mids_validate)) %>%
  mutate(c_statistic_training = 0.5*(Dxy_training+1),
         c_statistic_test = 0.5*(Dxy_test+1),
         c_statistic_optimism = c_statistic_training-c_statistic_test,
         slope_optimism = Slope_training - Slope_test,
         slope_index_corrected = 1-slope_optimism,
         intercept_optimism = Intercept_training - Intercept_test,
         intercept_index_corrected = 0-intercept_optimism,
         Brier_optimism = B_training - B_test,
         R2_optimism = R2_training - R2_test)

imp.c_stat <- list()
imp.R2 <- list()
imp.Brier <- list()

for (i in 1:10) {
  model.fit <- lrm(Clavien.model, data=complete(mids_data,i))
  name <- paste0('imp.',i)
  imp.c_stat[name] <- model.fit$stats[6]
  imp.R2[name] <- model.fit$stats[10]
  imp.Brier[name] <- model.fit$stats[11]
}

imp.c_stat <- cbind.data.frame(tibble(matrix(unlist(imp.c_stat))),
                               tibble(matrix(unlist(imp.R2))),
                               tibble(matrix(unlist(imp.Brier)))) %>%
  mutate(.imp = 1:10) %>%
  dplyr::rename(.,
                original_c_statistic = `matrix(unlist(imp.c_stat))`,
                original_Brier = `matrix(unlist(imp.Brier))`,
                original_R2 = `matrix(unlist(imp.R2))`)

validation_data_Clavien <- left_join(validation_data_Clavien, imp.c_stat, by = ".imp") %>%
  mutate(original_c_stat = c_statistic_training,
         optimism_corrected_c_stat = original_c_statistic-c_statistic_optimism,
         optimism_corrected_brier = original_Brier - Brier_optimism,
         optimism_corrected_R2 = original_R2 - R2_optimism) %>%
  group_by(.imp) %>%
  mutate(mean_original_c_stat = mean(original_c_stat),
         mean_c_stat_imp = mean(optimism_corrected_c_stat),
         mean_Brier_imp = mean(optimism_corrected_brier)) %>%
  ungroup() %>%
  mutate(original_c_stat_SE = original_c_stat - mean_original_c_stat,
         original_c_stat_SE.sq = (original_c_stat - mean_original_c_stat)^2,
         c_stat_SE.sq = (optimism_corrected_c_stat - mean_c_stat_imp)^2,
         c_stat_SE = optimism_corrected_c_stat - mean_c_stat_imp)

mean(validation_data_Clavien$optimism_corrected_c_stat)
mean(validation_data_Clavien$slope_index_corrected)
mean(validation_data_Clavien$intercept_optimism)



slope_data_Clavien <- validation_data_Clavien %>%
  dplyr::group_by(.imp) %>%
  dplyr::summarise(mean_slope_index_corrected = mean(slope_index_corrected),
                   var.within = sd(slope_index_corrected)/sqrt(1000),
                   variance = Var(slope_index_corrected),
                   SE = sd(slope_index_corrected)/sqrt(1000),
                   SD = sd(slope_index_corrected),
                   mean_intercept = mean(intercept_index_corrected),
                   variance.intercept = Var(intercept_index_corrected))


## optimism corrected slope
slope.var_within_Clavien <- 1/10 * sum(slope_data_Clavien$variance)

slope.var_between_Clavien <- (1/(10-1))*sum((slope_data_Clavien$mean_slope_index_corrected - mean(slope_data_Clavien$mean_slope_index_corrected))^2)

slope.var_total_Clavien <- slope.var_within_Clavien + (slope.var_between_Clavien*(1+1/10))

slope.se_pooled_Clavien <- sqrt(slope.var_total_Clavien)

slope.pooled_95_conf_int_Clavien <- c(mean(slope_data_Clavien$mean_slope_index_corrected)-1.96*slope.se_pooled_Clavien,
                                      mean(slope_data_Clavien$mean_slope_index_corrected),
                                      mean(slope_data_Clavien$mean_slope_index_corrected)+1.96*slope.se_pooled_Clavien)
slope.pooled_95_conf_int_Clavien
## optimism corrected intercept
intercept.var_within_Clavien <- 1/10 * sum(slope_data_Clavien$variance.intercept)

intercept.var_between_Clavien <- (1/(10-1))*sum((slope_data_Clavien$mean_intercept - mean(slope_data_Clavien$mean_intercept))^2)

intercept.var_total_Clavien <- intercept.var_within_Clavien + (intercept.var_between_Clavien*(1+1/10))

intercept.se_pooled_Clavien <- sqrt(intercept.var_total_Clavien)

intercept.pooled_95_conf_int_Clavien <- c(mean(slope_data_Clavien$mean_intercept)-1.96*intercept.se_pooled_Clavien,
                                          mean(slope_data_Clavien$mean_intercept),
                                          mean(slope_data_Clavien$mean_intercept)+1.96*intercept.se_pooled_Clavien)
intercept.pooled_95_conf_int_Clavien            


perf.Clavien <- pool_performance(data=complete(mids_data, action = "long", include = FALSE), nimp=10,
                                 impvar=".imp", 
                                 Outcome = "ClavienGradeIIabove", predictors = c("ModeSurgeryCombined",
                                                                                 "Rockwood.combined",
                                                                                 "S01Gender",
                                                                                 "SORT_severity.combined",
                                                                                 "S02PatientsASAGrade.combined",
                                                                                 "rcs(S01BMI.centred, 4)",
                                                                                 "rcs(S02SerumSodium.centred, 4)",
                                                                                 "rcs(S01AgeYears.centred, 3)",
                                                                                 "IMD_quintile_adjusted",
                                                                                 "rcs(log.Creatinine.centred, 4)",
                                                                                 "rcs(S02WhiteCellCount.centred, 4)",
                                                                                 "rcs(log.Urea.centred, 4)"), 
                                 cal.plot=TRUE, plot.indiv=TRUE, groups_cal = 15)

c_stat_data_Clavien <- validation_data_Clavien %>%
  group_by(.imp) %>%
  dplyr::summarise(mean_opt_corr_C = mean(optimism_corrected_c_stat),
                   variance_corrected_Cstat = mean(Var(optimism_corrected_c_stat)),
                   SD_Cstat = sqrt(variance_corrected_Cstat))

optimism_corrected_C_stat_CI95_Clavien <- pool_auc(est_auc = c_stat_data_Clavien$mean_opt_corr_C, 
                                                   est_se = c_stat_data_Clavien$SD_Cstat, nimp = 10, log_auc = TRUE)

optimism_corrected_C_stat_CI95_Clavien
