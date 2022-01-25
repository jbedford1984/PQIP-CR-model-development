## create calibration plots grouped by specific variables
calibration_plot_data <- list()

for (i in 0:10) {
  preds <- complete(mids_data, i) %>% 
    mutate(pred.logit = -3.3081876 +
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
             0.0000047440728*pmax(S01AgeYears.centred-16,0)^3) %>%
    mutate(pred = inv.logit(pred.logit),
           .imp = paste(i),
           pred.logit.corr = mean(validation_data$intercept_index_corrected) + (pred.logit * mean(validation_data$slope_index_corrected)),
           pred.calibration.corr = inv.logit(pred.logit.corr)) %>%
    select(.imp, POMS.overall, pred.logit, pred, pred.logit.corr, pred.calibration.corr,
           Rockwood.combined, SORT_severity.combined, S02PatientsASAGrade.combined,IMD_quintile_adjusted)
  
  name <- paste0('imp.',i)
  calibration_plot_data[[name]] <- tibble(preds)
  
}

calibration_plot_data <- rbind.data.frame(calibration_plot_data$imp.1,
                                calibration_plot_data$imp.2,
                                calibration_plot_data$imp.3,
                                calibration_plot_data$imp.4,
                                calibration_plot_data$imp.5,
                                calibration_plot_data$imp.6,
                                calibration_plot_data$imp.7,
                                calibration_plot_data$imp.8,
                                calibration_plot_data$imp.9,
                                calibration_plot_data$imp.10) 


ASA_calibration_plot = calibration_plot_data %>%
  select(POMS.overall, pred, S02PatientsASAGrade.combined) %>%
  group_by(S02PatientsASAGrade.combined) %>%
  mutate(binPred=ntile(pred,10),
         Outcome=ifelse(POMS.overall==TRUE,1,0),
         Freq=1) %>%
  ungroup() %>%
  group_by(binPred, S02PatientsASAGrade.combined) %>% 
  summarise(Observed=sum(Outcome, na.rm = TRUE)/sum(Freq, na.rm = TRUE),
            Predicted=mean(pred, na.rm=TRUE),
            n=sum(Freq, na.rm = TRUE)) %>%
  ggplot(., aes(x=Predicted, y=Observed)) +
  geom_abline(intercept = 0, slope = 1, colour="grey") + 
  geom_point(size=1, stroke=1) +
  theme(strip.background = element_rect(colour="black", fill="white")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0,1), breaks = seq(0,1, by=0.1)) +
  scale_x_continuous(expand = c(0, 0), limits = c(0,1), breaks = seq(0,1, by=0.1)) +
  labs(x="Predicted risk", y="Observed risk",
       caption = "Predicted vs. observed risk of postoperative morbidity by ASA grade")+
  theme(legend.position = "none") +
  facet_wrap(~ S02PatientsASAGrade.combined) +
  theme_cowplot()

Rockwood_calibration_plot = calibration_plot_data %>%
  select(POMS.overall, pred, Rockwood.combined) %>%
  group_by(Rockwood.combined) %>%
  mutate(binPred=ntile(pred,10),
         Outcome=ifelse(POMS.overall==TRUE,1,0),
         Freq=1) %>%
  ungroup() %>%
  group_by(binPred, Rockwood.combined) %>% 
  summarise(Observed=sum(Outcome, na.rm = TRUE)/sum(Freq, na.rm = TRUE),
            Predicted=mean(pred, na.rm=TRUE),
            n=sum(Freq, na.rm = TRUE)) %>%
  ggplot(., aes(x=Predicted, y=Observed)) +
  geom_abline(intercept = 0, slope = 1, colour="grey") + 
  geom_point(size=1, stroke=1) +
  #theme_thesis() +
  #scale_colour_manual(values = cbPalette) +
  theme(strip.background = element_rect(colour="black", fill="white")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0,1), breaks = seq(0,1, by=0.1)) +
  scale_x_continuous(expand = c(0, 0), limits = c(0,1), breaks = seq(0,1, by=0.1)) +
  labs(x="Predicted risk", y="Observed risk",
       caption = "Predicted vs. observed risk of postoperative morbidity by Rockwood Clinical Frailty Scale")+
  theme(legend.position = "none") +
  facet_wrap(~ Rockwood.combined) + 
  theme_cowplot()

IMD_calibration_plot = calibration_plot_data %>%
  select(POMS.overall, pred, IMD_quintile_adjusted) %>%
  group_by(IMD_quintile_adjusted) %>%
  mutate(binPred=ntile(pred,10),
         Outcome=ifelse(POMS.overall==TRUE,1,0),
         Freq=1) %>%
  ungroup() %>%
  group_by(binPred, IMD_quintile_adjusted) %>% 
  summarise(Observed=sum(Outcome, na.rm = TRUE)/sum(Freq, na.rm = TRUE),
            Predicted=mean(pred, na.rm=TRUE),
            n=sum(Freq, na.rm = TRUE)) %>%
  ggplot(., aes(x=Predicted, y=Observed)) +
  geom_abline(intercept = 0, slope = 1, colour="grey") + 
  geom_point(size=1, stroke=1) +
  theme(strip.background = element_rect(colour="black", fill="white")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0,1), breaks = seq(0,1, by=0.1)) +
  scale_x_continuous(expand = c(0, 0), limits = c(0,1), breaks = seq(0,1, by=0.1)) +
  labs(x="Predicted risk", y="Observed risk",
       caption = "Predicted vs. observed risk of postoperative morbidity by IMD quintile") +
  theme(legend.position = "none") +
  facet_wrap(~ IMD_quintile_adjusted) + 
  theme_cowplot()

Severity_calibration_plot = calibration_plot_data %>%
  select(POMS.overall, pred, SORT_severity.combined) %>%
  group_by(SORT_severity.combined) %>%
  mutate(binPred=ntile(pred,10),
         Outcome=ifelse(POMS.overall==TRUE,1,0),
         Freq=1) %>%
  ungroup() %>%
  group_by(binPred, SORT_severity.combined) %>% 
  summarise(Observed=sum(Outcome, na.rm = TRUE)/sum(Freq, na.rm = TRUE),
            Predicted=mean(pred, na.rm=TRUE),
            n=sum(Freq, na.rm = TRUE)) %>%
  ggplot(., aes(x=Predicted, y=Observed)) +
  geom_abline(intercept = 0, slope = 1, colour="grey") + 
  geom_point(size=1, stroke=1) +
  theme(strip.background = element_rect(colour="black", fill="white")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0,1), breaks = seq(0,1, by=0.1)) +
  scale_x_continuous(expand = c(0, 0), limits = c(0,1), breaks = seq(0,1, by=0.1)) +
  labs(x="Predicted risk", y="Observed risk",
       caption = "Predicted vs. observed risk of postoperative morbidity by severity of surgery")+
  theme(legend.position = "none") +
  facet_wrap(~ SORT_severity.combined) + 
  theme_cowplot()

plot_grid(ASA_calibration_plot, Rockwood_calibration_plot, IMD_calibration_plot, Severity_calibration_plot)
