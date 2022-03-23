## create calibration plot for manuscript - comparing PQIP model, POSSUM and SORT morbidity model performance in our cohort
calibration_data <- export_data %>% select(POMS.overall, POSSUM.morbidity.intercept_slope_calibrated,
                                           SORT.morbidity.intercept_slope_calibrated) %>%
  filter(!is.na(POSSUM.morbidity.intercept_slope_calibrated)) %>%
  pivot_longer(!POMS.overall, names_to = "model", values_to = "pred") %>%
  group_by(model) %>%
  mutate(binPred=ntile(pred,10),
         Outcome=ifelse(POMS.overall==TRUE,1,0),
         Freq=1) %>%
  group_by(model, binPred) %>% 
  summarise(Observed=sum(Outcome, na.rm = TRUE)/sum(Freq, na.rm = TRUE),
            Predicted=mean(pred, na.rm=TRUE),
            n=sum(Freq, na.rm = TRUE))

calibration_data_PQIPmodel = predictions %>%
  mutate(binPred=ntile(pred,10),
         Outcome=ifelse(POMS.overall==TRUE,1,0),
         Freq=1) %>%
  group_by(binPred) %>% 
  summarise(Observed=sum(Outcome, na.rm = TRUE)/sum(Freq, na.rm = TRUE),
            Predicted=mean(pred, na.rm=TRUE),
            n=sum(Freq, na.rm = TRUE)) %>% 
  mutate(model = "PQIP_model") %>%
  select(model, binPred, Observed, Predicted, n)

calibration_data <- rbind.data.frame(calibration_data, calibration_data_PQIPmodel) %>%
  as_tibble(.) %>%
  mutate(model = dplyr::recode(model, 
                               `PQIP_model` = "PQIP-CR model",
                               `POSSUM.morbidity.intercept_slope_calibrated` = "POSSUM morbidity",
                               `SORT.morbidity.intercept_slope_calibrated` = "SORT morbidity"),
         model = ordered(model, levels = c("PQIP-CR model", "POSSUM morbidity","SORT morbidity")),
         Observed = Observed*100,
         Predicted = Predicted*100)
## ==================================================
## ggplot calibration object
## ==================================================
fig_calibration_comparison <- ggplot(calibration_data, aes(x=Predicted, y=Observed, shape = model, colour = model)) +
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

#ggsave(filename = "./figures/Calibration_plot_rcs3_4.eps", plot = fig_calibration_comparison, dpi = 400, width = 200, height = 75, units = "mm", device = cairo_ps)
