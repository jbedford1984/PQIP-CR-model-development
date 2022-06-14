## create calibration plot for manuscript - comparing PQIP model, POSSUM and SORT morbidity model performance in our cohort
calibration_data_PQIPmodel = predictions %>%
  mutate(POMS.major = rep(export_data$POMS.major,10)) %>%
  mutate(POMSmajor_pred = inv.logit(lrm.mult.impute.POMSmajor$coefficients[1] + lrm.mult.impute.POMSmajor$coefficients[2]*pred.logit)) %>%
  select(POMS.overall, POMS.major, pred, POMSmajor_pred) %>%
  pivot_longer(c(-POMS.overall,-POMS.major), names_to = "model", values_to = "pred") %>%
  mutate(model = dplyr::recode(model, 
                               `pred` = "POMS",
                               `POMSmajor_pred` = "POMSmajor")) %>%
  group_by(model) %>%
  mutate(binPred=ntile(pred,10),
         Freq=1,
         Outcome = ifelse(model=="POMS", POMS.overall, 
                          ifelse(model=="POMSmajor", POMS.major, NA))) %>%
  select(model, binPred, Freq, pred, Outcome) %>%
  group_by(model, binPred) %>% 
  summarise(Observed=sum(Outcome)/sum(Freq),
            Predicted=mean(pred),
            n=sum(Freq)) %>%
  mutate(Observed = Observed*100,
         Predicted = Predicted*100)


## ==================================================
## ggplot calibration object
## ==================================================
fig_calibration_comparison <- ggplot(calibration_data_PQIPmodel, aes(x=Predicted, y=Observed, shape = model, colour = model)) +
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

#ggsave(filename = "./figures/Calibration_plot_POMS_POMSmajor.eps", plot = fig_calibration_comparison, dpi = 400, width = 200*0.67, height = 75, units = "mm", device = cairo_ps)
