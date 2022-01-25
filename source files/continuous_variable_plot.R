## cowplot code

a.1 <- ggplot(mice::complete(mids_data, "long"), aes(x = S01AgeYears, y = POMS.overall)) +
  Hmisc::histSpikeg(POMS.overall ~ S01AgeYears, lowess = TRUE, data = mice::complete(mids_data, "long")) +
  labs(x = "\nAge (years)", y = "Probability\nPOMS defined morbidity\n") + 
  theme_cowplot(font_family = "Calibri") +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.05)) +
  theme(axis.title=element_text(size=12))

a <- ggplot(mice::complete(mids_data, "long"), aes(x = S01BMI, y = POMS.overall)) +
  Hmisc::histSpikeg(POMS.overall ~ S01BMI, lowess = TRUE, data = mice::complete(mids_data, "long")) +
  labs(x = expression(paste("Body mass index (kg ", m^2^-1,")")), 
       y = "Probability\nPOMS defined morbidity\n") + 
  theme_cowplot(font_family = "Calibri") +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.05)) +
  theme(axis.title=element_text(size=12))

b <- ggplot(mice::complete(mids_data, "long"), aes(x = S02SerumSodium, y = POMS.overall)) +
  Hmisc::histSpikeg(POMS.overall ~ S02SerumSodium, lowess = TRUE, data = mice::complete(mids_data, "long")) +
  labs(x = expression(paste("Sodium (mmol ", L^-1,")")), y = "Probability\nPOMS defined morbidity\n") + 
  theme_cowplot(font_family = "Calibri") +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.05)) +
  theme(axis.title=element_text(size=12))

c <- ggplot(mice::complete(mids_data, "long"), aes(x = S02SerumPotassium, y = POMS.overall)) +
  Hmisc::histSpikeg(POMS.overall ~ S02SerumPotassium, lowess = TRUE, data = mice::complete(mids_data, "long")) +
  labs(x = expression(paste("Potassium (mmol ", L^-1,")")), y = "Probability\nPOMS defined morbidity\n") + 
  theme_cowplot(font_family = "Calibri") +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.05)) +
  theme(axis.title=element_text(size=12))

d <- ggplot(mice::complete(mids_data, "long"), aes(x = log.Urea, y = POMS.overall)) +
  Hmisc::histSpikeg(POMS.overall ~ log.Urea, lowess = TRUE, data = mice::complete(mids_data, "long")) +
  labs(x = expression(paste("log Urea (mmol ", L^-1,")")), 
       y = "Probability\nPOMS defined morbidity\n") + 
  theme_cowplot(font_family = "Calibri") +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.05)) +
  theme(axis.title=element_text(size=12))

e <- ggplot(mice::complete(mids_data, "long"), aes(x = log.Creatinine, y = POMS.overall)) +
  Hmisc::histSpikeg(POMS.overall ~ log.Creatinine, lowess = TRUE, data = mice::complete(mids_data, "long")) +
  labs(x = expression(paste("log Creatinine (",mu,"mol ", L^-1,")")), y = "Probability\nPOMS defined morbidity\n") + 
  theme_cowplot(font_family = "Calibri") +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.05)) +
  theme(axis.title=element_text(size=12))

f <- ggplot(mice::complete(mids_data, "long"), aes(x = S02SerumAlbumin, y = POMS.overall)) +
  Hmisc::histSpikeg(POMS.overall ~ S02SerumAlbumin, lowess = TRUE, data = mice::complete(mids_data, "long")) +
  labs(x = expression(paste("Albumin (g ", L^-1,")")), y = "Probability\nPOMS defined morbidity\n") + 
  theme_cowplot(font_family = "Calibri") +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.05)) +
  theme(axis.title=element_text(size=12))

g <- ggplot(mice::complete(mids_data, "long"), aes(x = S02WhiteCellCount, y = POMS.overall)) +
  Hmisc::histSpikeg(POMS.overall ~ S02WhiteCellCount, lowess = TRUE, data = mice::complete(mids_data, "long")) +
  labs(x = expression(paste("White cell count (x10^9 ", L^-1,")")), y = "Probability\nPOMS defined morbidity\n") + 
  theme_cowplot(font_family = "Calibri") +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.05)) +
  theme(axis.title=element_text(size=12))

h <- ggplot(mice::complete(mids_data, "long"), aes(x = S02Haemoglobin, y = POMS.overall)) +
  Hmisc::histSpikeg(POMS.overall ~ S02Haemoglobin, lowess = TRUE, data = mice::complete(mids_data, "long")) +
  labs(x = expression(paste("Haemoglobin (g d", L^-1,")")), 
       y = "Probability\nPOMS defined morbidity\n") + 
  theme_cowplot(font_family = "Calibri") +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.05)) +
  theme(axis.title=element_text(size=12))

i <- ggplot(mice::complete(mids_data, "long"), aes(x = S02PulseRate, y = POMS.overall)) +
  Hmisc::histSpikeg(POMS.overall ~ S02PulseRate, lowess = TRUE, data = mice::complete(mids_data, "long")) +
  labs(x = "Heart rate (bpm)", y = "Probability\nPOMS defined morbidity\n") + 
  theme_cowplot(font_family = "Calibri") +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.05)) +
  theme(axis.title=element_text(size=12))

j <- ggplot(mice::complete(mids_data, "long"), aes(x = S02SystolicBP, y = POMS.overall)) +
  Hmisc::histSpikeg(POMS.overall ~ S02SystolicBP, lowess = TRUE, data = mice::complete(mids_data, "long")) +
  labs(x = "\nSystolic BP (mmHg)", y = "Probability\nPOMS defined morbidity\n") + 
  theme_cowplot(font_family = "Calibri") +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.05)) +
  theme(axis.title=element_text(size=12))

k <- ggplot(mice::complete(mids_data, "long"), aes(x = S02SpO2, y = POMS.overall)) +
  Hmisc::histSpikeg(POMS.overall ~ S02SpO2, lowess = TRUE, data = mice::complete(mids_data, "long")) +
  labs(x = "\nOxygen saturations (%)", y = "Probability\nPOMS defined morbidity\n") + 
  theme_cowplot(font_family = "Calibri") +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.05)) +
  theme(axis.title=element_text(size=12))

continuous_plot <- cowplot::plot_grid(a.1,a,b,c,d,e,f,g,h,i,j,k,  nrow = 4, ncol = 3, scale = .9, labels = "auto")

ggsave(filename = "./figures/continuous.eps", plot = continuous_plot, dpi = 500, width = 300, height = 300, units = "mm", device = cairo_ps)
