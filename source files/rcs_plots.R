### plot restricted cubic splines for continuous variables vs. log odds 

library(grid)

rcs_data <- mice::complete(mids_data, "long")

rcs_plot3 <- function(data, variable, xlab) {
  
  gline = linesGrob(y = c(-0.1, 0.1),  gp = gpar(col = "red", lwd = 2, lty  ="dotted")) 
  
  rcs_temp <- data %>% select({{variable}}, POMS.overall) %>%
    mutate(predictor = {{variable}})
  rcs_temp2 <- rcspline.plot(rcs_temp$predictor, rcs_temp$POMS.overall, nk=3, show = "prob")
  
  rcs_temp <- rcs_temp %>% 
    mutate(decile = ntile(predictor, 15),
           total=1) %>%
    dplyr::group_by(decile) %>%
    dplyr::summarise(mean.pred = mean(predictor),
                     prop.POMS = sum(POMS.overall)/n(),
                     n.POMS = sum(POMS.overall)/10,
                     n = n(),
                     n.adj = n/10)
  
  prop.CI <- as_tibble(BinomCI(rcs_temp$n.POMS, rcs_temp$n.adj))
  
  rcs_temp <- rcs_temp %>%
    mutate(lower = prop.CI$lwr.ci,
           upper = prop.CI$upr.ci)
  
  knot_placement <- as.vector(rcs_temp2$knots)
  
  rcs_temp2 <- as_tibble(rcs_temp2[2:5])
  
  plot <- ggplot(rcs_temp2, aes(x=x,y=xbeta)) + 
    geom_pointrange(data=rcs_temp, aes(x=mean.pred, y=prop.POMS, ymin=lower, ymax = upper),
                    colour = "dark grey", size=0.1) +
    #Hmisc::histSpikeg(POMS.overall ~ predictor, lowess = TRUE, data = rcs_temp) +
    labs(x = xlab, y = "Probability\nPOMS defined morbidity\n") + 
    theme_cowplot(font_family = "Calibri") +
    scale_y_continuous(labels = scales::number_format(accuracy = 0.05), 
                       limits = c(min(c(rcs_temp2$xbeta,rcs_temp$lower))-0.05,max(c(rcs_temp2$xbeta,rcs_temp$upper))+0.05),
                       expand = c(0, 0)) +
    theme(axis.title=element_text(size=12)) +
    geom_line(colour = "red") + 
    #geom_ribbon(aes(ymin = lower, ymax = upper), fill = "light blue", alpha=0.2) + 
    annotation_custom(gline, xmin=knot_placement[1], xmax=knot_placement[1], ymin=-Inf, ymax=Inf) + 
    annotation_custom(gline, xmin=knot_placement[2], xmax=knot_placement[2], ymin=-Inf, ymax=Inf) +
    annotation_custom(gline, xmin=knot_placement[3], xmax=knot_placement[3], ymin=-Inf, ymax=Inf)
  
  return(plot)
}

rcs_plot4 <- function(data, variable, xlab) {
  
  gline = linesGrob(y = c(-0.1, 0.1),  gp = gpar(col = "red", lwd = 2, lty  ="dotted")) 
  
  rcs_temp <- data %>% select({{variable}}, POMS.overall) %>%
    mutate(predictor = {{variable}})
  rcs_temp2 <- rcspline.plot(rcs_temp$predictor, rcs_temp$POMS.overall, nk=4, show = "prob")
  
  rcs_temp <- rcs_temp %>% 
    mutate(decile = ntile(predictor, 15),
           total=1) %>%
    dplyr::group_by(decile) %>%
    dplyr::summarise(mean.pred = mean(predictor),
                     prop.POMS = sum(POMS.overall)/n(),
                     n.POMS = sum(POMS.overall)/10,
                     n = n(),
                     n.adj = n/10)
  
  prop.CI <- as_tibble(BinomCI(rcs_temp$n.POMS, rcs_temp$n.adj))
  
  rcs_temp <- rcs_temp %>%
    mutate(lower = prop.CI$lwr.ci,
           upper = prop.CI$upr.ci)
  
  knot_placement <- as.vector(rcs_temp2$knots)
  
  rcs_temp2 <- as_tibble(rcs_temp2[2:5])
  
  plot <- ggplot(rcs_temp2, aes(x=x,y=xbeta)) + 
    geom_pointrange(data=rcs_temp, aes(x=mean.pred, y=prop.POMS, ymin=lower, ymax = upper),
                    colour = "dark grey", size=0.1) +
    #Hmisc::histSpikeg(POMS.overall ~ predictor, lowess = TRUE, data = rcs_temp) +
    labs(x = xlab, y = "Probability\nPOMS defined morbidity\n") + 
    theme_cowplot(font_family = "Calibri") +
    scale_y_continuous(labels = scales::number_format(accuracy = 0.05), 
                       limits = c(min(c(rcs_temp2$xbeta,rcs_temp$lower))-0.05,max(c(rcs_temp2$xbeta,rcs_temp$upper))+0.05),
                       expand = c(0, 0)) +
    theme(axis.title=element_text(size=12)) +
    geom_line(colour = "red") + 
    #geom_ribbon(aes(ymin = lower, ymax = upper), fill = "light blue", alpha=0.2) + 
    annotation_custom(gline, xmin=knot_placement[1], xmax=knot_placement[1], ymin=-Inf, ymax=Inf) + 
    annotation_custom(gline, xmin=knot_placement[2], xmax=knot_placement[2], ymin=-Inf, ymax=Inf) +
    annotation_custom(gline, xmin=knot_placement[3], xmax=knot_placement[3], ymin=-Inf, ymax=Inf) +
    annotation_custom(gline, xmin=knot_placement[4], xmax=knot_placement[4], ymin=-Inf, ymax=Inf)
  
  return(plot)
}

rcs_plot5 <- function(data, variable, xlab) {
  
  gline = linesGrob(y = c(-0.1, 0.1),  gp = gpar(col = "red", lwd = 2, lty  ="dotted")) 
  
  rcs_temp <- data %>% select({{variable}}, POMS.overall) %>%
    mutate(predictor = {{variable}})
  rcs_temp2 <- rcspline.plot(rcs_temp$predictor, rcs_temp$POMS.overall, nk=5, show = "prob")
  
  rcs_temp <- rcs_temp %>% 
    mutate(decile = ntile(predictor, 15),
           total=1) %>%
    dplyr::group_by(decile) %>%
    dplyr::summarise(mean.pred = mean(predictor),
                     prop.POMS = sum(POMS.overall)/n(),
                     n.POMS = sum(POMS.overall)/10,
                     n = n(),
                     n.adj = n/10)
  
  prop.CI <- as_tibble(BinomCI(rcs_temp$n.POMS, rcs_temp$n.adj))
  
  rcs_temp <- rcs_temp %>%
    mutate(lower = prop.CI$lwr.ci,
           upper = prop.CI$upr.ci)
  
  knot_placement <- as.vector(rcs_temp2$knots)
  
  rcs_temp2 <- as_tibble(rcs_temp2[2:5])
  
  plot <- ggplot(rcs_temp2, aes(x=x,y=xbeta)) + 
    geom_pointrange(data=rcs_temp, aes(x=mean.pred, y=prop.POMS, ymin=lower, ymax = upper),
                    colour = "dark grey", size=0.1) +
    #Hmisc::histSpikeg(POMS.overall ~ predictor, lowess = TRUE, data = rcs_temp) +
    labs(x = xlab, y = "Probability\nPOMS defined morbidity\n") + 
    theme_cowplot(font_family = "Calibri") +
    scale_y_continuous(labels = scales::number_format(accuracy = 0.05), 
                       limits = c(min(c(rcs_temp2$xbeta,rcs_temp$lower))-0.05,max(c(rcs_temp2$xbeta,rcs_temp$upper))+0.05),
                       expand = c(0, 0)) +
    theme(axis.title=element_text(size=12)) +
    geom_line(colour = "red") + 
    #geom_ribbon(aes(ymin = lower, ymax = upper), fill = "light blue", alpha=0.2) + 
    annotation_custom(gline, xmin=knot_placement[1], xmax=knot_placement[1], ymin=-Inf, ymax=Inf) + 
    annotation_custom(gline, xmin=knot_placement[2], xmax=knot_placement[2], ymin=-Inf, ymax=Inf) +
    annotation_custom(gline, xmin=knot_placement[3], xmax=knot_placement[3], ymin=-Inf, ymax=Inf) +
    annotation_custom(gline, xmin=knot_placement[4], xmax=knot_placement[4], ymin=-Inf, ymax=Inf) +
    annotation_custom(gline, xmin=knot_placement[5], xmax=knot_placement[5], ymin=-Inf, ymax=Inf)
  
  return(plot)
}

Age_plot <- rcs_plot3(rcs_data, S01AgeYears, "Age (years)")
BMI_plot <- rcs_plot4(rcs_data, S01BMI, expression(paste("Body mass index (kg ", m^2^-1,")")))
Na_plot <- rcs_plot4(rcs_data, S02SerumSodium, expression(paste("Sodium (mmol ", L^-1,")")))
K_plot <- rcs_plot3(rcs_data, S02SerumPotassium, expression(paste("Potassium (mmol ", L^-1,")")))
WCC_plot <- rcs_plot4(rcs_data, S02WhiteCellCount, expression(paste("White cell count (x10^9 ", L^-1,")")))
Albumin_plot <- rcs_plot4(rcs_data, S02SerumAlbumin, expression(paste("Albumin (g ", L^-1,")")))
Hb_plot <- rcs_plot3(rcs_data, S02Haemoglobin, expression(paste("Haemoglobin (g d", L^-1,")")))
Pulse_plot <- rcs_plot4(rcs_data, S02PulseRate, "Heart rate (bpm)")
BP_plot <- rcs_plot3(rcs_data, S02SystolicBP, "Systolic BP (mmHg)")
SpO2_plot <- rcs_plot3(rcs_data, S02SpO2, "Oxygen saturations (%)")
Urea_plot <- rcs_plot4(rcs_data, log.Urea, expression(paste("log Urea (mmol ", L^-1,")")))
Creat_plot <- rcs_plot4(rcs_data, log.Creatinine, expression(paste("log Creatinine (",mu,"mol ", L^-1,")")))

rcs_plot <- cowplot::plot_grid(Age_plot,BMI_plot,Na_plot,
                                      K_plot,WCC_plot,Albumin_plot,
                                      Hb_plot,Pulse_plot,BP_plot,
                                      SpO2_plot,Urea_plot,Creat_plot,  nrow = 4, ncol = 3, scale = .95, labels = "auto")

rcs_model_plot <- cowplot::plot_grid(Age_plot,BMI_plot,Na_plot,
                                     WCC_plot,Creat_plot,
                                     Urea_plot,  nrow = 6, ncol = 1, scale = .95, labels = "auto")

ggsave(filename = "./figures/rcs_plot_3_4.eps", plot = rcs_plot, dpi = 500, width = 300, height = 300, units = "mm", device = cairo_ps)
ggsave(filename = "./figures/rcs_model_plot3_4.eps", plot = rcs_model_plot, dpi = 500, width = 150, height = 340, units = "mm", device = cairo_ps)

