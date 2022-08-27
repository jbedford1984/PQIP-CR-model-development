## create calibration curve ## method is based on Harrell's RMS calibrate function, with code produced using 
## https://randomeffect.net/post/2021/03/10/bias-corrected-calibration-curve-from-scratch/


cal_simulation_POMS <- function(data, n, model, n.boot, seed) {
  
  to.return <- list()
  
  ## Add predictions to main dataframe
  data <- complete(data, include = TRUE, action = "long") %>%
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
    mutate(pred = inv.logit(pred.logit)) %>%
    as.mids()
  
  ## Storage for bootstrap optimism (one row per bootstrap resample)
  ## Number of bootstrap replicates
  nsim <- n.boot
  opt.out <- matrix(NA, nsim, 60)
    
    dat <- complete(data, n)
    
    ## Range of inputs on which to calculate calibrations
    srange <- seq(min(dat$pred), max(dat$pred), length.out = 52)
    
    ## Discard the smallest and largest probs for robustness, and agreement with rms::calibrate
    srange <- srange[5 : (length(srange) - 4)]
    
    ## The apparent calibration is determined by this loess curve.
    apparent.cal.fit <- with(dat, lowess(pred, POMS.overall, iter = 0))
    app.cal.fun <- approxfun(apparent.cal.fit$x, apparent.cal.fit$y)
    app.cal.pred <- app.cal.fun(srange)
  
for (i in 1:nsim) {
  
  set.seed(seed*i)
  ## Sample bootstrap data set from original data
  dat.boot <- dat[sample(nrow(dat), nrow(dat), TRUE), ]
  
  fit <- glm(model, 
             family = "binomial", data = dat)
  
  ## Fit logistic model using the bootstrap data
  fit.boot <- update(fit, data = dat.boot)
  
  ## Make a DF of the bootstrap model and bootstrap predictions
  pdat.boot <- data.frame(y = ifelse(dat.boot$POMS.overall == TRUE, 1, 0),
                          prob = predict(fit.boot, dat.boot, type = "response"))
  
  ## Fit a calibration curve to the bootstrap data
  boot.cal.fit <- with(pdat.boot, lowess(prob, y, iter = 0))
  boot.cal.fun <- approxfun(boot.cal.fit$x, boot.cal.fit$y)
  
  ## Collect a set of them for comparison
  boot.cal.pred <- boot.cal.fun(srange)
  
  ## Apply the bootstrap model to the original data
  prob.boot.orig <- predict(fit.boot, dat, type = "response")
  
  ## Make a DF of the boot model predictions on original data
  pdat.boot.orig <- data.frame(y = ifelse(dat$POMS.overall == TRUE, 1, 0),
                               prob = prob.boot.orig)
  
  ## Fit a calibration curve to the original data w/ boot model predictions
  boot.cal.orig.fit <- with(pdat.boot.orig, lowess(prob, y, iter = 0))
  boot.cal.orig.fun <- approxfun(boot.cal.orig.fit$x, boot.cal.orig.fit$y)
  
  ## Collect a set of them for comparison
  boot.cal.orig.pred <- boot.cal.orig.fun(srange)
  
  ## Take the difference for estimate of optimism
  opt <- boot.cal.pred - boot.cal.orig.pred
  
  opt.out[i,] <- opt 
}
    to.return[[1]] <- opt.out
    to.return[[2]] <- srange
    to.return[[3]] <- app.cal.pred
    
    return(to.return)
  }
  

calibration_curve_POMS_CI <- function(n.imputations){ 
  
  table_return <- list()

for (.imp in 1:n.imputations) {
  calibration_POMS <- cal_simulation_POMS(mids_data, n=.imp, model = final.model, n.boot=500, seed=1010*.imp)

## The bias corrected calibration curve is the apparent calibration less the average bootstrap optimism
opt.out_POMS <- as.data.frame(calibration_POMS[[1]]) %>% 
  summarise(across(everything(), ~ mean(.x, na.rm = TRUE)))

opt.out_POMS_var <- as.data.frame(calibration_POMS[[1]]) %>% 
  summarise(across(everything(), ~ var(.x, na.rm = TRUE)))

bias.corrected.cal_POMS <- calibration_POMS[[3]] - opt.out_POMS
ppdat_POMS <- data.frame(calibration_POMS[[2]], calibration_POMS[[3]], as.numeric(bias.corrected.cal_POMS[1,]),
                           as.numeric(opt.out_POMS_var))
names(ppdat_POMS) <- c("srange", "app.cal.pred", "bias.corrected.cal", "var.within")


table_return[[.imp]] <- ppdat_POMS

}
  return(table_return)
}

POMS_calibration_data <- calibration_curve_POMS_CI(10)

POMS_calibration_rubins <- as.data.frame(rbind(POMS_calibration_data[[1]],
                                 POMS_calibration_data[[2]],
                                 POMS_calibration_data[[3]],
                                 POMS_calibration_data[[4]],
                                 POMS_calibration_data[[5]],
                                 POMS_calibration_data[[6]],
                                 POMS_calibration_data[[7]],
                                 POMS_calibration_data[[8]],
                                 POMS_calibration_data[[9]],
                                 POMS_calibration_data[[10]])) %>%
  mutate(.imp = rep(1:10,each=52))

rubins_combine <- function(data, n.imp) {
  
  matrix.data <- matrix(NA,nrow = nrow(data)/n.imp, 2)
  
  data <- data %>%
    mutate(.group = rep(seq(1,(nrow(.)/n.imp)),n.imp))
  
  for (i in 1:(nrow(data)/10)){
  
  temp.data <- pool.scalar(data$bias.corrected.cal[c(seq(i,nrow(data),nrow(data)/n.imp))], 
                           data$var.within[seq(i,nrow(data),nrow(data)/n.imp)])
  
  
  matrix.data[i,1] <- temp.data$qbar
  matrix.data[i,2] <- temp.data$t  
    
  }
  
  temp.2 <- data %>% group_by(.group) %>% 
                         summarise(srange = mean(srange),
                                   app.cal.pred = mean(app.cal.pred)) %>% 
    select(srange, app.cal.pred)
  
  matrix.data <- as.data.frame(matrix.data)
  names(matrix.data) <- c("univariate_estimate", "total_variance")
  matrix.data <- matrix.data %>% 
    mutate(lower.CI = univariate_estimate-1.96*sqrt(total_variance),
           upper.CI = univariate_estimate+1.96*sqrt(total_variance),
           srange = temp.2$srange,
           app.cal.pred = temp.2$app.cal.pred)
  
  return(as.data.frame(matrix.data))
  
}

calibration_POMS_CIs <- rubins_combine(data = POMS_calibration_rubins, n.imp = 10)

POMS_cal_plot <- ggplot(calibration_POMS_CIs, aes(srange*100, app.cal.pred*100)) +
  geom_abline(slope = 1, intercept = 0, color = "light grey") +
  geom_line(linetype = "dotted", color = "black") +
  geom_line(aes(y = univariate_estimate*100), color = "black") +
  geom_ribbon(aes(ymin = lower.CI*100, 
                  ymax=upper.CI*100),
              alpha=0.1, fill = "black") +
  scale_x_continuous(breaks = seq(0, 100, 10), limits = c(0,70),
                     expand = c(0,0)) +
  scale_y_continuous(breaks = seq(0, 100, 10), limits = c(0,70),
                     expand = c(0,0)) +
  theme_thesis() +
  scale_colour_manual(values = cbPalette) +
  xlab("Predicted risk (%)") +
  ylab("Observed risk (%)") + 
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14)) +
  theme_classic()

  

calibration_POMS <- predictions %>% 
  select(pred, Id) %>% 
  group_by(Id) %>%
  summarise(pred = mean(pred)*100)

hist_top_POMS <- ggplot()+geom_histogram(aes(calibration_POMS$pred), 
                                              bins = 150, colour = "white") +
  theme_void() +
  scale_x_continuous(breaks = seq(0,70,10),
                     limits = c(0,70),
                     expand = c(0.05,0.05))

  

#### POMS-major calibration curve
cal_simulation_POMSmajor <- function(data, n, model, n.boot, seed) {
  
  to.return <- list()
  
  ## Add predictions to main dataframe
  data <- complete(data, include = TRUE, action = "long") %>%
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
                  POMS.pred = inv.logit(POMS.logit)) %>%
             mutate(Major_pred = inv.logit(lrm.mult.impute.POMSmajor$coefficients[1]+
                                             lrm.mult.impute.POMSmajor$coefficients[2]*POMS.logit)) %>%
             select(.imp, .id, POMS.logit, POMS.major, Major_pred) %>%
             as.mids()
  
  ## Storage for bootstrap optimism (one row per bootstrap resample)
  ## Number of bootstrap replicates
  nsim <- n.boot
  opt.out <- matrix(NA, nsim, 52)
  
  dat <- complete(data, n)
  
  ## Range of inputs on which to calculate calibrations
  srange <- seq(min(dat$Major_pred), max(dat$Major_pred), length.out = 60)
  
  ## Discard the smallest and largest probs for robustness, and agreement with rms::calibrate
  srange <- srange[5 : (length(srange) - 4)]
  
  ## The apparent calibration is determined by this loess curve.
  apparent.cal.fit <- with(dat, lowess(Major_pred, POMS.major, iter = 0))
  app.cal.fun <- approxfun(apparent.cal.fit$x, apparent.cal.fit$y)
  app.cal.pred <- app.cal.fun(srange)
  
  for (i in 1:nsim) {
    
    set.seed(seed*i)

    ## Sample bootstrap data set from original data
    dat.boot <- dat[sample(nrow(dat), nrow(dat), TRUE), ]
    
    fit <- glm(model, 
               family = "binomial", data = dat)
    
    ## Fit logistic model using the bootstrap data
    fit.boot <- update(fit, data = dat.boot)
    
    ## Make a DF of the bootstrap model and bootstrap predictions
    pdat.boot <- data.frame(y = ifelse(dat.boot$POMS.major == TRUE, 1, 0),
                            prob = predict(fit.boot, dat.boot, type = "response"))
    
    ## Fit a calibration curve to the bootstrap data
    boot.cal.fit <- with(pdat.boot, lowess(prob, y, iter = 0))
    boot.cal.fun <- approxfun(boot.cal.fit$x, boot.cal.fit$y)
    
    ## Collect a set of them for comparison
    boot.cal.pred <- boot.cal.fun(srange)
    
    ## Apply the bootstrap model to the original data
    prob.boot.orig <- predict(fit.boot, dat, type = "response")
    
    ## Make a DF of the boot model predictions on original data
    pdat.boot.orig <- data.frame(y = ifelse(dat$POMS.major == TRUE, 1, 0),
                                 prob = prob.boot.orig)
    
    ## Fit a calibration curve to the original data w/ boot model predictions
    boot.cal.orig.fit <- with(pdat.boot.orig, lowess(prob, y, iter = 0))
    boot.cal.orig.fun <- approxfun(boot.cal.orig.fit$x, boot.cal.orig.fit$y)
    
    ## Collect a set of them for comparison
    boot.cal.orig.pred <- boot.cal.orig.fun(srange)
    
    ## Take the difference for estimate of optimism
    opt <- boot.cal.pred - boot.cal.orig.pred
    
    opt.out[i,] <- opt 
  }
  to.return[[1]] <- opt.out
  to.return[[2]] <- srange
  to.return[[3]] <- app.cal.pred
  
  return(to.return)
}

calibration_curve_POMSmajor_CI <- function(n.imputations){ 
  
  table_return <- list()
  
  for (.imp in 1:n.imputations) {
    calibration_POMSmajor <- cal_simulation_POMSmajor(mids_data, n=.imp, model = model_POMS.major, n.boot=500, seed=767*.imp)
    
    ## The bias corrected calibration curve is the apparent calibration less the average bootstrap optimism
    opt.out_POMS <- as.data.frame(calibration_POMSmajor[[1]]) %>% 
      summarise(across(everything(), ~ mean(.x, na.rm = TRUE)))
    
    opt.out_POMS_var <- as.data.frame(calibration_POMSmajor[[1]]) %>% 
      summarise(across(everything(), ~ var(.x, na.rm = TRUE)))
    
    bias.corrected.cal_POMS <- calibration_POMSmajor[[3]] - opt.out_POMS
    ppdat_POMS <- data.frame(calibration_POMSmajor[[2]], calibration_POMSmajor[[3]], as.numeric(bias.corrected.cal_POMS[1,]),
                             as.numeric(opt.out_POMS_var))
    names(ppdat_POMS) <- c("srange", "app.cal.pred", "bias.corrected.cal", "var.within")
    
    
    table_return[[.imp]] <- ppdat_POMS
    
  }
  return(table_return)
}


POMSmajor_calibration_data <- calibration_curve_POMSmajor_CI(10)

POMSmajor_calibration_rubins <- as.data.frame(rbind(POMSmajor_calibration_data[[1]],
                                               POMSmajor_calibration_data[[2]],
                                               POMSmajor_calibration_data[[3]],
                                               POMSmajor_calibration_data[[4]],
                                               POMSmajor_calibration_data[[5]],
                                               POMSmajor_calibration_data[[6]],
                                               POMSmajor_calibration_data[[7]],
                                               POMSmajor_calibration_data[[8]],
                                               POMSmajor_calibration_data[[9]],
                                               POMSmajor_calibration_data[[10]])) %>%
  mutate(.imp = rep(1:10,each=52))


calibration_POMSmajor_CIs <- rubins_combine(data = POMSmajor_calibration_rubins, n.imp = 10)

POMSmajor_plot <- ggplot(calibration_POMSmajor_CIs, aes(srange*100, app.cal.pred*100)) +
  geom_abline(slope = 1, intercept = 0, color = "light grey") +
  geom_line(linetype = "dotted", color = "black") +
  geom_line(aes(y = univariate_estimate*100), color = "black") +
  geom_ribbon(aes(ymin = lower.CI*100, 
                  ymax=upper.CI*100),
              alpha=0.1, fill = "black") +
  scale_x_continuous(breaks = seq(0, 100, 10), limits = c(0,70),
                     expand = c(0,0)) +
  scale_y_continuous(breaks = seq(0, 100, 10), limits = c(0,70),
                     expand = c(0,0)) +
  theme_thesis() +
  scale_colour_manual(values = cbPalette) +
  xlab("Predicted risk (%)") +
  ylab("Observed risk (%)") + 
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14)) +
  theme_classic()


calibration_POMSmajor <- complete(calibration_data_sensitivity_POMSmajor, action = "long",
                                  include = FALSE) %>% 
  mutate(Major_pred = inv.logit(lrm.mult.impute.POMSmajor$coefficients[1]+
                                  lrm.mult.impute.POMSmajor$coefficients[2]*POMS.logit)) %>%
  select(.imp, .id, POMS.logit, POMS.major, Major_pred) %>%
  #mutate(Major_pred = Major_pred*100) 
  group_by(.id) %>%
  summarise(Major_pred = mean(Major_pred)*100)


hist_top_POMSmajor <- ggplot()+geom_histogram(aes(calibration_POMSmajor$Major_pred), 
                                    bins = 150, colour = "white") +
  theme_void() +
  scale_x_continuous(breaks = seq(0,70,10),
                     limits = c(0,70),
                     expand = c(0.05,0.05)) 


figure_2 <- plot_grid(hist_top_POMS + theme(plot.margin = unit(c(0.5, 0, 0, 0), "cm")), 
          hist_top_POMSmajor + theme(plot.margin = unit(c(0.5, 0, 0, 0), "cm")), 
          POMS_cal_plot, 
          POMSmajor_plot,
           align = 'v',
           labels = c("a", "b"),
           rel_heights = c(1,5),
           nrow = 2,
          label_size = 18)

save_plot("./figures/figure_2.jpg", figure_2, ncol = 2, nrow = 2, dpi=600)

