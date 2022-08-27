## Compute 95% CIs for ROC curves
ROC_CIs_POMS_plot <- function(data, imputation, seed, n.boot) {
  
  to.return <- list()
  return.matrix <- matrix(ncol = length(seq(0,1,0.01)), nrow = length(1:n.boot))
  
  for (i in 1:n.boot) {
    
  dat <- complete(data, imputation) %>% select(POMS.overall, pred)
  
  set.seed <- seed+i
  boot.dat <- dat[sample(nrow(dat), nrow(dat), TRUE), ]
  
  roc_dat <- roc(boot.dat$POMS.overall, boot.dat$pred)
  roc_coords <- as.data.frame(coords(roc=roc_dat, x=seq(0,1,0.01), 
                                     input="specificity", transpose = FALSE)) %>%
    select(specificity, sensitivity) %>%
    pivot_wider(names_from = specificity, values_from = sensitivity)
  
  return.matrix[i,] <- as.numeric(roc_coords[1,])
  
  }
  
  POMS_roc <- as.data.frame(return.matrix) %>%
    pivot_longer(names_to = "specificity", cols = everything(), values_to = "sensitivity") %>%
    mutate(specificity = rep(seq(0,1,0.01), n.boot)) %>%
    group_by(specificity) %>%
    summarise(mean.sensitivity = log(mean(sensitivity)),
              within.variance = log(var(sensitivity))) %>%
    mutate(mean.sensitivity = exp(mean.sensitivity),
           within.variance = exp(within.variance))
  
  
  return(POMS_roc)
}

POMS_ROC_plot_data <- function(data, seed, boot.n, n.imp) {
  
  temp.list <- list()
  
  data <- complete(data, action = "long", include = TRUE) %>%
    mutate(pred = inv.logit(-3.3081876 +
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
                              0.0000047440728*pmax(S01AgeYears.centred-16,0)^3)) %>%
    as.mids()
  
  for (p in 1:n.imp){
    
    .imp <- p
    
    roc_temp <- ROC_CIs_POMS_plot(data, imputation = .imp, seed = seed, n.boot = boot.n) %>%
      mutate(imp = .imp)
    
    temp.list[[.imp]] <- roc_temp
    
  }
  
  return.dataframe <- bind_rows(temp.list)
  
  matrix.data <- matrix(NA,nrow = nrow(return.dataframe)/n.imp, 2)
  
  return.dataframe <- return.dataframe %>%
    mutate(.group = rep(seq(1,(nrow(.)/n.imp)),n.imp))
  
  for (i in 1:(nrow(return.dataframe)/10)){
    
    temp.data <- pool.scalar(return.dataframe$mean.sensitivity[c(seq(i,nrow(return.dataframe),nrow(return.dataframe)/n.imp))], 
                             return.dataframe$within.variance[seq(i,nrow(return.dataframe),nrow(return.dataframe)/n.imp)])
    
    
    matrix.data[i,1] <- temp.data$qbar
    matrix.data[i,2] <- temp.data$t  
    
  }
  
  matrix.data <- as.data.frame(matrix.data)
  names(matrix.data) <- c("sensitivity", "total_variance")
  matrix.data <- matrix.data %>% 
    mutate(lower.CI = sensitivity-1.96*sqrt(total_variance),
           upper.CI = sensitivity+1.96*sqrt(total_variance),
           specificity = seq(0,1,0.01))
  
  return(matrix.data)
}

roc_POMS <- POMS_ROC_plot_data(mids_data, 10, seed = 1107, boot.n = 1000)

save(roc_POMS, file = "roc_POMS.RData")
load("roc_POMS.Rdata")

##POMSmajor
POMSmajor_ROC_plot_data <- function(data, seed, boot.n, n.imp) {
  
  temp.list <- list()
  
  data <- complete(data, action = "long", include = TRUE) %>%
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
                              0.0000047440728*pmax(S01AgeYears.centred-16,0)^3) %>%
    mutate(Major_pred = inv.logit(lrm.mult.impute.POMSmajor$coefficients[1]+
                                    lrm.mult.impute.POMSmajor$coefficients[2]*POMS.logit)) %>%
    select(.imp, .id, POMS.major, Major_pred) %>%
    as.mids()
  
  for (p in 1:n.imp){
    
    .imp <- p
    
    roc_temp <- ROC_CIs_POMSmajor_plot(data, imputation = .imp, seed = seed, n.boot = boot.n) %>%
      mutate(imp = .imp)
    
    temp.list[[.imp]] <- roc_temp
    
  }
  
  return.dataframe <- bind_rows(temp.list)
  
  matrix.data <- matrix(NA,nrow = nrow(return.dataframe)/n.imp, 2)
  
  return.dataframe <- return.dataframe %>%
    mutate(.group = rep(seq(1,(nrow(.)/n.imp)),n.imp))
  
  for (i in 1:(nrow(return.dataframe)/10)){
    
    temp.data <- pool.scalar(return.dataframe$mean.sensitivity[c(seq(i,nrow(return.dataframe),nrow(return.dataframe)/n.imp))], 
                             return.dataframe$within.variance[seq(i,nrow(return.dataframe),nrow(return.dataframe)/n.imp)])
    
    
    matrix.data[i,1] <- temp.data$qbar
    matrix.data[i,2] <- temp.data$t  
    
  }
  
  matrix.data <- as.data.frame(matrix.data)
  names(matrix.data) <- c("sensitivity", "total_variance")
  matrix.data <- matrix.data %>% 
    mutate(lower.CI = sensitivity-1.96*sqrt(total_variance),
           upper.CI = sensitivity+1.96*sqrt(total_variance),
           specificity = seq(0,1,0.01))
  
  return(matrix.data)
}

ROC_CIs_POMSmajor_plot <- function(data, imputation, seed, n.boot) {
  
  to.return <- list()
  return.matrix <- matrix(ncol = length(seq(0,1,0.01)), nrow = length(1:n.boot))
  
  for (i in 1:n.boot) {
    
    dat <- complete(data, imputation) %>% select(POMS.major, Major_pred)
    
    set.seed <- seed+i
    boot.dat <- dat[sample(nrow(dat), nrow(dat), TRUE), ]
    
    roc_dat <- roc(boot.dat$POMS.major, boot.dat$Major_pred)
    roc_coords <- as.data.frame(coords(roc=roc_dat, x=seq(0,1,0.01), 
                                       input="specificity", transpose = FALSE)) %>%
      select(specificity, sensitivity) %>%
      pivot_wider(names_from = specificity, values_from = sensitivity)
    
    return.matrix[i,] <- as.numeric(roc_coords[1,])
    
  }
  
  POMSmajor_roc <- as.data.frame(return.matrix) %>%
    pivot_longer(names_to = "specificity", cols = everything(), values_to = "sensitivity") %>%
    mutate(specificity = rep(seq(0,1,0.01), n.boot)) %>%
    group_by(specificity) %>%
    summarise(mean.sensitivity = log(mean(sensitivity)),
              within.variance = log(var(sensitivity))) %>%
    mutate(mean.sensitivity = exp(mean.sensitivity),
           within.variance = exp(within.variance))
  
  
  return(POMSmajor_roc)
}

roc_POMSmajor <- POMSmajor_ROC_plot_data(mids_data, 10, seed = 1202, boot.n = 1000)

save(roc_POMSmajor, file = "roc_POMSmajor.RData")
load("roc_POMSmajor.Rdata")




POMS_ROC_plot <- ggplot(roc_POMS) +
  geom_abline(slope = 1, intercept = 0, color = "light grey") +
  geom_line(aes(x=1-specificity,
            y = sensitivity))  +
  geom_ribbon(aes(x=1-specificity,
                  ymin = lower.CI,
                  ymax = upper.CI),
              alpha = 0.6, fill = "grey") +
  theme_thesis() +
  labs(x = "1 - specificity",
       y = "sensitivity") +
  scale_y_continuous(limits = c(0,1),
                     breaks = seq(0,1,0.1),
                     expand = c(0, 0)) +
  scale_x_continuous(limits = c(0,1),
                     breaks = seq(0,1,0.1),
                     expand = c(0, 0))

POMSmajor_ROC_plot <- ggplot(roc_POMSmajor) +
  geom_abline(slope = 1, intercept = 0, color = "light grey") +
  geom_line(aes(x=1-specificity,
                y = sensitivity))  +
  geom_ribbon(aes(x=1-specificity,
                  ymin = lower.CI,
                  ymax = upper.CI),
              alpha = 0.6, fill = "grey") +
  theme_thesis() +
  labs(x = "1 - specificity",
       y = "sensitivity") +
  scale_y_continuous(limits = c(0,1),
                     breaks = seq(0,1,0.1),
                     expand = c(0, 0)) +
  scale_x_continuous(limits = c(0,1),
                     breaks = seq(0,1,0.1),
                     expand = c(0, 0))



ROC_figure <- plot_grid(POMS_ROC_plot, POMSmajor_ROC_plot, 
                        align = 'h',
                      labels = c("a", "b"),
                      nrow = 1,
                      label_size = 14)

save_plot("./figures/ROC_figure.jpg", ROC_figure, ncol = 2, nrow = 1, dpi=600,
          base_asp = 1)

