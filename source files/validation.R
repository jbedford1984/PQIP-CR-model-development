### Function to extract individual fits from boostrap validate object
## M = number of multiple imputations 
## mids.object = multiple imputation dataset, type 'mids'

extract_fits_validate <- function(mids.object, M, model, seed) {
  
  validation_list <- list()
  for (i in 1:M) {
    
    mid_data <- complete(mids.object,i)
    
    mids_fit <- lrm(model, data = mid_data, x=TRUE, y=TRUE)
    
    print(paste0("validation ",i,":", 1))
    set.seed(seed+i)
    mids_validate <- validate(mids_fit, B=1000)
    
    print(paste0("validation ",i,":", 2))
    set.seed(seed+i)
    capture_mids_validate <- capture.output(print(validate(mids_fit, B=1000, pr = TRUE)))
    
    capture_mids_validate <- gsub("\\s+", ",", gsub("^\\s+|\\s+$", "",capture_mids_validate))
    capture_mids_validate <- gsub("Iteration:,([0-9]{2}|[0-9]{3}|[0-9]{4}),of,([0-9]{3}|[0-9]{4}),", '', capture_mids_validate) 
    capture_mids_validate <- gsub("Iteration:,([0-9]{2}|[0-9]{3}|[0-9]{4}),of,([0-9]{2}|[0-9]{3}|[0-9]{4})", '', capture_mids_validate) %>%
      str_replace(., "training,test", "measure,training,test") %>% 
      str_split(., ",")
    
    capture_mids_validate <- capture_mids_validate[-c((length(capture_mids_validate)-13):length(capture_mids_validate))]
    capture_mids_validate <- capture_mids_validate[-c((length(capture_mids_validate)-11))]
    
    options(pillar.sigfig = 10)
    capture_mids_validate <- data.frame(matrix(unlist(capture_mids_validate), ncol = 3, byrow=TRUE)) %>%
      rename(measure = X1, training = X2, test = X3) %>% 
      filter(measure != "measure") %>%
      mutate(.imp = i,
             bootstrap = rep(1:1000, each = 10)) %>%
      pivot_longer(names_to = "fit_data", cols = c(training, test)) %>%
      pivot_wider(names_from = c(measure, fit_data), values_from = value) %>%
      mutate_if(is.character,as.numeric)
    
    name <- paste('validation_m',i,sep='')
    tmp <- list(mids_validate=capture_mids_validate, validate=mids_validate)
    validation_list[[name]] <- tmp
    
  }
  
  return(validation_list)
}


validation_calculations <- function(validation_data, original_mids_object, model) {
  
  temp_data <- rbind.data.frame(tibble(validation_data$validation_m1$mids_validate),
                                tibble(validation_data$validation_m2$mids_validate),
                                tibble(validation_data$validation_m3$mids_validate),
                                tibble(validation_data$validation_m4$mids_validate),
                                tibble(validation_data$validation_m5$mids_validate),
                                tibble(validation_data$validation_m6$mids_validate),
                                tibble(validation_data$validation_m7$mids_validate),
                                tibble(validation_data$validation_m8$mids_validate),
                                tibble(validation_data$validation_m9$mids_validate),
                                tibble(validation_data$validation_m10$mids_validate)) %>%
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
    model.fit <- lrm(model, data=complete(original_mids_object,i))
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
  
  
  temp_data <- left_join(temp_data, imp.c_stat, by = ".imp") %>%
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
           c_stat_SE = optimism_corrected_c_stat - mean_c_stat_imp,
           Brier_SE.sq = (optimism_corrected_brier-mean_Brier_imp)^2,
           Brier_SE = optimism_corrected_brier-mean_Brier_imp)
  
  
}
