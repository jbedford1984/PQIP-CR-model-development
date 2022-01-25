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
