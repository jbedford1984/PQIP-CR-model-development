## Decide number of knots for analysis based on AIC of univariate models
#Age

AIC_extract_age <- function (data) {
  
  AIC_list <- vector()
  
  AIC_ <- vector()
  
  for (i in 1:10) {
  m1 <- lrm(formula = POMS.overall ~ rcs(S01AgeYears.centred,3), data = mice::complete(data, i),
          x=TRUE, y=TRUE)
  
  AIC_[i] <- AIC(m1)
    }
  
  k.3.AIC <- mean(AIC_)
  AIC_list[1] <- k.3.AIC
  
  for (i in 1:10) {
    m2 <- lrm(formula = POMS.overall ~ rcs(S01AgeYears.centred,4), data = complete(data, i),
              x=TRUE, y=TRUE)
    
    AIC_[i] <- AIC(m2)
    }
  
  k.4.AIC <- mean(AIC_)
  AIC_list[2] <- k.4.AIC
  
  for (i in 1:10) {
    m3 <- lrm(formula = POMS.overall ~ rcs(S01AgeYears.centred,5), data = complete(data, i),
              x=TRUE, y=TRUE)
    
    AIC_[i] <- AIC(m3)
    
  }
  
  k.5.AIC <- mean(AIC_)
  AIC_list[3] <- k.5.AIC
  
  knots <- 2 + which.min(c(k.3.AIC, k.4.AIC)) #k.5.AIC))
  return(knots)
  #return(AIC_list)
  
}
  
Age.knots <- AIC_extract_age(mids_data)

### BMI ###

AIC_extract_BMI <- function (data) {
  
  AIC_list <- vector()
  
  AIC_ <- vector()
  
  for (i in 1:10) {
    m1 <- lrm(formula = POMS.overall ~ rcs(S01BMI.centred,3), data = mice::complete(data, i),
              x=TRUE, y=TRUE)
    
    AIC_[i] <- AIC(m1)
  }
  
  k.3.AIC <- mean(AIC_)
  AIC_list[1] <- k.3.AIC
  
  for (i in 1:10) {
    m2 <- lrm(formula = POMS.overall ~ rcs(S01BMI.centred,4), data = complete(data, i),
              x=TRUE, y=TRUE)
    
    AIC_[i] <- AIC(m2)
  }
  
  k.4.AIC <- mean(AIC_)
  AIC_list[2] <- k.4.AIC
  
  for (i in 1:10) {
    m3 <- lrm(formula = POMS.overall ~ rcs(S01BMI.centred,5), data = complete(data, i),
              x=TRUE, y=TRUE)
    
    AIC_[i] <- AIC(m3)
    
  }
  
  k.5.AIC <- mean(AIC_)
  AIC_list[3] <- k.5.AIC
  
  knots <- 2 + which.min(c(k.3.AIC, k.4.AIC)) #k.5.AIC))
  return(knots)
  #return(AIC_list)
  
}

BMI.knots <- AIC_extract_BMI(mids_data)


### Sodium ###

AIC_extract_Na <- function (data) {
  
  AIC_list <- vector()
  
  AIC_ <- vector()
  
  for (i in 1:10) {
    m1 <- lrm(formula = POMS.overall ~ rcs(S02SerumSodium.centred,3), data = mice::complete(data, i),
              x=TRUE, y=TRUE)
    
    AIC_[i] <- AIC(m1)
  }
  
  k.3.AIC <- mean(AIC_)
  AIC_list[1] <- k.3.AIC
  
  for (i in 1:10) {
    m2 <- lrm(formula = POMS.overall ~ rcs(S02SerumSodium.centred,4), data = complete(data, i),
              x=TRUE, y=TRUE)
    
    AIC_[i] <- AIC(m2)
  }
  
  k.4.AIC <- mean(AIC_)
  AIC_list[2] <- k.4.AIC
  
  for (i in 1:10) {
    m3 <- lrm(formula = POMS.overall ~ rcs(S02SerumSodium.centred,5), data = complete(data, i),
              x=TRUE, y=TRUE)
    
    AIC_[i] <- AIC(m3)
    
  }
  
  k.5.AIC <- mean(AIC_)
  AIC_list[3] <- k.5.AIC
  
  knots <- 2 + which.min(c(k.3.AIC, k.4.AIC)) #k.5.AIC))
  return(knots)
  #return(AIC_list)
  
}

Na.knots <- AIC_extract_Na(mids_data)

### Potassium ###

AIC_extract_K <- function (data) {
  
  AIC_list <- vector()
  
  AIC_ <- vector()
  
  for (i in 1:10) {
    m1 <- lrm(formula = POMS.overall ~ rcs(S02SerumPotassium.centred,3), data = mice::complete(data, i),
              x=TRUE, y=TRUE)
    
    AIC_[i] <- AIC(m1)
  }
  
  k.3.AIC <- mean(AIC_)
  AIC_list[1] <- k.3.AIC
  
  for (i in 1:10) {
    m2 <- lrm(formula = POMS.overall ~ rcs(S02SerumPotassium.centred,4), data = complete(data, i),
              x=TRUE, y=TRUE)
    
    AIC_[i] <- AIC(m2)
  }
  
  k.4.AIC <- mean(AIC_)
  AIC_list[2] <- k.4.AIC
  
  for (i in 1:10) {
    m3 <- lrm(formula = POMS.overall ~ rcs(S02SerumPotassium.centred,5), data = complete(data, i),
              x=TRUE, y=TRUE)
    
    AIC_[i] <- AIC(m3)
    
  }
  
  k.5.AIC <- mean(AIC_)
  AIC_list[3] <- k.5.AIC
  
  knots <- 2 + which.min(c(k.3.AIC, k.4.AIC)) #k.5.AIC))
  return(knots)
  #return(AIC_list)
  
}

K.knots <- AIC_extract_K(mids_data)

### Urea ###

AIC_extract_Urea <- function (data) {
  
  AIC_list <- vector()
  
  AIC_ <- vector()
  
  for (i in 1:10) {
    m1 <- lrm(formula = POMS.overall ~ rcs(log.Urea.centred,3), data = mice::complete(data, i),
              x=TRUE, y=TRUE)
    
    AIC_[i] <- AIC(m1)
  }
  
  k.3.AIC <- mean(AIC_)
  AIC_list[1] <- k.3.AIC
  
  for (i in 1:10) {
    m2 <- lrm(formula = POMS.overall ~ rcs(log.Urea.centred,4), data = complete(data, i),
              x=TRUE, y=TRUE)
    
    AIC_[i] <- AIC(m2)
  }
  
  k.4.AIC <- mean(AIC_)
  AIC_list[2] <- k.4.AIC
  
  for (i in 1:10) {
    m3 <- lrm(formula = POMS.overall ~ rcs(log.Urea.centred,5), data = complete(data, i),
              x=TRUE, y=TRUE)
    
    AIC_[i] <- AIC(m3)
    
  }
  
  k.5.AIC <- mean(AIC_)
  AIC_list[3] <- k.5.AIC
  
  knots <- 2 + which.min(c(k.3.AIC, k.4.AIC)) #k.5.AIC))
  return(knots)
  #return(AIC_list)
  
}

Urea.knots <- AIC_extract_Urea(mids_data)

### Creatinine ###

AIC_extract_Creatinine <- function (data) {
  
  AIC_list <- vector()
  
  AIC_ <- vector()
  
  for (i in 1:10) {
    m1 <- lrm(formula = POMS.overall ~ rcs(log.Creatinine.centred,3), data = mice::complete(data, i),
              x=TRUE, y=TRUE)
    
    AIC_[i] <- AIC(m1)
  }
  
  k.3.AIC <- mean(AIC_)
  AIC_list[1] <- k.3.AIC
  
  for (i in 1:10) {
    m2 <- lrm(formula = POMS.overall ~ rcs(log.Creatinine.centred,4), data = complete(data, i),
              x=TRUE, y=TRUE)
    
    AIC_[i] <- AIC(m2)
  }
  
  k.4.AIC <- mean(AIC_)
  AIC_list[2] <- k.4.AIC
  
  for (i in 1:10) {
    m3 <- lrm(formula = POMS.overall ~ rcs(log.Creatinine.centred,5), data = complete(data, i),
              x=TRUE, y=TRUE)
    
    AIC_[i] <- AIC(m3)
    
  }
  
  k.5.AIC <- mean(AIC_)
  AIC_list[3] <- k.5.AIC
  
  knots <- 2 + which.min(c(k.3.AIC, k.4.AIC)) #k.5.AIC))
  return(knots)
  #return(AIC_list)
  
}

Creatinine.knots <- AIC_extract_Creatinine(mids_data)

### Albumin ###

AIC_extract_Albumin <- function (data) {
  
  AIC_list <- vector()
  
  AIC_ <- vector()
  
  for (i in 1:10) {
    m1 <- lrm(formula = POMS.overall ~ rcs(S02SerumAlbumin.centred,3), data = mice::complete(data, i),
              x=TRUE, y=TRUE)
    
    AIC_[i] <- AIC(m1)
  }
  
  k.3.AIC <- mean(AIC_)
  AIC_list[1] <- k.3.AIC
  
  for (i in 1:10) {
    m2 <- lrm(formula = POMS.overall ~ rcs(S02SerumAlbumin.centred,4), data = complete(data, i),
              x=TRUE, y=TRUE)
    
    AIC_[i] <- AIC(m2)
  }
  
  k.4.AIC <- mean(AIC_)
  AIC_list[2] <- k.4.AIC
  
  for (i in 1:10) {
    m3 <- lrm(formula = POMS.overall ~ rcs(S02SerumAlbumin.centred,5), data = complete(data, i),
              x=TRUE, y=TRUE)
    
    AIC_[i] <- AIC(m3)
    
  }
  
  k.5.AIC <- mean(AIC_)
  AIC_list[3] <- k.5.AIC
  
  knots <- 2 + which.min(c(k.3.AIC, k.4.AIC)) #k.5.AIC))
  return(knots)
  #return(AIC_list)
  
}

Albumin.knots <- AIC_extract_Albumin(mids_data)

### WCC ###

AIC_extract_WCC <- function (data) {
  
  AIC_list <- vector()
  
  AIC_ <- vector()
  
  for (i in 1:10) {
    m1 <- lrm(formula = POMS.overall ~ rcs(S02WhiteCellCount.centred,3), data = mice::complete(data, i),
              x=TRUE, y=TRUE)
    
    AIC_[i] <- AIC(m1)
  }
  
  k.3.AIC <- mean(AIC_)
  AIC_list[1] <- k.3.AIC
  
  for (i in 1:10) {
    m2 <- lrm(formula = POMS.overall ~ rcs(S02WhiteCellCount.centred,4), data = complete(data, i),
              x=TRUE, y=TRUE)
    
    AIC_[i] <- AIC(m2)
  }
  
  k.4.AIC <- mean(AIC_)
  AIC_list[2] <- k.4.AIC
  
  for (i in 1:10) {
    m3 <- lrm(formula = POMS.overall ~ rcs(S02WhiteCellCount.centred,5), data = complete(data, i),
              x=TRUE, y=TRUE)
    
    AIC_[i] <- AIC(m3)
    
  }
  
  k.5.AIC <- mean(AIC_)
  AIC_list[3] <- k.5.AIC
  
  knots <- 2 + which.min(c(k.3.AIC, k.4.AIC)) #k.5.AIC))
  return(knots)
  #return(AIC_list)
  
}

WCC.knots <- AIC_extract_WCC(mids_data)

### Haemoglobin ###

AIC_extract_Hb <- function (data) {
  
  AIC_list <- vector()
  
  AIC_ <- vector()
  
  for (i in 1:10) {
    m1 <- lrm(formula = POMS.overall ~ rcs(S02Haemoglobin.centred,3), data = mice::complete(data, i),
              x=TRUE, y=TRUE)
    
    AIC_[i] <- AIC(m1)
  }
  
  k.3.AIC <- mean(AIC_)
  AIC_list[1] <- k.3.AIC
  
  for (i in 1:10) {
    m2 <- lrm(formula = POMS.overall ~ rcs(S02Haemoglobin.centred,4), data = complete(data, i),
              x=TRUE, y=TRUE)
    
    AIC_[i] <- AIC(m2)
  }
  
  k.4.AIC <- mean(AIC_)
  AIC_list[2] <- k.4.AIC
  
  for (i in 1:10) {
    m3 <- lrm(formula = POMS.overall ~ rcs(S02Haemoglobin.centred,5), data = complete(data, i),
              x=TRUE, y=TRUE)
    
    AIC_[i] <- AIC(m3)
    
  }
  
  k.5.AIC <- mean(AIC_)
  AIC_list[3] <- k.5.AIC
  
  knots <- 2 + which.min(c(k.3.AIC, k.4.AIC)) #k.5.AIC))
  return(knots)
  #return(AIC_list)
  
}

Hb.knots <- AIC_extract_Hb(mids_data)

### Pulse ###

AIC_extract_Pulse <- function (data) {
  
  AIC_list <- vector()
  
  AIC_ <- vector()
  
  for (i in 1:10) {
    m1 <- lrm(formula = POMS.overall ~ rcs(S02PulseRate.centred,3), data = mice::complete(data, i),
              x=TRUE, y=TRUE)
    
    AIC_[i] <- AIC(m1)
  }
  
  k.3.AIC <- mean(AIC_)
  AIC_list[1] <- k.3.AIC
  
  for (i in 1:10) {
    m2 <- lrm(formula = POMS.overall ~ rcs(S02PulseRate.centred,4), data = complete(data, i),
              x=TRUE, y=TRUE)
    
    AIC_[i] <- AIC(m2)
  }
  
  k.4.AIC <- mean(AIC_)
  AIC_list[2] <- k.4.AIC
  
  for (i in 1:10) {
    m3 <- lrm(formula = POMS.overall ~ rcs(S02PulseRate.centred,5), data = complete(data, i),
              x=TRUE, y=TRUE)
    
    AIC_[i] <- AIC(m3)
    
  }
  
  k.5.AIC <- mean(AIC_)
  AIC_list[3] <- k.5.AIC
  
  knots <- 2 + which.min(c(k.3.AIC, k.4.AIC)) #k.5.AIC))
  return(knots)
  #return(AIC_list)
  
}

Pulse.knots <- AIC_extract_Pulse(mids_data)

### BP ###

AIC_extract_BP <- function (data) {
  
  AIC_list <- vector()
  
  AIC_ <- vector()
  
  for (i in 1:10) {
    m1 <- lrm(formula = POMS.overall ~ rcs(S02SystolicBP.centred,3), data = mice::complete(data, i),
              x=TRUE, y=TRUE)
    
    AIC_[i] <- AIC(m1)
  }
  
  k.3.AIC <- mean(AIC_)
  AIC_list[1] <- k.3.AIC
  
  for (i in 1:10) {
    m2 <- lrm(formula = POMS.overall ~ rcs(S02SystolicBP.centred,4), data = complete(data, i),
              x=TRUE, y=TRUE)
    
    AIC_[i] <- AIC(m2)
  }
  
  k.4.AIC <- mean(AIC_)
  AIC_list[2] <- k.4.AIC
  
  for (i in 1:10) {
    m3 <- lrm(formula = POMS.overall ~ rcs(S02SystolicBP.centred,5), data = complete(data, i),
              x=TRUE, y=TRUE)
    
    AIC_[i] <- AIC(m3)
    
  }
  
  k.5.AIC <- mean(AIC_)
  AIC_list[3] <- k.5.AIC
  
  knots <- 2 + which.min(c(k.3.AIC, k.4.AIC)) #k.5.AIC))
  return(knots)
  #return(AIC_list)
  
}

BP.knots <- AIC_extract_BP(mids_data)

### SpO2 ###

AIC_extract_SpO2 <- function (data) {
  
  AIC_list <- vector()
  
  AIC_ <- vector()
  
  for (i in 1:10) {
    m1 <- lrm(formula = POMS.overall ~ rcs(S02SpO2.centred,3), data = mice::complete(data, i),
              x=TRUE, y=TRUE)
    
    AIC_[i] <- AIC(m1)
  }
  
  k.3.AIC <- mean(AIC_)
  AIC_list[1] <- k.3.AIC
  
  for (i in 1:10) {
    m2 <- lrm(formula = POMS.overall ~ rcs(S02SpO2.centred,4), data = complete(data, i),
              x=TRUE, y=TRUE)
    
    AIC_[i] <- AIC(m2)
  }
  
  k.4.AIC <- mean(AIC_)
  AIC_list[2] <- k.4.AIC
  
  for (i in 1:10) {
    m3 <- lrm(formula = POMS.overall ~ rcs(S02SpO2.centred,5), data = complete(data, i),
              x=TRUE, y=TRUE)
    
    AIC_[i] <- AIC(m3)
    
  }
  
  k.5.AIC <- mean(AIC_)
  AIC_list[3] <- k.5.AIC
  
  knots <- 2 + which.min(c(k.3.AIC, k.4.AIC)) #k.5.AIC))
  return(knots)
  #return(AIC_list)
  
}

SpO2.knots <- AIC_extract_SpO2(mids_data)


Age.knots
BMI.knots
Na.knots
K.knots
Urea.knots
Creatinine.knots
Albumin.knots
WCC.knots
Hb.knots
Pulse.knots
BP.knots
SpO2.knots

