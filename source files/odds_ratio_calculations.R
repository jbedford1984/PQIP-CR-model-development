
# set data distribution
dd <- datadist(mice::complete(mids_data, "long", include = FALSE))
options(datadist="dd")

## calcuate odds ratios for change in continuous variables
row_names_POMSoverall <- c("S01BMI.centred", "Odds_ratio_bmi",
               "S02SerumSodium.centred", "Odds_ratio_sodium",
               "log.Creatinine.centred","Odds_ratio_Creat",
               "S02WhiteCellCount.centred","Odds_ratio_wcc",
               "log.Urea.centred","Odds_ratio_urea",
               "S01AgeYears.centred","Odds_ratio_age",
               "ModeSurgeryCombined - Opn:Lap","Odds_ratio",
               "ModeSurgeryCombined - Rob:Lap","Odds_ratio", 
               "Rockwood.combined - 2:1","Odds_ratio",
               "Rockwood.combined - 3:1","Odds_ratio",
               "Rockwood.combined - 4:1","Odds_ratio",
               "Rockwood.combined - 5:1","Odds_ratio",
               "Rockwood.combined - 6-9:1","Odds_ratio",
               "S01Gender - M:F","Odds_ratio",
               "SORT_severity.combined - Xma:Min/Int/Maj","Odds_ratio",
               "SORT_severity.combined - Com:Min/Int/Maj","Odds_ratio",
               "S02PatientsASAGrade.combined - II:I","Odds_ratio",
               "S02PatientsASAGrade.combined - III:I","Odds_ratio",
               "S02PatientsASAGrade.combined - IV/V:I","Odds_ratio",
               "IMD_quintile_adjusted - 4:5 - least deprived","Odds_ratio",
               "IMD_quintile_adjusted - 3:5 - least deprived","Odds_ratio",
               "IMD_quintile_adjusted - 2:5 - least deprived", "Odds_ratio",
               "IMD_quintile_adjusted - 1 - most deprived:5 - least deprived","Odds_ratio")

row_names_POMSmajor <- c("S01BMI.centred", "Odds_ratio_bmi",
  "S02SerumSodium.centred", "Odds_ratio_sodium",
  "log.Creatinine.centred","Odds_ratio_Creat",
  "S02WhiteCellCount.centred","Odds_ratio_wcc",
  "log.Urea.centred","Odds_ratio_urea",
  "S01AgeYears.centred","Odds_ratio_age",
  "ModeSurgeryCombined - Opn:Lap","Odds_ratio",
  "ModeSurgeryCombined - Rob:Lap","Odds_ratio", 
  "Rockwood.combined - 2:1","Odds_ratio",
  "Rockwood.combined - 3:1","Odds_ratio",
  "Rockwood.combined - 4:1","Odds_ratio",
  "Rockwood.combined - 5:1","Odds_ratio",
  "Rockwood.combined - 6-9:1","Odds_ratio",
  "S01Gender - M:F","Odds_ratio",
  "SORT_severity.combined - Xma:Min/Int/Maj","Odds_ratio",
  "SORT_severity.combined - Com:Min/Int/Maj","Odds_ratio",
  "S02PatientsASAGrade.combined - II:I","Odds_ratio",
  "S02PatientsASAGrade.combined - III:I","Odds_ratio",
  "S02PatientsASAGrade.combined - IV/V:I","Odds_ratio",
  "IMD_quintile_adjusted - 4:5 - least deprived","Odds_ratio",
  "IMD_quintile_adjusted - 3:5 - least deprived","Odds_ratio",
  "IMD_quintile_adjusted - 2:5 - least deprived", "Odds_ratio",
  "IMD_quintile_adjusted - 1 - most deprived:5 - least deprived","Odds_ratio")

row_names_Clavien <- c("S01BMI.centred", "Odds_ratio_bmi",
                         "S02SerumSodium.centred", "Odds_ratio_sodium",
                         "log.Creatinine.centred","Odds_ratio_Creat",
                         "S02WhiteCellCount.centred","Odds_ratio_wcc",
                         "log.Urea.centred","Odds_ratio_urea",
                         "S01AgeYears.centred","Odds_ratio_age",
                         "ModeSurgeryCombined - Opn:Lap","Odds_ratio",
                         "ModeSurgeryCombined - Rob:Lap","Odds_ratio", 
                         "Rockwood.combined - 2:1","Odds_ratio",
                         "Rockwood.combined - 3:1","Odds_ratio",
                         "Rockwood.combined - 4:1","Odds_ratio",
                         "Rockwood.combined - 5:1","Odds_ratio",
                         "Rockwood.combined - 6-9:1","Odds_ratio",
                         "S01Gender - M:F","Odds_ratio",
                         "SORT_severity.combined - Xma:Min/Int/Maj","Odds_ratio",
                         "SORT_severity.combined - Com:Min/Int/Maj","Odds_ratio",
                         "S02PatientsASAGrade.combined - II:I","Odds_ratio",
                         "S02PatientsASAGrade.combined - III:I","Odds_ratio",
                         "S02PatientsASAGrade.combined - IV/V:I","Odds_ratio",
                         "IMD_quintile_adjusted - 4:5 - least deprived","Odds_ratio",
                         "IMD_quintile_adjusted - 3:5 - least deprived","Odds_ratio",
                         "IMD_quintile_adjusted - 2:5 - least deprived", "Odds_ratio",
                         "IMD_quintile_adjusted - 1 - most deprived:5 - least deprived","Odds_ratio")

age.reference <- 30
OR.age <- function(age.adj, model, rowNames) { 
  
  row_names <- rowNames

OR <- as_tibble(summary(model, 
                    S01AgeYears.centred = c(age.reference - mids.centred$S01AgeYears.mean,
                                            age.adj - mids.centred$S01AgeYears.mean),
                    ModeSurgeryCombined = "Lap",
                    Rockwood.combined = "1",
                    S01Gender = "F",
                    SORT_severity.combined = "Min/Int/Maj",
                    S02PatientsASAGrade.combined = "I",
                    S01BMI.centred = 0,
                    S02SerumSodium.centred = 0,
                    IMD_quintile_adjusted = "5 - least deprived",
                    S02WhiteCellCount.centred = 0,
                    log.Urea.centred = 0,
                    log.Creatinine.centred = 0)) %>%
  mutate(variable = row_names,
         age = age.adj) %>%
  select(variable, age, Effect, `Lower 0.95`, `Upper 0.95`) %>%
  filter(variable == "Odds_ratio_age")

return(OR)

}

OddsRatio.age <- rbind(OR.age(30, lrm.mult.impute, row_names_POMSoverall),
                       OR.age(40, lrm.mult.impute, row_names_POMSoverall),
                       OR.age(50, lrm.mult.impute, row_names_POMSoverall),
                       OR.age(60, lrm.mult.impute, row_names_POMSoverall),
                       OR.age(70, lrm.mult.impute, row_names_POMSoverall),
                       OR.age(80, lrm.mult.impute, row_names_POMSoverall),
                       OR.age(90, lrm.mult.impute, row_names_POMSoverall))


bmi.reference <- 25
OR.bmi <- function(bmi.adj, model, rowNames) { 
  row_names <- rowNames
  OR <- as_tibble(summary(model, 
                              S01AgeYears.centred = 0,
                              ModeSurgeryCombined = "Lap",
                              Rockwood.combined = "1",
                              S01Gender = "F",
                              SORT_severity.combined = "Min/Int/Maj",
                              S02PatientsASAGrade.combined = "I",
                              S01BMI.centred = c(bmi.reference - mids.centred$S01BMI.mean,
                                                 bmi.adj - mids.centred$S01BMI.mean),
                              S02SerumSodium.centred = 0,
                              IMD_quintile_adjusted = "5 - least deprived",
                              S02WhiteCellCount.centred = 0,
                              log.Urea.centred = 0,
                          log.Creatinine.centred = 0)) %>%
    mutate(variable = row_names,
           bmi = bmi.adj) %>%
    select(variable, bmi, Effect, `Lower 0.95`, `Upper 0.95`) %>%
    filter(variable == "Odds_ratio_bmi")
  
  return(OR)
  
}

OddsRatio.bmi <- rbind(OR.bmi(18,lrm.mult.impute, row_names_POMSoverall),
                       OR.bmi(25,lrm.mult.impute, row_names_POMSoverall), 
                       OR.bmi(30,lrm.mult.impute, row_names_POMSoverall),
                       OR.bmi(35,lrm.mult.impute, row_names_POMSoverall),
                       OR.bmi(40,lrm.mult.impute, row_names_POMSoverall))


na.reference <- 140

OR.sodium <- function(na.adj, model, rowNames) { 
  row_names <- rowNames
  OR <- as_tibble(summary(model, 
                              S01AgeYears.centred = 0,
                              ModeSurgeryCombined = "Lap",
                              Rockwood.combined = "1",
                              S01Gender = "F",
                              SORT_severity.combined = "Min/Int/Maj",
                              S02PatientsASAGrade.combined = "I",
                              S01BMI.centred = 0,
                              S02SerumSodium.centred = c(na.reference - mids.centred$S02SerumSodium.mean,
                                                         na.adj - mids.centred$S02SerumSodium.mean),
                              IMD_quintile_adjusted = "5 - least deprived",
                              S02WhiteCellCount.centred = 0,
                              log.Urea.centred = 0,
                          log.Creatinine.centred = 0)) %>%
    mutate(variable = row_names,
           Na = na.adj) %>%
    select(variable, Na, Effect, `Lower 0.95`, `Upper 0.95`) %>%
    filter(variable == "Odds_ratio_sodium")
  
  return(OR)
  
}

OddsRatio.na <- rbind(OR.sodium(120,lrm.mult.impute, row_names_POMSoverall),
                      OR.sodium(130,lrm.mult.impute, row_names_POMSoverall), 
                      OR.sodium(140,lrm.mult.impute, row_names_POMSoverall),
                      OR.sodium(150,lrm.mult.impute, row_names_POMSoverall),
                      OR.sodium(160,lrm.mult.impute, row_names_POMSoverall))


wcc.reference <- 7

OR.wcc <- function(wcc.adj, model, rowNames) { 
  row_names <- rowNames
  OR <- as_tibble(summary(model, 
                          S01AgeYears.centred = 0,
                          ModeSurgeryCombined = "Lap",
                          Rockwood.combined = "1",
                          S01Gender = "F",
                          SORT_severity.combined = "Min/Int/Maj",
                          S02PatientsASAGrade.combined = "I",
                          S01BMI.centred = 0,
                          S02SerumSodium.centred = 0,
                          IMD_quintile_adjusted = "5 - least deprived",
                          S02WhiteCellCount.centred = c(wcc.reference - mids.centred$S02WhiteCellCount.mean,
                                                        wcc.adj - mids.centred$S02WhiteCellCount.mean),
                          log.Urea.centred = 0,
                          log.Creatinine.centred = 0)) %>%
    mutate(variable = row_names,
           wcc = wcc.adj) %>%
    select(variable, wcc, Effect, `Lower 0.95`, `Upper 0.95`) %>%
    filter(variable == "Odds_ratio_wcc")
  
  return(OR)
  
}

OddsRatio.wcc <- rbind(OR.wcc(3,lrm.mult.impute, row_names_POMSoverall),
                       OR.wcc(7,lrm.mult.impute, row_names_POMSoverall), 
                       OR.wcc(11,lrm.mult.impute, row_names_POMSoverall),
                       OR.wcc(15,lrm.mult.impute, row_names_POMSoverall),
                       OR.wcc(20,lrm.mult.impute, row_names_POMSoverall))


urea.reference <- 8

OR.urea <- function(urea.adj, model, rowNames) { 
  row_names <- rowNames
  OR <- as_tibble(summary(model, 
                          S01AgeYears.centred = 0,
                          ModeSurgeryCombined = "Lap",
                          Rockwood.combined = "1",
                          S01Gender = "F",
                          SORT_severity.combined = "Min/Int/Maj",
                          S02PatientsASAGrade.combined = "I",
                          S01BMI.centred = 0,
                          S02SerumSodium.centred = 0,
                          IMD_quintile_adjusted = "5 - least deprived",
                          S02WhiteCellCount.centred = 0,
                          log.Urea.centred = c(log(urea.reference) - mids.centred$log.Urea.mean,
                                               log(urea.adj) - mids.centred$log.Urea.mean),
                          log.Creatinine.centred = 0)) %>%
    mutate(variable = row_names,
           urea = urea.adj) %>%
    select(variable, urea, Effect, `Lower 0.95`, `Upper 0.95`) %>%
    filter(variable == "Odds_ratio_urea")
  
  return(OR)
  
}

OddsRatio.urea <- rbind(OR.urea(3,lrm.mult.impute, row_names_POMSoverall),
                        OR.urea(8,lrm.mult.impute, row_names_POMSoverall), 
                        OR.urea(15,lrm.mult.impute, row_names_POMSoverall),
                        OR.urea(20,lrm.mult.impute, row_names_POMSoverall))


creat.reference <- 70

OR.creat <- function(creat.adj, model, rowNames) { 
  row_names <- rowNames
  OR <- as_tibble(summary(model, 
                          S01AgeYears.centred = 0,
                          ModeSurgeryCombined = "Lap",
                          Rockwood.combined = "1",
                          S01Gender = "F",
                          SORT_severity.combined = "Min/Int/Maj",
                          S02PatientsASAGrade.combined = "I",
                          S01BMI.centred = 0,
                          S02SerumSodium.centred = 0,
                          IMD_quintile_adjusted = "5 - least deprived",
                          S02WhiteCellCount.centred = 0,
                          log.Urea.centred = 0,
                          log.Creatinine.centred = c(log(creat.reference) - mids.centred$log.Creatinine.mean,
                                                     log(creat.adj) - mids.centred$log.Creatinine.mean))) %>%
    mutate(variable = row_names,
           creat = creat.adj) %>%
    select(variable, creat, Effect, `Lower 0.95`, `Upper 0.95`) %>%
    filter(variable == "Odds_ratio_Creat")
  
  return(OR)
  
}

OddsRatio.creat <- rbind(OR.creat(30, lrm.mult.impute, row_names_POMSoverall),
                         OR.creat(70, lrm.mult.impute, row_names_POMSoverall), 
                         OR.creat(120, lrm.mult.impute, row_names_POMSoverall),
                         OR.creat(150, lrm.mult.impute, row_names_POMSoverall))



OddsRatio.age
OddsRatio.bmi
OddsRatio.na
OddsRatio.wcc
OddsRatio.urea
OddsRatio.creat
#OddsRatio.albumin


### Calculate Odds ratios for continuous variables in sensitivity anaylsis - POMSmajor and Clavien-Dindo models
###POMS major
##--------------------------
OddsRatio.age.POMSmajor <- rbind(OR.age(30,lrm.mult.impute.POMSmajor, row_names_POMSmajor),
                       OR.age(40, lrm.mult.impute.POMSmajor, row_names_POMSmajor),
                       OR.age(50, lrm.mult.impute.POMSmajor, row_names_POMSmajor),
                       OR.age(60, lrm.mult.impute.POMSmajor, row_names_POMSmajor),
                       OR.age(70, lrm.mult.impute.POMSmajor, row_names_POMSmajor),
                       OR.age(80, lrm.mult.impute.POMSmajor, row_names_POMSmajor),
                       OR.age(90, lrm.mult.impute.POMSmajor, row_names_POMSmajor))



OddsRatio.bmi.POMSmajor <- rbind(OR.bmi(18,lrm.mult.impute.POMSmajor, row_names_POMSmajor),
                       OR.bmi(25,lrm.mult.impute.POMSmajor, row_names_POMSmajor), 
                       OR.bmi(30,lrm.mult.impute.POMSmajor, row_names_POMSmajor),
                       OR.bmi(35,lrm.mult.impute.POMSmajor, row_names_POMSmajor),
                       OR.bmi(40,lrm.mult.impute.POMSmajor, row_names_POMSmajor))


OddsRatio.na.POMSmajor <- rbind(OR.sodium(120,lrm.mult.impute.POMSmajor, row_names_POMSmajor),
                      OR.sodium(130,lrm.mult.impute.POMSmajor, row_names_POMSmajor), 
                      OR.sodium(140,lrm.mult.impute.POMSmajor, row_names_POMSmajor),
                      OR.sodium(150,lrm.mult.impute.POMSmajor, row_names_POMSmajor),
                      OR.sodium(160,lrm.mult.impute.POMSmajor, row_names_POMSmajor))

OddsRatio.wcc.POMSmajor <- rbind(OR.wcc(3,lrm.mult.impute.POMSmajor, row_names_POMSmajor),
                       OR.wcc(7,lrm.mult.impute.POMSmajor, row_names_POMSmajor), 
                       OR.wcc(11,lrm.mult.impute.POMSmajor, row_names_POMSmajor),
                       OR.wcc(15,lrm.mult.impute.POMSmajor, row_names_POMSmajor),
                       OR.wcc(20,lrm.mult.impute.POMSmajor, row_names_POMSmajor))



OddsRatio.urea.POMSmajor <- rbind(OR.urea(3,lrm.mult.impute.POMSmajor, row_names_POMSmajor),
                        OR.urea(8,lrm.mult.impute.POMSmajor, row_names_POMSmajor), 
                        OR.urea(15,lrm.mult.impute.POMSmajor, row_names_POMSmajor),
                        OR.urea(20,lrm.mult.impute.POMSmajor, row_names_POMSmajor))

OddsRatio.creat.POMSmajor <- rbind(OR.creat(30,lrm.mult.impute.POMSmajor, row_names_POMSmajor),
                                   OR.creat(70,lrm.mult.impute.POMSmajor, row_names_POMSmajor), 
                                   OR.creat(120,lrm.mult.impute.POMSmajor, row_names_POMSmajor),
                                   OR.creat(150,lrm.mult.impute.POMSmajor, row_names_POMSmajor))


#OddsRatio.albumin.POMSmajor <- rbind(OR.albumin(20,lrm.mult.impute.POMSmajor, row_names_POMSmajor),
#                           OR.albumin(30,lrm.mult.impute.POMSmajor, row_names_POMSmajor), 
#                           OR.albumin(40,lrm.mult.impute.POMSmajor, row_names_POMSmajor),
#                           OR.albumin(50,lrm.mult.impute.POMSmajor, row_names_POMSmajor))



OddsRatio.age.POMSmajor
OddsRatio.bmi.POMSmajor
OddsRatio.na.POMSmajor
OddsRatio.wcc.POMSmajor
OddsRatio.urea.POMSmajor
OddsRatio.creat.POMSmajor
#OddsRatio.albumin.POMSmajor

### Calculate Odds ratios for continuous variables in sensitivity anaylsis - POMSmajor and Clavien-Dindo models
###PClavien-Dindo
##--------------------------
OddsRatio.age.Clavien <- rbind(OR.age(30,lrm.mult.impute.Clavien, row_names_Clavien),
                                 OR.age(40, lrm.mult.impute.Clavien, row_names_Clavien),
                                 OR.age(50, lrm.mult.impute.Clavien, row_names_Clavien),
                                 OR.age(60, lrm.mult.impute.Clavien, row_names_Clavien),
                                 OR.age(70, lrm.mult.impute.Clavien, row_names_Clavien),
                                 OR.age(80, lrm.mult.impute.Clavien, row_names_Clavien),
                                 OR.age(90, lrm.mult.impute.Clavien, row_names_Clavien))



OddsRatio.bmi.Clavien <- rbind(OR.bmi(18,lrm.mult.impute.Clavien, row_names_Clavien),
                                 OR.bmi(25,lrm.mult.impute.Clavien, row_names_Clavien), 
                                 OR.bmi(30,lrm.mult.impute.Clavien, row_names_Clavien),
                                 OR.bmi(35,lrm.mult.impute.Clavien, row_names_Clavien),
                                 OR.bmi(40,lrm.mult.impute.Clavien, row_names_Clavien))


OddsRatio.na.Clavien <- rbind(OR.sodium(120,lrm.mult.impute.Clavien, row_names_Clavien),
                                OR.sodium(130,lrm.mult.impute.Clavien, row_names_Clavien), 
                                OR.sodium(140,lrm.mult.impute.Clavien, row_names_Clavien),
                                OR.sodium(150,lrm.mult.impute.Clavien, row_names_Clavien),
                                OR.sodium(160,lrm.mult.impute.Clavien, row_names_Clavien))

OddsRatio.wcc.Clavien <- rbind(OR.wcc(3,lrm.mult.impute.Clavien, row_names_Clavien),
                                 OR.wcc(7,lrm.mult.impute.Clavien, row_names_Clavien), 
                                 OR.wcc(11,lrm.mult.impute.Clavien, row_names_Clavien),
                                 OR.wcc(15,lrm.mult.impute.Clavien, row_names_Clavien),
                                 OR.wcc(20,lrm.mult.impute.Clavien, row_names_Clavien))



OddsRatio.urea.Clavien <- rbind(OR.urea(3,lrm.mult.impute.Clavien, row_names_Clavien),
                                  OR.urea(8,lrm.mult.impute.Clavien, row_names_Clavien), 
                                  OR.urea(15,lrm.mult.impute.Clavien, row_names_Clavien),
                                  OR.urea(20,lrm.mult.impute.Clavien, row_names_Clavien))


#OddsRatio.albumin.Clavien <- rbind(OR.albumin(20,lrm.mult.impute.Clavien, row_names_Clavien),
 #                                    OR.albumin(30,lrm.mult.impute.Clavien, row_names_Clavien), 
#                                     OR.albumin(40,lrm.mult.impute.Clavien, row_names_Clavien),
#                                     OR.albumin(50,lrm.mult.impute.Clavien, row_names_Clavien))

OddsRatio.creat.Clavien <- rbind(OR.creat(30,lrm.mult.impute.Clavien, row_names_POMSmajor),
                                   OR.creat(70,lrm.mult.impute.Clavien, row_names_POMSmajor), 
                                   OR.creat(120,lrm.mult.impute.Clavien, row_names_POMSmajor),
                                   OR.creat(150,lrm.mult.impute.Clavien, row_names_POMSmajor))

OddsRatio.age.Clavien
OddsRatio.bmi.Clavien
OddsRatio.na.Clavien
OddsRatio.wcc.Clavien
OddsRatio.urea.Clavien
OddsRatio.creat.Clavien
#OddsRatio.albumin.Clavien

