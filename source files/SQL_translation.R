translate_sql(data %>%
                mutate(S02PlannedProcedureSurgeryModeThoracoscopic = ifelse(!is.na(S02PlannedProcedureSurgeryModeThoracoscopic),
                                                                            S02PlannedProcedureSurgeryModeThoracoscopic, NA)) %>%
                mutate(ModeSurgery = ifelse(S03WasProcedureTheSameAsThePlannedProcedure == "Y" & S02PlannedProcedureSurgeryModeOpen==1 &
                                              S02PlannedProcedureSurgeryModeLaparoscopic==2 & S02PlannedProcedureSurgeryModeRobotic==2 &
                                              S02PlannedProcedureSurgeryModeThoracoscopic==2,"Opn",
                                            ifelse(S03WasProcedureTheSameAsThePlannedProcedure == "Y" & S02PlannedProcedureSurgeryModeOpen==1
                                                   & S02PlannedProcedureSurgeryModeLaparoscopic==1 & S02PlannedProcedureSurgeryModeRobotic==2
                                                   & S02PlannedProcedureSurgeryModeThoracoscopic==2,"Opn",
                                                   ifelse(S03WasProcedureTheSameAsThePlannedProcedure == "Y" & S02PlannedProcedureSurgeryModeOpen==1 &
                                                            S02PlannedProcedureSurgeryModeLaparoscopic==2 & S02PlannedProcedureSurgeryModeRobotic==1 &
                                                            S02PlannedProcedureSurgeryModeThoracoscopic==2,"Opn",
                                                          ifelse(S03WasProcedureTheSameAsThePlannedProcedure == "Y" & S02PlannedProcedureSurgeryModeOpen==1 &
                                                                   S02PlannedProcedureSurgeryModeLaparoscopic==2 & S02PlannedProcedureSurgeryModeRobotic==2 &
                                                                   S02PlannedProcedureSurgeryModeThoracoscopic==1,"Opn",
                                                                 ifelse(S03WasProcedureTheSameAsThePlannedProcedure == "Y" & S02PlannedProcedureSurgeryModeOpen==2 &
                                                                          S02PlannedProcedureSurgeryModeLaparoscopic==1 &
                                                                          S02PlannedProcedureSurgeryModeRobotic==2 &
                                                                          S02PlannedProcedureSurgeryModeThoracoscopic==2,"Lap", 
                                                                        ifelse(S03WasProcedureTheSameAsThePlannedProcedure == "Y" & S02PlannedProcedureSurgeryModeOpen==2 &
                                                                                 S02PlannedProcedureSurgeryModeLaparoscopic==1 & S02PlannedProcedureSurgeryModeRobotic==1 &
                                                                                 S02PlannedProcedureSurgeryModeThoracoscopic==2,"Rob",
                                                                               ifelse(S03WasProcedureTheSameAsThePlannedProcedure == "Y" & S02PlannedProcedureSurgeryModeOpen==2 &
                                                                                        S02PlannedProcedureSurgeryModeLaparoscopic==1 & S02PlannedProcedureSurgeryModeRobotic==2 &
                                                                                        S02PlannedProcedureSurgeryModeThoracoscopic==1,"Lap",
                                                                                      ifelse(S03WasProcedureTheSameAsThePlannedProcedure == "Y" & S02PlannedProcedureSurgeryModeOpen==2 &
                                                                                               S02PlannedProcedureSurgeryModeLaparoscopic==2 & S02PlannedProcedureSurgeryModeRobotic==1 &
                                                                                               S02PlannedProcedureSurgeryModeThoracoscopic==2,"Rob",
                                                                                             ifelse(S03WasProcedureTheSameAsThePlannedProcedure == "Y" & S02PlannedProcedureSurgeryModeOpen==2 &
                                                                                                      S02PlannedProcedureSurgeryModeLaparoscopic==2 & S02PlannedProcedureSurgeryModeRobotic==1 &
                                                                                                      S02PlannedProcedureSurgeryModeThoracoscopic==1,"Rob",
                                                                                                    ifelse(S03WasProcedureTheSameAsThePlannedProcedure == "Y" & S02PlannedProcedureSurgeryModeOpen==2 &
                                                                                                             S02PlannedProcedureSurgeryModeLaparoscopic==2 & S02PlannedProcedureSurgeryModeRobotic==2 &
                                                                                                             S02PlannedProcedureSurgeryModeThoracoscopic==1,"Lap",
                                                                                                           ifelse(S03WasProcedureTheSameAsThePlannedProcedure == "Y" & S02PlannedProcedureSurgeryModeOpen==1 &
                                                                                                                    S02PlannedProcedureSurgeryModeLaparoscopic==1 & S02PlannedProcedureSurgeryModeRobotic==1 &
                                                                                                                    S02PlannedProcedureSurgeryModeThoracoscopic==2,"Opn",
                                                                                                                  ifelse(S03WasProcedureTheSameAsThePlannedProcedure == "Y" & S02PlannedProcedureSurgeryModeOpen==1 &
                                                                                                                           S02PlannedProcedureSurgeryModeLaparoscopic==1 & S02PlannedProcedureSurgeryModeRobotic==2 &
                                                                                                                           S02PlannedProcedureSurgeryModeThoracoscopic==1,"Opn",
                                                                                                                         ifelse(S03WasProcedureTheSameAsThePlannedProcedure == "Y" & S02PlannedProcedureSurgeryModeOpen==2 &
                                                                                                                                  S02PlannedProcedureSurgeryModeLaparoscopic==1 & S02PlannedProcedureSurgeryModeRobotic==1 &
                                                                                                                                  S02PlannedProcedureSurgeryModeThoracoscopic==1,"Rob",
                                                                                                                                NA)))))))))))))) %>%
                mutate(ModeSurgery = ifelse(S03WasProcedureTheSameAsThePlannedProcedure == "N", 
                                            S03ActualProcedureSurgeryMode, ModeSurgery)) %>%
                mutate(ModeSurgery = ifelse(is.na(ModeSurgery) & S02PlannedProcedureSurgeryModeOpen==1 &
                                              S02PlannedProcedureSurgeryModeLaparoscopic==2 & S02PlannedProcedureSurgeryModeRobotic==2 &
                                              S02PlannedProcedureSurgeryModeThoracoscopic==2,"Opn",
                                            ifelse(is.na(ModeSurgery) & S02PlannedProcedureSurgeryModeOpen==1
                                                   & S02PlannedProcedureSurgeryModeLaparoscopic==1 & S02PlannedProcedureSurgeryModeRobotic==2
                                                   & S02PlannedProcedureSurgeryModeThoracoscopic==2,"Opn",
                                                   ifelse(is.na(ModeSurgery) & S02PlannedProcedureSurgeryModeOpen==2 &
                                                            S02PlannedProcedureSurgeryModeLaparoscopic==1 &
                                                            S02PlannedProcedureSurgeryModeRobotic==2 &
                                                            S02PlannedProcedureSurgeryModeThoracoscopic==2,"Lap",
                                                          ifelse(is.na(ModeSurgery) & S02PlannedProcedureSurgeryModeOpen==2 &
                                                                   S02PlannedProcedureSurgeryModeLaparoscopic==2 & S02PlannedProcedureSurgeryModeRobotic==1 &
                                                                   S02PlannedProcedureSurgeryModeThoracoscopic==2,"Rob", ModeSurgery))))))



## IMD quintile linkage
translate_sql(data %>%
  mutate(S01PostcodeOut = str_to_upper(S01PostcodeOut),
       S01PostcodeOut = trimws(S01PostcodeOut, "both"),
       S01PostcodeIn = str_to_upper(S01PostcodeIn),
       S01PostcodeIn = trimws(S01PostcodeIn, "both")) %>%
  #join out and in postcodes 
  mutate(Postcode = paste0(S01PostcodeOut, S01PostcodeIn)))

## create POMS variables
translate_sql(data %>% mutate(Pulmonary.overall=S06PatientHasNewRequirementForO2Therapy == 1 | 
                         S06PatientHasNewRequirementForVentilator == 1,
                       Infectious.overall=S06PatientIsOnIVAntibiotic == 1 | 
                         S06HasHadATemperatureGreaterThan38InThePast24h == 1,
                       Gastrointestinal.overall=S06PatientHasBeenUnableToTolerateEnteralNutrition == 1 | 
                         S06PatientHasHadNauseaVomitingOrAbdominalDistensionInPast24h == 1,
                       Renal.overall=S06PatientHadSinceSurgeryOliguria == 1 | S06PatientHadSinceSurgeryCreatinine30 == 1 |
                         S06PatientHadSinceSurgeryUrinaryCatheterBeyondDay1 == 1,
                       Cardiovascular.overall=S06PatientHasHadDiagnosticTestsHypotension==1 |
                         S06PatientHasHadDiagnosticTestsNewMyocardialInfarctionOrIschaemia==1 |
                         S06PatientHasHadDiagnosticTestsThromboticEvent==1 | 
                         S06PatientHasHadDiagnosticTestsArrhythmias==1 |
                         S06PatientHasHadDiagnosticTestsCardiogenicPulmonaryOedema==1,
                       Neurological.overall=S06PatientHasDevelopedSinceSurgeryNewNeurologicalDeficit==1 | 
                         S06PatientHasDevelopedSinceSurgeryDeliriumOrConfusion==1 |
                         S06PatientHasDevelopedSinceSurgerySedativeInducedComa==1 |
                         S06PatientHasDevelopedSinceSurgeryNonSedativeAassociatedComa==1,
                       Wound.overall=S06PatientHadWoundRequiringSurgicalExploration==1 |
                         S06PatientHadDrainageOfPus==1,
                       Haematological.overall=S06PatientRequiredRedCellTransfusion==1 |
                         S06PatientRequiredPlasma==1,
                       Pain.overall=S06PatientSurgicalPainParenteralOpioids==1 |
                         S06PatientSurgicalPainRegionalAnaesthesia==1,
                       ## Create variable for POMS overall 
                       POMS=Pulmonary.overall == TRUE | Infectious.overall == TRUE | 
                         Gastrointestinal.overall == TRUE | Renal.overall == TRUE | 
                         Cardiovascular.overall == TRUE | Neurological.overall == TRUE | 
                         Wound.overall == TRUE | Haematological.overall == TRUE | 
                         Pain.overall == TRUE,
                       ## Create high-grade (POMS major) fields and POMS.major variable (as TRUE/FALSE)
                       Pulmonary.major=S06PatientHasNewRequirementForO2Therapy == 1 | 
                         S06PatientHasNewRequirementForVentilator == 1,
                       Infectious.major=S06PatientIsOnIVAntibiotic == 1,
                       Renal.major=S06PatientHadSinceSurgeryOliguria == 1 | 
                         S06PatientHadSinceSurgeryCreatinine30 == 1,
                       Cardiovascular.major=S06PatientHasHadDiagnosticTestsHypotension==1 |
                         S06PatientHasHadDiagnosticTestsNewMyocardialInfarctionOrIschaemia==1 |
                         S06PatientHasHadDiagnosticTestsThromboticEvent==1 | 
                         S06PatientHasHadDiagnosticTestsArrhythmias==1 |
                         S06PatientHasHadDiagnosticTestsCardiogenicPulmonaryOedema==1,
                       Neurological.major=S06PatientHasDevelopedSinceSurgeryNewNeurologicalDeficit==1 |                 
                         S06PatientHasDevelopedSinceSurgeryDeliriumOrConfusion==1 |
                         S06PatientHasDevelopedSinceSurgerySedativeInducedComa==1 |
                         S06PatientHasDevelopedSinceSurgeryNonSedativeAassociatedComa==1 ,
                       Wound.major=S06PatientHadWoundRequiringSurgicalExploration==1 |
                         S06PatientHadDrainageOfPus==1,
                       Haematological.major=S06PatientRequiredRedCellTransfusion==1 |
                         S06PatientRequiredPlasma==1,
                       Pain.major=S06PatientSurgicalPainRegionalAnaesthesia==1,
                       POMSmajor=Pulmonary.major == TRUE | Infectious.major == TRUE | 
                         Renal.major == TRUE | Cardiovascular.major == TRUE | Neurological.major == TRUE | 
                         Wound.major == TRUE | Haematological.major == TRUE | Pain.major == TRUE))

translate_sql(data %>%
                mutate(BMI.centred = S01BMI-28) %>%
                mutate(Sodium.centred = S02SerumSodium-140) %>%
                mutate(log.Creatinine.centred = log(S02SerumCreatinine)-4.3) %>%
                mutate(log.Urea.centred = log(S02SerumUrea)-1.6) %>%
                mutate(WhiteCellCount.centred = S02WhiteCellCount-7.6) %>%
                mutate(Age.centred = S01AgeYears-64) %>%
                mutate(PQIP_CR_POMS_logit = -3.3081876 +
                         0.71919942*(ModeSurgery=="Opn") +
                         0.34618954*(ModeSurgery=="Rob") +
                         0.2551226*(S02RockwoodClinicalFrailtyScore=="2") +
                         0.51657126*(S02RockwoodClinicalFrailtyScore=="3") +
                         0.70509523*(S02RockwoodClinicalFrailtyScore=="4") +
                         0.8082908*(S02RockwoodClinicalFrailtyScore=="5") +
                         0.90823978*(S02RockwoodClinicalFrailtyScore=="6") +
                         0.90823978*(S02RockwoodClinicalFrailtyScore=="7") +
                         0.90823978*(S02RockwoodClinicalFrailtyScore=="8") +
                         0.90823978*(S02RockwoodClinicalFrailtyScore=="9") +
                         0.30969644*(S01Gender=="M") +
                         0.17454359*(SORT_severity=="Xma") +
                         0.56964698*(SORT_severity=="Com") +
                         0.15952372*(S02PatientsASAGrade=="2") +
                         0.42110205*(S02PatientsASAGrade=="3") +
                         0.44385062*(S02PatientsASAGrade=="4") +
                         0.44385062*(S02PatientsASAGrade=="5") +
                         -0.022455417* BMI.centred +
                         0.00058549124*pmax(BMI.centred+7.99,0)^3 +
                         -0.0017445337*pmax(BMI.centred+2.92,0)^3 +
                         0.0013330039*pmax(BMI.centred-0.9,0)^3 +
                         -0.00017396144*pmax(BMI.centred-9.2875,0)^3 +
                         -0.029228382* Sodium.centred +
                         -0.00012250672*pmax(Sodium.centred+6,0)^3 +
                         0.0034580052*pmax(Sodium.centred+1,0)^3 +
                         -0.0053549862*pmax(Sodium.centred-1,0)^3 +
                         0.0020194878*pmax(Sodium.centred-4,0)^3 +
                         0.030910885*(IMD_quintile=="4") +
                         -0.014178209*(IMD_quintile=="3") +
                         0.17637341*(IMD_quintile=="2") +
                         0.15955264*(IMD_quintile=="1 - most deprived") +
                         -0.78754659* log.Creatinine.centred +
                         4.3784501*pmax(log.Creatinine.centred+0.34875628,0)^3 +
                         -14.909756*pmax(log.Creatinine.centred+0.065893495,0)^3 +
                         12.759045*pmax(log.Creatinine.centred-0.11884061,0)^3 +
                         -2.2277398*pmax(log.Creatinine.centred-0.43619845,0)^3 +
                         -0.010351246* WhiteCellCount.centred +
                         0.0021637774*pmax(WhiteCellCount.centred+3.2,0)^3 +
                         -0.0048087225*pmax(WhiteCellCount.centred+1.2,0)^3 +
                         0.0026861286*pmax(WhiteCellCount.centred-0.5,0)^3 +
                         -0.000041183419*pmax(WhiteCellCount.centred-4.6,0)^3 +
                         -0.5855539* log.Urea.centred +
                         0.73580682*pmax(log.Urea.centred+0.53528926,0)^3 +
                         -2.4752866*pmax(log.Urea.centred+0.073943697,0)^3 +
                         1.8187039*pmax(log.Urea.centred-0.14046617,0)^3 +
                         -0.079224044*pmax(log.Urea.centred-0.56332303,0)^3 +
                         0.0017750254* Age.centred +
                         0.0000026814324*pmax(Age.centred+20,0)^3 +
                         -0.0000074255052*pmax(Age.centred-3,0)^3 +
                         0.0000047440728*pmax(Age.centred-16,0)^3) %>%
                mutate(PQIP_CR_POMS_probability = exp(PQIP_CR_POMS_logit)/(1+exp(PQIP_CR_POMS_logit)),
                       PQIP_CR_POMSmajor_logit= -0.5849197 + (0.9656530*PQIP_CR_POMS_logit),
                       PQIP_CR_POMSmajor_probability = exp(PQIP_CR_POMSmajor_logit)/(1+exp(PQIP_CR_POMSmajor_logit))) %>%
                select(-BMI.centred,-Sodium.centred,-log.Creatinine.centred,-log.Urea.centred,-WhiteCellCount.centred,-Age.centred)
)


## create VLAD calculation
translate_sql(data %>%
                mutate(VLAD_calc_POMS = PQIP_CR_POMS_probability - POMS,
                       VLAD_calc_POMSmajor = PQIP_CR_POMSmajor_probability - POMSmajor))
