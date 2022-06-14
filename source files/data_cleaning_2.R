## Continue data cleaning steps

# select variables relevant to modelling process
export_data <- export_data %>% 
  dplyr::select(CaseId, Locked, SiteName, S01PostcodeOut, S01Gender, S01DateofSurgery, S01AgeYears, S01AgeMonths,                                                      
         S01Height, S01Weight, S01BMI, 
         S02SurgicalSpecialty, S02PlannedProcedureOperation, S02PlannedProcedureSurgeryModeOpen,                                
         S02PlannedProcedureSurgeryModeLaparoscopic, S02PlannedProcedureSurgeryModeRobotic,                             
         S02PlannedProcedureSurgeryModeThoracoscopic, S02PlannedProcedureSurgeryMultiStageProcedure, 
         S02PlannedProcedureSurgeryMultiStageProcedureDate, S02UrgencyOfSurgery, S02CancerDiagnosis,                                                 
         S02CancerPreoperativeTStaging, S02CancerPreoperativeNStaging, S02CancerPreoperativeMStaging,                                      
         S02SerumSodium, S02SerumSodium_NK, S02SerumPotassium, S02SerumPotassium_NK,                                              
         S02SerumUrea, S02SerumUrea_NK, S02SerumCreatinine, S02SerumCreatinine_NK,                                             
         S02SerumTroponin, S02SerumTroponin_NK, S02SerumAlbumin, S02SerumAlbumin_NK,                                                
         S02WhiteCellCount, S02WhiteCellCount_NK,                                              
         S02Haemoglobin, S02Haemoglobin_NK,                                                 
         S02PulseRate, S02SystolicBP,                                                     
         S02GlasgowComaScaleTotal, S02SpO2,                                                           
         S02ECGFindings, S02CardiacHistoryFindings,                                         
         S02NYHAHeartFailureClassification, S02RespiratoryHistoryFindings,                                     
         S02PatientHadRespiratoryInfectionInTheLastMonth, S02PatientHasHistoryOfCerebrovascularDisease,
         S02PatientHasDiagnosisOfCancerCurrentOrLessThan5YearsNo,           
         S02PatientHasDiagnosisOfCancerCurrentOrLessThan5YearsYesSolidLocal,
         S02PatientHasDiagnosisOfCancerCurrentOrLessThan5YearsYesSolidMeta, 
         S02PatientHasDiagnosisOfCancerCurrentOrLessThan5YearsYesLymphoma,  
         S02PatientHasDiagnosisOfCancerCurrentOrLessThan5YearsYesLeukaemia, 
         S02PatientHasDementia,                                             
         S02PatientHasDiabetes,                                             
         S02HbA1c,S02HbA1c_NK, S02PatientHasLiverDisease, S02PatientHasLiverDiseaseType,                                     
         S02PatientHasLiverDiseaseChildPughGrade, S02PatientsASAGrade,                                               
         S02HasPreoperativeCardiopulmonaryExerciseTestDone, S02PatientsSmokingHistory,                                         
         S02PatientsCurrentAlcoholConsumption,                              
         S02RockwoodClinicalFrailtyScore,
         S03HowManyOperationsInPast30Days,                                  
         S03WasProcedureTheSameAsThePlannedProcedure,                       
         S03ActualSurgicalSpecialty, S03ActualProcedureOperation, S03ActualProcedureSurgeryMode,                                     
         S03ActualSecondaryProcedureMainGroup, S03ActualSecondaryProcedureSubGroup,                               
         S03ActualSecondaryProcedure, S03WhatSurgicalIncisionDidThisPatientHaveThoracic,                 
         S03WhatSurgicalIncisionDidThisPatientHaveUpperAbdominal, S03WhatSurgicalIncisionDidThisPatientHaveLowerAbdominal,
         S03WhatSurgicalIncisionDidThisPatientHaveOther, S03WhatSurgicalIncisionDidThisPatientHave, S03BloodLoss,                                                      
         S03BloodLossDetails, S03DegreeOfPeritonealSoiling, S03DurationOfSurgery,
         S06IsThePatientStillInHospital, S06WhatIsTheCurrentPatientLocation,                                
         S06PatientHasNewRequirementForO2Therapy, S06PatientHasNewRequirementForVentilator,                          
         S06PatientHasNone, S06PatientIsOnIVAntibiotic, S06HasHadATemperatureGreaterThan38InThePast24h,
         S06NoneOfPatientIsOnIVAntibioticOrGreaterThan38, S06PatientHasBeenUnableToTolerateEnteralNutrition,                 
         S06PatientHasHadNauseaVomitingOrAbdominalDistensionInPast24h, S06PatientHasNoneOfUnableToTolerateOrVomiting,                     
         S06PatientHadSinceSurgeryOliguria, S06PatientHadSinceSurgeryCreatinine30, S06PatientHadSinceSurgeryUrinaryCatheterBeyondDay1,                
         S06PatientHadSinceSurgeryNone, S06PatientHasHadDiagnosticTestsHypotension, 
         S06PatientHasHadDiagnosticTestsNewMyocardialInfarctionOrIschaemia, 
         S06PatientHasHadDiagnosticTestsThromboticEvent, S06PatientHasHadDiagnosticTestsArrhythmias,
         S06PatientHasHadDiagnosticTestsCardiogenicPulmonaryOedema, S06PatientHasHadDiagnosticTestsNone,
         S06PatientHasDevelopedSinceSurgeryNewNeurologicalDeficit, S06PatientHasDevelopedSinceSurgeryDeliriumOrConfusion,
         S06PatientHasDevelopedSinceSurgerySedativeInducedComa,
         S06PatientHasDevelopedSinceSurgeryNonSedativeAassociatedComa,
         S06PatientHasDevelopedSinceSurgeryNone,
         S06PatientHadWoundRequiringSurgicalExploration,
         S06PatientHadDrainageOfPus,
         S06PatientHadNone, S06PatientRequiredRedCellTransfusion, S06PatientRequiredPlasma, 
         S06PatientRequiredNoneOfRedCellTransfusionOrPlasma, S06PatientSurgicalPainParenteralOpioids,
         S06PatientSurgicalPainRegionalAnaesthesia, S06PatientSurgicalPainNone,
         S06PatientReturnedToTheirBaselineLevelOfMobility, S06PatientStillRequiringMedicalNursingCare,                        
         S06PatientStillRequiringMobilityIssue, S06PatientStillRequiringAwaitingSocialPackage,
         S06PatientStillRequiringAwaitingOccupationalTherapyReview,
         S06PatientStillRequiringOrganisationalFailure, S06PatientStillRequiringNone,                                       
         S06AntibioticTreatmentForGT24Hours, S06AntibioticWhatIsTheInfectionSource, S06PulmonarySupportOverLast7Days,                                  
         S06PulmonaryPharmalogicalSupport, S06PulmonaryPharmalogicalSupportYes, S06PulmonaryPharmalogicalSupportYesOtherDetails,                   
         S06CardiovascularOverLast7DaysSerumTroponin, S06CardiovascularOverLast7DaysSerumTroponinUnits, 
         S06CardiovascularOverLast7DaysSerumCreatinineUnits, S06CardiovascularOverLast7DaysSerumCreatinineUnits_NK,             
         S06SerumCreatinineValueRecorded, S06SerumCreatinineValue, S06RequiredNewRRTInTheLast7Days,
         S07DateOfDischargeDeath, S07DischargeDestination, S07DateOfDischarge, S07DateOfDeath,                                                    
         S07DateOfWithdrawl, S07GradeLevelOfComplicationsNone, S07GradeLevelOfComplicationsGradeI, 
         S07GradeLevelOfComplicationsGradeII, S07GradeLevelOfComplicationsGradeIIIA, 
         S07GradeLevelOfComplicationsGradeIIIB, S07GradeLevelOfComplicationsGradeIVA,
         S07GradeLevelOfComplicationsGradeIVB,
         S07GradeLevelOfComplicationsGradeV, S07SuspectedInfectionNone, 
         S07SuspectedInfectionSurgical, S07SuspectedInfectionChest, S07SuspectedInfectionUrine, S07SuspectedInfectionNeurological,                                 
         S07SuspectedInfectionEmpirical, S07OtherComplicationsNone, S07OtherComplicationsCardiovascular, S07OtherComplicationsRespiratory,                                  
         S07OtherComplicationsRespiratoryType, S07OtherComplicationsVenousThromboembolism, S07OtherComplicationsGastrointestinal,
         S07OtherComplicationsStroke, S07OtherComplicationsDelirium,                                     
         S07OtherComplicationsRenalReplacementTherapy, 
         DOS, procedure_code, SORT_severity.planned, SORT_severity.actual, SORT_severity.secondary, SORT_severity,
         S14LookAfterPersonalToiletHygieneUnaided,S14AbleToBreatheEasily,
         S14WorkOrUndertakeUsualHomeActivities, 
         EQ5D01Mobility, EQ5D02Selfcare, EQ5D03UsualActivities,
         S15StandingForLongPeriodsSuchAs30Minutes,S15TakingCareOfHouseholdResponsibilities,
         S15HowMuchProblemJoiningCommunityActivitiesInTheSameWayAsAnyoneElseCan,
         S15WalkingALongDistanceSuchAsAKilometreOrEquivalent, S15WashingYourWholeBody,
         S15GettingDressed)


## Correct missing ages where data is available
n.missing.ages = nrow(export_data %>% filter(is.na(S01AgeYears)))
export_data %>% select(CaseId, S01AgeYears) %>% filter(is.na(S01AgeYears)) %>% arrange(CaseId)

missing.ages <- c(116,728,1135,1137,1166,1557,1558,2102,4350,4494,5039,5079)

export_data$S01AgeYears[export_data$CaseId %in% missing.ages] <- c(65,73,77,64,75,82,24,72,49,31,66,58)
export_data$S01AgeMonths[export_data$CaseId %in% missing.ages] <- c(9,6,4,4,6,6,0,7,6,6,5,11)

## Remove patients who withdrew from the study before postoperative day 7
Withdrawl.cases <- export_data %>% select(CaseId, S06IsThePatientStillInHospital, S01DateofSurgery, 
                                        S07DateOfDischarge, S07DateOfWithdrawl, S07DischargeDestination) %>%
  filter(S07DischargeDestination=="WFS") %>% 
  mutate(TimeToWithdrawl=as.numeric(S07DateOfWithdrawl-S01DateofSurgery)) %>% 
  filter(TimeToWithdrawl<7 | S06IsThePatientStillInHospital=="N") %>% arrange(CaseId) %>% .$CaseId 

## from manual inspection of cases with missing data there are futher cases that withdrew but the date of withdrawal has been entered incorrectly by a year (2018 instead of 2017)
## These are cases 2630 and 7008
export_data %>% filter(S02SurgicalSpecialty == "Not recorded") %>%
  select(CaseId, S02SurgicalSpecialty, S01DateofSurgery, S07DateOfDeath, S07DateOfDischarge, S07DateOfWithdrawl)## This is mandatory field so if not recorded suggests there is an issue with this record

## we will manually remove these cases now

# calculate number of patients who withdrew before day 7 and therefore have no outcome data available
n.withdrawl <- length(Withdrawl.cases)

# remove cases who withdrew before day 7 from analysis
export_data <- export_data %>% filter(!CaseId %in% Withdrawl.cases)

## Remove patients where calculated LOS >7 days but no POMS data available (n=17)
LOS.cleaning <- export_data %>% select(CaseId, S01DateofSurgery, S06IsThePatientStillInHospital, 
                                     S07DateOfDischarge) %>%
  mutate(LengthOfStay=as.numeric(S07DateOfDischarge-S01DateofSurgery)) %>% 
  filter(LengthOfStay >7 & S06IsThePatientStillInHospital == "N") %>% arrange(CaseId) %>% .$CaseId

n.LOS.cleaning <- length(LOS.cleaning)

export_data <- export_data %>% filter(!CaseId %in% LOS.cleaning)

## add LOS column to dataset
export_data = export_data %>% mutate(LengthOfStay=as.numeric(S07DateOfDischarge-S01DateofSurgery))

export_data$LengthOfStay[is.na(export_data$LengthOfStay)] = 999

# calculate number of patients with outcome available
n.WithOutcome <- nrow(export_data)

## Create POMS domain outcomes (TRUE/FALSE by domain)
export_data = export_data %>% mutate(Pulmonary.overall=S06PatientHasNewRequirementForO2Therapy == 1 | 
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
                                 POMS.overall=Pulmonary.overall == TRUE | Infectious.overall == TRUE | 
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
                                 POMS.major=Pulmonary.major == TRUE | Infectious.major == TRUE | 
                                   Renal.major == TRUE | Cardiovascular.major == TRUE | Neurological.major == TRUE | 
                                   Wound.major == TRUE | Haematological.major == TRUE | Pain.major == TRUE)

## Create columns for Clavie-Dindo grading of surgical complications
export_data <- export_data %>% 
  replace_na(list(S07GradeLevelOfComplicationsNone=2, S07GradeLevelOfComplicationsGradeI=2, 
                  S07GradeLevelOfComplicationsGradeII=2, S07GradeLevelOfComplicationsGradeIIIA=2, 
                  S07GradeLevelOfComplicationsGradeIIIB=2,
                  S07GradeLevelOfComplicationsGradeIVA=2, 
                  S07GradeLevelOfComplicationsGradeIVB=2,
                  S07GradeLevelOfComplicationsGradeV=2)) %>% 
  mutate(ClavienGradeIIabove = S07GradeLevelOfComplicationsGradeII==1 |
           S07GradeLevelOfComplicationsGradeIIIA == 1 | 
           S07GradeLevelOfComplicationsGradeIIIB == 1 | 
           S07GradeLevelOfComplicationsGradeIVA == 1 | 
           S07GradeLevelOfComplicationsGradeIVB == 1 |
           S07GradeLevelOfComplicationsGradeV == 1,
         max_clavien_complication = ifelse(S07GradeLevelOfComplicationsGradeV==1,"II_V",
                                                ifelse(S07GradeLevelOfComplicationsGradeIVB==1,"II_V",
                                                       ifelse(S07GradeLevelOfComplicationsGradeIVA==1,"II_V",
                                                              ifelse(S07GradeLevelOfComplicationsGradeIIIB==1,"II_V",
                                                                     ifelse(S07GradeLevelOfComplicationsGradeIIIA==1,"II_V", 
                                                                            ifelse(S07GradeLevelOfComplicationsGradeII==1, "II-V",
                                                                                   "zero_I")))))))


## Change POMS outcome to positive for patients who died before day 7
number.died <- nrow(export_data %>% filter(S07DischargeDestination == "Die" & S06IsThePatientStillInHospital == "N"))

cases.died7 <- export_data %>% filter(S07DischargeDestination == "Die" & S06IsThePatientStillInHospital == "N") %>% 
  select(CaseId) %>% arrange(CaseId)

cases.died7 <- as.vector(cases.died7$CaseId)

export_data$POMS.overall[export_data$CaseId %in% cases.died7] <- TRUE
export_data$POMS.major[export_data$CaseId %in% cases.died7] <- TRUE

## Calculate patients remaining with missing outcome
export_data %>% select(CaseId, POMS.overall) %>% filter(is.na(POMS.overall))

## we have no outcome data for the following patients and we will therefore exclude them from further analysis
missing.outcome <- export_data %>% select(CaseId, POMS.overall) %>% filter(is.na(POMS.overall)) %>% .$CaseId

export_data <- export_data %>% filter(!CaseId %in% missing.outcome)

n.missing.outcome = length(LOS.cleaning) + length(missing.outcome) + n.withdrawl
         
n_final_analysis <- nrow(export_data)        
         