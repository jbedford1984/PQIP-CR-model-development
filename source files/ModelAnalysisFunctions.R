## colour scheme for plots
cbPalette <- c("#E69F00", "#009E73", "#CC79A7", "#999999", "#56B4E9", "#F0E442", "#0072B2", "#D55E00")

## plot theme for thesis
theme_thesis <- function(base_size = 11,
                      base_family = "Calibri",
                      base_line_size = base_size / 170,
                      base_rect_size = base_size / 170){
  theme_bw(base_size = base_size, 
                base_family = base_family,
                base_line_size = base_line_size) %+replace%
    theme(plot.title = element_text(
        color = rgb(25, 43, 65, maxColorValue = 255), 
        face = "bold",
        hjust = 0),
      axis.title = element_text(
        color = rgb(105, 105, 105, maxColorValue = 255),
        size = rel(0.75)),
      axis.text = element_text(
        color = rgb(105, 105, 105, maxColorValue = 255),
        size = rel(0.5)),
      panel.grid.major = element_blank(),  
      panel.grid.minor = element_blank(), 
      
      complete = TRUE
    )
}

## =================================================
## function to calculate AUC from validate dataframe
## =================================================
CalculateAucFromDxy <- function(validate) {
  ## Test if the object is correct
  stopifnot(class(validate) == "validate")
  
  ## Calculate AUCs from Dxy's
  aucs <- (validate["Dxy", c("index.orig","training","test","optimism","index.corrected")])/2 + 0.5
  
  ## Get n
  n <- validate["Dxy", c("n")]
  
  ## Combine as result
  res <- rbind(validate, AUC = c(aucs, n))
  
  ## Fix optimism
  res["AUC","optimism"] <- res["AUC","optimism"] - 0.5
  
  ## Return results
  res
}


## =================================================================
## function to calculate POSSUM and SORT variables from dataframe
## =================================================================
# Create dataframe to calculate POSSUM for POMS_data

#This is an attempt to write a function to parse a dataframe and compute POSSUM scores
#To use the function, you will need to manipulate your dataframe to have columns with the structure below:
#
#This function will take a dataframe containing the following variables:
#Age            continuous
#JVP            binary, whether the patient has raised JVP or not
#Cardiomegaly   binary, whether the patient has cardiomegaly on CXR or not
#Oedema         binary, whether the patient has peripheral oedema or not
#Warfarin       binary, whether the patient normally takes warfarin or not
#Diuretics      binary, whether the patient normally takes a diuretic medication or not
#AntiAnginals   binary, whether the patient normally takes anti-anginal medication or not
#Digoxin        binary, whether the patient normally takes digoxin or not
#AntiHypertensives  binary, whether the patient normally takes blood pressure meds or not
#Dyspnoea       categorical, can be: Non = None; OME = On exertion; L = Limiting activities; AR = At rest
#Consolidation  binary, whether the patient has consolidation on CXR
#PulmonaryFibrosis  binary, whether the patient has a history of pulmonary fibrosis or imaging findings of fibrosis
#COPD           binary, whether the patient has COPD or not
#SysBP          continuous, pre-op systolic blood pressure (mmHg)
#HR             continuous, pre-op pulse/heart rate
#GCS            continuous, pre-op GCS
#Hb             continuous, pre-op Hb (g/L)
#WCC            continuous, pre-op WCC (* 10^9cells/L)
#Ur             continuous, pre-op Urea (mmol/L)
#Na             continuous, pre-op Na (mmol/L)
#K              continuous, pre-op K (mmol/L)
#ECG            categorical, ND = Not done; NOR = Normal ECG; AF6090 = AF 60-90; AF>90 = AF>90; QW = Q-waves; 4E = >4 ectopics; ST = ST or T wave changes; O = Any other abnormal rhythm
#OpSeverity     categorical, Min = Minor; Int = Intermediate; Maj = Major; Xma = Xmajor; Com = Complex
#ProcedureCount categorical, 1 = 1; 2 = 2; GT2 = >2
#EBL            categorical, 0 = 0-100ml; 101 = 101-500ml; 501 = 501-999ml; 1000 = >=1000
#PeritonealContamination  categorical, NA = Not applicable; NS = No soiling; MS = Minor soiling; LP = Local pus; FBC = Free bowel content pus or blood
#Malignancy     categorical, NM = Not malignant; PM = Primary malignancy only; MNM = Malignancy + nodal metastases; MDM = Malignancy + distal metastases
#OpUrgency      categorical, NCEPOD classifications. Ele = Elective; Exp = Expedited; U = Urgent; I = Immediate
#
#The function will then return a dataframe (tibble), which you can assign to an object, with the following variables:
#PhysScore      The physciological score for POSSUM
#OpScore        The operative score for POSSUM
#POSSUMLogit    The log-odds for morbidity as calculated by POSSUM
#pPOSSUMLogit   The log-odds for mortatlity as calculated by pPOSSUM

gen.POSSUM <- function(x){
  require(dplyr)
  
  #Compute the physiological score
  possum.df <- x %>% 
    mutate(AgeCat = cut(Age, breaks = c(0,60,70, Inf))) %>%
    mutate(AgeScore = ifelse(AgeCat == "(0,60]", 1,
                             ifelse(AgeCat == "(60,70]", 2, 
                                    ifelse(AgeCat == "(70,Inf]", 4, NA)))) %>%
    mutate(CardioScore = ifelse((JVP %in% c("Y", "1", "TRUE") |
                                   Cardiomegaly %in% c("Y", "1", "TRUE")), 8,
                                ifelse((Oedema %in% c("Y", "1", "TRUE") | 
                                          Warfarin %in% c("Y", "1", "TRUE")), 4,
                                       ifelse((Diuretics %in% c("Y", "1", "TRUE") | 
                                                 AntiAnginals %in% c("Y", "1", "TRUE") | 
                                                 Digoxin %in% c("Y", "1", "TRUE") | 
                                                 AntiHypertensives %in% c("Y", "1", "TRUE")), 2, 1)))) %>%
    mutate(RespScore = ifelse((Dyspnoea %in% "AR" |
                                 Consolidation %in% c("Y", "1", "TRUE") |
                                 PulmonaryFibrosis %in% c("Y", "1", "TRUE")), 8, 
                              ifelse((Dyspnoea %in% "L" |
                                        COPD %in% c("Y", "1", "TRUE")), 4, 
                                     ifelse((Dyspnoea %in% "OME"), 2, 1)))) %>%
    mutate(BPCat = cut(SysBP, breaks = c(0, 89, 99, 109, 130, 170, Inf))) %>%
    mutate(BPScore = ifelse((BPCat %in% "(0,89]"), 8, 
                            ifelse((BPCat %in% "(170,Inf]" | BPCat %in% "(89,99]"), 4,
                                   ifelse((BPCat %in% "(130,170]" | BPCat %in% "(99,109]"), 2, 1)))) %>%
    mutate(HRCat = cut(HR, breaks = c(0, 39, 49, 80, 100, 120, Inf))) %>%
    mutate(HRScore = ifelse((HRCat %in% "(0,39]" | HRCat %in% "(120,Inf]"), 8, 
                            ifelse((HRCat %in% "(100,120]"), 4,
                                   ifelse((HRCat %in% "(39,49]" | HRCat %in% "(80,100]"), 2, 1)))) %>%
    mutate(GCSCat = cut(GCS, breaks = c(0, 8, 11, 14, Inf))) %>%
    mutate(GCSScore = ifelse((GCSCat %in% "(0,8]"), 8, 
                             ifelse((GCSCat %in% "(8,11]"), 4,
                                    ifelse((GCSCat %in% "(11,14]"), 2, 1)))) %>%
    mutate(HbCat = cut(Hb, breaks = c(0, 9.9, 11.4, 12.9, 16, 17, 18, Inf))) %>%
    mutate(HbScore = ifelse((HbCat %in% "(0,9.9]" | HbCat %in% "(18,Inf]"), 8, 
                            ifelse((HbCat %in% "(9.9,11.4]" | HbCat %in% "(17,18]"), 4,
                                   ifelse((HbCat %in% "(11.4,12.9]" | HbCat %in% "(16,17]"), 2, 1)))) %>%
    mutate(WCCCat = cut(WCC, breaks = c(0, 3, 4, 10, 20, Inf))) %>%
    mutate(WCCScore = ifelse((WCCCat %in% "(0,3]" | WCCCat %in% "(20,Inf]"), 4, 
                             ifelse((GCSCat %in% "(10,20]" | GCSCat %in% "(3,4]"), 2, 1))) %>%
    mutate(UrCat = cut(Ur, breaks = c(0, 7.5, 10, 15, Inf))) %>%
    mutate(UrScore = ifelse((UrCat %in% "(15,Inf]"), 8, 
                            ifelse((UrCat %in% "(10,15]"), 4,
                                   ifelse((UrCat %in% "(7.5,10]"), 2, 1)))) %>%
    mutate(NaCat = cut(Na, breaks = c(0, 125, 130, 135, Inf))) %>%
    mutate(NaScore = ifelse((NaCat %in% "(0,125]"), 8, 
                            ifelse((NaCat %in% "(125,130]"), 4,
                                   ifelse((NaCat %in% "(130,135]"), 2, 1)))) %>%
    mutate(KCat = cut(K, breaks = c(0, 2.8, 3.1, 3.4, 5, 5.3, 5.9,  Inf))) %>%
    mutate(KScore = ifelse((KCat %in% "(0,2.8]" | KCat %in% "(5.9, Inf]"), 8, 
                           ifelse((KCat %in% "(2.8,3.1]" | KCat %in% "(5.3,5.9]"), 4,
                                  ifelse((KCat %in% "(3.1,3.4]" | KCat %in% "(5,5.3]"), 2, 1)))) %>%
    mutate(ECGScore = ifelse((ECG %in% c("AF>90", "QW", "4E", "ST", "O")), 8,
                             ifelse((ECG %in% "AF6090"), 4, 1))) %>%
    mutate(PhysScore = AgeScore + CardioScore + RespScore + HRScore + GCSScore + HbScore + WCCScore + UrScore + NaScore + KScore + ECGScore)
  
  #Next compute Operative score
  possum.df <- possum.df %>%
    mutate(OpSeverityScore = ifelse((OpSeverity %in% c("Xma", "Com")), 8, 
                                    ifelse((OpSeverity %in% "Maj"), 4,
                                           ifelse((OpSeverity %in% "Int"), 2, 1)))) %>%
    mutate(MultiProcedureScore = ifelse((ProcedureCount %in% "GT2"), 8,
                                        ifelse((ProcedureCount %in% "2"), 4, 1))) %>%
    mutate(EBLScore = ifelse((EBL %in% "1000"), 8,
                             ifelse((EBL %in% "501"), 4,
                                    ifelse((EBL %in% "101"), 2, 1)))) %>%
    mutate(SoilingScore = ifelse((PeritonealContamination %in% "FBC"), 8,
                                 ifelse((PeritonealContamination %in% "LP"), 4,
                                        ifelse((PeritonealContamination %in% "MS"), 2, 1)))) %>%
    mutate(MalignancyScore = ifelse((Malignancy %in% "MDM"), 8,
                                    ifelse((Malignancy %in% "MNM"), 4,
                                           ifelse((Malignancy %in% "PM"), 2, 1)))) %>%
    mutate(UrgencyScore = ifelse((OpUrgency %in% "I"), 8,
                                 ifelse((OpUrgency %in% "U"), 4, 1))) %>%
    mutate(OpScore = OpSeverityScore + MultiProcedureScore + EBLScore + SoilingScore + MalignancyScore + UrgencyScore)
  
  #Now compute the POSSUM and pPOSSUM logit values
  possum.df <- possum.df %>%
    mutate(pPOSSUMLogit = -9.065 + (0.1692 * PhysScore)+ (0.1550 * OpScore)) %>%
    mutate(POSSUMLogit = -5.91 + (0.16 * PhysScore)+ (0.19 * OpScore))
  
  return(possum.df)
}


POSSUM.SORT.calculation = function(data) {

  POSSUM.SORT_data <- data %>% 
    drop_na(S01AgeYears,S02CardiacHistoryFindings,S02RespiratoryHistoryFindings,S02SystolicBP,
            S02PulseRate, S02GlasgowComaScaleTotal, S02Haemoglobin, S02WhiteCellCount, S02SerumUrea,
            S02SerumSodium,S02SerumPotassium,S02ECGFindings,S03HowManyOperationsInPast30Days,S03BloodLoss,
            S03DegreeOfPeritonealSoiling,S02PatientHasDiagnosisOfCancerCurrentOrLessThan5YearsYesSolidLocal,
            S02PatientHasDiagnosisOfCancerCurrentOrLessThan5YearsYesSolidMeta,S02UrgencyOfSurgery) %>%
  mutate(Age=S01AgeYears,
         JVP= ifelse(S02CardiacHistoryFindings=="R",TRUE,FALSE),
         Cardiomegaly= ifelse(S02CardiacHistoryFindings=="P",TRUE,FALSE),
         Oedema = ifelse(S02CardiacHistoryFindings=="P",TRUE,FALSE),
         Warfarin = ifelse(S02CardiacHistoryFindings=="P",TRUE,FALSE),
         Diuretics = ifelse(S02CardiacHistoryFindings=="D",TRUE,FALSE),
         AntiAnginals = ifelse(S02CardiacHistoryFindings=="D",TRUE,FALSE),
         Digoxin = ifelse(S02CardiacHistoryFindings=="D",TRUE,FALSE),
         AntiHypertensives = ifelse(S02CardiacHistoryFindings=="D",TRUE,FALSE),
         Dyspnoea = dplyr::recode(S02RespiratoryHistoryFindings, `N`="Non", 
                                  `DOE`="OME",
                                  `DLE`="L",
                                  `DAR`="AR"),
         Consolidation = ifelse(S02RespiratoryHistoryFindings=="DAR", TRUE, FALSE),
         PulmonaryFibrosis = ifelse(S02RespiratoryHistoryFindings=="DAR", TRUE, FALSE),
         COPD = ifelse(S02RespiratoryHistoryFindings=="DAR", TRUE, FALSE),
         SysBP = S02SystolicBP,
         HR = S02PulseRate,
         GCS = S02GlasgowComaScaleTotal,
         Hb = S02Haemoglobin,
         WCC = S02WhiteCellCount,
         Ur = S02SerumUrea,
         Na = S02SerumSodium,
         K = S02SerumPotassium,
         ECG = dplyr::recode(S02ECGFindings, `ND`="ND", `N`="NOR", `A69`="AF6090", `A90`="AF>90"),
         OpSeverity = SORT_severity,
         ProcedureCount = dplyr::recode(S03HowManyOperationsInPast30Days, `G2`="GT2"),
         EBL = dplyr::recode_factor(S03BloodLoss, `L101`="0", `500`="101", `1000`="501", `G1000`="1000"),
         PeritonealContamination = dplyr::recode(S03DegreeOfPeritonealSoiling, `N`="NS", `SF`="MS"),
         Malignancy = ifelse(S02PatientHasDiagnosisOfCancerCurrentOrLessThan5YearsYesSolidLocal==1,"PM",
                             ifelse(S02PatientHasDiagnosisOfCancerCurrentOrLessThan5YearsYesSolidMeta==1,"MDM","NM")),
         OpUrgency = dplyr::recode(S02UrgencyOfSurgery, `El`="Ele", `Ex`="Exp")) %>%
  gen.POSSUM(.) %>%
  mutate(POSSUM.morbidity=exp(POSSUMLogit) / (1 + exp(POSSUMLogit)),
         PPOSSUM.mortality=exp(pPOSSUMLogit) / (1 + exp(pPOSSUMLogit))) %>%
  mutate(SORT_Logit_Score = (- 3.228 +
                               (S02PatientsASAGrade == "II") * 0.332 +
                               (S02PatientsASAGrade == "III") * 1.140 + 
                               (S02PatientsASAGrade == "IV") * 1.223 +
                               (S02PatientsASAGrade == "V") * 1.223 +
                               (S02SurgicalSpecialty == "Colorectal") * 1.658 +
                               (S02SurgicalSpecialty == "Abdominal - Upper gastrointestinal") * -0.929 +
                               (S02SurgicalSpecialty == "Urology" |
                                  S02SurgicalSpecialty == "Thoracics" |
                                  S02SurgicalSpecialty == "Abdominal - other" |
                                  S02SurgicalSpecialty == "Head and neck" |
                                  S02SurgicalSpecialty == "Abdominal - Hepatobiliary") * 0.181 +
                               (SORT_severity == "Xma") * 1.238 + 
                               (SORT_severity == "Com") * 1.238 +
                               (CancerDiagnosis == "Cancer diagnosis") * 0.897 + 
                               (S01AgeYears >= 65 & S01AgeYears <80) * 0.118 + 
                               (S01AgeYears >= 80) * 0.550)) %>%
  mutate(POMS_SORT = arm::invlogit(SORT_Logit_Score)) %>%
  mutate(POMSminor.SORT = arm::invlogit(SORT_Logit_Score * 1.008 - 0.316)) %>%
  mutate(POMSmajor.SORT = arm::invlogit(SORT_Logit_Score * 0.827 - 0.874)) %>%
    mutate(SORT_Logit_Score.POMSmajor = SORT_Logit_Score * 0.827 - 0.874) %>%
    mutate(SORT.Age = ifelse(S01AgeYears < 65, "18-64",
                             ifelse(S01AgeYears >= 65 & S01AgeYears < 80, "65-79",
                                    ifelse(S01AgeYears >= 80, "80+", NA))),
           SORT.ASA = dplyr::recode_factor(S02PatientsASAGrade, 
                                           `IV` = "IV/V",
                                           `V` = "IV/V"),
           SORT.severity = dplyr::recode_factor(SORT_severity, 
                                                `Min` = "Min/Int/Maj",
                                                `Int` = "Min/Int/Maj",
                                                `Maj` = "Min/Int/Maj")) %>%
  dplyr::select(CaseId, PhysScore, OpScore, POSSUMLogit, POSSUM.morbidity, PPOSSUM.mortality,
                SORT.Age, SORT.ASA, SORT.severity, SORT_Logit_Score, SORT_Logit_Score.POMSmajor, POMS_SORT, POMSminor.SORT, POMSmajor.SORT)
  
  
  POSSUM.SORT_data = left_join(data, POSSUM.SORT_data, by="CaseId")
  
  POSSUM.int_calibration.model <- glm(POMS.overall ~ POSSUMLogit, data = POSSUM.SORT_data, family = "binomial")
  SORT.int_calibration.model <- glm(POMS.overall ~ SORT_Logit_Score, data = POSSUM.SORT_data, family = "binomial") 
  
  POSSUM.int_slope_calibration <- glm(POMS.overall ~ PhysScore + OpScore, data = POSSUM.SORT_data, family = "binomial")
  SORT.int_slope_calibration <- glm(POMS.overall ~ SORT.Age + SORT.ASA + SORT.severity + CancerDiagnosis, data = POSSUM.SORT_data, family = "binomial")
  
  POSSUM.SORT_data <- POSSUM.SORT_data %>%
    mutate(POSSUM.morbidity.intercept_calibrated = predict(POSSUM.int_calibration.model, newdata = ., type = "response"),
           POSSUM.morbidity.intercept_slope_calibrated = predict(POSSUM.int_slope_calibration, newdata = ., type = "response"),
           SORT.morbidity.intercept_calibrated = predict(SORT.int_calibration.model, newdata = ., type = "response"),
           SORT.morbidity.intercept_slope_calibrated = predict(SORT.int_slope_calibration, newdata = ., type = "response"))

return(POSSUM.SORT_data)
}

calibration_data_POSSUM_SORT <- function(data) {
  
  Predicted.POSSUM = data %>%
    select(POMS.overall, POSSUM.morbidity.calibrated) %>%
    mutate(binPred=ntile(POSSUM.morbidity.calibrated,10),
           Outcome=ifelse(POMS.overall==TRUE,1,0),
           Freq=1) %>%
    group_by(binPred) %>% 
    summarise(Observed.POSSUM=sum(Outcome, na.rm = TRUE)/sum(Freq, na.rm = TRUE),
              Predicted.POSSUM=mean(POSSUM.morbidity.calibrated, na.rm=TRUE),
              n.POSSUM=sum(Freq, na.rm = TRUE)) %>%
    filter(!is.na(binPred))
  
  Predicted.SORT = data %>%
    select(POMS.overall, SORT.morbidity.calibrated) %>%
    mutate(binPred=ntile(SORT.morbidity.calibrated,10),
           Outcome=ifelse(POMS.overall==TRUE,1,0),
           Freq=1) %>%
    group_by(binPred) %>% 
    summarise(Observed.SORT=sum(Outcome, na.rm = TRUE)/sum(Freq, na.rm = TRUE),
              Predicted.SORT=mean(SORT.morbidity.calibrated, na.rm=TRUE),
              n.SORT=sum(Freq, na.rm = TRUE)) %>%
    filter(!is.na(binPred))
  
   Calibration.SORT.POSSUM <- left_join(Predicted.POSSUM, Predicted.SORT, by = "binPred")
   
   return(Calibration.SORT.POSSUM)
  
}

summary_variable_POMS_numerical <- function(data, variable, breaks) {
  
    tab <- mice::complete(data, "long") %>%
      as_tibble(.) %>%
      select({{variable}}, POMS.overall) %>%
      mutate(cuts = cut({{variable}}, breaks = breaks),
             n = 1) %>%
      group_by(cuts) %>%
      summarise(POMS.n = sum(POMS.overall)/10,
                n = sum(n)/10) %>%
      mutate(perc = round(n/sum(n)*100,3),
             perc.POMS = round(POMS.n/n*100,3))
  
  return(tab)
}

summary_variable_POMS_factor <- function(data, variable) {
  
  tab <- mice::complete(data, "long") %>%
    as_tibble(.) %>%
    select({{variable}}, POMS.overall) %>%
    mutate(n = 1) %>%
    group_by({{variable}}) %>%
    summarise(POMS.n = sum(POMS.overall)/10,
              n = sum(n)/10) %>%
    mutate(perc = round(n/sum(n)*100,3),
           perc.POMS = round(POMS.n/n*100,3))
  
  return(tab)
}

