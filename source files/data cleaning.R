#================================================================================================================#
## initial data cleaning script
#================================================================================================================#
#================================================================================================================#
# remove data from test sites
export_data <- export_data %>% filter(SiteName != "Test Site 001" & SiteName !="Test Site 002") %>%
  mutate(SiteName = factor(SiteName))

n_total_export <- nrow(export_data)

# remove unlocked records
export_data$Locked <- as.logical(export_data$Locked)
export_data <- export_data %>% filter(Locked==TRUE)

n_locked <- nrow(export_data)

## reformat postcode field
export_data <- export_data %>% 
  mutate(S01PostcodeOut = str_to_upper(S01PostcodeOut),
       S01PostcodeOut = trimws(S01PostcodeOut, "both"))


#turn all relevant columns into date format YYYY-MM-DD
export_data$LockedDate <- substr(export_data$LockedDate, 1,10)
export_data$LockedDate <- lubridate::dmy(export_data$LockedDate)

export_data$S01DateofAdmissiontoHospital <- substr(export_data$S01DateofAdmissiontoHospital, 1,10)
export_data$S01DateofAdmissiontoHospital <- lubridate::dmy(export_data$S01DateofAdmissiontoHospital)

export_data$DOS <- substr(export_data$S01DateofSurgery, 1,10)
export_data$S01DateofSurgery <- lubridate::dmy(export_data$DOS)

export_data$S07DateOfDischarge <- substr(export_data$S07DateOfDischarge, 1,10)
export_data$S07DateOfDischarge <- lubridate::dmy(export_data$S07DateOfDischarge)

export_data$S07DateOfWithdrawl <- substr(export_data$S07DateOfWithdrawl, 1,10)
export_data$S07DateOfWithdrawl <- lubridate::dmy(export_data$S07DateOfWithdrawl)

## fix error with date of discharge (2017 instead of 2018) for single patient 
export_data$S01DateofSurgery[export_data$CaseId==6116] <- as.Date("2018-01-08")

# split procedure column to create column of procedure and column for SORT severity grading
export_data$procedure_code <- stringr::str_sub(export_data$S02PlannedProcedureOperation,1,-5)

# create SORT severity columns based on planned procedure, actual procedure
export_data$SORT_severity.planned <- factor(stringr::str_sub(export_data$S02PlannedProcedureOperation, -3,-1),
                                          levels = c("Min","Int","Maj","Xma","Com"), ordered=TRUE)
export_data$SORT_severity.actual <- factor(stringr::str_sub(export_data$S03ActualProcedureOperation, -3,-1),
                                         levels = c("Min","Int","Maj","Xma","Com"), ordered=TRUE)
export_data$SORT_severity.secondary <- factor(stringr::str_sub(export_data$S03ActualSecondaryProcedure, -3,-1),
                                            levels = c("Min","Int","Maj","Xma","Com"), ordered=TRUE)

# create final SORT_severity column which is based on the actual procedure performed
export_data = export_data %>% mutate(SORT_severity = ifelse(S03WasProcedureTheSameAsThePlannedProcedure=="Y", SORT_severity.planned,
                                  ifelse(S03WasProcedureTheSameAsThePlannedProcedure=="N" & is.na(S03ActualProcedureOperation)==FALSE &
                                           is.na(S03ActualSecondaryProcedure)==FALSE & SORT_severity.actual >= SORT_severity.secondary, SORT_severity.actual,
                                         ifelse(S03WasProcedureTheSameAsThePlannedProcedure=="N" & is.na(S03ActualProcedureOperation)==FALSE &
                                                  is.na(S03ActualSecondaryProcedure)==FALSE & SORT_severity.actual < SORT_severity.secondary, SORT_severity.secondary,
                                                ifelse(S03WasProcedureTheSameAsThePlannedProcedure=="N" & is.na(S03ActualProcedureOperation)==FALSE, SORT_severity.actual, SORT_severity.secondary)))))

export_data <- export_data %>% mutate(SORT_severity = ifelse(!is.na(SORT_severity), SORT_severity,
                                                             SORT_severity.planned))


# recode severity field to levels Min < Int < Maj <X ma < Com
export_data$SORT_severity <- dplyr::recode_factor(export_data$SORT_severity, `1`="Min", `2`="Int", `3`="Maj",
                                                `4`="Xma", `5`="Com", .ordered = TRUE)

# Make ASA category as ordinal factor
export_data$S02PatientsASAGrade <- dplyr::recode_factor(export_data$S02PatientsASAGrade, 
                                                                    `1`="I", `2`="II",`3`="III",`4`="IV",`5`="V")

# Make S02CardiacHistoryFindings category as ordinal factor
export_data$S02CardiacHistoryFindings <- ordered(export_data$S02CardiacHistoryFindings, levels = c("NF","D","P","R")) 
                                                        

# Make S02RespiratoryHistoryFindings category as ordinal factor
export_data$S02RespiratoryHistoryFindings <- ordered(export_data$S02RespiratoryHistoryFindings, levels = c("N","DOE","DLE","DAR"))


#recode surgical specialty field
export_data$S02SurgicalSpecialty <- dplyr::recode_factor(export_data$S02SurgicalSpecialty,  
                                                         `1`="Urology",
                                                         `2`="Abdominal - Upper gastrointestinal",
                                                         `3`="Abdominal - Lower gastrointestinal",
                                                         `4`="Abdominal - Hepatobiliary", 
                                                         `5`="Thoracics",
                                                         `6`="Head and neck",
                                                         `7`="Abdominal - other",
                                                         `8`="Orthopaedics",
                                                         `9`="Spinal",
                                                         `10`="Burns and Plastics",
                                                         `11`="Gynaecology",
                                                         `12`="Vascular",
                                                         .default = "Not recorded",
                                                         .missing = "Not recorded")


## filter to only incldue cases up to 31st March 2020
export_data <- export_data %>% filter(S01DateofSurgery <= lubridate::dmy("31032020"))

n_eligble_date <- nrow(export_data)

## filter to only include colorectal cases
export_data <- export_data %>% 
  filter(S02SurgicalSpecialty == "Abdominal - Lower gastrointestinal") %>% 
  mutate(S02SurgicalSpecialty = recode_factor(S02SurgicalSpecialty,
                                              `Abdominal - Lower gastrointestinal` = "Colorectal"))

n_colorectal_eligible <- nrow(export_data)




