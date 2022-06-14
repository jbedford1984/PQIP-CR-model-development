## Create table 2

table_2 <- kable(export_data %>% 
                   mutate(LOS7 = LengthOfStay>=7,
                          died7 = S07DischargeDestination == "Die" & S06IsThePatientStillInHospital == "N") %>%
                   select(LOS7, POMS.overall, Pulmonary.overall, Cardiovascular.overall,
                       Infectious.overall, Neurological.overall, Pain.overall, Renal.overall, Haematological.overall, 
                       Gastrointestinal.overall, Wound.overall, POMS.major,
                       Pulmonary.major,Cardiovascular.major, Infectious.major,
                       Neurological.major, Pain.major, Renal.major, Haematological.major,
                       Wound.major, died7, ClavienGradeIIabove) %>%
  summarise(across(.cols = everything(), ~ sum(.x))) %>%
  pivot_longer(everything(), names_to = "outcome", values_to = "n") %>%
  mutate(perc = round(n/nrow(export_data)*100,1)))
