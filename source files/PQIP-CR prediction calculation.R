## This R function calculates predicted risk of postoperative morbidity defined by POMS and POMSmajor, 
## as shown in the paper: Development and internal validation of a risk-adjustment model for postoperative morbidity 
## in adults undergoing major elective colorectal surgery: 
## the Perioperative Quality Improvement Programme – colorectal risk model 

## 'data' is a dataframe that should contain the following columns:
## ModeSurgery = mode of surgery, factor, 'Opn' = open, 'Rob' = robotic, 'Lap' = laparoscopic
## Rockwood = Rockwood clinical frailty scale, factor, 1,2,3,4,5,or 6-9
## Sex, factor, 'F' = female, 'M' = male
## SORT_severity = AXA/SORT severity of surgery grading, factor, 'Min/Int/Maj' = Minor, intermediate or major
### 'Xma' = Xmajor, 'Com' = complex
## ASA = ASA grade, factor, `I`=1,`II`=2,`III`=3, `IV/V`= 4 or 5
## BMI - body mass index, (kg/m2), numerical 
## Sodium - serum sodium (mmol/),  numerical
## IMD_quintile = IMD quintile based on lookup table available in data file in repository, factor, 
### `1 - least deprived`, `2`, `3`, `4`, `5 - most deprived`
## Creatinine = serum creatinine, umol/L, numerical
## WhiteCellCount = white cell count, x10^9/L, numerical
## Urea = serum urea, mmol/L, numerical
## Age = age (years), numerical


PQIP_CR_risk_calculation <- function(data) {
  
  data <- data %>%
    mutate(BMI.centred = BMI-28,
           Sodium.centred = Sodium-140,
           log.Creatinine.centred = log(Creatinine)-4.3,
           log.Urea.centred = log(Urea)-1.6,
           WhiteCellCount.centred = WhiteCellCount-7.6,
           Age.centred = Age-64) %>%
    mutate(PQIP_CR_POMS_logit = -3.3081876 +
             0.71919942*(ModeSurgery=="Opn") +
             0.34618954*(ModeSurgery=="Rob") +
             0.2551226*(Rockwood=="2") +
             0.51657126*(Rockwood=="3") +
             0.70509523*(Rockwood=="4") +
             0.8082908*(Rockwood=="5") +
             0.90823978*(Rockwood=="6-9") +
             0.30969644*(Sex=="M") +
             0.17454359*(SORT_severity=="Xma") +
             0.56964698*(SORT_severity=="Com") +
             0.15952372*(ASA=="II") +
             0.42110205*(ASA=="III") +
             0.44385062*(ASA=="IV/V") +
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
  }


