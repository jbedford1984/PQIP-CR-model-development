## Load IMD data 
## English IMD data
EIMD_2019 <- read_csv("data/IMD_data/EIMD_2019.csv")

EIMD_2019 <- EIMD_2019 %>%
  select(`LSOA code (2011)`, 
         `Index of Multiple Deprivation (IMD) Score`,
         `Income Score (rate)`,
         `Employment Score (rate)`) %>%
  select(`LSOA code (2011)`,
         `Index of Multiple Deprivation (IMD) Score`,
         `Income Score (rate)`,
         `Employment Score (rate)`)

names(EIMD_2019) <- c("LSOA_code", "IMD_score", "Income_score_rate", "Employment_score_rate")

## load exponentially transformed score dataset (IMD 2019)
EIMD_transformed_score <- read_csv("data/IMD_data/EIMD_transformed_score.csv")

EIMD_transformed_score <- EIMD_transformed_score %>%
  select(`LSOA code (2011)`,
         `Income Score - exponentially transformed`,
         `Employment Score - exponentially transformed`)

names(EIMD_transformed_score) <- c("LSOA_code", "Income_score_exptran", "Employment_score_exptran")

# join EIMD datatables together
EIMD_2019 <- left_join(EIMD_2019, EIMD_transformed_score, by = "LSOA_code") %>%
  mutate(Income_score = sqrt(Income_score_exptran),
         Employment_score = sqrt(Employment_score_exptran))

## Welsh IMD
WIMD_2019 <- read_csv("data/IMD_data/WIMD_2019.csv")

names(WIMD_2019)

WIMD_2019 <- WIMD_2019 %>%
  select(`LSOA Code`, 
         `WIMD 2019`,
         `Income`,
         `Employment`) %>% 
  filter(!is.na(`LSOA Code`))

names(WIMD_2019) <- c("LSOA_code", "IMD_score", "Income_score", "Employment_score")

## fit linear model using IMD score as outcome variable as Abel et al 2016
EIMD_model <- lm(IMD_score ~ Income_score_exptran + Employment_score_exptran, data = EIMD_2019)
EIMD_2019 <- EIMD_2019 %>% mutate(residuals = residuals(EIMD_model))

## fit linear model for Welse data
WIMD_model <- lm(IMD_score ~ Income_score + Employment_score, data = WIMD_2019)
WIMD_2019 <- WIMD_2019 %>% mutate(residuals = residuals(WIMD_model))

## get residual variance from model
# English residual variance
summary(EIMD_model)$sigma

# Welsh residual variance
summary(WIMD_model)$sigma

## Predict IMD score for Welsh areas based on English model, using method from Abel et al. 2016
WIMD_2019 <- WIMD_2019 %>% 
  mutate(IMD_score_adjusted = EIMD_model$coefficients[1] +
           Income_score*EIMD_model$coefficients[2] +
           Employment_score*EIMD_model$coefficients[3] +
           (summary(EIMD_model)$sigma * (residuals/summary(WIMD_model)$sigma)))

## Predict IMD score for Welsh areas based on English model, using method from Abel et al. 2016
EIMD_2019 <- EIMD_2019 %>% 
  mutate(IMD_score_adjusted = EIMD_model$coefficients[1] +
           Income_score_exptran*EIMD_model$coefficients[2] +
           Employment_score_exptran*EIMD_model$coefficients[3] +
           (summary(EIMD_model)$sigma * (residuals/summary(EIMD_model)$sigma)))

## Merge Welsh and English IMD data
IMD_adjusted <- dplyr::bind_rows(EIMD_2019, WIMD_2019) %>%
  mutate(IMD_decile_adjusted = ntile(IMD_score_adjusted, 10),
         IMD_quintile_adjusted = ntile(IMD_score_adjusted, 5)) %>%
  select(LSOA_code, IMD_score, residuals, IMD_score_adjusted, IMD_decile_adjusted, IMD_quintile_adjusted)

