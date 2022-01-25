## create lookup table for all English/Welsh postcodes with adjusted IMD score and IMD quintiles

postcode_IMD_lookup_adjusted <- left_join(LSOA_look, IMD_adjusted, by=c("lsoa11cd"="LSOA_code")) %>%
  select(pcds, lsoa11cd, IMD_score, IMD_score_adjusted, IMD_quintile_adjusted, IMD_decile_adjusted) %>% 
  filter(!is.na(IMD_score_adjusted)) %>%
  mutate(IMD_quintile_adjusted = dplyr::recode(IMD_quintile_adjusted, 
                                               `1`='5 - least deprived',
                                               `2`='4',
                                               `3`='3',
                                               `4`='2',
                                               `5`='1 - most deprived'),
         IMD_decile_adjusted = dplyr::recode(IMD_decile_adjusted, 
                                             `1`='10 - least deprived',
                                             `2`='9',
                                             `3`='8',
                                             `4`='7',
                                             `5`='6',
                                             `6`='5',
                                             `7`='4',
                                             `8`='3',
                                             `9`='2',
                                             `10`='1 - most deprived')) %>%
  mutate(postcode = pcds,
         LSOA_code = lsoa11cd) %>%
  select(postcode, LSOA_code, IMD_score, IMD_score_adjusted, IMD_quintile_adjusted, IMD_decile_adjusted)

write_csv(postcode_IMD_lookup_adjusted, file = "./data/postcode_IMD_loookup.csv")
