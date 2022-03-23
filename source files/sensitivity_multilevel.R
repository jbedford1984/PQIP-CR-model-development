## conduct sensitivity analysis including hospital site as random intercept term

## Due to failure of model to converge we will standardise continuous variables to see if this helps. 
## This will change interpretation of the odds ratios of these variables compared to the published models 
## but will not affect the interpretation on the odds ratio scales

multilevel_sensitivity_data <- mice::complete(mids_data, "long", include = TRUE) %>%
  mutate(SiteName = rep(data_impute$SiteName,11)) %>%
  mutate(SiteName = droplevels(SiteName)) %>%
  select(.imp, POMS.overall, SiteName, ModeSurgeryCombined, Rockwood.combined, S01Gender,
         SORT_severity.combined, S02PatientsASAGrade.combined, S01BMI.centred,
         S02SerumSodium.centred, IMD_quintile_adjusted, log.Creatinine.centred, 
         S02WhiteCellCount.centred, log.Urea.centred, S01AgeYears.centred) %>%
  mutate(S01BMI.centred.scale = as.vector(scale(S01BMI.centred, center = FALSE)),
         S02SerumSodium.centred.scale = as.vector(scale(S02SerumSodium.centred, center = FALSE)),
         log.Creatinine.centred.scale = as.vector(scale(log.Creatinine.centred, center = FALSE)),
         S02WhiteCellCount.centred.scale = as.vector(scale(S02WhiteCellCount.centred, center = FALSE)),
         log.Urea.centred.scale = as.vector(scale(log.Urea.centred, center = FALSE)),
         S01AgeYears.centred.scale = as.vector(scale(S01AgeYears.centred, center = FALSE))) %>%
  select(.imp, POMS.overall, SiteName, ModeSurgeryCombined, Rockwood.combined, S01Gender,
         SORT_severity.combined, S02PatientsASAGrade.combined, S01BMI.centred.scale,
         S02SerumSodium.centred.scale, IMD_quintile_adjusted, log.Creatinine.centred.scale, 
         S02WhiteCellCount.centred.scale, log.Urea.centred.scale, S01AgeYears.centred.scale) %>%
  as.mids(.)

## load lme4 packages
library(lme4)
library(merTools)
library(broom.mixed)

# fit multilevel model with random intercept term for hospital site
multilevel.model <- POMS.overall ~ 
                              ModeSurgeryCombined + 
                              Rockwood.combined + 
                              S01Gender + 
                              SORT_severity.combined + 
                              S02PatientsASAGrade.combined + 
                              rcs(S01BMI.centred.scale, 4) + 
                              rcs(S02SerumSodium.centred.scale, 4) + 
                              IMD_quintile_adjusted + 
                              rcs(log.Creatinine.centred.scale, 4) + 
                              rcs(S02WhiteCellCount.centred.scale, 4) + 
                              rcs(log.Urea.centred.scale, 4) + 
                              rcs(S01AgeYears.centred.scale, 3) + (1|SiteName)


# run model on impputation 1
randomeffects_1 <- glmer(multilevel.model, 
                         family = binomial,
                         data = mice::complete(multilevel_sensitivity_data, 1),
                         control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

# run model on impputation 2
randomeffects_2 <- glmer(multilevel.model, 
                         family = binomial,
                         data = mice::complete(multilevel_sensitivity_data, 2),
                         control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

# run model on impputation 3
randomeffects_3 <- glmer(multilevel.model, 
                         family = binomial,
                         data = mice::complete(multilevel_sensitivity_data, 3),
                         control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

# run model on impputation 4
randomeffects_4 <- glmer(multilevel.model, 
                         family = binomial,
                         data = mice::complete(multilevel_sensitivity_data, 4),
                         control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

# run model on impputation 5
randomeffects_5 <- glmer(multilevel.model, 
                         family = binomial,
                         data = mice::complete(multilevel_sensitivity_data, 5),
                         control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

# run model on impputation 6
randomeffects_6 <- glmer(multilevel.model, 
                         family = binomial,
                         data = mice::complete(multilevel_sensitivity_data, 6),
                         control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

# run model on impputation 7
randomeffects_7 <- glmer(multilevel.model, 
                         family = binomial,
                         data = mice::complete(multilevel_sensitivity_data, 7),
                         control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

# run model on impputation 8
randomeffects_8 <- glmer(multilevel.model, 
                         family = binomial,
                         data = mice::complete(multilevel_sensitivity_data, 8),
                         control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

# run model on impputation 9
randomeffects_9 <- glmer(multilevel.model, 
                         family = binomial,
                         data = mice::complete(multilevel_sensitivity_data, 9),
                         control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

# run model on impputation 10
randomeffects_10 <- glmer(multilevel.model, 
                         family = binomial,
                         data = mice::complete(multilevel_sensitivity_data, 10),
                         control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

## fit model using with function - we can then use the 'pool' function from mice to pool results using Rubin's rules
multilevel_model_fit <- with(multilevel_sensitivity_data, 
                             glmer(POMS.overall ~ 
                                     ModeSurgeryCombined + 
                                     Rockwood.combined + 
                                     S01Gender + 
                                     SORT_severity.combined + 
                                     S02PatientsASAGrade.combined + 
                                     rcs(S01BMI.centred.scale,  4) + 
                                     rcs(S02SerumSodium.centred.scale, 4) + 
                                     IMD_quintile_adjusted + 
                                     rcs(log.Creatinine.centred.scale, 4) + 
                                     rcs(S02WhiteCellCount.centred.scale, 4) + 
                                     rcs(log.Urea.centred.scale, 4) + 
                                     rcs(S01AgeYears.centred.scale, 3) + (1 | SiteName), 
                                   family = binomial,
                                   control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))))

multilevel_estimates = pool(multilevel_model_fit)

fixed_effects_OR <- as_tibble(summary(multilevel_estimates, conf.int = TRUE, exponentiate = TRUE)[,c(1,2,7,8)]) %>%
  filter(term != "sd__(Intercept)" &
           term != "(Intercept)") %>%
  mutate(term = dplyr::recode(term, 
                              `IMD_quintile_adjusted1 - most deprived` = "IMD quintile [1 - most deprived]",
                              `IMD_quintile_adjusted2` =  "IMD quintile [2]",                                                 
                              `IMD_quintile_adjusted3` =  "IMD quintile [3]",                                                
                              `IMD_quintile_adjusted4` =  "IMD quintile [4]",                                               
                              `ModeSurgeryCombinedOpn` =  "Mode of surgery [Open]",                                                  
                              `ModeSurgeryCombinedRob` =  "Mode of surgery [Robotic]",                                                 
                              `rcs(log.Creatinine.centred.scale, 4)log.Creatinine.centred.scale` =  "Creatinine",    
                              `rcs(log.Creatinine.centred.scale, 4)log.Creatinine.centred.scale'` =  "Creatinine*",        
                              `rcs(log.Creatinine.centred.scale, 4)log.Creatinine.centred.scale''` =  "Creatinine**",       
                              `rcs(log.Urea.centred.scale, 4)log.Urea.centred.scale`  =  "Urea",                  
                              `rcs(log.Urea.centred.scale, 4)log.Urea.centred.scale'` =  "Urea'",                   
                              `rcs(log.Urea.centred.scale, 4)log.Urea.centred.scale''` =  "Urea''",                 
                              `rcs(S01AgeYears.centred.scale, 3)S01AgeYears.centred.scale` = "Age",            
                              `rcs(S01AgeYears.centred.scale, 3)S01AgeYears.centred.scale'` = "Age'",             
                              `rcs(S01BMI.centred.scale, 4)S01BMI.centred.scale` = "Body mass index",                        
                              `rcs(S01BMI.centred.scale, 4)S01BMI.centred.scale'` = "Body mass index'",                      
                              `rcs(S01BMI.centred.scale, 4)S01BMI.centred.scale''` = "Body mass index''",                     
                              `rcs(S02SerumSodium.centred.scale, 4)S02SerumSodium.centred.scale` = "Sodium",       
                              `rcs(S02SerumSodium.centred.scale, 4)S02SerumSodium.centred.scale'` = "Sodium'",      
                              `rcs(S02SerumSodium.centred.scale, 4)S02SerumSodium.centred.scale''` = "Sodium''",     
                              `rcs(S02WhiteCellCount.centred.scale, 4)S02WhiteCellCount.centred.scale`  = "White cell count",
                              `rcs(S02WhiteCellCount.centred.scale, 4)S02WhiteCellCount.centred.scale'` = "White cell count'",
                              `rcs(S02WhiteCellCount.centred.scale, 4)S02WhiteCellCount.centred.scale''` = "White cell count''",
                              `Rockwood.combined2` = "Rockwood CFS [2]",
                              `Rockwood.combined3` = "Rockwood CFS [3]",                                                      
                              `Rockwood.combined4` = "Rockwood CFS [4]",                                                     
                              `Rockwood.combined5` = "Rockwood CFS [5]",                                                     
                              `Rockwood.combined6-9` = "Rockwood CFS [6-9]",                                                   
                              `S01GenderM` = "Sex [male]",                                                             
                              `S02PatientsASAGrade.combinedII` = "ASA-PS [II]",                                         
                              `S02PatientsASAGrade.combinedIII` = "ASA-PS [III]",                                        
                              `S02PatientsASAGrade.combinedIV/V` = "ASA-PS [IV/V]",                                       
                              `SORT_severity.combinedCom` = "Severity of surgergy [Complex]",                                              
                              `SORT_severity.combinedXma`= "Severity of surgergy [Xmajor]")) %>%
  ggplot(aes(x=estimate, y=factor(term))) +
  geom_vline(xintercept = 1, colour = "light grey") +
  geom_vline(aes(xintercept = 1.42, colour = "MOR"), lty = "dashed") +
  geom_point(size=0.5) +
  geom_errorbarh(aes(xmin = `2.5 %`, xmax = `97.5 %`), height = 0, lwd=0.5) + 
  theme_cowplot(font_size = 10) +
  labs(x = "Odds ratio",
       y = "Estimated fixed effects") +
  scale_x_continuous(limits = c(0,12),
                     breaks = seq(0,12,1)) +
  scale_color_manual(name = "", labels = c(MOR = "Median odds ratio of random intercept terms"), values = c(MOR = "light blue")) +
  theme(legend.position = "bottom")

fixed_effects_OR <- plot_grid(fixed_effects_OR, labels = c("c"))

#ggsave(filename = "./figures/re_odds_ratio.eps", plot = fixed_effects_OR, dpi = 500, width = 150, height = 150, units = "mm", device = cairo_ps)

## create replacement for SiteName
set.seed(100)
x <- sample(1:118)

re_dat = rbind(as.data.frame(ranef(randomeffects_1)),
               as.data.frame(ranef(randomeffects_2)),
               as.data.frame(ranef(randomeffects_3)),
                 as.data.frame(ranef(randomeffects_4)),
                 as.data.frame(ranef(randomeffects_5)),
                 as.data.frame(ranef(randomeffects_6)),
                 as.data.frame(ranef(randomeffects_7)),
                 as.data.frame(ranef(randomeffects_8)),
                 as.data.frame(ranef(randomeffects_9)),
                 as.data.frame(ranef(randomeffects_10))) %>%
  mutate(Site = factor(rep(x, 10)),
         .imp = rep(c(1:10),each=118)) %>%
  as_tibble() %>%
  mutate(estimate = condval,
         sd = condsd) %>%
  select(.imp, Site, estimate, sd) %>%
  group_by(Site) %>%
  mutate(mean.estimate = mean(estimate),
         var.within = 1/10*(sum(sd^2)),
         var.between = ((1/(10-1))*(sum((sd-var.within)^2))),
         var.total = var.within + var.between + var.between/10,
         se.pooled = sqrt(var.total)) %>%
  select(.imp, Site, mean.estimate, se.pooled) %>%
  group_by(Site) %>%
  summarise(mean.estimate = mean(mean.estimate),
            se.pooled = mean(se.pooled)) %>%
  mutate(oddsratio = exp(mean.estimate),
         lower.CI = exp(mean.estimate - 1.96*se.pooled),
         upper.CI = exp(mean.estimate + 1.96*se.pooled)) %>%
  arrange(-oddsratio)

## create plot of random effects - random intercept estimates with 95% CIs

re_plot <- ggplot(re_dat, aes(x=reorder(Site, mean.estimate), y=mean.estimate)) +
  geom_hline(yintercept = 0, colour = "light grey") +
  geom_point(size=0.5) +
  geom_linerange(aes(ymin = mean.estimate-1.96*se.pooled, ymax = mean.estimate+1.96*se.pooled), lwd=0.5) + 
  coord_flip() + 
  theme_cowplot(font_size = 8) +
  labs(x = "Pseudoanonymised hospital site",
       y = "Random intercept estimate (log odds scale)") + 
  scale_y_continuous(limits = c(-6,3),
                     breaks = seq(-6,3,1))

## fixed effects
fe_dat = rbind(as.data.frame(broom.mixed::tidy(randomeffects_1)),
               as.data.frame(broom.mixed::tidy(randomeffects_2)),
               as.data.frame(broom.mixed::tidy(randomeffects_3)),
               as.data.frame(broom.mixed::tidy(randomeffects_4)),
               as.data.frame(broom.mixed::tidy(randomeffects_5)),
               as.data.frame(broom.mixed::tidy(randomeffects_6)),
               as.data.frame(broom.mixed::tidy(randomeffects_7)),
               as.data.frame(broom.mixed::tidy(randomeffects_8)),
               as.data.frame(broom.mixed::tidy(randomeffects_9)),
               as.data.frame(broom.mixed::tidy(randomeffects_10))) %>%
  as_tibble() %>%
  mutate(.imp = rep(c(1:10),each=36)) %>%
  select(.imp, term, estimate, std.error) %>%
  group_by(term) %>%
  mutate(mean.estimate = mean(estimate),
         var.within = 1/10*(sum(std.error^2)),
         var.between = ((1/(10-1))*(sum((std.error-var.within)^2))),
         var.total = var.within + var.between + var.between/10,
         se.pooled = sqrt(var.total)) %>%
  select(.imp, term, mean.estimate, se.pooled) %>%
  group_by(term) %>%
  summarise(mean.estimate = mean(mean.estimate),
            se.pooled = mean(se.pooled)) %>%
  mutate(oddsratio = exp(mean.estimate)) %>%
  filter(term != "sd__(Intercept)" &
           term != "(Intercept)") %>%
  mutate(term = dplyr::recode(term, 
                              `IMD_quintile_adjusted1 - most deprived` = "IMD quintile [1 - most deprived]",
                              `IMD_quintile_adjusted2` =  "IMD quintile [2]",                                                 
                              `IMD_quintile_adjusted3` =  "IMD quintile [3]",                                                
                              `IMD_quintile_adjusted4` =  "IMD quintile [4]",                                               
                              `ModeSurgeryCombinedOpn` =  "Mode of surgery [Open]",                                                  
                              `ModeSurgeryCombinedRob` =  "Mode of surgery [Robotic]",                                                 
                              `rcs(log.Creatinine.centred.scale, 4)log.Creatinine.centred.scale` =  "Creatinine",    
                              `rcs(log.Creatinine.centred.scale, 4)log.Creatinine.centred.scale'` =  "Creatinine*",        
                              `rcs(log.Creatinine.centred.scale, 4)log.Creatinine.centred.scale''` =  "Creatinine**",       
                              `rcs(log.Urea.centred.scale, 4)log.Urea.centred.scale`  =  "Urea",                  
                              `rcs(log.Urea.centred.scale, 4)log.Urea.centred.scale'` =  "Urea*",                   
                              `rcs(log.Urea.centred.scale, 4)log.Urea.centred.scale''` =  "Urea**",                 
                              `rcs(S01AgeYears.centred.scale, 3)S01AgeYears.centred.scale` = "Age",            
                              `rcs(S01AgeYears.centred.scale, 3)S01AgeYears.centred.scale'` = "Age*",             
                              `rcs(S01BMI.centred.scale, 4)S01BMI.centred.scale` = "Body mass index",                        
                              `rcs(S01BMI.centred.scale, 4)S01BMI.centred.scale'` = "Body mass index*",                      
                              `rcs(S01BMI.centred.scale, 4)S01BMI.centred.scale''` = "Body mass index**",                     
                              `rcs(S02SerumSodium.centred.scale, 4)S02SerumSodium.centred.scale` = "Sodium",       
                              `rcs(S02SerumSodium.centred.scale, 4)S02SerumSodium.centred.scale'` = "Sodium*",      
                              `rcs(S02SerumSodium.centred.scale, 4)S02SerumSodium.centred.scale''` = "Sodium**",     
                              `rcs(S02WhiteCellCount.centred.scale, 4)S02WhiteCellCount.centred.scale`  = "White cell count",
                              `rcs(S02WhiteCellCount.centred.scale, 4)S02WhiteCellCount.centred.scale'` = "White cell count*",
                              `rcs(S02WhiteCellCount.centred.scale, 4)S02WhiteCellCount.centred.scale''` = "White cell count**",
                              `Rockwood.combined2` = "Rockwood CFS [2]",
                              `Rockwood.combined3` = "Rockwood CFS [3]",                                                      
                              `Rockwood.combined4` = "Rockwood CFS [4]",                                                     
                              `Rockwood.combined5` = "Rockwood CFS [5]",                                                     
                              `Rockwood.combined6-9` = "Rockwood CFS [6-9]",                                                   
                              `S01GenderM` = "Sex [male]",                                                             
                              `S02PatientsASAGrade.combinedII` = "ASA-PS [II]",                                         
                              `S02PatientsASAGrade.combinedIII` = "ASA-PS [III]",                                        
                              `S02PatientsASAGrade.combinedIV/V` = "ASA-PS [IV/V]",                                       
                              `SORT_severity.combinedCom` = "Severity of surgergy [Complex]",                                              
                              `SORT_severity.combinedXma`= "Severity of surgergy [Xmajor]"))

## create plot of fixed effects - random intercept estimates with 95% CIs
fe_plot <- ggplot(fe_dat, aes(x=factor(term), y=mean.estimate)) + 
  geom_hline(yintercept = 0, colour = "light grey") +
  geom_point(size=0.5, color = "navy") +
  geom_linerange(aes(ymin = mean.estimate-1.96*se.pooled, ymax = mean.estimate+1.96*se.pooled), lwd=0.5, color = "navy") + 
  coord_flip() + 
  theme_cowplot(font_size = 8) +
  labs(x = "Fixed effects (patient level variables)",
       y = "Estimated coefficient (log odds scale)") +
  scale_y_continuous(limits = c(-6,3),
                     breaks = seq(-6,3,1))

# create frig plot showing random and fixed effects
re_fe_plot <- plot_grid(fe_plot, re_plot, ncol = 1,
          align='vh', labels=c('a', 'b'),
          rel_heights = c(1,2))

 ggsave(filename = "./figures/re_fe_plot.eps", plot = re_fe_plot, dpi = 500, width = 150, height = 300, units = "mm", device = cairo_ps)



# Calculate mean variance across imputation dataset models
re_variances = rbind(as.data.frame(VarCorr(randomeffects_1)),
                     as.data.frame(VarCorr(randomeffects_2)),
                     as.data.frame(VarCorr(randomeffects_3)),
                     as.data.frame(VarCorr(randomeffects_4)),
                     as.data.frame(VarCorr(randomeffects_5)),
                     as.data.frame(VarCorr(randomeffects_6)),
                     as.data.frame(VarCorr(randomeffects_7)),
                     as.data.frame(VarCorr(randomeffects_8)),
                     as.data.frame(VarCorr(randomeffects_9)),
                     as.data.frame(VarCorr(randomeffects_10))) %>%
  summarise(mean.variance = mean(vcov))

# via https://stat.ethz.ch/pipermail/r-sig-mixed-models/2008q2/000874.html
# also credit Dr Steve Harris: https://gist.github.com/docsteveharris/264628336fa9f75c6b43
# Calculate the median odds ratio
# -------------------------------
MOR <- function(my.var, digits = 3)
{ # MOR arguments: my.var = variance associated with level 2 clustering variable
  # digits = number of decimal places to which MOR value will be rounded.
  
  Median.OR <- round(exp(sqrt(2*my.var)*qnorm(.75)), digits)
  paste("Median Odds-Ratio (MOR) = ", Median.OR)  }

# calculate median odds ratio for random effects in multilevel model
median.odds <- MOR(re_variances$mean.variance[1])

