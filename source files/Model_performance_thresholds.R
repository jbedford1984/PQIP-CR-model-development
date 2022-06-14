## Create sensitivity and specificity table at different threshold cutoffs on predicted scale
Sens_Spec_table <- predictions %>% group_by(Id) %>%
  summarise(mean.pred.POMS = mean(pred)) %>%
  mutate(pred.POMSmajor = inv.logit(-0.5849 + 0.9657*logit(mean.pred.POMS))) %>%
  mutate(POMS.overall = predictions$POMS.overall[1:11646],
         POMS.major = export_data$POMS.major) %>%
  mutate(POMS.overall = factor(ifelse(POMS.overall == TRUE, 1,0)),
         POMS.major = factor(ifelse(POMS.major == TRUE, 1,0))) %>%
  mutate(threshold_0.10_POMS = factor(ifelse(mean.pred.POMS >= 0.1,1,0)),
         threshold_0.15_POMS = factor(ifelse(mean.pred.POMS >= 0.15,1,0)),
         threshold_0.20_POMS = factor(ifelse(mean.pred.POMS >= 0.2,1,0)),
         threshold_0.25_POMS = factor(ifelse(mean.pred.POMS >= 0.25,1,0)),
         threshold_0.30_POMS = factor(ifelse(mean.pred.POMS >= 0.3,1,0)),
         threshold_0.40_POMS = factor(ifelse(mean.pred.POMS >= 0.4,1,0)),
         threshold_0.50_POMS = factor(ifelse(mean.pred.POMS >= 0.5,1,0)))  %>% 
  select(-Id, -mean.pred.POMS, -pred.POMSmajor, -POMS.major) %>%
  pivot_longer(cols = c(-POMS.overall), names_to = "threshold", values_to = "value") %>% 
  group_by(threshold) %>%
  summarise(TruePositive = sum(value==1 & POMS.overall==1),
            FalsePositive = sum(value==1 & POMS.overall==0),
            TrueNegative = sum(value==0 & POMS.overall==0),
            FalseNegative = sum(value==0 & POMS.overall==1)) %>%
  mutate(sensitivity = round(TruePositive/(TruePositive+FalseNegative)*100,1),
         specificity = round(TrueNegative/(TrueNegative+FalsePositive)*100,1),
         PPV = round(TruePositive/(TruePositive+FalsePositive)*100,1),
         NPV = round(TrueNegative/(FalseNegative+TrueNegative)*100,1))

Sens_Spec_table_POMSmajor <- predictions %>% group_by(Id) %>%
  summarise(mean.pred.POMS = mean(pred)) %>%
  mutate(pred.POMSmajor = inv.logit(-0.5849 + 0.9657*logit(mean.pred.POMS))) %>%
  mutate(POMS.overall = predictions$POMS.overall[1:11646],
         POMS.major = export_data$POMS.major) %>%
  mutate(POMS.overall = factor(ifelse(POMS.overall == TRUE, 1,0)),
         POMS.major = factor(ifelse(POMS.major == TRUE, 1,0))) %>%
  mutate(threshold_0.05_POMSmajor = factor(ifelse(pred.POMSmajor >= 0.05,1,0)),
         threshold_0.10_POMSmajor = factor(ifelse(pred.POMSmajor >= 0.1,1,0)),
         threshold_0.15_POMSmajor = factor(ifelse(pred.POMSmajor >= 0.15,1,0)),
         threshold_0.20_POMSmajor = factor(ifelse(pred.POMSmajor >= 0.2,1,0)),
         threshold_0.25_POMSmajor = factor(ifelse(pred.POMSmajor >= 0.25,1,0)),
         threshold_0.30_POMSmajor = factor(ifelse(pred.POMSmajor >= 0.3,1,0)),
         threshold_0.40_POMSmajor = factor(ifelse(pred.POMSmajor >= 0.4,1,0))) %>% 
  select(-Id, -mean.pred.POMS, -pred.POMSmajor, -POMS.overall) %>%
  pivot_longer(cols = c(-POMS.major), names_to = "threshold", values_to = "value") %>% 
  group_by(threshold) %>%
  summarise(TruePositive = sum(value==1 & POMS.major==1),
            FalsePositive = sum(value==1 & POMS.major==0),
            TrueNegative = sum(value==0 & POMS.major==0),
            FalseNegative = sum(value==0 & POMS.major==1)) %>%
  mutate(sensitivity = round(TruePositive/(TruePositive+FalseNegative)*100,1),
         specificity = round(TrueNegative/(TrueNegative+FalsePositive)*100,1),
         PPV = round(TruePositive/(TruePositive+FalsePositive)*100,1),
         NPV = round(TrueNegative/(FalseNegative+TrueNegative)*100,1))


  
