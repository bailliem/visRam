library(survival)
library(magrittr)
library(dplyr)
library(ggplot2)
library(broom)
library(patchwork)
library(gridExtra)

glimpse(lung)
fit <- survfit(Surv(time, status) ~ sex + ph.ecog , data = lung) 

fit %>% 
  tidy() %>%
  group_by(strata) %>%
  summarise(n = n())

ggkm(fit, timeby = 50, table = TRUE, ci = TRUE, pval = TRUE)


library(survival)
data(colon)
fit <- survfit(Surv(time,status)~rx + surg , data=colon)

## how to manage multiple strata which can explode number of unique groups
## i.e. number of curves to plot. 

fit %>% 
  tidy() %>%
  group_by(strata) %>%
  summarise(n = n())



ggkm(fit, table=TRUE, pval = TRUE, ci = TRUE) 

###how to return a ggplot object that can be modified?
###+  facet_wrap(~strata, ncol = 1)

