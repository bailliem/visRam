
library(survival)
library(magrittr)
library(dplyr)
library(ggplot2)
library(broom)


autotab_risk <- function(x, timeby = 10){
  df <- summary_surv(x, timeby) %>%
    dplyr::mutate(strata_id = group_indices(., strata))
  return(df)  
}

summary_surv <- function(x, timeby){
  times <- seq(0, max(x$time), by = timeby)
  df <- 
    tibble::tibble(strata = summary(x, times = times, extend = TRUE)$strata,
                   time = summary(x, times = times, extend = TRUE)$time,
                   n.risk = summary(x, times = times, extend = TRUE)$n.risk,
                   n.event = summary(x, times = times, extend = TRUE)$n.event, 
                   n.censor = summary(x, times = times, extend = TRUE)$n.censor, 
                   estimate = summary(x, times = times, extend = TRUE)$estimate,
                   std.error = summary(x, times = times, extend = TRUE)$std.error,
                   conf.high = summary(x, times = times, extend = TRUE)$conf.high,
                   conf.low = summary(x, times = times, extend = TRUE)$conf.low
                   ) 
  return(df)
} 




fit <- survival::survfit(Surv(time, status) ~ sex, data = lung)
fit %>% 
  tidy()

autotab_risk(fit, 50) %>%
  ggplot(aes(x = time, y = strata_id, label = n.risk, group= strata)) + 
  geom_text() 

times <- seq(0, max(fit$time), by = 50)
summary(fit, times = times, extend = TRUE)
fit
