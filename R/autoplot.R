#' Plot line plot based on a survival or time to event analysis
#'
#' @param x tidy tibble from broom
#'
#' @return ggplot object
#' @export
#'
#' @examples
#' library(survival)
#' survival::survfit(Surv(time, status) ~ sex, data = lung)%>%
#' tidy() %>%
#' autoplot.surv()

autoplot.surv <- function(df){
  gg <- 
    df %>%
    ggplot2::ggplot(aes(time, estimate, group = strata)) + 
    geom_line() +
    geom_linerange(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
    facet_wrap( ~ strata)
  return(gg)
}


