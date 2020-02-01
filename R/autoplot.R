#' Plot line plot based on a survival or time to event analysis
#'
#' @param x tidy tibble from broom
#'
#' @return ggplot object
#' @export
#'
#' @examples
#' library(survival)
#' library(magrittr)
#' library(dplyr)
#' library(ggplot2)
#' library(broom)
#' survival::survfit(Surv(time, status) ~ sex, data = lung)%>%
#'   tidy() %>%
#'   autoplot_surv()

autoplot_surv <- function(x) {
  df <- x
  gg <-
    df %>%
    ggplot2::ggplot(aes(time, estimate, group = strata)) +
    geom_line() +
    geom_linerange(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
    geom_point(
      data = df %>% dplyr::filter(n.censor > 0),
      aes(time, estimate),
      alpha = 0.5
    ) +
    ggtitle("Update the title", subtitle = "Fill out the population") +
    facet_wrap( ~ strata)
  return(gg)
}

