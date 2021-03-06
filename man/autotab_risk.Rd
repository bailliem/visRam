% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/autoplot.R
\name{autotab_risk}
\alias{autotab_risk}
\title{Create a ggplot of a risk table}
\usage{
autotab_risk(x, timeby = 10)
}
\arguments{
\item{x}{survfit object}

\item{timeby}{Integer to indicate range to summarise risk table}
}
\value{
ggplot object
}
\description{
Create a ggplot of a risk table
}
\examples{
library(survival)
library(magrittr)
library(dplyr)
library(ggplot2)
library(broom)
fit <- survival::survfit(Surv(time, status) ~ sex, data = lung)
autotab_risk(fit, 50) \%>\%
ggplot(aes(x = time, y = strata_id, label = n.risk, group= strata)) + 
geom_text() 
}
