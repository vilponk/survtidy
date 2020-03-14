\name{survfitmod.tidy}
\alias{survfitmod.tidy}
\title{survfitmod.tidy}
\usage{
survfitmod.tidy()
}
\description{
Prints Kaplan-Meier analysis (survfit) in a tidy format for corresponding years
}
\arguments{
  \item{fit}{
  survfit(Surv()) object for the Kaplan-Meier analysis
}
 \item{var1}{
  the grouping variable which forms the strata for the results
}

\item{year1}{
  the first year (numerical) that you are interested for results
}

\item{year2}{
  the second year (numerical) that you are interested for results
}

\item{data}{
  data for the Kaplan-Meier analysis
}
}
\examples{
data(veteran)

survfit.tidy(survfit(Surv(time, status) ~ treatment, df1), treatment, 50, 70, veteran)
}
