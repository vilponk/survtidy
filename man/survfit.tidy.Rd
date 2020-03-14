\name{survfit.tidy}
\alias{survfit.tidy}
\title{survfit.tidy}
\usage{
survfit.tidy()
}
\description{
Prints Kaplan-Meier analysis (survfit) in a tidy format for corresponding years
}
\arguments{
  \item{time}{
  time variable for the Kaplan-Meier analysis
}
 \item{status}{
  status variable for the Kaplan-Meier analysis
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
survfit.tidy(time, status, treatment, 50, 70, veteran)
}
