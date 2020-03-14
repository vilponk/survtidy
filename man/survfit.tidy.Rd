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
time: time in survival analysis.

status: status in survival analysis.

var1: the grouping variable.

year1: the first year for results.

year2: the second year for results.

data: the name of the data
}


\examples{
data(veteran)
survfit.tidy(time, status, treatment, 50, 70, veteran)
}
