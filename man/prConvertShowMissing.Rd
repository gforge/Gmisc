\name{prConvertShowMissing}
\alias{prConvertShowMissing}
\title{A functuon for converting a show_missing variable}
\usage{
prConvertShowMissing(show_missing)
}
\arguments{
  \item{show_missing}{Boolean or "no", "ifany", "always"}
}
\value{
string
}
\description{
The variable is suppose to be directly compatible with
table(..., useNA=show_missing). It throughs an error if not
compatible
}
\author{
max
}

