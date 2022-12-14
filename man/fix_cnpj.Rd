% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cnpj.R
\name{fix_cnpj}
\alias{fix_cnpj}
\title{Make cnpj strings consistent across different patterns}
\usage{
fix_cnpj(.cnpj)
}
\arguments{
\item{.cnpj}{cnpj string}
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#stable}{\figure{lifecycle-stable.svg}{options: alt='[Stable]'}}}{\strong{[Stable]}}

This function makes cnpj strings consistent across different patterns
}
\examples{
  fake_cnpj <- c(
   "01223446000126",
   "0,3.5;666123,5--2-3.12"
 )
 fix_cnpj(fake_cnpj)
}
