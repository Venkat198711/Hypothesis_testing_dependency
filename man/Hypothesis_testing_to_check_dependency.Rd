% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Hypothesis_testing_to_check_dependency.R
\name{Hypothesis_testing_to_check_dependency}
\alias{Hypothesis_testing_to_check_dependency}
\title{Hypothesis for checking dependency}
\usage{
Hypothesis_testing_to_check_dependency(data, type, target_variable,
  categoric_function, numeric_function, target_variable_index,
  conf_interval)
}
\arguments{
\item{data}{type target_variable, target_variable_index, conf_interval}
}
\description{
Summary of p-values for chi-square/t-test to check dependency based on target variable type (Categoric/Numeric)
}
\examples{
Hypothesis_testing_to_check_dependency(cars, type = "category" or "numeric", target_variable = "userinput depends on type parameter", categoric_function(Separate_categoric), numeric_function(Separate_numeric), target_variable_index(index of target variable returned by the function Separate_categoric/Separate_numeric), conf_interval = "0.95") [User is advised to run first "Separate_categoric/Separte_numeric function" to know the index of target variable][parameter data should be same as for the functions Separate_categoric/Separate_numeric]

}
