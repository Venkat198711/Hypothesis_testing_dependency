

#' @title Hypothesis testing - Step 1
#'
#' @description Separate the categorical variables
#'
#' @param data
#'
#' @return NULL
#'
#' @examples Separate_categoric(cars)
#'
#' @export Separate_categoric
#'

# Separating the categorical data from the user input dataset

Separate_categoric <- function(data)

{
  category <- c()


  # Creating a dataframe of categorical datas only from the input dataset

  for(var in 1:ncol(data))
  {

    if((is.factor(data[, var]) |
        (length(unique(data[,var]))/nrow(data)*100 < 5)))

    {

      category[var] <- var

    }
  }

  category = category[!is.na(category)]
  df1 = data.frame((data[,category]))

  return(df1)

}
