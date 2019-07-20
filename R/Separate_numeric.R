
#' @title Hypothesis testing - Step 2
#'
#' @description Separate the numeric variables
#'
#' @param data
#'
#' @return NULL
#'
#' @examples Separate_numeric(cars)
#'
#' @export Separate_numeric


Separate_numeric <- function(data)

{


  number <- c()

  # Creating a dataframe of numeric datas alone from the input dataset

  for(var1 in 1:ncol(data))
  {

    if((is.numeric(data[, var1]) &
        !(length(unique(data[,var1]))/nrow(data)*100 < 5)))

    {

      number[var1] <- var1

    }

  }

  number = number[!is.na(number)]

  df2 = data.frame((data[,number]))

  return(df2)

}
