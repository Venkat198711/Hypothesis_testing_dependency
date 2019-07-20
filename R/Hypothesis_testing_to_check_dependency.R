
#' @title Hypothesis for checking dependency
#'
#' @description Summary of p-values for chi-square/t-test to check dependency based on target variable type (Categoric/Numeric)
#'
#' @param data type target_variable, target_variable_index, conf_interval
#'
#' @return NULL
#'
#' @examples Hypothesis_testing_to_check_dependency(cars, type = "category" or "numeric", target_variable = "userinput depends on type parameter", categoric_function(Separate_categoric), numeric_function(Separate_numeric), target_variable_index(index of target variable returned by the function Separate_categoric/Separate_numeric), conf_interval = "0.95") [User is advised to run first "Separate_categoric/Separte_numeric function" to know the index of target variable][parameter data should be same as for the functions Separate_categoric/Separate_numeric]
#'
#' @export Hypothesis_testing_to_check_dependency


Hypothesis_testing_to_check_dependency <- function(data, type, target_variable, categoric_function, numeric_function, target_variable_index,
                                                   conf_interval)

{

  variable_name_1 <- c()

  variable_name_2 <- c()

  chisq_pvalue <- c()

  ttest_pvalue <- c()

  # If the user-input is categorical variable, the below function will
  # perform chi-square test and return the p-values for rest of the categorical variables as a dataframe

  if(type == "category")

  {
    target_variable_cat <- categoric_function[, target_variable_index]

    predictors_cat <- categoric_function[, -target_variable_index]

    predictors_cat <-data.frame(predictors_cat)

    for(var5 in 1:ncol(predictors_cat))

    {

      variable_name_1[var5] <- c(names(predictors_cat[var5]))

      tbl1 <- table(predictors_cat[, var5], target_variable_cat)

      chisq_pvalue[var5] <- chisq.test(tbl1)$p.value

    }


    df3 <- data.frame(variable_name_1, chisq_pvalue)

    names(df3)[names(df3) == 'variable_name_1'] <- "Variable name"

  }


  # If the user-input is numeric variable, the below function will perform t-test

  # and return the p-values for rest of the numeric variables as a dataframe

  if(type == "numeric")

  {

    target_variable_num <- numeric_function[, target_variable_index]

    predictors_num <- numeric_function[, -target_variable_index]

    for(var6 in 1:ncol(predictors_num))

    {

      variable_name_2[var6] <- c(names(predictors_num[var6]))

      ttest_pvalue[var6] <- t.test(predictors_num[,var6], y = NULL,
                                   alternative = c("two.sided", "less", "greater"),
                                   mu = 0, paired = FALSE, var.equal = FALSE,
                                   conf.level = conf_interval)$p.value

    }

    df4 <- data.frame(variable_name_2, ttest_pvalue)

    names(df4)[names(df4) == 'variable_name_2'] <- "Variable name"

  }
}
