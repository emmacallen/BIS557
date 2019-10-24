#' Cross-Validation for Ridge Regression
#'
#'@description methodology to find optimal lambdas using a given data set with ridge regression
#'
#' @param formula formula "y ~ ."
#' @param data a data frame
#' @param folds the number of folds used for cross-validation
#' @param lambdas a vector of lambdas
#' @param contrasts
#'
#' @return a tibble containing a summary of the statistics
#' @importFrom stats sd model.matrix predict var
#' @importFrom rsample vfold_cv testing training
#' @importFrom foreach %dopar% %do%
#' @importFrom magrittr %>%
#' @import casl dplyr ggplot2
#' @export
cross_validation <- function(formula, data, folds= 5, lambdas= seq(0, 0.5, 0.01), contrasts= NULL){
  # R CMD check
  i <- lambda <- `.` <- lower <- upper <- NULL

  # create folds
  folds <- vfold_cv(data,folds)

  # nested loop to find root mean squared error for each lambda for each fold

  rmses<- foreach(lambda = lambdas, .combine = rbind) %dopar% {
   foreach(i = seq_len(nrow(folds)), .combine = c) %do% {
    casl_util_rmse(
      testing(folds$splits[[i]])[[as.character(formula[2])]],
                   predict(ridge_regression(formula, training(folds$splits[[i]]),
                                            lambda = lambda, contrasts= contrasts),
                           testing(folds$splits[[i]])))
  }
}

  # tibble of results
  results <- tibble(mean =apply(rmse, 1, mean),
                    sd = apply(rmse, 1, sd),
                    lambda = lambdas) %>%
    mutate(upper = mean + 2 * sd / nrow(.),
           lower = mean - 2 * sd / nrow(.))

   ggplot(results, aes(x = lambdas, y = mean, ymin = lower, ymax = upper)) +
    geom_errorbar() +
    theme_minimal() +
    geom_point(aes(color = "red")) +
    ylab("Root Mean Square Error") +
    xlab(expression(lambda))

    # lambda min
  lambda_min <- results$lambda[which.min(results$mean)]


}

#should be extracting lamba values from ridge regression
#fvfold_cv creates implicit training and testing data sets
