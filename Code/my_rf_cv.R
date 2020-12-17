#' Random Forest Cross-Validation
#'
#' This function predicts \code{"body_mass_g"} in penguins data using
#' covariates \code{"bill_length_mm"}, \code{"bill_depth_mm"},
#' and \code{"flipper_length_mm"} and computes the cross-validation error.
#'
#' @param k number of folds.
#' @keywords prediction
#' @return a numeric with the cross-validation error
#' @examples
#' my_rf_cv(5)
#' @export
my_rf_cv <- function(k) {
  penguins <- na.omit(my_penguins)
  n <- nrow(penguins)
  fold <- sample(rep(1:k, length = n))

  penguins <- penguins %>%
    dplyr::select(body_mass_g, bill_length_mm, bill_depth_mm, flipper_length_mm)
  penguins$split <- fold

  est <- rep(NA, n)
  for (i in 1:k) {
    data_train <- penguins %>% dplyr::filter(split != i)
    data_test <- penguins %>% dplyr::filter(split == i)
    model <- randomForest::randomForest(body_mass_g ~ bill_length_mm + bill_depth_mm +
                            flipper_length_mm, data = data_train,
                          ntree = 100)
    est[fold == i] = predict(model, data_test[, -1])
  }
  # Compute MSE
  body_mass <- penguins$body_mass_g
  sum <- 0
  for (j in 1:length(body_mass)) {
    sum <- sum + (est[j] - body_mass[j]) ^ 2
  }
  mse <- sum / length(body_mass)

  return(mse)
}
