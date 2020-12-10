library(checkmate)

#' Simulates the coefficients of a linear model where error terms are t-distributed
#' 
#' @param reps number of repetitions of simulation
#' @param seed initial seed
#' @param data initial data.frame
#' @param true_coef vector of true coefficients
#' @param df degrees of freedom (parameter of t-distribution)

#' @return (n x reps)-matrix of the estimated coefficients (for each repetition)

simulate <- function(reps, seed, data, true_coef = 0:ncol(data), df = 4) {
  # input check
  assert_count(reps)
  assert_int(seed)
  assert_data_frame(data)
  assert_numeric(true_coef, len = ncol(data) + 1)
  assert_count(df)

  set.seed(seed)
  
  # create an 'empty' matrix of the correct size 
  coefs <- matrix(nrow = length(true_coef), ncol = reps)
  
  # design matrix and matrix multiplication should be computed only once
  design <- model.matrix(~., data = data)
  expected <- crossprod(t(design), true_coef) # faster than %*% due to documentation
  
  # for each iteration
  for (rep in seq_len(reps)) {
    # estimate regression coefficients
    coefs[, rep] <- simulate_once(design, expected, df)
  }
  return(structure(coefs, seed = seed))
}

# simulates estimated coefficients once
simulate_once <- function(design, expected, df) {
  y <- simulate_response(expected, df) 
  estimate_coef(design, y)
}

# simulates response vector of a linear model Y = X*beta + eps, where eps~t(4)
simulate_response <- function(expected, df) {
  return(expected + rt(length(expected), df = df)) # not necessary to store the values
}

# extimates coefficients of a linear model
estimate_coef <- function(y, design) {
  model <- lm(y ~ -1 + design)
  unname(coef(model))
}

