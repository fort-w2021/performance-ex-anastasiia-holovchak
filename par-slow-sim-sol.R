library(foreach)
library(doParallel)
library(doRNG)

# foreach ganz passend, da analog zu for-Schleifen

parallel_simulate <- function(reps, seed, data,
                              true_coef = 0:ncol(data), df = 4) {
  
  assert_count(reps)
  assert_int(seed)
  assert_data_frame(data)
  assert_numeric(true_coef, len = ncol(data) + 1)
  assert_count(df)
  
  set.seed(seed)
  
  design <- model.matrix(~., data = data)
  expected <- crossprod(t(design), true_coef)
  
  coefs <- foreach(rep = seq_len(reps), .combine = "cbind",
          .export = c("parallel_simulate_once", "parallel_simulate_response", 
                      "parallel_estimate_coef")) %dorng%
    parallel_simulate_once(design, expected, df)
  return(structure(coefs, seed = seed))
}

parallel_simulate_once <- function(design, expected, df) {
  y <- parallel_simulate_response(expected, df)
  parallel_estimate_coef(design, y)
}

parallel_simulate_response <- function(expected, df) {
  return(expected + rt(length(expected), df = df))
}

parallel_estimate_coef <- function(design, y) {
  model <- lm(y ~ -1 + design)
  unname(coef(model))
}

# backend
registerDoParallel(3)

test_par <- parallel_simulate(reps = 100, seed = 20141028, data = testdata)
