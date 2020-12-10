library(profvis)

source("slow-sim-sol.R")
source("par-slow-sim-sol.R")

set.seed <- 232323
observations <- 5000
covariates <- 10
testdata <- as.data.frame(
  matrix(rnorm(observations * covariates),
         nrow = observations
  ))

test <- simulate(reps = 100, seed = 20141028, data = testdata)
test_par <- parallel_simulate(reps = 100, seed = 20141028, data = testdata)

system.time(test <- simulate(reps = 100, seed = 20141028, data = testdata))
system.time(test_par <- parallel_simulate(reps = 100, seed = 20141028, data = testdata))

# a) Stellen, die meiste Zeit beanspruchen, identifizieren

# Wo wird meiste Zeit verbracht

profvis::profvis(
  test <- simulate(reps = 1000, seed = 20141028, data = testdata)
)

# line 6: coefs <- cbind(coefs, simulate_once(data, true_coef, df))
# cbind -> neue Spalte wird 'rangeklebt'
# line 12: data <- simulate_response(data, true_coef, df)
# model matrix + matrix multiplication
# line 13: estimate_coef(data)
# lm wird gefittet
# line 24: model <- lm(y ~ ., data = data)


