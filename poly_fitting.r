library(polynom)
library(cvTools)

## generate_data
# nobs - number of observations generated by the function
# foo - function used to generate the output data
# min - lower bound of the interval from which data will be
# generated
# max - upper bound of the interval from which input data will
# be generated
generate_data <- function(nobs, foo, min=0, max=10) {
  
  data <- matrix(ncol=2, nrow=nobs)
  
  for(i in c(1:nobs)) {
    data[i,1] <- runif(n=1, min=0, max=max)
    data[i,2] <- foo(data[i,1])
  }
  
  data.frame(X=data[,1], Y=data[,2])
  
}

## generate a polynomial of a specified degree
# degree - desidered degree

poly_gen <- function(degree) {
  
  f <- "Y ~ "
  
  for (d in 1:degree) {
    
    if (d == 1) {
      f <- paste0(f, paste0("X"))
    } else {
      f <- paste0(f, paste0(" + ", "I(X^", d, ")"))
    }
  }
  
  f
}

# Computes cross-validation in order to generate a polynomial
# which estimates the real model, in the sense of least mean squares
# data - the dataset that will be used 
# K - fold to be used in cross-validation
# maxdegree - maximum polynomial's degree that will be tested
# seed - the seed used to run cross-validation
pf_evaluation <- function(data, K, maxdegree=6, seed=1234) {
  results <- c()
  results$reslist <- array(dim=maxdegree)
  results$fitmodels <- array(dim=maxdegree)
  
  for(deg in c(1:maxdegree)) {
    currpoly <- poly_gen(deg)
    fit <- lm(formula=currpoly, data=data)
    results$fitmodels <- c(results$fitmodels, fit)
    cvres <- cvFit(fit, data=data, y=data$Y, K=K, R=K, seed=seed)
    results$reslist[deg] = cvres$cv
  }
  
  results
}
