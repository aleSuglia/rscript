data_summary <- function(X){
    ## Checks if the parameter is data matrix
    if(is.matrix(X)) {
      # takes the current number of dataset's variables
      num_var <- dim(X)[2];
      num_attr <- 5; # number of attributes that we want to compute
      # generates metrics matrix
      metrics_mat <- matrix(nrow=num_var, ncol=num_attr, 
                            dimnames=list(c(), 
                                          c("mean", "var", "std", "min", "max")));
      
      # Compute metrics
      for (i in c(1:num_var)) {
          metrics_mat[i,1] <- mean(X[,i]);
          metrics_mat[i,2] <- var(X[,i]);
          metrics_mat[i,3] <- sd(X[,i]);
          metrics_mat[i,4] <- min(X[,i]);
          metrics_mat[i,5] <- max(X[,i]);
      }
      
      metrics_mat
    }
}
