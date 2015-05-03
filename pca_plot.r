pca_plot <- function(full_data, class_col) {
    if (class_col > 0) {
    # split the dataset and get class value column
    class_values <- full_data[,class_col]
    data <- full_data[,1:class_col-1] 
    
    pca_res <- prcomp(data)
    
    # get transformed data into the new subspace
    # formed by the principal components
    # take only the first two components
    # that will be plotted
    trasf_data <- matrix(unlist(pca_res$x[,1:2]), ncol=2, nrow=nrow(data))
    
    classval <- factor(class_values)
    # get a color for each class value
    values <- setdiff(class_values, class_values)
    colors <- rainbow(length(values))
    
    plot(trasf_data, col=classval)
  }
}