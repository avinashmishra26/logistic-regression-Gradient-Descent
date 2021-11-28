logistic_regression_fit_gd6 = function(X, y, n=.01, epoch = 1000, initial_weight= -.7,end_weight= .7){
  
  no_of_features = ncol(X)
  W = runif(no_of_features, initial_weight, end_weight)  
  bias = rep(0.5, 1)
  
  new_W = W
  
  for(itr in seq(1,epoch)){

    new_W = gradient_descent(X,y,W,bias,n)
    
    W = new_W
  }
  return(new_W)
}

repeat_gd6_plot = function(X_train, y_train, x_test, y_test){
  
  mse_values = c()
  
  for(i in seq(1,100)){
    W_coefficients = logistic_regression_fit_gd6(x_train, y_train)
    #Test MSE
    y_pred = logistic_regression_predict(x_test,W_coefficients)
    mse_values[i] = get_mse_result(y_pred,y_test)
  }
  boxplot(mse_values,range = 0)
}