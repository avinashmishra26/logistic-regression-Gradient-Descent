logistic_regression_fit_gd5 = function(X, y, initial_weight= -.7,end_weight= .7){
  
  no_of_features = ncol(X)
  W = runif(no_of_features, initial_weight, end_weight)  
  #W = rep(0.5, no_of_features)
  bias = rep(0.5, 1)
  n = 10
  
  new_W = W
  
  W_change = c()
  i=1
  
  while(TRUE){
    n = n/10

    new_W = gradient_descent(X,y,W,bias,n)
    difference_in_W = abs(sum(new_W - W)/no_of_features)
    
    W_change[i] = difference_in_W
    
    if(length(W_change) > 10 && (W_change[i]- W_change[i-10]) < 0.01) {
        break
    }
    i = i +1
    W = new_W
  }
  return(new_W)
}