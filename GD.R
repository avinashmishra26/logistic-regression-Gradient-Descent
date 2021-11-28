sigmoid = function(z) {
  return (1/(1+exp(-z)))
}

gradient_descent = function(X, y, W, bias, n){
  no_of_datapoints = nrow(X)
  #print(no_of_datapoints)
  W_dotProd_X =  W %*% t(X)
  #print(W_dotProd_X)
  
  Z = W_dotProd_X + replicate(no_of_datapoints,bias)
  
  #print(Z)
  
  Z1 = sigmoid(Z)
  
  #print(dim(Z1))
  #print(dim(y))
  y_hat = Z1 - t(y)
  
  #print(y_hat)
  
  #print(Z1[1:1250])
  #print(dim(X))
  
  diff_res = as.matrix(y_hat) %*% as.matrix(X)
  
  mean_diff = diff_res/no_of_datapoints
  new_W = W - n*mean_diff
  return(new_W)
  
}

logistic_regression_fit = function(X, y, n=10, epoch = 1000, threshold=10**(-10), initial_weight= -.7,end_weight= .7){
  
  no_of_features = ncol(X)
  W = runif(no_of_features, initial_weight, end_weight)  
  #W = rep(0.5, no_of_features)
  bias = rep(0.5, 1)
  
  new_W = W
  
  for(itr in seq(1,epoch)){
    n = n/10**itr
    new_W = gradient_descent(X,y,W,bias,n)
    difference_in_W = abs(sum(new_W - W)/no_of_features)
    
    if(difference_in_W < threshold) {
      #print("converged")
      #break
    }
    W = new_W
  }
  return(new_W)
}

# Predicting 1 or 0 with sigmoid function
logistic_regression_predict = function(X, W) {
  z =  W %*% t(X)
  Z1 = sigmoid(z)
  predict_vec = c() ## an empty vector to collect the result
  #print(Z1[1,])
  
  for (i in 1:length(Z1[1,])) { 
      if(Z1[1,i] >= 0.5)
        predict_vec[i] = 1
      else
        predict_vec[i] = 0
  }
  predict_vec
}

get_mse_result = function(predicted_value, actual_value){
  
  predicted_value = as.vector(predicted_value)
  predicted_value_count = length(predicted_value)
  actual_value = as.vector(actual_value)
  actual_value_count = length(actual_value)
  
 
  total_mse = 0
  
  if( predicted_value_count == actual_value_count) {
    for (i in 1:predicted_value_count) {
      total_mse = total_mse + (actual_value[i] - predicted_value[i])**2
    }
    total_mse = total_mse/predicted_value_count
    return(total_mse)
  }
  return(0)
}