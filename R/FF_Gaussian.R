FF_Gaussian <- function(Covariate_mat = NA, target_variable_vec, pred_length = NA, M_pred = 1000,
                        data_type, trend_type = "slope", seasonality = TRUE ,
                        component_size_daily_weekly = 3, component_size_daily_yearly = 12,
                        component_size_weekly_yearly = 6,
                        prior_trend = NA , prior_seasonality_sd = NA, prior_covariate_sd_vec = NA,
                        n0 = 10 , s0 = NA){
  y_vec = target_variable_vec
  T = length(y_vec)
  if(is.na(s0)){
    s0 = var(y_vec)
  }
  covariate_flag = all(!is.na(Covariate_mat))
  if(covariate_flag){
    X_mat = as.matrix(Covariate_mat)
    if(dim(X_mat)[2] == 1 & dim(X_mat)[1] > 1){
      X_mat = t(X_mat)
    }

    if(is.na(pred_length)){
      if(dim(X_mat)[2] != T){
        stop("Covariate_mat must be (p,T)-dimensional if pred_length is NA.")
      }
    }else{
      if(dim(X_mat)[2] != T + pred_length){
        stop("Covariate_mat must be (p,T+pred_length)-dimensional if pred_length is not NA.")
      }
    }

    p = dim(X_mat)[1]
  }

  # Make Evolution Matrix ------------
  ## trend
  if(trend_type == "intercept"){
    trend_mat = diag(1)
    dim_trend = 1
    if(is.na(prior_trend)){
      prior_trend_mean_vec = rep(mean(y_vec),dim_trend)
      prior_trend_sd_vec = rep(sd(y_vec),dim_trend)
    }else{
      if(is.list(prior_trend)){

        if(length(prior_trend[[1]]) != dim_trend){
          stop("length(prior_trend[[1]]) must be 1 if trend_type == intercept")
        }else{
          prior_trend_mean_vec = prior_trend[[1]]
        }

        if(length(prior_trend[[2]]) != dim_trend){
          stop("length(prior_trend[[2]]) must be 1 if trend_type == intercept")
        }else{
          prior_trend_sd_vec = prior_trend[[2]]
        }

      }else{
        stop("prior_trend must be a list object")
      }
    }
  }
  if(trend_type == "slope"){
    trend_mat = matrix(c(1,1,0,1) , nrow = 2, ncol = 2, byrow = TRUE)
    dim_trend = 2
    if(is.na(prior_trend)){
      prior_trend_mean_vec = c(mean(y_vec) , 0)
      prior_trend_sd_vec = rep(sd(y_vec),dim_trend)
    }else{
      if(is.list(prior_trend)){

        if(length(prior_trend[[1]]) != dim_trend){
          stop("length(prior_trend[[1]]) must be 2 if trend_type == slope")
        }else{
          prior_trend_mean_vec = prior_trend[[1]]
        }

        if(length(prior_trend[[2]]) != dim_trend){
          stop("length(prior_trend[[2]]) must be 2 if trend_type == slope")
        }else{
          prior_trend_sd_vec = prior_trend[[2]]
        }

      }else{
        stop("prior_trend must be a list object")
      }
    }
  }
  if(trend_type != "intercept" & trend_type != "slope"){
    stop("trend_type must be intercept or slope.")
  }

  ## seasonality
  if(is.na(prior_seasonality_sd)){
    prior_seasonality_sd = sd(y_vec)
  }
  if(data_type == "daily"){
    seasonality_mat_list = list()
    seasonality_mat_list[[1]] = Make_daily_weekly_seasonality(component_size_daily_weekly)
    seasonality_mat_list[[2]] = Make_daily_yearly_seasonality(component_size_daily_yearly)
    seasonality_mat = BlockDiag(seasonality_mat_list)
    dim_seasonality = 2*(component_size_daily_weekly + component_size_daily_yearly)
    delta = 0.999
    beta = 0.999
  }
  if(data_type == "weekly"){
    seasonality_mat = Make_weekly_yearly_seasonality(component_size_weekly_yearly)
    dim_seasonality = 2*component_size_weekly_yearly
    delta = 0.99
    beta = 0.99
  }
  if(data_type != "daily" & data_type != "weekly"){
    stop("data_type must be daily or weekly.")
  }

  ## covariate
  if(covariate_flag){
    if( is.na(prior_covariate_sd_vec) ){
      prior_covariate_sd_vec = rep(mean(y_vec), p)
    }else{
      if(length(prior_covariate_sd_vec)!= p){
        stop("length(prior_covariate_sd_vec) must be dim(Covariate_mat)[1]")
      }
    }
    evol_covariate_mat = diag(p)
  }

  ## make evolution matrix
  if(seasonality == TRUE){
    if(covariate_flag){
      evol_mat = BlockDiag(list(trend_mat , seasonality_mat , evol_covariate_mat))
    }else{
      evol_mat = BlockDiag(list(trend_mat , seasonality_mat))
    }
  }else{
    if(covariate_flag){
      evol_mat = BlockDiag(list(trend_mat , evol_covariate_mat))
    }else{
      evol_mat = BlockDiag(list(trend_mat))
    }
  }
  dim_evol = dim(evol_mat)[1]
  G = evol_mat

  ## make parts of Z matrix --------------------
  if(trend_type == "intercept"){
    Z_trend = c(1)
  }
  if(trend_type == "slope"){
    Z_trend = c(1,0)
  }
  Z_seasonality = rep(c(1,0), dim_seasonality/2)


  ## Forward Filtering -------------------
  m_list = C_list = n_list = s_list = list()
  if(seasonality == TRUE){
    if(covariate_flag){
      m = c(prior_trend_mean_vec , rep(0,dim_seasonality), rep(0,p))
      C = diag(c(prior_trend_sd_vec^2 ,
                 rep(prior_seasonality_sd^2 , dim_seasonality),
                 prior_covariate_sd_vec))
    }else{
      m = c(prior_trend_mean_vec , rep(0,dim_seasonality))
      C = diag(c(prior_trend_sd_vec^2 ,
                 rep(prior_seasonality_sd^2 , dim_seasonality)
      ))
    }
  }else{
    if(covariate_flag){
      m = c(prior_trend_mean_vec, rep(0,p))
      C = diag(c(prior_trend_sd_vec^2 ,
                 prior_covariate_sd_vec))
    }else{
      m = c(prior_trend_mean_vec)
      C = diag(c(prior_trend_sd_vec^2
      ))
    }
  }
  n = n0
  s = s0

  for(t in 1:T){
    ## make Z matrix
    {
      if(seasonality == TRUE){
        if(covariate_flag){
          Z_coef = as.numeric(X_mat[,t])
          Z = c(Z_trend , Z_seasonality , Z_coef)
        }else{
          Z = c(Z_trend , Z_seasonality)
        }
      }else{
        if(covariate_flag){
          Z_coef = as.numeric(X_mat[,t])
          Z = c(Z_trend , Z_coef)
        }else{
          Z = c(Z_trend)
        }
      }
      Z = matrix(Z , nrow = 1)
    }

    ## posterior to prior
    a = G %*% m
    R = G%*%C%*%t(G) / delta
    ## forecasting
    f = as.numeric(Z %*% a)
    q = as.numeric(Z %*% R %*% t(Z) + s)
    e = y_vec[t] - f
    A = R%*%t(Z)/q
    ## prior to posterior
    r = (beta * n + e^2/q)/(beta * n + 1)
    m = m_list[[t]] = a + e * A
    C = C_list[[t]] = r * (R - q*A%*%t(A))
    n = n_list[[t]] = beta * n + 1
    s = s_list[[t]] = r * s
  }
  res = list()
  res[[1]] = m_list
  res[[2]] = C_list
  res[[3]] = n_list
  res[[4]] = s_list
  res[[5]] = delta
  res[[6]] = beta
  res[[7]] = T
  res[[8]] = G
  if(is.na(pred_length)){
    ## make dim list
    dim_list = list()
    dim_list[[1]] = dim_trend
    if(data_type == "daily"){
      if(seasonality == TRUE){
        dim_list[[2]] = 2*component_size_daily_weekly
        dim_list[[3]] = 2*component_size_daily_yearly
      }else{
        dim_list[[2]] = 0
        dim_list[[3]] = 0
      }
      dim_list[[4]] = ifelse(covariate_flag , p , 0)
      names(dim_list) = c("dim_trend", "dim_seasonality_daily_weekly", "dim_seasonality_daily_yearly" ,
                          "dim_covariate")
      res[[9]] = dim_list

      names(res) = c("m","C","n","s","delta","beta","T","G","dim_list")
      return(res)
    }
    if(data_type == "weekly"){
      if(seasonality == TRUE){
        dim_list[[2]] = 2*component_size_weekly_yearly
      }else{
        dim_list[[2]] = 0
      }
      dim_list[[3]] = ifelse(covariate_flag , p , 0)
      names(dim_list) = c("dim_trend", "dim_seasonality_weekly_yearly" ,
                          "dim_covariate")
      res[[9]] = dim_list

      names(res) = c("m","C","n","s","delta","beta","T","G","dim_list")
      return(res)
    }

  }else{
    y_pred_mat = matrix(NA , nrow = M_pred , ncol = pred_length)
    for(i in 1:M_pred){
      m_tmp = m_list[[T]]
      C_tmp = C_list[[T]]
      n_tmp = n_list[[T]]
      s_tmp = s_list[[T]]

      for(t in 1:pred_length){
        ## make Z matrix
        {
          if(seasonality == TRUE){
            if(covariate_flag){
              Z_coef = as.numeric(X_mat[,t+T])
              Z = c(Z_trend , Z_seasonality , Z_coef)
            }else{
              Z = c(Z_trend , Z_seasonality)
            }
          }else{
            if(covariate_flag){
              Z_coef = as.numeric(X_mat[,t+T])
              Z = c(Z_trend , Z_coef)
            }else{
              Z = c(Z_trend)
            }
          }
          Z = matrix(Z , nrow = 1)
        }


        ## posterior to prior
        a = G %*% m_tmp
        R = G%*%C_tmp%*%t(G) / delta
        ## forecasting
        f = as.numeric(Z %*% a)
        q = as.numeric(Z %*% R %*% t(Z) + s_tmp)
        y_pred = y_pred_mat[i,t] = f + sqrt(q) * rt(n = 1 , df = beta * n_tmp)
        e = y_pred - f
        A = R%*%t(Z)/q
        ## prior to posterior
        r = (beta * n_tmp + e^2/q)/(beta * n_tmp + 1)
        m_tmp = a + e * A
        C_tmp = r * (R - q*A%*%t(A))
        n_tmp = beta * n_tmp + 1
        s_tmp = r * s_tmp
      }
    }
    ## make dim_list
    dim_list = list()
    dim_list[[1]] = dim_trend
    if(data_type == "daily"){
      if(seasonality == TRUE){
        dim_list[[2]] = 2*component_size_daily_weekly
        dim_list[[3]] = 2*component_size_daily_yearly
      }else{
        dim_list[[2]] = 0
        dim_list[[3]] = 0
      }
      dim_list[[4]] = ifelse(covariate_flag , p , 0)
      names(dim_list) = c("dim_trend", "dim_seasonality_daily_weekly", "dim_seasonality_daily_yearly" ,
                          "dim_covariate")
      res[[9]] = dim_list
    }
    if(data_type == "weekly"){
      if(seasonality == TRUE){
        dim_list[[2]] = 2*component_size_weekly_yearly
      }else{
        dim_list[[2]] = 0
      }
      dim_list[[3]] = ifelse(covariate_flag , p , 0)
      names(dim_list) = c("dim_trend", "dim_seasonality_weekly_yearly" ,
                          "dim_covariate")
      res[[9]] = dim_list
    }
    res[[10]] = y_pred_mat
    names(res) = c("m","C","n","s","delta","beta","T","G","dim_list","y_pred_mat")
    return(res)
  }
}





