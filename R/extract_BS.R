extract_BS <- function(res_BS_Gaussian, data_type){
  if(data_type != "daily" & data_type != "weekly"){
    stop("data_type must be daily or weekly.")
  }

  BS_list = res_BS_Gaussian$BS_list
  dim_list = res_BS_Gaussian$dim_list
  M_BS = length(BS_list)
  T = dim(BS_list[[1]])[2]
  dim_trend = dim_list$dim_trend
  dim_covariate = dim_list$dim_covariate

  ## extract trend component
  trend_mat = matrix(NA , nrow = M_BS , ncol = T)
  for(i in 1:M_BS){
    for(t in 1:T){
      trend_mat[i,t] = BS_list[[i]][1,t]
    }
  }

  ## extract seasonality component
  if(data_type == "daily"){
    ## weekly cycle
    weekly_cycle_mat = matrix(NA , nrow = M_BS , ncol = T)
    dim_seasonality_daily_weekly = dim_list$dim_seasonality_daily_weekly
    if(dim_seasonality_daily_weekly != 0){
      dim_tmp = dim_trend + seq(1 , dim_seasonality_daily_weekly -1 , 2)
      for(i in 1:M_BS){
        for(t in 1:T){
          weekly_cycle_mat[i,t] = sum(BS_list[[i]][dim_tmp,t])
        }
      }
    }else{
      weekly_cycle_mat = NA
    }

    ## yearly cycle
    yearly_cycle_mat = matrix(NA , nrow = M_BS , ncol = T)
    dim_seasonality_daily_yearly = dim_list$dim_seasonality_daily_yearly
    if(dim_seasonality_daily_yearly != 0){
      dim_tmp = dim_trend + dim_seasonality_daily_weekly + seq(1 , dim_seasonality_daily_yearly -1 , 2)
      for(i in 1:M_BS){
        for(t in 1:T){
          yearly_cycle_mat[i,t] = sum(BS_list[[i]][dim_tmp,t])
        }
      }
    }else{
      yearly_cycle_mat = NA
    }
    dim_now = dim_trend + dim_seasonality_daily_weekly + dim_seasonality_daily_yearly
  }
  if(data_type == "weekly"){
    ## yearly cycle
    yearly_cycle_mat = matrix(NA , nrow = M_BS , ncol = T)
    dim_seasonality_weekly_yearly = dim_list$dim_seasonality_weekly_yearly
    if(dim_seasonality_weekly_yearly != 0){
      dim_tmp = dim_trend + seq(1 , dim_seasonality_weekly_yearly -1 , 2)
      for(i in 1:M_BS){
        for(t in 1:T){
          yearly_cycle_mat[i,t] = sum(BS_list[[i]][dim_tmp,t])
        }
      }
    }else{
      yearly_cycle_mat = NA
    }

    dim_now = dim_trend + dim_seasonality_weekly_yearly
  }

  ## extract coef component
  if(dim_covariate != 0){
    coef_mat_list = list()
    for(d in 1:dim_covariate){
      coef_mat = matrix(NA , nrow = M_BS , ncol = T)
      for(i in 1:M_BS){
        for(t in 1:T){
          coef_mat[i,t] = BS_list[[i]][dim_now + d ,t]
        }
      }
      coef_mat_list[[d]] = coef_mat
    }
  }else{
    coef_mat_list = NA
  }

  if(data_type == "daily"){
    res = list()
    res[[1]] = trend_mat
    res[[2]] = weekly_cycle_mat
    res[[3]] = yearly_cycle_mat
    res[[4]] = coef_mat_list
    names(res) = c("trend_mat" , "weekly_cycle_mat", "yearly_cycle_mat", "coef_mat_list")
  }
  if(data_type == "weekly"){
    res = list()
    res[[1]] = trend_mat
    res[[2]] = yearly_cycle_mat
    res[[3]] = coef_mat_list
    names(res) = c("trend_mat", "yearly_cycle_mat", "coef_mat_list")
  }
  return(res)
}




