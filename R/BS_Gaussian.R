BS_Gaussian <- function(res_FF_Gaussian , M_BS = 1000, print_BS = TRUE){
  T = res_FF_Gaussian$T
  delta = res_FF_Gaussian$delta
  beta = res_FF_Gaussian$beta
  G = res_FF_Gaussian$G
  invG = solve(G)
  dim_list = res_FF_Gaussian$dim_list

  m_list = res_FF_Gaussian$m
  C_list = res_FF_Gaussian$C
  n_list = res_FF_Gaussian$n
  s_list = res_FF_Gaussian$s

  BS_list = list()
  dim_theta = length(m_list[[1]])

  for(i in 1:M_BS){
    BS_mat = matrix(NA , nrow = dim_theta , ncol = T)
    var_tmp = 1/rgamma(n = 1 , shape = n_list[[T]]/2 , rate = n_list[[T]]*s_list[[T]]/2)
    theta = BS_mat[,T] = mvrnorm(n = 1 , mu = as.numeric(m_list[[T]]) , Sigma = C_list[[T]] * var_tmp/s_list[[T]] )

    for(t in (T-1):1){
      var_tmp = 1/( beta /var_tmp + rgamma(n = 1, shape = (1 - beta)*n_list[[t]]/2, rate = n_list[[t]]*s_list[[t]]/2)  )
      mu_tmp = as.numeric( m_list[[t]] + delta * (invG %*% theta - m_list[[t]]))
      Sigma_tmp = C_list[[t]] * (1 - delta) * var_tmp / s_list[[t]]
      theta =  BS_mat[,t] = mvrnorm(n = 1 , mu = mu_tmp , Sigma = Sigma_tmp)
    }

    BS_list[[i]] = BS_mat
    if(print_BS == TRUE){
      print(paste0(i , " / ", M_BS))
    }
  }
  res_list = list()
  res_list[[1]] = BS_list
  res_list[[2]] = dim_list
  names(res_list) = c("BS_list", "dim_list")
  return(res_list)
}
