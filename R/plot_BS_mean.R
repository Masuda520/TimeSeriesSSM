plot_BS_mean <- function(res_extract_BS , data_type , xlim_weekly = NA ,
                         plot_type , coef_nth = NA){
  if(data_type != "daily" & data_type != "weekly"){
    stop("data_type must be daily or weekly.")
  }
  if(plot_type != "trend" & plot_type != "seasonality" & plot_type != "coef"){
    stop("plot_type must be trend, seasonality, or coef.")
  }

  if(plot_type == "trend"){
    plot(colMeans(res_extract_BS$trend_mat) , type = "l" ,xlab=NA,main="trend",ylab=NA)
  }
  if(plot_type == "seasonality"){
    if(data_type == "daily"){
      if(any(is.na(xlim_weekly))){
        plot(colMeans(post_list$weekly_cycle_mat), type = "l",xlab=NA,main="weekly_cycle",ylab=NA)
      }else{
        plot(colMeans(post_list$weekly_cycle_mat), type = "l",xlab=NA,main="weekly_cycle",ylab=NA,
             xlim = xlim_weekly)
      }
      plot(colMeans(post_list$yearly_cycle_mat), type = "l",xlab=NA,main="yearly_cycle",ylab=NA)
    }
    if(data_type == "weekly"){
      plot(colMeans(post_list$yearly_cycle_mat), type = "l",xlab=NA,main = "yearly_cycle",ylab=NA)
    }
  }
  if(plot_type == "coef"){
    if(is.na(coef_nth)){
      stop("select which coef is shown.")
    }else{
      plot(colMeans(post_list$coef_mat_list[[coef_nth]]), type = "l",xlab=NA,ylab=NA,
           main = paste0("coef, ",coef_nth))
    }
  }
}
