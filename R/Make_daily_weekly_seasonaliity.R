Make_daily_weekly_seasonality <- function(component_size = 3){
  omega = 7
  res = list()
  for(j in 1:component_size){
    res[[j]] <- Make_atom_cycle(j = j, omega = omega)
  }
  return(BlockDiag(res))
}
