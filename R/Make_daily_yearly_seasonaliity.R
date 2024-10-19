Make_daily_yearly_seasonality <- function(component_size = 12){
  omega = 365.25
  res = list()
  for(j in 1:component_size){
    res[[j]] <- Make_atom_cycle(j = j, omega = omega)
  }
  return(BlockDiag(res))
}
