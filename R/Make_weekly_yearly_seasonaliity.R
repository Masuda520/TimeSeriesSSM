Make_weekly_yearly_seasonality <- function(component_size = 6){
  omega = 365.25/7
  res = list()
  for(j in 1:component_size){
    res[[j]] <- Make_atom_cycle(j = j, omega = omega)
  }
  return(BlockDiag(res))
}
