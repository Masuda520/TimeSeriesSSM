Make_atom_cycle <- function(j , omega){
  theta = 2*pi*j/omega
  return(
    matrix(c(cos(theta), sin(theta),
             -sin(theta),cos(theta)) ,
           nrow = 2,
           ncol = 2,
           byrow = TRUE)
  )
}
