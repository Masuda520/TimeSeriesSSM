BlockDiag <- function(list_matrices){
    matrices <- list_matrices
    sizes <- lapply(matrices, function(mat) dim(mat)[1])
    total_size <- sum(unlist(sizes))
    block_diag_matrix <- matrix(0, total_size, total_size)
    current_position <- 1
    for (i in 1:length(matrices)) {
      size <- sizes[[i]]
      block_diag_matrix[current_position:(current_position + size - 1),
                        current_position:(current_position + size - 1)] <- matrices[[i]]
      current_position <- current_position + size
    }

    return(block_diag_matrix)
}
