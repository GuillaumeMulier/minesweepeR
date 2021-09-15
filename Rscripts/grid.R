

create_grid <- function(cote, n_bombes) {
  
  # Set the number of bombs to the maximum if it exceeds the maximum value
  if (n_bombes > cote ^ 2) n_bombes <- cote ^ 2
  
  indexs <- matrix(seq_len(cote ^ 2), cote, cote)
  # No seed to be a little more random for the bombs, but it might be better to control that in the future
  bool_bombes <- seq_len(cote ^ 2) %in% sample(seq_len(cote ^ 2), n_bombes)
  vec_bombes <- rep("", cote ^ 2)
  vec_bombes[bool_bombes] <- "X" 
  matrice <- matrix(vec_bombes, cote, cote)
  
  vec_bombes
  
  return(list(matrice = matrice,
              indexs = indexs))
  
}

compte_voisins <- function(vec_bombes, cote) {
  
  if (length(vec_bombes) != cote ^ 2) stop(paste0("\"vec_bombes\" should be of length", cote ^ 2, "."), call. = FALSE) 
  
  # Get the position in the matrix of the cell
  lignes <- ifelse(seq_along(vec_bombes) %% cote == 0, cote, seq_along(vec_bombes) %% cote)
  colonnes <- ifelse(seq_along(vec_bombes) %% cote == 0, seq_along(vec_bombes) %/% cote, seq_along(vec_bombes) %/% cote + 1)
  
  # Count the bombs in neighbourhood
  
  for (i in seq_along(vec_bombes)) {
    
    if (lignes[i] == 1 & colonnes[i] == 1) {
      
    }
    
  }
    
}
