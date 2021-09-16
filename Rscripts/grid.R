

create_grid <- function(cote1, cote2, n_bombes) {
  
  # Set the number of bombs to the maximum if it exceeds the maximum value
  if (n_bombes > cote1 * cote2) n_bombes <- cote1 * cote2
  
  indexs <- matrix(seq_len(cote1 * cote2), cote1, cote2)
  # No seed to be a little more random for the bombs, but it might be better to control that in the future
  bool_bombes <- seq_len(cote1 * cote2) %in% sample(seq_len(cote1 * cote2), n_bombes)
  vec_bombes <- rep("", cote1 * cote2)
  vec_bombes[bool_bombes] <- "X" 
  matrice <- matrix(vec_bombes, cote1, cote2)
  
  return(list(matrice = matrice,
              indexs = indexs, 
              vec_bombes = vec_bombes))
  
}


compte_voisins <- function(vec_bombes, cote1, cote2) {
  
  if (length(vec_bombes) != cote1 * cote2) stop(paste0("\"vec_bombes\" should be of length ", cote1 * cote2, "."), call. = FALSE) 
  
  # Get the position in the matrix of the cell
  lignes <- ifelse(seq_along(vec_bombes) %% cote1 == 0, cote1, seq_along(vec_bombes) %% cote1)
  colonnes <- ifelse(seq_along(vec_bombes) %% cote1 == 0, seq_along(vec_bombes) %/% cote1, seq_along(vec_bombes) %/% cote1 + 1)
  
  # Count the bombs in neighbourhood
  matrice <- matrix(seq_along(vec_bombes), cote1, cote2)
  compte_voisins <- vapply(seq_along(vec_bombes), 
                           function(x) {
                             index_voisins <- data.frame(position = c("Nord", "NordEst", "Est", "SudEst", "Sud", "SudOuest", "Ouest", "NordOuest"),
                                                         ligne = c(lignes[x] - 1, lignes[x] - 1, lignes[x], lignes[x] + 1, lignes[x] + 1, lignes[x] + 1, lignes[x], lignes[x] - 1),
                                                         colonne = c(colonnes[x], colonnes[x] + 1, colonnes[x] + 1, colonnes[x] + 1, colonnes[x], colonnes[x] - 1, colonnes[x] - 1, colonnes[x] - 1))
                             index_voisins <- subset(index_voisins, index_voisins$ligne > 0 & index_voisins$ligne <= cote1 & index_voisins$colonne > 0 & index_voisins$colonne <= cote2)
                             index_voisins <- matrice[as.matrix(index_voisins[, -1])]
                             sum(vec_bombes[index_voisins] == "X")
                           },
                           integer(1))
  compte_voisins[vec_bombes == "X"] <- -99
  compte_voisins <- matrix(compte_voisins, cote1, cote2)
  
  return(compte_voisins)
    
}


transform_mat_tab <- function(mat_bombes, img = "images/boomboom.png", vide = "images/R.png") {
  
  {mat_bombes[, ] == "X"} %>% 
    as.data.frame() %>% 
    `colnames<-`(seq_len(ncol(.))) %>% 
    `rownames<-`(seq_len(nrow(.))) %>% 
    rownames_to_column() %>% 
    pivot_longer(cols = -1) %>% 
    mutate(across(c(rowname, name), ~ as.numeric(.x))) %>% 
    mutate(value = ifelse(value, img, vide))
  
}
