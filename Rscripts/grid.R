

# Create the grid with the bombs
create_grid <- function(cote1, cote2, n_bombes) {
  
  # Set the number of bombs to the maximum if it exceeds the maximum value
  if (n_bombes > cote1 * cote2) n_bombes <- cote1 * cote2
  
  indexs <- matrix(seq_len(cote1 * cote2), cote1, cote2)
  # No seed to be a little more random for the bombs, but it might be better to control that in the future
  bool_bombes <- seq_len(cote1 * cote2) %in% sample(seq_len(cote1 * cote2), n_bombes)
  vec_bombes <- rep("", cote1 * cote2)
  vec_bombes[bool_bombes] <- "X" 
  matrice <- matrix(vec_bombes, cote1, cote2)
  
  return(matrice = matrice)
  
}


# Return the matrix of the count of bombs in the neighbourhood of each field
compte_voisins <- function(vec_bombes, cote1, cote2) {
  
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


tableau_compte_init <- function(mat_bombes, mat_comptes, img = "images/boomboom.png", vide = "images/R.png") {
  
  # Transform the matrix of bombs into a plotable data.frame
  df_bombes <- mat_bombes %>% 
    as.data.frame() %>% 
    `colnames<-`(seq_len(ncol(.))) %>% 
    `rownames<-`(seq_len(nrow(.))) %>% 
    rownames_to_column(var = "ordonnee") %>% 
    pivot_longer(cols = -1, names_to = "abscisse", values_to = "bombes") %>% 
    mutate(across(c(abscisse, ordonnee), ~ as.numeric(.x))) %>% 
    mutate(image = ifelse(bombes == "X", img, vide),
           drapeau = FALSE)
  
  # Transform the matrix fo counts for bombs into a plotable data.frame
  df_comptes <- mat_comptes %>% 
    as.data.frame() %>% 
    `colnames<-`(seq_len(ncol(.))) %>% 
    `rownames<-`(seq_len(nrow(.))) %>% 
    rownames_to_column(var = "ordonnee") %>% 
    pivot_longer(cols = -1, names_to = "abscisse", values_to = "decompte_bombes") %>% 
    mutate(across(c(abscisse, ordonnee), ~ as.numeric(.x))) %>% 
    mutate(affich = FALSE,
           remplissage = "#E6D37F")
  
  # Transform the index matrix into the same data.frame structure
  df_indexs <- matrix(seq_along(mat_bombes), nrow = dim(mat_bombes)[1], ncol = dim(mat_bombes)[2]) %>% 
    as.data.frame() %>% 
    `colnames<-`(seq_len(ncol(.))) %>% 
    `rownames<-`(seq_len(nrow(.))) %>% 
    rownames_to_column(var = "ordonnee") %>% 
    pivot_longer(cols = -1, names_to = "abscisse", values_to = "index_field") %>% 
    mutate(across(c(abscisse, ordonnee), ~ as.numeric(.x)))
  
  # Get all the tables together into the plot data
  df_plot <- df_bombes %>% 
    left_join(df_comptes, by = c("ordonnee", "abscisse")) %>% 
    left_join(df_indexs, by = c("ordonnee", "abscisse")) %>% 
    left_join(lookup_text, by = "decompte_bombes") %>% 
    mutate(image_affich = ifelse(affich, image, vide),
           couleur_affich = ifelse(affich, couleur, "transparent"))
  
  return(df_plot)
  
} 


# Reveal the fields selected (with flood fill for 0s)
remplir_lac <- function(mat_comptes_voisins, abscisse, ordonnee) {
  
  nb_lignes <- nrow(mat_comptes_voisins)
  nb_colonnes <- ncol(mat_comptes_voisins)
  mat_indexs <- matrix(seq_along(mat_comptes_voisins), nrow = dim(mat_comptes_voisins)[1], ncol = dim(mat_comptes_voisins)[2])
  
  # Get the 4 neighbours of the interest field
  voisins <- function(abscisse, ordonnee) {
    matrice <- matrix(c(ordonnee, abscisse - 1, ordonnee + 1, abscisse, ordonnee, abscisse + 1, ordonnee - 1, abscisse), 
                      ncol = 2, byrow = TRUE)
    matrice <- matrice[matrice[, 1] > 0 & matrice[, 2] > 0, , drop = FALSE] # Remove out of bounds (line and columns of 0 or less)
    matrice <- matrice[matrice[, 1] <= nb_lignes & matrice[, 2] <= nb_colonnes, , drop = FALSE] # Remove out of bounds (line and columns of more than mat_comptes_voisins)
    return(matrice)
  }
  
  # Get the 4 diagonal neighbours of the interest field
  voisins_diago <- function(abscisse, ordonnee) {
    matrice <- matrix(c(ordonnee + 1, abscisse - 1, ordonnee + 1, abscisse + 1, ordonnee - 1, abscisse + 1, ordonnee - 1, abscisse - 1),
                      ncol = 2, byrow = TRUE)
    matrice <- matrice[matrice[, 1] > 0 & matrice[, 2] > 0, , drop = FALSE] # Remove out of bounds (line and columns of 0 or less)
    matrice <- matrice[matrice[, 1] <= nb_lignes & matrice[, 2] <= nb_colonnes, , drop = FALSE] # Remove out of bounds (line and columns of more than mat_comptes_voisins)
    return(matrice)
  }
  
  if (mat_comptes_voisins[ordonnee, abscisse] != 0) {
    revealed_fields <- mat_indexs[ordonnee, abscisse]
  } else {
    ToTest <- mat_indexs[voisins(abscisse, ordonnee)]
    Tested <- NULL
    diago_case <- mat_indexs[voisins_diago(abscisse, ordonnee)]
    revealed_fields <- mat_indexs[ordonnee, abscisse]
    while(length(ToTest) > 0) {
      EnTest <- ToTest[1]
      Tested <- c(Tested, EnTest)
      revealed_fields <- c(revealed_fields, EnTest)
      if (mat_comptes_voisins[EnTest] == 0) {
        diago_case <- union(diago_case, mat_indexs[voisins_diago(which(mat_indexs == EnTest, arr.ind = TRUE)[2], which(mat_indexs == EnTest, arr.ind = TRUE)[1])])
        ToTest <- union(ToTest, setdiff(mat_indexs[voisins(which(mat_indexs == EnTest, arr.ind = TRUE)[2], which(mat_indexs == EnTest, arr.ind = TRUE)[1])], Tested))
      }
      ToTest <- setdiff(ToTest, EnTest)
    }
    revealed_fields <- union(revealed_fields, diago_case)
  }
  
  return(revealed_fields)
  
}


# Update the dataset with revealed fields
update_grid <- function(df_plot, liste_index) {
  
  df_plot$affich[df_plot$index_field %in% liste_index] <- TRUE
  df_plot$remplissage[df_plot$index_field %in% liste_index] <- "#FFFFFF"
  df_plot$couleur_affich[df_plot$index_field %in% liste_index] <- df_plot$couleur[df_plot$index_field %in% liste_index]
  df_plot$image_affich[df_plot$index_field %in% liste_index] <- df_plot$image[df_plot$index_field %in% liste_index]
  return(df_plot)
  
}
