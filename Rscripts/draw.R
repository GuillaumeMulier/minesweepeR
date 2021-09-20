

# Draw the grid (the main plotting function of the app)
draw_board <- function(df_plot, selectY = NULL, selectX = NULL) {
  
  nb_lignes <- max(df_plot$ordonnee)
  nb_colonnes <- max(df_plot$abscisse)
  
  graphe <- ggplot(df_plot, aes(abscisse, nb_lignes + 1 - ordonnee)) +
    geom_tile(aes(fill = remplissage)) +
    geom_text(aes(label = decompte_bombes, color = couleur_affich)) +
    geom_image(aes(image = image_affich)) +
    geom_hline(yintercept = seq(.5, nb_lignes + .5), size = 1.5) +
    geom_vline(xintercept = seq(.5, nb_colonnes + .5), size = 1.5) +
    theme_void() +
    scale_x_continuous(limits = c(.5, nb_colonnes + .5), expand = c(0, 0)) +
    scale_y_continuous(limits = c(.5, nb_lignes + .5), expand = c(0, 0)) +
    scale_fill_identity() +
    scale_color_identity() +
    coord_fixed() +
    theme(plot.margin=unit(c(0,0,0,0), "null"))
  
  if (!is.null(selectX) & !is.null(selectY)) {
    graphe <- graphe +
      annotate("rect", xmin = selectX - .5, xmax = selectX + .5, ymin = selectY - .5, ymax = selectY + .5, 
               color = "#2A9421", fill = "#2A9421", size = 2, alpha = .3)
  }
  
  return(graphe)
  
}


# Losing screen
draw_losing_screen <- function(df_plot, x_b, y_b) {
  
  nb_lignes <- max(df_plot$ordonnee)
  nb_colonnes <- max(df_plot$abscisse)
  
  graphe_lose <- ggplot(df_plot, aes(abscisse, nb_lignes + 1 - ordonnee)) +
    geom_tile(aes(fill = remplissage)) +
    geom_image(aes(image = image)) +
    geom_hline(yintercept = seq(.5, nb_lignes + .5), size = 1.5) +
    geom_vline(xintercept = seq(.5, nb_colonnes + .5), size = 1.5) +
    theme_void() +
    scale_x_continuous(limits = c(.5, nb_colonnes + .5), expand = c(0, 0)) +
    scale_y_continuous(limits = c(.5, nb_lignes + .5), expand = c(0, 0)) +
    scale_fill_identity() +
    scale_color_identity() +
    coord_fixed() +
    annotate("rect", xmin = abscisse - .5, xmax = abscisse + .5, ymin = ordonnee - .5, ymax = ordonnee + .5, 
             color = "transparent", fill = "#EA131B", alpha = .5)
  
  return(graphe_lose)
  
}


