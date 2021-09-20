

# Draw the grid (the main plotting function of the app)
draw_board <- function(df_plot, selectY = NULL, selectX = NULL) {
  
  nb_lignes <- max(df_plot$ordonnee)
  nb_colonnes <- max(df_plot$abscisse)
  
  graphe <- ggplot(df_plot, aes(abscisse, nb_lignes + 1 - ordonnee)) +
    geom_tile(aes(fill = remplissage)) +
    geom_text(aes(label = decompte_bombes, color = couleur_affich)) +
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


# End screen
draw_ending_screen <- function(df_plot, y_b, x_b, statut = c("win", "lose")) {
  
  statut <- match.arg(statut, c("win", "lose"))
  
  nb_lignes <- max(df_plot$ordonnee)
  nb_colonnes <- max(df_plot$abscisse)
  
  graphe_end <- ggplot(df_plot, aes(abscisse, nb_lignes + 1 - ordonnee)) +
    geom_tile(fill = "#FFFFFF") +
    theme_void() +
    scale_x_continuous(limits = c(.5, nb_colonnes + .5), expand = c(0, 0)) +
    scale_y_continuous(limits = c(.5, nb_lignes + .5), expand = c(0, 0)) +
    scale_color_identity() +
    coord_fixed()
  
  # Scale the image style relative to the greater side
  if (nb_lignes > nb_colonnes) {
    graphe_end <- graphe_end +
      geom_image(aes(image = image), size = 1 / (1.2 * nb_lignes), by = "height", asp = nb_colonnes / nb_lignes)
  } else {
    graphe_end <- graphe_end +
      geom_image(aes(image = image), size = 1 / (1.2 * nb_colonnes), by = "width", asp = nb_colonnes / nb_lignes)
  }
  
  graphe_end <- graphe_end +
    geom_hline(yintercept = seq(.5, nb_lignes + .5), size = 1.5) +
    geom_vline(xintercept = seq(.5, nb_colonnes + .5), size = 1.5)
  
  # Change the graph relative to the win or the lose
  if (statut == "lose") {
    graphe_end <- graphe_end +
      annotate("rect", xmin = x_b - .5, xmax = x_b + .5, ymin = y_b - .5, ymax = y_b + .5, 
               color = "transparent", fill = "#EA131B", alpha = .5)
  } else {
    graphe_end <- graphe_end +
      map2(.x = df_plot$abscisse[df_plot$bombes == "X"], .y = df_plot$ordonnee[df_plot$bombes == "X"],
           .f = ~ annotate("rect", xmin = .x - .5, xmax = .x + .5, ymin = nb_lignes - .y + .5, ymax = nb_lignes - .y + 1.5, 
                           color = "transparent", fill = "#A5F0CC", alpha = .5))
  }
  
  return(graphe_end)
  
}

