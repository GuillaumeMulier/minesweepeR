

draw_empty_grid <- function(cote1, cote2) {
  
  graphe_vide <- ggplot(data = NULL) +
    geom_hline(yintercept = seq(.5, cote1 + .5), size = 1.5) +
    geom_vline(xintercept = seq(.5, cote2 + .5), size = 1.5) +
    theme_void() +
    scale_x_continuous(limits = c(.5, cote2 + .5), expand = c(0, 0)) +
    scale_y_continuous(limits = c(.5, cote1 + .5), expand = c(0, 0)) +
    coord_fixed()
  return(graphe_vide)
  
}


draw_losing_screen <- function(tableau_bombes, cote1, cote2, abscisse, ordonnee) {
  
  ggplot(data = NULL) +
    geom_hline(yintercept = seq(.5, cote1 + .5), size = 1.5) +
    geom_vline(xintercept = seq(.5, cote2 + .5), size = 1.5) +
    theme_void() +
    scale_x_continuous(limits = c(.5, cote2 + .5), expand = c(0, 0)) +
    scale_y_continuous(limits = c(.5, cote1 + .5), expand = c(0, 0)) +
    coord_fixed() +
    geom_image(data = tableau_bombes, aes(x = name, y = rowname, image = value)) +
    annotate("rect", xmin = abscisse - .5, xmax = abscisse + .5, ymin = ordonnee - .5, ymax = ordonnee + .5, color = "transparent", fill = "#EA131B", alpha = .5)
  
}