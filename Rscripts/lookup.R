

# Look-up table for colors of the field
lookup_fill <- tribble(
  ~affich, ~remplissage,
  FALSE, "#E6D37F",
  TRUE, "#FFFFFF"
)


# Look-up table for color of the texts
lookup_text <- tribble(
  ~decompte_bombes, ~couleur,
  -99, "transparent",
  0, "#E41A1C",
  1, "#377EB8",
  2, "#4DAF4A",
  3, "#984EA3",
  4, "#FF7F00",
  5, "#FFFF33",
  6, "#A65628",
  7, "#F781BF",
  8, "#999999"
)
