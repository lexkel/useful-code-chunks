  
  # Function to format number with certain number of decimal points

  num.dec <- function(x, dec.places = 1) sprintf(paste0("%.", dec.places,"f"), x)
                                                 