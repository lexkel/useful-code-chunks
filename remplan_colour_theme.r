
  #====================================================================================================
  #
  # REMPLAN colour palette - two examples down the bottom if needed
  #
  # Source: https://drsimonj.svbtle.com/creating-corporate-colour-palettes-for-ggplot2
  #====================================================================================================

  
  # Remplan colour theme
  remplan_colours <- c(remplan     = "#D71335",
                       community   = "#00ACE5", 
                       economy     = "#F17C33",
                       health      = "#A3258D",
                       investment  = "#53308C",
                       business    = "#00619C",
                       environment = "#44B04E",
                       forecast    = "#006D5D",
                       mapbuilder  = "#E90080",
                       consulting  = "#FEC929",
                       black       = "#000000",
                       grey        = "#808285")
  
  # Functions to extract colours and create palettes
  get_remplan_cols <- function(...) {
    cols <- c(...)
    if (is.null(cols))
      return (remplan_colours)
    remplan_colours[cols]
  }
  
  # Remplan colour palettes
  remplan_palettes <- list(warm = get_remplan_cols("remplan", "economy", "consulting"),
                           cool = get_remplan_cols("community", "environment"),
                           fcst2 = get_remplan_cols("forecast", "environment"),
                           main2 = get_remplan_cols("remplan", "economy"),
                           all8 = get_remplan_cols("remplan", "economy", "consulting", "environment", "forecast", "community", "business", "investment"))
  
  # Functions to extract colours
  get_remplan_palette <- function(palette = "warm", reverse = FALSE, ...) {
    pal <- remplan_palettes[[palette]]
    if (reverse) pal <- rev(pal)
    colorRampPalette(pal, ...)
  }
  
  scale_colour_remplan <- function(palette = "warm", discrete = TRUE, reverse = FALSE, ...) {
    pal <- get_remplan_palette(palette = palette, reverse = reverse)
    if (discrete) {
      discrete_scale("colour", paste0("remplan_", palette), palette = pal, ...)
    } else {
      scale_colour_gradientn(colours = pal(256), ...)
    }
  }
  
  scale_fill_remplan <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
    pal <- get_remplan_palette(palette = palette, reverse = reverse)
    if (discrete) {
      discrete_scale("fill", paste0("remplan_", palette), palette = pal, ...)
    } else {
      scale_fill_gradientn(colours = pal(256), ...)
    }
  }

  # Example of ggplot with remplan colours
  # library(tidyverse)
  # theme_set(theme_minimal())
  # 
  # ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Species)) +
  #   geom_point(size = 4) +
  #   scale_colour_remplan()
  # 
  # ggplot(mpg, aes(manufacturer, fill = manufacturer)) +
  #   geom_bar() +
  #   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  #   scale_fill_remplan(palette = "all8", guide = "none")
  