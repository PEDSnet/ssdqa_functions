library(ggplot2)
library(scales)
library(ggpubr)
# tutorial: https://drsimonj.svbtle.com/creating-corporate-colour-palettes-for-ggplot2
#' Function to extract colors as hex codes
#'
#' @param ... Character names of ssdqa_colors_standard
#' @return name and hex code/s for specified color
#' example usage: extract_color() [returns all] or extract_color("rust) [just returns rust]
#'
extract_color <- function(...) {
  cols <- c(...)
  
  if (is.null(cols))
    return (ssdqa_colors_standard)
  
  ssdqa_colors_standard[cols]
}

#' Return function to interpolate a color palette
#'
#' @param palette Character name of palette in ssdqa_palettes_standard
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments to pass to colorRampPalette()
#' @return color palettes, interpolated if necessary, with specified scheme and number of colors
#' example usage: ssdqa_pal("beachy")(10)
ssdqa_pal <- function(palette, reverse = FALSE, ...) {
  pal <- ssdqa_palettes_standard[[palette]]
  
  if (reverse) pal <- rev(pal)
  
  colorRampPalette(pal, ...)
}

#' Setting standard colors
ssdqa_colors_standard<-c(`brightpink`="#FF4D6FFF", 
                         `lightblue`="#579EA4FF", 
                         `burntorange`="#DF7713FF", 
                         `yellow`="#F9C000FF", 
                         `lightgreen`="#86AD34FF", 
                         `dustblue`="#5D7298FF", 
                         `seagreen`="#81B28DFF", 
                         `rust`="#7E1A2FFF", 
                         `violet`="#2D2651FF", 
                         `redorange`="#C8350DFF", 
                         `rosypink`="#BD777AFF",
                         `grey=`="#E2D8D6FF")

ssdqa_palettes_standard<-list(
  `dark` = extract_color("rust", "violet", "redorange"),
  `fun` = extract_color("brightpink", "lightblue", "yellow"),
  `beachy`=extract_color("lightblue","dustblue","seagreen"),
  `diverging`=extract_color("dustblue", "grey", "rust"),
  `sequential`=extract_color("grey", "rosypink", "rust"),
  `main`=extract_color("brightpink", "lightblue", "burntorange", "yellow",
                      "lightgreen","dustblue", "seagreen", "rust",
                      "violet", "redorange", "rosypink")
)

# usage: ssdqa_pal("dark")(10)


#' Fill scale constructor for ssdqa colors
#'
#' @param palette Character name of palette in ssdqa_palettes_standard
#'                  If no palette specified, defaults to "main" palette
#' @param discrete Boolean indicating whether fill aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_color_gradientn(), used respectively when discrete is TRUE or FALSE
#' @export
#'
scale_fill_ssdqa <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- ssdqa_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("fill", paste0("ssdqa_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}

#' Color scale constructor for ssdqa colors
#'
#' @param palette Character name of palette in ssdqa_palettes_standard.
#'                  If no palette specified, defaults to "main" palette
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_color_gradientn(), used respectively when discrete is TRUE or FALSE
#' @export
#'
scale_color_ssdqa <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- ssdqa_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("colour", paste0("ssdqa_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}

