#' Blank Canvas Theme
#'
#' An empty theme similar to theme_void() but with a few subtle differences.
#' @param bg_col Plot background (and border) colour. Defaults to "transparent".
#' @keywords theme
#' @export
#' @examples
#' theme_blankcanvas()

theme_blankcanvas <- function(bg_col = "transparent") {
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        legend.position = "none",
        panel.background = element_rect(fill = bg_col, colour = bg_col),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = bg_col, colour = bg_col),
        plot.margin = unit(c(0, 0, -2, -1), "mm"), # top, right, bottom, left
        strip.background = element_blank(),
        strip.text = element_blank())
}
