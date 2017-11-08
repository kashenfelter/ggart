#' Render polygon
#'
#' Creates renderings that can be added as layers to polygon plots
#' @param n The number of elements used for the rendering
#' @param polygon A data frame with x and y coordinates for the vertices
#' @param type The type of rendering to be applied (currently only one option)
#' @keywords theme
#' @export
#' @examples
#' render_polygon()

render_polygon <- function(n, polygon, type = "segments") {
  t_prev <- 0
  edges <- polygon %>% mutate(xend = lead(x, default = .$x[1]), yend = lead(y, default = .$y[1]),
                              length = sqrt((xend - x)^2 + (yend - y)^2), id = 1:nrow(.))
  ls <- vector(mode = "list", length = n)
  for(i in 1:n) {
    valid <- FALSE
    while (!valid) {
      # Obtain a point on a random edge
      edge <- sample_n(edges, 2, weight = length, replace = TRUE)
      id1 <- edge[1, "id"]
      id2 <- edge[2, "id"]
      a1 <- as.numeric(edge[1, c("x", "y")])
      a2 <- as.numeric(edge[1, c("xend", "yend")])
      b1 <- as.numeric(edge[2, c("x", "y")])
      b2 <- as.numeric(edge[2, c("xend", "yend")])
      alpha1 <- runif(1, 0, 1)
      alpha2 <- runif(1, 0, 1)
      p1 <- alpha1 * a1 + (1 - alpha1) * a2
      p2 <- alpha2 * b1 + (1 - alpha2) * b2
      # Check if the line segmenet between p1 and p2 intersects an edge of the polygon
      violations <- 0
      j <- 1
      while(j <= nrow(edges)) {
        if(j %in% c(id1, id2)) {
          j <- j + 1
        }
        v1 <- as.numeric(edges[j, c("x", "y")])
        v2 <- as.numeric(edges[j, c("xend", "yend")])
        if(does_intersect(p1, p2, v1, v2, smidgin = -1e-6) |
           point.in.polygon((p1[1] + p2[1]) / 2, (p1[2] + p2[2]) / 2, polygon$x, polygon$y) == 0) {
          break
        }
        j <- j + 1
      }
      if(j > nrow(edges)) {
        ls[[i]] <- c(p1, p2, id1, id2)
        valid <- TRUE
      }
    }
    t_now <- floor(i/n*100)
    if(t_now != t_prev) print(paste("Progress: ", floor(i/n*100), "%", sep = ""))
    t_prev <- floor(i/n*100)
  }

  result <- data.frame(matrix(unlist(ls), nrow=n, byrow=T)) %>%
    rename(x = X1, y = X2, xend = X3, yend = X4, edge1 = X5, edge2 = X6)
  result
}
