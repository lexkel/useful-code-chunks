From flowing data:
https://flowingdata.com/2016/07/18/how-to-make-square-pie-charts-in-r/

squarePie <- function(values, col=terrain.colors(length(values)), col.grid="#e0e0e0", col.border="black", main="Hello, Square Pie") {
  
  # Convert pcts to parts of a whole
  total <- sum(values)
  pcts <- round(100 * values / total)
  
  # Naive way to handle rounding
  pcts[length(pcts)] <- 100 - sum(pcts[-length(pcts)])
  
  # x- and y-coordinates of rows and columns
  x_row <- 1:10
  y_col <- 1:10
  
  # Put together full coordinate vectors
  x <- rep(x_row, 10)
  y <- rep(y_col, each=10)
  
  # Colors
  fill_col <- c()
  for (i in 1:length(pcts)) {
    fill_col <- c(fill_col, rep(col[i], pcts[i]))
  }
  
  # Plot
  plot(0, 0, type="n", xlab="", ylab="", main=main, xlim=c(0,11), ylim=c(0,10.5), asp=1, bty="n", axes=FALSE)
  symbols(x, y, asp=1, squares=rep(1, 100), inches=FALSE, add=TRUE, bg=fill_col, fg=col.grid, lwd=0.5)
  rect(.5, .5, 10.5, 10.5, lwd=2, border=col.border)
  
}
