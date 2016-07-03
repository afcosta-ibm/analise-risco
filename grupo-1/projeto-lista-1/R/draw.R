#'  Draw the frequency graph.
#'  
#'  @name draw 
#' 
#'  @param data Vector of frequency
#'  @param name Graph title
#'  @param filename File name 
#'   
#'  @description Draw the frequency graph.
#'  
#'  @export
#'  
#'  @keywords 
#'
#'  @family draw
#'
#'  @examples
#'
#' draw(c(0,1,2), "title", to.file = TRUE, "output/draw.png")
#' 
 
draw <- function(data, name = "Graphic", to.file=FALSE, filename="output/draw.png") {
    if(to.file) {png(filename)}
    plot(data, type = "h", 
         col = "red", lwd = 4,
         xlab = "Round", 
         ylab = "Frequency", ylim = c(0,1))
    title(name)
    abline(h = 0.5, col = "blue", lwd = 2)
    if(to.file) {dev.off()}
  }