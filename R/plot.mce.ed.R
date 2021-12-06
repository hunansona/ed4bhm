
## Plot.mce.ed

plot.mce.ed <- function(object,
                        ranges = NULL,
                        title = NULL){
  
  r.names <- rownames(object) 
  c.names <- colnames(object) 
  
  par(mfrow = c(2, 3),
      oma = c(1, 1, 1, .3), 
      mar = c(1, 1.5, 1.2, 1.5),
      mgp = c(3, .5, 0))
  
  for (j in 1:5){
    
    if (is.null(ranges)){
      ranges.p <- rep(list(c(min(object[,j] - qnorm(.975) * object[,j+5]),
                             max(object[,j] + qnorm(.975) * object[,j+5]))),5)
    } else { ranges.p <- ranges}
    
    plot(c(1:nrow(object)), object[,j], xlab=" ", 
         ylim = ranges.p[[j]],
         cex = 1, cex.axis = .8, xaxt = 'n', pch = 1, col = "red") 
    title(main = c.names[j], line = .2)
    segments(c(1:nrow(object)), object[,j] - qnorm(.975) * object[,j+5],
             c(1:nrow(object)), object[,j] + qnorm(.975) * object[,j+5])
    if ((j == 3)|(j == 4)|(j==5)){
      axis(1, 1:nrow(object), r.names)
    }
    
  }
  title(title, line = 0, outer = TRUE)
  
}