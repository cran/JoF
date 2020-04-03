#' Output of judgment of frequecny simulation
#' @param x output of JoF simulation
#' @param ... further arguments
#' @return Displays the judgment of frequency as proportion of all
#' inputs
print.JoF <- function(x, ...) {
    m1 <- round(x$percent_estimate, 3)
    names(m1) <- x$object_names
    jof <- paste(x$object_names, ": ", m1, sep = "")
    cat("relative judgment of frequency\n", jof, sep = "\n")
}
#' plot progress of judgment of frequency simulation
#' @param x output of JoF simulation
#' @param type "l" for lines, "p" for points, "b" for both
#' @param ... further arguments
#' @return Displays the judgment of frequency as proportion of all
#' inputs
plot.JoF <- function(x, type = "l", ...) {
    x_max <- sum(x$input_freq)
    y_max <- max(unlist(x$progress))
    no <- length(x$object_names)
    if (no > 8){
        tc <- rainbow(no)
        } else {
            tc <- palette()[1:no]
        }
    add_x <- ceiling(0.1 * x_max)
    plot(0, 0, ylim = c(0, y_max), xlim = c(1, (x_max + add_x)),
         type = "n",
         xlab = "Progress", ylab = "relative JoF",
         cex.axis=.8, cex.lab=.8)
    for (i in 1:no) {
        lines(1:x_max, x$progress[[i]], col = tc[i], type = type)
        text(x_max + add_x, x$progress[[i]][x_max],
             x$object_names[i], col = tc[i], cex=.8)
    }
}
