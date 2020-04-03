akt_fct2 <- function(x) {
    lo <- length(x)
    matrix(rep(x, lo), ncol = lo, byrow = T) +
    matrix(rep(x, lo), ncol = lo, byrow = F)
}
noise_fct <- function(x, noi, lo) {
    replace(x, sample(lo ^ 2 , noi, replace = F), 2)
}
stand_to_1 <- function(x) x / sum(x)
find_winner <- function(x, y) {
    out <- NULL
    for (i in 1:ncol(y)) {
        out[i] <- sum(x * y[, i])
    }
    return(out)
}
min_o <- function(x){
    return(replace(x, which(x == 0), -1))
    }
and_scale <- function(x) {
    z <- (x - mean(x)) / sd(x)
    and <- z / sqrt(length(x))
    return(and)
}
convolute <- function(x, y) {
    out <- matrix(nrow = length(x), ncol = length(y))
    for (i in 1:length(y)) {
        out[, i] <- x * y[i]
    }
    nx <- length(x)
    n <- length(x) + length(y) - 1
    shift_mat <- matrix(nrow = nrow(out), ncol = n)
    for (i in 1:nrow(out)) {
        shift_mat[i, ] <- c(rep(0, i - 1), out[i, ], rep(0, (nx) - i))
    }
    return(colSums(shift_mat))
}
correlate <- function(x, y) {
    x <- rev(x)
    out <- matrix(nrow = length(x), ncol = length(y))
    for (i in 1:length(y)) {
        out[, i] <- x * y[i]
    }
    nx <- length(x)
    n <- length(x) + length(y) - 1
    shift_mat <- matrix(nrow = nrow(out), ncol = n)
    for (i in 1:nrow(out)) {
        shift_mat[i, ] <- c(rep(0, i - 1), out[i, ], rep(0, (nx) - i))
    }
    adn <- n - length(y)
    rc <- (adn + 1):(n - adn)
    return(colSums(shift_mat[, rc]))
}
tod_scale <- function(x) {
    n1 <- length(which(x == 0))
    neg <- -abs(rnorm(n1, 0, 1))
    n2 <- length(which(x == 1))
    pos <- abs(rnorm(n2, 0, 1))
    out <- (ifelse(x == 0, neg, pos))
    out <- (out - mean(out)) / (sd(out) * sqrt(length(x)))
    return(out)
}
