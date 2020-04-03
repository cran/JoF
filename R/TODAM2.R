#' Modeling Judgments of Frequency with TODAM 2
#'
#' @param x input handled by TODAM 2. Normal distributed inputs
#' with mean = 0 and sd = 1 / n are allowed. This representation
#' enables discrimination and similarity between different items.
#' See vignette for details.
#' @param y another input handled by TODAM 2. At least two
#' inputs are needed for the simulation.
#' @param ... other inputs handled by TODAM 2.
#' @param sqc sequence of the different objects. Each input gets
#' an ascending number. \code{x} gets the value \code{1},
#' \code{y} gets the value \code{2}, \code{...} gets the value
#' \code{3} and so on.
#' The argument \code{sqc = c(1, 2, 3, 2)} means: first
#' input \code{x} is processed, second input \code{y} is
#' processed followed by processing input number three and
#' fourth, th input \code{y} is used again.
#' So \code{sqc} contains the frequency information too.
#' In \code{c(1, 2, 3, 2)}, \code{x} and  the third input
#' are presented once. The input \code{y} is presented twice.
#' @param gamma is the atttention- or learningparameter. Values
#' between 0 and 1 are allowed. 1 represents perfect learning.
#' If \code{gamma} iis a vector, each input
#' could be handled differently. So \code{gamma = c(.5, .6, 1)}
#' means, the third input is stored correctly and betther than
#' the \code{y} better than first input \code{x}).
#' @param alpha represents the decay. If \code{alpha = 1},
#' the complete memory vector is used (and no forgetting takes
#' place). If \code{alpha} is an numeric Vector e. g.
#' \code{alpha = c(.8, .9, 1)}, the memory vector is weighted.
#' The memory for the first input is weaker than the second
#' than the third.
#' @details In the original publication TODAM 2 is more complex
#'  and has more parameters. Especially the design for the input
#'  is a concatenation between item and context. The normal
#'  distributed input has a \emph{mean = 0} and \emph{sd = 1/n}.
#'  A pragmatic solution to make the models input comparable is
#'  to use a binary input like in PASS. There is no explicit argument
#'  for noise.
#'
#' Convolution:
#' \deqn{ F_{i}^{2}  = \sum_{i=1} f_{i} * f_{m-i+1} and m = 2n - 1}
#' Memory:
#' \deqn{M_{t} = \alpha M_{t-1} + \gamma F_{t}^{2} }
#' Correlation
#' \deqn{R_{m} = \sum_{(i;j)\in S(m)} F_{t}^{2} there
#' S(m)(i;j)| -(n-1)/2 \le i,j \le (n-1)/2 and i-j = m }
#'
#'@references
#'Murdock, B. B., Smith, D., & Bai, J. (2001). Judgments of frequency
#'and recency in a distributed memory model. \emph{Journal of
#'Mathematical Psychology, 45,} 564–602.
#' \url{https://doi.org/10.1006/jmps.2000.1339}
#' @examples o1 <- c(-0.27, -0.24, -0.24, 0.75)
#' o2 <- c(-0.06, -0.55,  0.66, -0.06)
#' o3 <- c(0.04,  0.57, -0.65,  0.04)
#' o4 <- c(0.73, -0.39, -0.20, -0.14)
#' TODAM2(o1, o2, o3, o4, gamma = rep(c(0.7, 0.8), 5),
#' alpha = 0.95, sqc = rep(1:4, 4:1))
TODAM2 <- function(x, y, ..., sqc, gamma = 1, alpha = 1) {
    if (missing(y)) {
        stop("at least two inputs are needed")
    }
    objn <- as.character(unname(as.list(match.call())))
    L1 <- list(x, y, ...)
    no <- length(L1)
    obj_names <- objn[2:(no + 1)]
    lo <- length(x)
    all_o <- c(x, y, ...)
    if (anyNA(c(all_o, alpha, gamma, sqc))) {
        stop("NAs are not allowed")
    }
    if (lo != length(y)) {
        stop("Input has not the same length")
    }
    if (length(all_o) %% lo != 0) {
        stop("Input has not the same length")
    }
    if (length(alpha) == 1) {
        alpha <- rep(alpha, sum(table(sqc)))
    }
    if (length(gamma) == 1) {
        gamma <- rep(gamma, sum(table(sqc)))
    }
    if (length(names(table(sqc))) != length(1:no)) {
        stop("sqc doesn't match number of inputs")
    }
    if (!(all.equal(as.numeric(names(table(sqc))), 1:no))) {
        stop("sqc doesn't match number of inputs")
    }
    if (length(alpha) != length(sqc)) {
        stop("length of alpha doesn't match length of sqc")
    }
    if (length(gamma) != length(sqc)) {
        stop("length of gamma doesn't match length of sqc")
    }
    if (max(gamma) > 1 | min(gamma) < 0) {
        stop("gamma must between 0 and 1")
    }
    if (max(alpha) > 1 | min(alpha) < 0) {
        stop("alpha must between 0 and 1")
    }
    pres_mat <- matrix(all_o, byrow = T, ncol = lo)
    tot_freq <- sum(table(sqc))
    pm <- matrix(NA, ncol = ncol(pres_mat), nrow = tot_freq)
    for (i in 1:tot_freq) {
        pm[i, ] <- pres_mat[sqc[i], ]
    }
    Memory <- rep(0, ncol(pm) + ncol(pm) - 1)
    Mem <- NULL
    dot <- NULL
    dot_end <- NULL
    for (j in 1:tot_freq) {
        Memory <- (alpha[j] * Memory) +
            (gamma[j] * convolute(pm[j, ], pm[j, ]))
        Mem[[j]] <- Memory
        for (k in 1:nrow(pres_mat)) {
            dot_end[k] <- correlate(Memory, pres_mat[k, ]) %*%
              pres_mat[k, ]
        }
        dot[[j]] <- dot_end
    }
    dot <- lapply(dot, function(x) ifelse(x < 0, 0, x))
    activation <- lapply(dot, stand_to_1)
    dot_out <- dot[[tot_freq]]
    perc_est <- dot_out / sum(dot_out)
    freq <- unname(table(sqc))
    out <- list(dot_out, perc_est, freq,
                obj_names, activation, "TODAM2")
    names(out) <- c("raw_estimate", "percent_estimate", "input_freq",
                    "object_names", "progress", "what")
    class(out) <- c("JoF")
    return(out)
}
