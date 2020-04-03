#' Modeling Judgments of Frequency with MINERVA 2
#'
#' @param x input handled by MINERVA 2. Values -1, 0 and 1 are
#' allowed. -1 represents the absence of a feature, 0 the
#' irrelevance of a feature and 1 the presence of a feature.
#' @param y another input handled by MINERVA 2. At least two
#' inputs are needed for the simulation.
#' @param ... other inputs for modeling.
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
#' @param L learning parameter. This is the proportion of a
#' correctly stored vector. \code{L = 1} means 100 \% of the input
#' is processed correctly. If \code{L} is a vector, each input
#' could be handled differently. So \code{L = c(.5, .6, .9)} means,
#' input \code{x} is correctly stored to 50 \%, input
#' \code{y} is stored to 60 \% and the third input (inserted
#' in \code{...}) is stored with 90 \% probability.
#' @param dec decay is not part of the original version of MINERVA 2.
#' This is just implemented for a better comparison with the other
#' models of JoF. In \code{dec = NULL}, decay has no effect.
#' For \code{dec = 'curve'} decay uses a forgetting curve.
#' If dec is a numeric Vector e. g. \code{dec = c(.8, .9, 1)}
#' the memory traces are weighted. The first represented trace is
#' weighted by .8 the second by .9 and the youngest trace by 1.
#' The value \code{dec = 1} corresponds with the original model.
#' @details Calculations of MINERVA 2 contain four steps.
#' \deqn{S_{i} = \frac{\sum_{j=1}^{N}{P_{j}T_{ij}}}{N_{i}}}{Si =
#' (sum(Pj)*Tj) / Ni }
#' \deqn{A_{i} = S_{i}^{3}}{Ai = Si^3}
#' \deqn{I = \sum_{i=1}^{M}{A_{i}}}{I = sum(Ai)}
#' \deqn{relative JoF = \frac{I_{j}}{\sum_{j}^{K}{I_{j}}}}{
#' relative JoF = Ij / Sum(Ij)}
#' @return MINERVA2 returns the relative judgment of frequency
#' @references Dougherty, M. R., Gettys, C. F., & Ogden, E. E.
#' (1999). MINERVA-DM: A memory processes model for judgments
#' of likelihood. \emph{Psychological Review, 106}(1), 180.
#'
#' Hintzman, D. L. (1984). MINERVA 2: A simulation model of human
#' memory. \emph{Behavior Research Methods, Instruments, and
#' Computers, 16,}  96–101.
#' @examples #This example is presented in Dougherty,
#' #Gettys, & Ogden, 1999 (p. 185)
#' H1 <- c(-1, 1, 0, 1, 0, -1, 1, -1, 0)
#' H2 <- c(-1, 0, 0, 1, 0, 0, 1, 0, 0)
#' x <- MINERVA2(H1, H2, sqc = c(2, 1), L = 1)
MINERVA2 <- function(x, y, ..., sqc, L, dec = NULL) {
    if (missing(y)) {
        stop("at least two inputs are needed")
    }
    L1 <- list(x, y, ...)
    lo <- length(x)
    all_o <- c(x, y, ...)
    objn <- as.character(unname(as.list(match.call())))
    if (missing(dec)) {
        obj_names <- objn[2:(length(objn) - 2)]
    } else {
        obj_names <- objn[2:(length(objn) - 3)]
    }
    no <- length(obj_names)
    if (anyNA(c(all_o, dec, L, sqc))) {
        stop("NAs are not allowed")
    }
    if (lo != length(y)) {
        stop("Input has not the same length")
    }
    if (length(all_o) %% lo != 0) {
        stop("Input has not the same length")
    }
    if (missing(sqc)) {
        stop("sqc is missing: no default")
    }
    if (length(L) == 1) {
        L <- rep(L, length(sqc))
    }
    if (length(names(table(sqc))) != length(1:no)) {
        stop("sqc doesn't match number of inputs")
    }
    if (!(all.equal(as.numeric(names(table(sqc))), 1:no))) {
        stop("sqc doesn't match number of inputs")
    }
    if (length(L) != length(sqc)) {
        stop("length of L doesn't match length of sqc")
    }
    if (!identical(
        abs(as.vector(all_o)), as.numeric(as.logical(all_o)))
        ) {
        stop("Input must be -1, 0 or 1")
    }
    if (max(L) > 1 | min(L) < 0) {
        stop("L must between 0 and 1")
    }
    if (is.null(dec)) {
        dec <- rep(1, length(sqc))
    }
    if (dec[1] == "curve") {
        t_param <- 1:length(sqc)
        s_param <- length(sqc / 2)
        dec <- rev( (exp(1) ^ (-t_param / s_param)) )

    }
    if (is.numeric(dec) & length(dec) != length(sqc)) {
        stop("length decay doesn't match length sqc")
    }
    L2 <- NULL
    Lp <- L
    for (i in 1:length(sqc)) {
        L2[[i]] <- L1[[sqc[i]]]
    }
    M1 <- do.call(rbind, L2)
    L_mat <- M1
    for (i in 1:nrow(M1)) {
        L_mat[i, ] <- sample(c(1, 0), lo, T, c(Lp[i], 1 - Lp[i]))
    }
    M2 <- replace(M1, which(L_mat == 0), 0)
    S1 <- NULL
    S2 <- NULL
    for (i in 1:length(L1)) {
        for (j in 1:nrow(M2)) {
            N <- length(which((L1[[i]] + M2[j, ]) != 0))
            S1[j] <- (sum((L1[[i]] * M2[j, ])) / N) * dec[j]
        }
        S2[[i]] <- S1
    }
    A <- lapply(S2, FUN = function(x) return(x ^ 3))
    activation <- do.call(rbind, A)
    act <- t(apply(activation, 1, cumsum))
    act <- apply(act, 2, stand_to_1)
    act <- lapply(seq_len(nrow(act)), function(i) act[i, ])
    I <- lapply(A, sum)
    perc_est <- unlist(I) / sum(unlist(I))
    tot_freq <- unname(table(sqc))
    out <- list(unlist(I), perc_est, tot_freq, obj_names, act,
                "MINERVA2")
    names(out) <- c("raw_estimate", "percent_estimate", "input_freq",
                    "object_names", "progress", "what")
    class(out) <- c("JoF")
    return(out)
}
