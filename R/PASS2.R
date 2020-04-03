#' Modelling Judgments of Frequency with PASS 2
#'
#' @param x input handled by PASS 2. Only binary input is allowed.
#' @param y a second binary input handled by PASS 1. At least
#' two inputs are needed for the simulation.
#' @param ... other binary inputs for modeling.
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
#' @param att attention is a vector with numeric values
#' between 0 and 1. \code{att} has the same length like
#' \code{sqc}, so each input processing have its own value
#' and PASS 1 can modulate attention by time or input.
#' If  \code{att} is exact one numeric value
#' (e.g. \code{ att = .1}), all inputs get the
#' same parameter of attention.
#' @param n_output_units number of output units as numeric value.
#' This must be between 1 and the maximum number of input units.
#' \code{n_output_units = 'half'} determines the half of the input
#' units.
#' @param rdm_weights a logical value indicating whether random
#' weights in the neural network are used or not. If
#' \code{rdm_weights = FALSE} all network connections are zero
#' at the beginning.
#' @param noise a proportion between 0 and 1 which determines
#' the number of random activated input units (higher numbers
#' indicate higher noise).
#' @details PASS 2 uses a competitive learning algorithm, which
#' usually clusters the input as side effect. If weights are
#' equal, the winning unit is chosen randomly, because of this,
#' each simulation is slightly different.
#' \deqn{if an outputuni O_{i} losses:  \Delta w_{ij} = 0}{if
#' an outputunit Oi losses:  \Delta wij = 0}
#' \deqn{if an outputuni O_{i} wins:  \Delta w_{ij} = g_{w}
#' \frac{a_{i}}{\sum_{i}^{n}{a_{i}}} - g_{w}w_{ij}}{if
#' an outputunit Oi wins:  \Delta wij = gw * (ai / sum(ai)) -
#' gw*wij}
#' @return \code{PASS2} returns the relative judgment of frequency
#' for each input.
#' @references Sedlmeier, P. (2002). Associative learning and
#' frequency judgements: The PASS model. In P. Sedlmeier,
#' T. Betsch (Eds.), \emph{Etc.: Frequency processing and cognition}
#' (pp. 137-152). New York: Oxford University Press.
#' @examples o1 <- c(1, 0, 0, 0)
#' o2 <- c(0, 1, 0, 0)
#' o3 <- c(0, 0, 1, 0)
#' o4 <- c(0, 0, 0, 1)
#' PASS2(o1, o2, o3, o4,
#'       sqc = rep(1:4, 4:1), att = .1, n_output_units = 2,
#'       rdm_weights = FALSE, noise = 0)
PASS2 <- function(x, y, ..., sqc, att, n_output_units = "half",
                  rdm_weights = F, noise = 0) {
    lo <- length(x)
    all_o <- c(x, y, ...)
    no <- length(list(x, y, ...))
    objn <- as.character(unname(as.list(match.call())))
    obj_names <- objn[2:(no + 1)]
    lp <- att
    # return(obj_names) }
    if (length(lp) == 1) {
        lp <- rep(lp, length(sqc))
    }
    if (anyNA(c(all_o, lp, sqc))) {
        stop("NAs in Input not allowed")
    }
    if (lo != length(y)) {
        stop("Input has not the same length")
    }
    if (length(all_o) %% lo != 0) {
        stop("Input has not the same length")
    }
    if (!identical(as.vector(all_o), as.numeric(as.logical(all_o)))) {
        stop("Input must 0 or 1")
    }
    length(all_o)
    obj_mat <- matrix(all_o, ncol = lo, byrow = T)
    if (!identical(c(1:nrow(obj_mat)), as.integer(sort(unique(sqc))))) {
        stop("No match between number of objects and sqc argument")
    }
    if (max(lp) > 1 | min(lp) < 0) {
        stop("att must between 0 and 1")
    }
    if (length(sqc) != length(lp)) {
        stop("length sqc doesn't match length att")
    }
    if (!is.logical(rdm_weights)) {
        stop("rdm_weights must be TRUE or FALSE")
    }
    if (noise < 0 | noise > 1) {
        stop("noise must netween 0 and 1")
    }
    if (is.character(n_output_units) & n_output_units != "half") {
        stop("either half or numeric values are allowed")
    }
    if (is.numeric(n_output_units) & n_output_units > lo) {
        stop("n_output_units greater than number of inputunits")
    }
    if (is.numeric(n_output_units) & n_output_units < 1) {
        stop("n_output_units can't be less than 1")
    }
    pres_mat <- matrix(rep(0, length(sqc) * lo), ncol = lo)
    for (i in 1:length(sqc)) {
        pres_mat[i, ] <- obj_mat[sqc[i], ]
    }
    if (noise > 0) {
        noise_input <- round(noise * lo)
        for (i in 1:length(sqc)) {
            pres_mat[i, sample(1:lo, noise_input, F)] <- 1
        }
    }
    if (n_output_units == "half") {
        n_output_units <- ceiling(lo / 2)
    }
    if (rdm_weights == T) {
        output_l <- matrix(
          round(runif(ncol(obj_mat) * n_output_units, 0, 1), 4),
          ncol = n_output_units, nrow = ncol(obj_mat))
        output_l_strt <- apply(output_l, 2, stand_to_1)
    } else {
        output_l <- matrix(
          0, ncol = n_output_units, nrow = ncol(obj_mat))
        output_l_strt <- output_l
    }
    output_list <- NULL
    output_l_u <- output_l_strt
    for (i in 1:nrow(pres_mat)) {
        scalar <- find_winner(pres_mat[i, ], output_l_u)
        fw <- which(scalar == max(scalar))
        if (length(fw) > 1) {
            fw <- sample(fw, 1)
        }
        output_l_u[, fw] <- output_l_u[, fw] +
          lp[i] * (pres_mat[i, ] / sum(pres_mat[i, ])) -
          (lp[i] * output_l_u[, fw])
        output_list[[i]] <- output_l_u
    }
    rel_weight <- NULL
    weight_progress <- NULL
    for (i in 1:nrow(obj_mat)) {
        rel_weight[i] <- sum(find_winner(obj_mat[i, ], output_l_u))
        weight_progress[[i]] <- unlist(
          lapply(output_list, function(x) {
            return(sum(find_winner(obj_mat[i, ], x)))
          }
          )
          )
    }
    t1 <- t((apply(do.call(rbind, weight_progress), 2, stand_to_1)))
    weight_progress <- split(t1, rep(1:ncol(t1), each = nrow(t1)))
    perc_est <- rel_weight / sum(rel_weight)
    tot_freq <- unname(table(sqc))
    out <- list(rel_weight, perc_est, tot_freq, obj_names,
                weight_progress, "PASS2")
    names(out) <- c("raw_estimate", "percent_estimate", "input_freq",
                    "object_names", "progress", "what")
    class(out) <- c("JoF")
    return(out)
}
