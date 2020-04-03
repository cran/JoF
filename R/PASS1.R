#' Modeling Judgments of Frequency with PASS 1
#'
#' @param x input handled by PASS 1. Only binary input is allowed.
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
#' @param dec decay is a vector with numeric values between
#' -1 and 0. \code{dec} has the same length as \code{sqc}, so each
#' input processing have its own value and PASS 1 can modulate
#' decay by time. If \code{dec} is exact
#' one numeric value (e.g. \code{dec = -.1}), all inputs get the
#' same parameter of decay.
#' @param ifc interference is a vector with numeric values
#' between -1 and 0. \code{ifc} must have the same length as
#' \code{sqc}. So each inputprocessing have its own value and
#' PASS 1 can modulate inference by time. If \code{ifc} is exact
#' one numeric value (e.g. \code{ ifc = -.1}), all inputs get the
#' same parameter of inference.
#' @param rdm_weights a logical value indicating whether random
#' weights in the neural network are used or not. If
#' \code{rdm_weights = FALSE} all network connections are zero
#' at the beginning.
#' @param noise a proportion between 0 and 1 which determine
#' the number of randome activiated inputunits (hihger numbers
#' indicate higher noise).
#' @details PASS 1 is a simple neural pattern associator learning
#' by delta rule.
#'
#' Learning:
#' \deqn{if U_{i} and U_{j} are activated, then
#' \Delta w_{ij} = \Theta_{1} ( 1 - w_{ij})}{if Ui and Uj are
#' activated, then \Delta wij = \Theta1 * ( 1 - wij)}
#' Interference:
#' \deqn{if either U_{i} or U_{j} is activated, then
#' \Delta w_{ij} = \Theta_{2} * w_{ij}}{if either Ui or Uj is
#' activated, then \Delta wij = \Theta2 * wij}
#' Decay:
#' \deqn{if neither U_{i} nor U_{j} is activated, then
#' \Delta w_{ij} = \Theta_{3} * w_{ij}}{if neither Ui nor Uj is
#' activated, then \Delta wij = \Theta3 * wij}
#' @return \code{PASS1} returns the relative judgment of frequency
#' for each input.
#' @references Sedlmeier, P. (2002). Associative learning and
#' frequency judgements: The PASS model. In P. Sedlmeier,
#' T. Betsch (Eds.), \emph{Etc.: Frequency processing and cognition}
#' (pp. 137-152). New York: Oxford University Press.
#' @examples o1 <- c(1, 0, 0, 0)
#' o2 <- c(0, 1, 0, 0)
#' o3 <- c(0, 0, 1, 0)
#' o4 <- c(0, 0, 0, 1)
#' PASS1(o1, o2, o3, o4,
#'       sqc = rep(1:4, 4:1), att = .1, dec = -.05,
#'       ifc = -.025, rdm_weights = FALSE, noise = 0)
PASS1 <- function(x, y, ..., sqc, att, dec, ifc,
                  rdm_weights = TRUE, noise = 0) {
    lo <- length(x)
    all_o <- c(x, y, ...)
    no <- length(list(x, y, ...))
    objn <- as.character(unname(as.list(match.call())))
    obj_names <- objn[2:(no + 1)]
    if (anyNA(c(all_o, sqc, att, dec, ifc))) {
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
    obj_mat <- matrix(all_o, ncol = lo, byrow = T)
    if (!identical(c(1:nrow(obj_mat)), as.integer(sort(unique(sqc))))) {
        stop("No match between number of objects and sqc argument")
    }
    if (length(att) == 1) {
        att <- rep(att, length(sqc))
    }
    if (length(dec) == 1) {
        dec <- rep(dec, length(sqc))
    }
    if (length(ifc) == 1) {
        ifc <- rep(ifc, length(sqc))
    }
    if (max(att) > 1 | min(att) < 0) {
        stop("att must between 0 and 1")
    }
    if (length(sqc) != length(att)) {
        stop("length sqc doesn't match length att")
    }
    if (max(dec) > 0 | min(dec) < -1) {
        stop("dec must between 0 and -1")
    }
    if (length(sqc) != length(dec)) {
        stop("length sqc doesn't match length dec")
    }
    if (max(ifc) > 0 | min(ifc) < -1) {
        stop("ifc must between 0 and -1")
    }
    if (length(sqc) != length(ifc)) {
        stop("length sqc doesn't match length ifc")
    }
    if (!is.logical(rdm_weights)) {
        stop("rdm_weights must be TRUE or FALSE")
    }
    if (noise < 0 | noise > 1) {
        stop("noise must netween 0 and 1")
    }
    pres_mat <- matrix(rep(0, length(sqc) * lo), ncol = lo)
    for (i in 1:length(sqc)) {
        pres_mat[i, ] <- obj_mat[sqc[i], ]
    }
    akt_mat <- apply(pres_mat, 1, akt_fct2)
    akt_mat_unique <- apply(obj_mat, 1, akt_fct2)
    if (noise > 0) {
        noise <- round(noise * lo)
        akt_mat <- apply(akt_mat, 2, FUN = noise_fct, noi = noise,
                         lo = lo)
    }
    ifelse(rdm_weights == TRUE,
           wei_mat <- matrix(sample(1:1000 / 1000, lo ^ 2, replace = T),
                             ncol = 1),
           wei_mat <- matrix(rep(0, lo ^ 2), ncol = 1))
    learn_mat <- wei_mat
    end_weights <- vector("list", length(sqc))
    for (i in 1:ncol(akt_mat)) {
        learn_mat <- ifelse(
            akt_mat[, i] == 2,
            learn_mat + (att[i] * (1 - learn_mat)),
            ifelse(
                akt_mat[, i] == 1,
                learn_mat + (ifc[i] * learn_mat),
                learn_mat + (dec[i] * learn_mat)
                )
            )
        end_weights[[i]] <- learn_mat
    }
    lew <- end_weights[[length(sqc)]]
    pass_est <- NULL
    for (i in 1:max(sqc)) {
        pass_est[i] <- sum(
            lew[which(akt_mat[, which(sqc == i)[1]] == 2)]
            )
    }
    weight_progress <- NULL
    for (i in 1:no) {
        index <- which(akt_mat_unique[, i] == 2)
        weight_progress[[i]] <- unlist(
            lapply(end_weights, FUN = function(x) x[index])
            )
    }
    weight_progress <- apply(
        do.call(rbind, weight_progress), 2, stand_to_1)
    weight_progress <- lapply(
        seq_len(nrow(weight_progress)),
        function(i) weight_progress[i,])
    perc_est <- pass_est / sum(pass_est)
    tot_freq <- unname(table(sqc))
    out <- list(pass_est, perc_est, tot_freq, obj_names,
                weight_progress, "PASS1")
    names(out) <- c("raw_estimate", "percent_estimate", "input_freq",
                    "object_names", "progress", "what")
    class(out) <- c("JoF")
    return(out)
}
