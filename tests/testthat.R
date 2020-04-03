library(testthat)
library(JoF)
RNGkind(sample.kind = "Rejection")
# TEST PASS
o1 <- c(0, 0, 0, 1)
o2 <- c(0, 0, 1, 0)
o3 <- c(0, 1, 0, 0)
o4 <- c(1, 0, 0, 0)
all_o <- c(o1, o2, o3, o4)
sqc1 <- c(1, 1, 1, 1, 2, 2, 2, 3, 3, 4)
att <- rep(0.1, length(sqc1))
dec <- rep(-0.05, length(sqc1))
ifc <- dec / 2
x <- o1
y <- o2
lo <- length(x)

expect_error(PASS1(c(o1,NA), o2, o3, o4, sqc = c(sqc1, 5),
                   att = att, dec = dec,
                   ifc = ifc, rdm_weights = FALSE, noise = 0),
             "NAs in Input not allowed")
expect_error(PASS1(o1, o2, o3, o4,
                   sqc = c(sqc1, NA), att = att, dec = dec,
                   ifc = ifc, rdm_weights = F, noise = 0),
             "NAs in Input not allowed")
expect_error(PASS1(o1, o2, o3, o4,
                   sqc = c(sqc1, 5), att = att, dec = dec,
                   ifc = ifc, rdm_weights = F, noise = 0),
             "No match between number of objects and sqc argument")
expect_error(PASS1(o1, o2, o3, o4,
                   sqc = c(sqc1), att = -1.3, dec = dec,
                   ifc = ifc, rdm_weights = F, noise = 0),
             "att must between 0 and 1")
expect_error(PASS1(o1, o2, o3, o4,
                   sqc = c(sqc1), att = att, dec = 3,
                   ifc = ifc, rdm_weights = F, noise = 0),
             "dec must between 0 and -1")
expect_error(PASS1(o1, o2, o3, o4,
                   sqc = c(sqc1), att = c(att, 1), dec = 3,
                   ifc = ifc, rdm_weights = F, noise = 0),
             "length sqc doesn't match length at")
expect_error(PASS1(o1, o2, o3, o4,
                   sqc = c(sqc1), att = c(att), dec = dec,
                   ifc = ifc, rdm_weights = 5, noise = 0),
             "rdm_weights must be TRUE or FALSE")
expect_error(PASS1(o1, o2, o3, o4,
                   sqc = c(sqc1), att = c(att), dec = dec,
                   ifc = ifc, rdm_weights = F, noise = 8),
             "noise must netween 0 and 1")
x <- PASS1(o1, o2, o3, o4,
           sqc = sqc1, att = att, dec = dec,
           ifc = ifc, rdm_weights = F, noise = 0)
expect_equal(round(x$raw_estimate, 2), c(0.25, 0.23, 0.18, 0.1))
x <- PASS1(o1, o2, o3, att = att,
           sqc = c(3, 2, 1, 1, 2, 3, 1, 1, 1, 1), dec = dec,
    ifc = ifc, rdm_weights = F, noise = 0)
expect_equal(round(x$percent_estimate, 2), c(0.62, 0.19, 0.19))
x <- PASS1(o1, o2, o3, att = att,
           sqc = c(3, 2, 1, 1, 2, 3, 1, 1, 1, 1), dec = dec,
           ifc = ifc, rdm_weights = F)
expect_equal(round(x$percent_estimate, 2), c(0.62, 0.19, 0.19))
x <- PASS1(o1, o2, o3, att = 0.1,
           sqc = c(3, 2, 1, 1, 2, 3, 1, 1, 1, 1), dec = -0.05,
    ifc = -0.025, rdm_weights = F)
expect_equal(round(x$percent_estimate, 2), c(0.62, 0.19, 0.19))
set.seed(345)
x <- PASS1(o1, o2, o3, att = att,
           sqc = c(3, 2, 1, 1, 2, 3, 1, 1, 1, 1), dec = dec,
           ifc = ifc)
expect_equal(round(x$percent_estimate, 2), c(0.43, 0.35, 0.22))

############################ PASS 2 Test
o1 <- replace(rep(0, 9), 1, 1)
o2 <- replace(rep(0, 9), 2, 1)
o3 <- replace(rep(0, 9), 3, 1)
o4 <- replace(rep(0, 9), 4, 1)
o5 <- replace(rep(0, 9), 5, 1)
o6 <- replace(rep(0, 9), 6, 1)
o7 <- replace(rep(0, 9), 7, 1)
o8 <- replace(rep(0, 9), 8, 1)
o9 <- replace(rep(0, 9), 9, 1)
rf1 <- rep(1:9, 9:1)
att <- rep(0.09, sum(9:1))

## test der Parameter
set.seed(567)
x <- PASS2(o1, o2, o3, o4, o5, o6, o7, o8, o9,
           sqc = rf1, att = att, rdm_weights = F, noise = 0)
expect_equal(round(x$percent_estimate, 2),
             c(0.06, 0.16, 0.09, 0.14, 0.18, 0.15, 0.10, 0.08, 0.04))
set.seed(567)
x <- PASS2(o1, o2, o3, o4, o5, o6, o7, o8, o9,
           sqc = rf1, att = 0.09, rdm_weights = F, noise = 0)
expect_equal(round(x$percent_estimate, 2),
             c(0.06, 0.16, 0.09, 0.14, .18, 0.15, 0.10, 0.08, 0.04))
expect_error(PASS2(c(NA, o1), o2, o3, o4, o5, o6, o7, o8, o9,
                   sqc = rf1, att = 0.09, rdm_weights = F, noise = 0),
             "NAs in Input not allowed")
expect_error(PASS2(c(o1), o2, o3, o4, o5, o6, o7, o8, o9,
                   sqc = c(NA, rf1), att = 0.09, rdm_weights = F,
                   noise = 0),
             "NAs in Input not allowed")
expect_error(PASS2(o1, o2, o3, o4, o5, o6, o7, o8, o9,
                   sqc = rf1, att = NA, rdm_weights = F, noise = 0),
             "NAs in Input not allowed")
expect_error(PASS2(o1, o2, o3, o4, o5, o6, o7, o8, c(o9, 0),
                   sqc = rf1, att = att, rdm_weights = F, noise = 0),
             "Input has not the same length")
expect_error(PASS2(o1, o2, o3, o4, o5, o6, o7, o8, c(o9[1:8], -1),
                   sqc = rf1, att = att, rdm_weights = F, noise = 0),
             "Input must 0 or 1")
expect_error(PASS2(o1, o2, o3, o4, o5, o6, o7, o8, c(o9),
                   sqc = c(0, rf1), att = att, rdm_weights = F,
                   noise = 0),
             "No match between number of objects and sqc argument")
expect_error(PASS2(o1, o2, o3, o4, o5, o6, o7, o8, c(o9),
                   sqc = c(rf1), att = c(3, att), rdm_weights = F,
                   noise = 0),
             "att must between 0 and 1")
expect_error(PASS2(o1, o2, o3, o4, o5, o6, o7, o8, c(o9),
                   sqc = c(rf1), att = att, rdm_weights = -2, noise = 0),
             "rdm_weights must be TRUE or FALSE")
expect_error(PASS2(o1, o2, o3, o4, o5, o6, o7, o8, c(o9),
                   sqc = c(rf1)[2:45], att = att, rdm_weights = T,
                   noise = 0),
             "length sqc doesn't match length att")
expect_error(PASS2(o1, o2, o3, o4, o5, o6, o7, o8, c(o9),
                   sqc = c(rf1), att = att, rdm_weights = T, noise = -1),
             "noise must netween 0 and 1")
expect_error(PASS2(o1, o2, o3, o4, o5, o6, o7, o8, c(o9),
                   sqc = c(rf1), att = att, rdm_weights = T, noise = 0,
                   n_output_units = "one"),
             "either half or numeric values are allowed")
expect_error(PASS2(o1, o2, o3, o4, o5, o6, o7, o8, c(o9),
                   sqc = c(rf1), att = att, rdm_weights = T, noise = 0,
                   n_output_units = 10),
             "n_output_units greater than number of inputunits")
expect_error(PASS2(o1, o2, o3, o4, o5, o6, o7, o8, c(o9),
                   sqc = c(rf1), att = att, rdm_weights = T, noise = 0,
                   n_output_units = -10),
             "n_output_units can't be less than 1")
set.seed(2019)
x <- PASS2(o1, o2, o3, o4, o5, o6, o7, o8, o9,
           sqc = c(rf1), att = att, rdm_weights = F, noise = 0)
expect_equal(round(x$percent_estimate, 2),
             c(0.17, 0.06, 0.21, 0.09, 0.12, 0.14, 0.11, 0.07, 0.04))
set.seed(2019)
x <- PASS2(o1, o2, o3, o4, o5, o6, o7, o8, o9,
           sqc = c(rf1), att = att, rdm_weights = F, noise = 0,
           n_output_units = 8)
expect_equal(round(x$percent_estimate, 2),
             c(0.16, 0.07, 0.20, 0.10, 0.14, 0.13, 0.10, 0.07, 0.04))
set.seed(2019)
x <- PASS2(o1, o2, o3, o4, o5, o6, o7, o8, o9,
           sqc = c(rf1), att = att, rdm_weights = F, noise = 0)
expect_equal(round(x$percent_estimate, 2),
             c(0.17, 0.06, 0.21, 0.09, 0.12, 0.14, 0.11, 0.07, 0.04))
set.seed(2019)
o1 <- c(1, 0, 0, 0, 0)
o2 <- c(0, 1, 0, 0, 0)
o3 <- c(0, 0, 1, 0, 0)
o4 <- c(0, 0, 0, 1, 0)
x <- PASS2(o1, o2, o3, o4, n_output_units = 3,
           sqc = sample(rep(1:4, c(10, 20, 30, 40)), 100),
           att = rep(0.02, 100), rdm_weights = F, noise = 0)
expect_equal(round(x$percent_estimate, 2), c(0.09, 0.19, 0.32, 0.39))
set.seed(876)
x <- PASS2(o1, o2, o3, o4, n_output_units = 3,
           sqc = sample(rep(1:4, c(10, 20, 30, 40)), 100),
           att = rep(0.02, 100), rdm_weights = F, noise = 0.6)
expect_equal(round(x$percent_estimate, 2), c(0.22, 0.23, 0.28, 0.28))
#################### MINERVA 2 Objektgruppe A für PASS
o1 <- replace(rep(0, 9), 1, 1)
o2 <- replace(rep(0, 9), 2, 1)
o3 <- replace(rep(0, 9), 3, 1)
o4 <- replace(rep(0, 9), 4, 1)
o5 <- replace(rep(0, 9), 5, 1)
o6 <- replace(rep(0, 9), 6, 1)
o7 <- replace(rep(0, 9), 7, 1)
o8 <- replace(rep(0, 9), 8, 1)
o9 <- replace(rep(0, 9), 9, 1)
o1m <- replace(o1, which(o1 == 0), -1)
o2m <- replace(o2, which(o2 == 0), -1)
o3m <- replace(o3, which(o3 == 0), -1)
o4m <- replace(o4, which(o4 == 0), -1)
o5m <- replace(o5, which(o5 == 0), -1)
o6m <- replace(o6, which(o6 == 0), -1)
o7m <- replace(o7, which(o7 == 0), -1)
o8m <- replace(o8, which(o8 == 0), -1)
o9m <- replace(o9, which(o9 == 0), -1)
expect_error(MINERVA2(o1m, sqc = c(1, 1, 1),
                      L = c(0.5, 0.8, 0.9)),
             "at least two inputs are needed")
expect_error(MINERVA2(o1m, c(o2m, NA), sqc = c(1, 1, 1),
                      L = c(0.5, 0.8, 0.9)),
             "NAs are not allowed")
expect_error(MINERVA2(o1m, c(o2m), sqc = c(1, 1, 1),
                      L = c(0.5, 0.8, 0.9)),
             "sqc doesn't match number of inputs")
expect_error(MINERVA2(o1m, c(o2m), sqc = c(1, 2),
                      L = c(NA, 0.8, 0.9)),
             "NAs are not allowed")
expect_error(MINERVA2(o1m, c(o2m), sqc = c(1, 2),
                      L = c(NA, 0.8, 0.9), dec = "curve"),
             "NAs are not allowed")
expect_error(MINERVA2(c(1:9), c(o2m), sqc = c(1, 2),
                      L = c(0.8, 0.9)),
             "Input must be -1, 0 or 1")
expect_error(MINERVA2(o1m, c(o2m), sqc = c(1, 2),
                      L = c(0.8, -0.9)),
             "L must between 0 and 1")
expect_error(MINERVA2(o1m, c(o2m), sqc = c(1, 2),
                      L = c(0.8, 0.9), dec = 0.9),
             "length decay doesn't match length sqc")
expect_error(MINERVA2(o1m, o2m, o3m, o4m, o5m, o6m, o7m, o8m, o9m,
                      sqc = rep(1:9, c(3, 3, 3, 5, 5, 5, 8, 8, 8)),
                      L = rep(c(0.7, 0.8, 0.9), 3), dec = "curve"),
             "length of L doesn't match length of sqc"
)
set.seed(6789)
x <- MINERVA2(o1m, o2m, o3m, o4m, o5m, o6m, o7m, o8m, o9m,
              sqc = rep(1:9, c(3, 3, 3, 5, 5, 5, 8, 8, 8)),
              L = rep(c(0.7, 0.8, 0.9), 16), dec = "curve")
expect_equal(round(x$percent_estimate, 1), c(rep(0.1, 8), 0.2))
set.seed(789)
x <- MINERVA2(o1m, o2m, o3m, sqc = c(1, 2, 3),
              L = c(0.5, 0.8, 0.9), dec = c(1, 1, 1))
expect_equal(round(x$percent_estimate, 2), c(0.22, 0.39, 0.39))
set.seed(789)
x <- MINERVA2(o1m, o2m, o3m, sqc = c(1, 2, 3),
              L = c(0.5, 0.8, 0.9), dec = NULL)
expect_equal(round(x$percent_estimate, 2), c(0.22, 0.39, 0.39))
H1 <- c(0, 0, 1, 0, 0, 1, 0, 1, -1)
H2 <- c(-1, 0, 0, 1, 0, 0, 1, 0, 0)
o1 <- H1
o2 <- H2
dec <- NULL
x <- MINERVA2(H1, H2, sqc = c(1, 2, 2, 2, 2, 2, 2, 2, 2, 2), L = 1)
expect_equal(x$raw_estimate, c(1,9))

expect_equal(round(x$percent_estimate, 2), c(0.1, 0.9))
H1 <- c(0, 0, 1, 0, 0, 1, 0, -1, -1)
H2 <- c(0, 0, 1, -1, 0, 1, 1, -1, -1)
H3 <- c(-1, 0, 0, 1, 0, 0, 1, 0, 0)
H4 <- c(-1, 1, 0, 1, 0, -1, 1, -1, 0)
x <- MINERVA2(H1, H2, H3, H4, sqc = c(1, 2, 3, 4), L = 1)
expect_equal(round(x$raw_estimate,2), c(1.3, 1.3, 1.12, 1.12))
o1m <- rep(-1, 25)
o1m <- replace(x = o1m, list = c(1, 9, 18, 23), 1)
o2m <- rep(-1, 25)
o2m <- replace(x = o2m, list = c(4, 5, 8, 11, 15, 17), 1)
o3m <- rep(-1, 25)
o3m <- replace(x = o3m, list = c(3, 7, 10, 12, 16, 19, 20, 22), 1)
set.seed(2000)
x <- MINERVA2(o1m, o2m, o3m, sqc = rep(1:3, 5), L = 0.92, dec = NULL)
expect_equal(round(x$percent_estimate,2), c(.34,.34,.32))
# test Recency in Curve
o1 <- replace(rep(0, 9), 1, 1)
o2 <- replace(rep(0, 9), 2, 1)
o3 <- replace(rep(0, 9), 3, 1)
x <- MINERVA2(o1, o2, o3, sqc = c(1, 1, 1, 2, 2, 2, 3, 3, 3),
              L = 0.92, dec = "curve")
expect_equal(round(x$percent_estimate,2), c(0.09, .24, .67))
# manual primacy effect
x <- MINERVA2(o1, o2, o3, sqc = c(1, 2, 3, 1, 2, 3, 1, 2, 3),
              L = 0.92, dec = c((9:1) / 10))
expect_equal(round(x$raw_estimate, 2), c(.76, .51, .07))
############################### TODAM 2
o1t <- c(0.64674833, 0.21434930, -0.15687620, -0.58940477,
         0.13129742, -0.14824431, 0.04138066, -0.10147358,
         -0.03777685)
o2t <- c(-0.15830435, 0.77803646, -0.38283038, -0.23417027,
         0.10707975, 0.09012698, -0.09689966, 0.05526582,
         -0.15830435)
o3t <- c(-0.24756320, 0.04604810, 0.70896125, -0.33098683,
         0.08848085, 0.16114108, 0.13688848, -0.31540654,
         -0.24756320)
o4t <- c(0.06802671, -0.04041221, -0.15647938, 0.77154798,
         -0.40431648, -0.08927660, -0.28634534, 0.06922861,
         0.06802671)
o5t <- c(-0.123004787, -0.466563581, -0.385104075, 0.364176903,
         0.589158210, 0.027406206, 0.110050375, 0.006885537,
         -0.123004787)
o6t <-  c(0.210165937, 0.007638298, -0.126493013, -0.608976726,
          0.348087230, 0.386898230, -0.374072158, -0.053413736,
          0.210165937)
o7t <- c(-0.51010930, -0.08674326,  0.10833918, 0.21782265,
         0.00103713, 0.01011938, 0.43715898, 0.33248453,
         -0.51010930)
o8t <- c(-0.02331016, -0.23927021, -0.13157296, -0.15653227,
         -0.20945133, -0.02996638, -0.04779825,  0.86121172,
         -0.02331016)
o9t <- c(-0.16744378, -0.19979342,  0.23984618,  0.15121136,
         0.04783422, -0.44107828, -0.44440975,  0.32019204,
         0.49364143)
rf <- rep(1:9, 1:9)
expect_error(TODAM2(o1t, gamma = rep(c(0.7, 0.8, 0.9), 3),
                    alpha = 0.95, sqc = rf),
             "at least two inputs are needed")
expect_error(TODAM2(o1t, o2t, o3t, o4t, o5t, o6t, o7t, o8t, o9t,
                    gamma = rep(c(NA, 0.8, 0.9), 3), alpha = 0.95,
                    sqc = rf),
             "NAs are not allowed")
expect_error(TODAM2(o1t, o2t, o3t, o4t, o5t, o6t, o7t, o8t, o9t,
                    gamma = rep(c(7, 0.8, 0.9), 15), alpha = 0.95,
                    sqc = rf),
             "gamma must between 0 and 1")
expect_error(TODAM2(o1t, o2t, o3t, o4t, o5t, o6t, o7t, o8t, o9t,
                    gamma = rep(c(7, 0.8, 0.9), 4), alpha = 0.95,
                    sqc = rf),
             "length of gamma doesn't match length of sqc")
expect_error(TODAM2(o1t, o2t, o3t, o4t, o5t, o6t, o7t, o8t, o9t,
                    gamma = rep(c(7, 0.8, 0.9), 15),
                    alpha = rep(c(7, 0.8, 0.9), 35), sqc = rf),
             "length of alpha doesn't match length of sqc")
expect_error(TODAM2(o1t, o2t, o3t, o4t, o5t, o6t, o7t, o8t, o9t,
                    gamma = rep(c(0.7, 0.8, 0.9), 3),
                    alpha = rep(c(0.7, 0.8, 0.9), 3), sqc = c(10, rf)),
             "sqc doesn't match number of inputs")
expect_error(TODAM2(o1t, o2t, gamma = rep(c(0.7, 0.8, 0.9), 3),
                    alpha = 0.95, sqc = rf),
             "sqc doesn't match number of inputs")
set.seed(987)
x <- TODAM2(o1t, o2t, o3t, o4t, o5t, o6t, o7t, o8t, o9t,
            gamma = rep(c(0.7, 0.8, 0.9), 15),
            alpha = rep(c(0.7, 0.8, 0.9), 15), sqc = c(rf))
expect_equal(round(x$percent_estimate, 2),
             c(0.11, 0.07, 0.03, .17, 0.00, 0.00, 0.00, .00, .62))
o1 <- replace(rep(0, 9), 1, 1)
o2 <- replace(rep(0, 9), 2, 1)
o3 <- replace(rep(0, 9), 3, 1)
o4 <- replace(rep(0, 9), 4, 1)
o5 <- replace(rep(0, 9), 5, 1)
o6 <- replace(rep(0, 9), 6, 1)
o7 <- replace(rep(0, 9), 7, 1)
o8 <- replace(rep(0, 9), 8, 1)
o9 <- replace(rep(0, 9), 9, 1)

x <- TODAM2(o1, o2, o3, o4, o5, o6, o7, o8, o9,
            alpha = 1, gamma = 1, sqc = c(rf))
expect_equal(round(x$percent_estimate, 2),
             c(0.02, 0.04, 0.07, 0.09, 0.11, 0.13, 0.16, 0.18, 0.2))
x <- TODAM2(o1, o2, o3, o4, o5, o6, o7, o8, o9,
            gamma = 1, sqc = c(rf))
expect_equal(round(x$percent_estimate, 2),
             c(0.02, 0.04, 0.07, 0.09, 0.11, 0.13, 0.16, 0.18, 0.2))
x <- TODAM2(o1, o2, o3, o4, o5, o6, o7, o8, o9,
            alpha = 1, sqc = c(rf))
expect_equal(round(x$percent_estimate, 2),
             c(0.02, 0.04, 0.07, 0.09, 0.11, 0.13, 0.16, 0.18, 0.2))
x <- TODAM2(o1, o2, o3, o4, o5, o6, o7, o8, o9,
            sqc = c(rf), alpha = 1)
expect_equal(round(x$percent_estimate, 2),
             c(0.02, 0.04, 0.07, 0.09, 0.11, 0.13, 0.16, 0.18, 0.2))

