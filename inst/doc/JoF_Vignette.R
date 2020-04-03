## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval=TRUE, include=TRUE--------------------------------------------------
# create 4 items
item1 <- c(1, 0, 0, 0) 
item2 <- c(0, 1, 0, 0)
item3 <- c(0, 0, 1, 0)
item4 <- c(0, 0, 0, 1)
# create a sequence
sqc1 <- c(1, 2, 3, 4, 2, 3, 3)

## ----eval=TRUE, include=TRUE--------------------------------------------------
# Simulation with PASS 1 
library(JoF)
set.seed(123)
fit_pass1 <- PASS1(item1, item2, item3, item4, 
                   sqc = sqc1, att = .1, ifc = -.025, dec = -.05,
                   rdm_weights = FALSE, noise = 0)
fit_pass1
plot(fit_pass1)

## ----eval=TRUE, include=TRUE--------------------------------------------------
# Simulation with PASS 2 
set.seed(123)
fit_pass2 <- PASS2(item1, item2, item3, item4, 
      sqc=sqc1, att = .1, n_output_units = "half",
      rdm_weights = FALSE, noise = 0)
# Simulation with MINERVA 2
fit_minerva2 <- MINERVA2( item1, item2, item3, item4, 
          sqc = sqc1, L = 1, dec=NULL)
fit_todam2 <- TODAM2(item1, item2, item3, item4, 
       sqc = sqc1, gamma = 1, alpha = 1)

matrix(
  round(c(fit_pass1$percent_estimate,fit_pass2$percent_estimate,
    fit_minerva2$percent_estimate,fit_todam2$percent_estimate),3),
  byrow = T, ncol = 4, 
  dimnames = list(
    c("PASS 1", "PASS 2", "MINERVA 2", "TODAM 2"),
    c("item1", "item2", "item3", "item4"))
  )

## ----eval=TRUE, include=TRUE--------------------------------------------------
# Simulation with 100 replications 
set.seed(123)
# PASS 1
p1 <- replicate(100,
               {
               random_sqc <- sample(sqc1, 7, FALSE)
               PASS1(item1, item2, item3, item4, 
                     sqc = random_sqc, 
                     att = .1, ifc = -.025, dec = -.05,
                     rdm_weights = FALSE, 
                     noise = 0 
                    )$percent_estimate
               }
)
# PASS 2
p2 <- replicate(100,
               {
               random_sqc <- sample(sqc1, 7, FALSE)
               PASS2(item1, item2, item3, item4, 
                     sqc = random_sqc, 
                     att = .1, 
                     n_output_units = "half",
                     rdm_weights = F, noise = 0
                     )$percent_estimate
               }
)
# MINERVA 2
m2 <- replicate(100,
               {
                random_sqc <- sample(sqc1, 7, FALSE)
               MINERVA2(item1, item2, item3, item4,
                        sqc = random_sqc, 
                        L = .9,
                        dec = "curve"
                        )$percent_estimate
               }
)
# TODAM 2
t2 <- replicate(100,
               {
               random_sqc <- sample(sqc1, 7, FALSE)
               TODAM2(item1, item2, item3, item4,
                      sqc = random_sqc, 
                      gamma = .9,
                      alpha = .9
                      )$percent_estimate
               }
)
matrix(round(
  c(rowMeans(p1), rowMeans(p2), rowMeans(m2), rowMeans(t2)), 3), 
  ncol = 4, byrow = T, 
  dimnames = list(c("PASS 1", "PASS 2", "MINERVA 2", "TODAM 2"),
                  c("Item 1", "Item 2", "Item 3", "Item 4"))
)
# theoretical frequancy
table(sqc1)/7

