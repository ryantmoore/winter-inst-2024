## ----setup, include=FALSE-----------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----warning = FALSE, echo=FALSE, eval=FALSE----------------------------
## library(knitr)
## us <- c("LucasA", "Carine", "Tanesia", "Hannah", "Kelly", "Marc", "JessieG", "Jocelyn", "LucasG", "Sophie", "Kathleen", "Cameron", "Lauren", "JessicaK", "Bryce", "Mark", "Ethan", "Edward", "Erin", "Milika", "Zeinabou", "Hubbert", "Olan", "Kate", "AndrewZ")
## us <- sample(us)
## nrows_us <- ceiling(length(us) / 2)
## One <- us[1:nrows_us]
## Two <- us[(nrows_us + 1):length(us)]
## if(length(Two) < length(One)){Two <- c(Two, "")}
## kable(cbind(One, Two))


## ----warning = FALSE, message=FALSE-------------------------------------
library(here)
library(tidyverse)

# Bayes:
library(coda)
library(MCMCpack)

# LASSO, ridge, elastic net:
library(glmnet)

# tidymodels:
library(tidymodels)


## -----------------------------------------------------------------------
# Set random seed, so that samples, train-test sets consistent:
set.seed(233559574) 


## ----warning = FALSE, message=FALSE-------------------------------------
social <- read_csv("http://j.mp/2Et71U0")

social <- social |> mutate(age = 2006 - yearofbirth,
                            isFemale = (sex == "female"),
                            isFemale = as.numeric(isFemale),
                            sentNeighbors = (messages == "Neighbors"),
                            sentNeighbors = as.numeric(sentNeighbors))

social <- social |> sample_n(50000)


## ----warning = FALSE----------------------------------------------------
mc_post <- MCMClogit(primary2006 ~ messages, data = social)


## ----warning = FALSE, results = 'hide', echo = FALSE--------------------
mc_post_full <- MCMClogit(primary2006 ~ primary2004 + age + I(age ^ 2) + messages, 
                          data = social,
                          burnin = 500,
                          mcmc = 2000,
                          thin = 2)


## ---- eval=FALSE--------------------------------------------------------
## HPDinterval(mc_post_full)


## ----warning = FALSE, eval=FALSE----------------------------------------
## pdf("mcmc_diagnose.pdf")
## plot(mc_post_full)
## dev.off()


## ----warning = FALSE----------------------------------------------------
mean(mc_post_full[, "messagesHawthorne"] < mc_post_full[, "messagesNeighbors"])


## ----warning = FALSE----------------------------------------------------
predictors <- c("isFemale", "primary2004", "sentNeighbors", "hhsize", "age")

X <- social[, predictors]


## ----warning = FALSE----------------------------------------------------
X <- X |> mutate(age2 = age^2,
                  age3 = age^3,
                  age4 = age^4,
                  age5 = age^5) |>
  as.matrix()

# Extract outcome as raw numeric vector, for glmnet:
y <- social[, "primary2006"] |> unlist() |> as.numeric()


## ----estLSall, warning = FALSE------------------------------------------
lm_out <- lm(primary2006 ~ isFemale + primary2004 + sentNeighbors + hhsize + 
               age + I(age ^ 2) + I(age ^ 3) + I(age ^ 4) + I(age ^ 5), 
             data = social)

coefs_lm <- coef(lm_out)


## ----warning = FALSE----------------------------------------------------
lasso_out <- glmnet(X, y, alpha = 1)


## ----warning = FALSE----------------------------------------------------
cv_lasso_out <- cv.glmnet(X, y, alpha = 1)


## ----warning = FALSE----------------------------------------------------
coef(cv_lasso_out, s = "lambda.min")


## ----warning = FALSE----------------------------------------------------
coef(cv_lasso_out, s = "lambda.1se")

coefs_lasso <- coef(cv_lasso_out, s = "lambda.1se")


## ----warning = FALSE----------------------------------------------------
cbind(coefs_lasso, coefs_lm)

