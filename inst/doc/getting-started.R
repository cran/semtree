## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(semtree)
library(OpenMx)

## ----simdata------------------------------------------------------------------
set.seed(23)
N <- 1000
M <- 5
icept <- rnorm(N, 10, sd = 4)
slope <- rnorm(N, 3, sd = 1.2)
p1 <- sample(c(0, 1), size = N, replace = TRUE)
loadings <- 0:4
x <-
  (slope + p1 * 5) %*% t(loadings) + 
  matrix(rep(icept, each = M), byrow = TRUE, ncol = M) + 
  rnorm(N * M, sd = .08)
growth.data <- data.frame(x, factor(p1))
names(growth.data) <- c(paste0("X", 1:M), "P1")

## -----------------------------------------------------------------------------
manifests <- names(growth.data)[1:5]
growthCurveModel <- mxModel("Linear Growth Curve Model Path Specification",
    type="RAM",
       manifestVars=manifests,
    latentVars=c("intercept","slope"),
    mxData(growth.data, type="raw"),
    # residual variances
    mxPath(
        from=manifests,
        arrows=2,
        free=TRUE,
        values = c(.1, .1, .1, .1, .1),
        labels=c("residual","residual","residual","residual","residual")
    ),
    # latent variances and covariance
    mxPath(
        from=c("intercept","slope"),
        arrows=2,
        connect="unique.pairs",
        free=TRUE,
        values=c(2, 0, 1),
        labels=c("vari", "cov", "vars")
    ),
    # intercept loadings
    mxPath(
        from="intercept",
        to=manifests,
        arrows=1,
        free=FALSE,
        values=c(1, 1, 1, 1, 1)
    ),
    # slope loadings
    mxPath(
        from="slope",
        to=manifests,
        arrows=1,
        free=FALSE,
        values=c(0, 1, 2, 3, 4)
    ),
    # manifest means
    mxPath(
        from="one",
        to=manifests,
        arrows=1,
        free=FALSE,
        values=c(0, 0, 0, 0, 0)
    ),
    # latent means
    mxPath(
        from="one",
        to=c("intercept", "slope"),
        arrows=1,
        free=TRUE,
        values=c(1, 1),
        labels=c("meani", "means")
    )
) # close model

# fit the model to the entire dataset
growthCurveModel <- mxRun(growthCurveModel)

## ----message=FALSE,warning=FALSE,results="hide"-------------------------------
tree <- semtree(model = growthCurveModel, 
                data = growth.data)

## -----------------------------------------------------------------------------
plot(tree)

