## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(semtree)

## -----------------------------------------------------------------------------
library(psychTools)
data(affect)

knitr::kable(head(affect))

affect$Film <- as.factor(affect$Film)
affect$lie <- as.ordered(affect$lie)
affect$imp <- as.ordered(affect$imp)

## -----------------------------------------------------------------------------
library(OpenMx)
manifests<-c("state2")
latents<-c()
model <- mxModel("Simple Model", 
type="RAM",
manifestVars = manifests,
latentVars = latents,
mxPath(from="one",to=manifests, free=c(TRUE), value=c(1.0) , arrows=1, label=c("mu") ),
mxPath(from=manifests,to=manifests, free=c(TRUE), value=c(1.0) , arrows=2, label=c("sigma2") ),
mxData(affect, type = "raw")
);

result <- mxRun(model)

## -----------------------------------------------------------------------------
summary(result)

## -----------------------------------------------------------------------------
control <- semforest.control(num.trees = 5)
print(control)

## -----------------------------------------------------------------------------
forest <- semforest( model=model, data = affect, control = control )

## -----------------------------------------------------------------------------
vim <- varimp(forest)
print(vim, sort.values=TRUE)
plot(vim)

