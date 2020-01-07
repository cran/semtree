## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, warning=FALSE, message=FALSE,echo=FALSE---------------------------
library(semtree)
library(MASS)

## ----eval=FALSE---------------------------------------------------------------
#  library(semtree)
#  
#  cnst <- semtree.constraints(local.invariance=NULL,
#  global.invariance=NULL,
#  focus.parameters=NULL)
#  
#  semtree(model.x, data=df, constraints=cnst)

## ----difsim-------------------------------------------------------------------
N <- 2000
p1 <- sample(size=N, x=c(0,1), replace=TRUE)
p2 <- sample(size=N, x=c(0,1), replace=TRUE)
lat <- rnorm(N,mean = 0+p1)
loadings <- c(.5,.8,.7,.9)
observed <- lat %*% t(loadings) + rnorm(N*length(loadings),sd = .1)
observed[,3]<-observed[,3]+p2*0.5*lat
cfa.sim <- data.frame(observed,p1,p2)
names(cfa.sim)[1:4] <- paste0("x",1:4)

## ----cfadefinition, dependson="difsim", echo=TRUE-----------------------------
require("OpenMx");
manifests<-c("x1","x2","x3","x4")
latents<-c("F")
model.cfa <- mxModel("CFA", type="RAM", manifestVars = manifests, 
                     latentVars = latents,
mxPath(from="F",to=c("x1","x2","x3","x4"), 
       free=c(TRUE,TRUE,TRUE,TRUE), value=c(1.0,1.0,1.0,1.0) , 
       arrows=1, label=c("F__x1","F__x2","F__x3","F__x4") ),
mxPath(from="one",to=c("x2","x3","x4"), 
       free=c(TRUE,TRUE,TRUE), value=c(1.0,1.0,1.0) , 
       arrows=1, label=c("const__x2","const__x3","const__x4") ),
mxPath(from="one",to=c("F"), free=c(TRUE), 
       value=c(1.0) , arrows=1, label=c("const__F") ),
mxPath(from="x1",to=c("x1"), free=c(TRUE), 
       value=c(1.0) , arrows=2, label=c("VAR_x1") ),
mxPath(from="x2",to=c("x2"), free=c(TRUE), 
       value=c(1.0) , arrows=2, label=c("VAR_x2") ),
mxPath(from="x3",to=c("x3"), free=c(TRUE), 
       value=c(1.0) , arrows=2, label=c("VAR_x3") ),
mxPath(from="x4",to=c("x4"), free=c(TRUE), 
       value=c(1.0) , arrows=2, label=c("VAR_x4") ),
mxPath(from="F",to=c("F"), free=c(FALSE), 
       value=c(1.0) , arrows=2, label=c("VAR_F") ),
mxPath(from="one",to=c("x1"), free=F, value=0, arrows=1),
mxData(cfa.sim, type = "raw")
);

## -----------------------------------------------------------------------------
tree.gc <- semtree(model.cfa, data=cfa.sim, constraints=
                   semtree.constraints(global.invariance = 
                                         c("F__x1","F__x2","F__x3","F__x4")))


## ----fig.height=3-------------------------------------------------------------
plot(tree.gc)

## ----warning=FALSE, message=FALSE, error=FALSE--------------------------------
tree.lc <- semtree(model.cfa, data=cfa.sim, constraints=
                   semtree.constraints(
                     local.invariance= c("F__x1","F__x2","F__x3","F__x4")))


## -----------------------------------------------------------------------------
plot(tree.lc)

## -----------------------------------------------------------------------------
set.seed(123)
N <- 1000
grp1 <- sample(x = c(0,1), size=N, replace=TRUE)
grp2 <- sample(x = c(0,1), size=N, replace=TRUE)
Sigma <- matrix(byrow=TRUE,
                        nrow=2,c(2,0.2,
                                0.2,1))
obs <- MASS::mvrnorm(N,mu=c(0,0),
                      Sigma=Sigma)
obs[,1] <- obs[,1] + ifelse(grp1,3,0)
obs[,2] <- obs[,2] + ifelse(grp2,3,0)
df.biv <- data.frame(obs, grp1, grp2)
names(df.biv)[1:2] <- paste0("x",1:2)


## -----------------------------------------------------------------------------
manifests<-c("x1","x2")
model.biv <- mxModel("Bivariate_Model", 
type="RAM",
manifestVars = manifests,
latentVars = c(),
mxPath(from="x1",to=c("x1","x2"), 
       free=c(TRUE,TRUE), value=c(1.0,.2) , 
       arrows=2, label=c("VAR_x1","COV_x1_x2") ),
mxPath(from="x2",to=c("x2"), free=c(TRUE), 
       value=c(1.0) , arrows=2, label=c("VAR_x2") ),
mxPath(from="one",to=c("x1","x2"), label=c("mu1","mu2"),
       free=TRUE, value=0, arrows=1),
mxData(df.biv, type = "raw")
);

## -----------------------------------------------------------------------------
result <- mxRun(model.biv)
summary(result)

## ----growbivtree--------------------------------------------------------------

tree.biv <- semtree(model.biv, data=df.biv)


## -----------------------------------------------------------------------------
plot(tree.biv)

## ----warning=FALSE, message=FALSE---------------------------------------------
require("ggplot2")
ggplot(data = df.biv, aes(x=x1, y=x2))+ 
  geom_density_2d()+ 
  theme_classic()

## -----------------------------------------------------------------------------
df.biv.pred <- data.frame(df.biv, 
  leaf=factor(getLeafs(tree=tree.biv, data = df.biv)))
ggplot(data = df.biv.pred, aes(x=x1, y=x2))+ 
  geom_density_2d(aes(colour=leaf))+ 
  theme_classic()

## -----------------------------------------------------------------------------

tree.biv2 <- semtree(model.biv, df.biv, constraints=
                      semtree.constraints(focus.parameters = "mu1"))

plot(tree.biv2)

## -----------------------------------------------------------------------------

tree.biv3 <- semtree(model.biv, df.biv, constraints=
                      semtree.constraints(focus.parameters = "mu2"))


## -----------------------------------------------------------------------------
plot(tree.biv3)

## -----------------------------------------------------------------------------

tree.biv4 <- semtree(model.biv, df.biv, constraints=
                      semtree.constraints(focus.parameters = "VAR_x2"))

plot(tree.biv4)


