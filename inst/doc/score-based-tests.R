## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(psychTools)
data(affect)

affect$Film <- factor(affect$Film, ordered = FALSE, 
                      labels=c("Frontline", "Halloween", "Nat. Geographic","Parenthood"))


tree.data <- affect[,c("Film","neur","ext","soc","traitanx","NA1","PA1")] 
tree.data$DeltaPA <- affect$PA2-affect$PA1

knitr::kable(head(tree.data))

## -----------------------------------------------------------------------------
library(OpenMx)
manifests<-c("DeltaPA")
latents<-c()
model <- mxModel("Simple Model", 
type="RAM",
manifestVars = manifests,
latentVars = latents,
mxPath(from="one",to=manifests, free=c(TRUE), value=c(1.0) , arrows=1, label=c("mu") ),
mxPath(from=manifests,to=manifests, free=c(TRUE), value=c(1.0) , arrows=2, label=c("sigma2") ),
mxData(tree.data, type = "raw")
);

result <- mxRun(model)
summary(result)

## -----------------------------------------------------------------------------
library(semtree)
ctrl = semtree.control(
  method="score", 
  bonferroni = TRUE)

## ----message=FALSE, warning=FALSE, results="hide"-----------------------------
tree = semtree( model = result, 
                data = tree.data, 
                control=ctrl)

## ----out.width="75%",dpi=300--------------------------------------------------
plot(tree)

## ----dpi=300, out.width="100%"------------------------------------------------
tndata <- semtree::getTerminalNodes(tree)

cols <- viridis::plasma(nrow(tndata))

pl <- ggplot2::ggplot(data = data.frame(x = c(-20, 20)), ggplot2::aes(x))+
  ggplot2::xlab("Change in Positive Affect")

for (i in 1:nrow(tndata)) {
  pl <- pl + ggplot2::stat_function(fun = dnorm, 
      n = 101, col=cols[i], args = list(mean = tndata[i,2], sd = sqrt(tndata[i,1])))
}

plot(pl)

## ----dpi=300, out.width="100%"------------------------------------------------
i <- which.min(tndata$mu)

pl <- pl +ggplot2::geom_area(stat = "function", 
                             fun = function(x){dnorm(x,mean=tndata[i,2],sd=sqrt(tndata[i,1]))}, 
                             fill = cols[i])+
      ggplot2::geom_label(x=-10,y=.12, label="People with very high positive \naffect, which then watched\n a war film")

plot(pl)

