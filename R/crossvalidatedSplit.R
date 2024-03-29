crossvalidatedSplit <- function(model=NULL, mydata=NULL, control=NULL, 
                                invariance=NULL, meta=NULL, constraints=NULL, ...) {

  if (!is.null(constraints$focus.parameters)) {
    stop("Method 'cv' currently does not support focus parameters!")
  }
  
 folds <- control$num.folds
 N <- dim(mydata)[1]
 fold_association <- rep(1:folds, N/folds + 1)[1:N]
 fold_association <- sample(fold_association, N, replace=F)

 

	#mvars <- length(model@manifestVars)
	n.comp <- 0

	LL.btn <- c()
	split.btn <- c()
	cov.btn.names <- c()
	cov.btn.cols <- c()
	cov.type <- c()
	cov.col <- c()
	cov.name <- c()
	
	LL.within <- c()
	within.split <- c()

	LL.baseline <- cvLikelihood(model, mydata, NULL, fold_association, NULL, control, invariance)

#for(c in (mvars+1):ncol(mydata)) {
for (cur_col in meta$covariate.ids)  {
	#case for factored covariates##############################
	if(is.factor(mydata[,cur_col])) {
		#unordered factors#####################################
		if(!is.ordered(mydata[,cur_col])) {
			var.type = 1
			v <- as.numeric(mydata[,cur_col])
			val.sets <- sort(union(v,v))
			if(length(val.sets) > 1) {
				#create binaries for comparison of all combinations
				result <- recodeAllSubsets(mydata[,cur_col],colnames(mydata)[cur_col])
				test1 <- c()
				test2 <- rep(NA, length(mydata[,cur_col]))
	
				for(j in 1:ncol(result$columns)) {
					for(i in 1:length(mydata[,cur_col])) {
						if(isTRUE(result$columns[i,j])) {test1[i] <- 1}
						else {test1[i] <- 0}
					}
					test1 <- as.factor(test1)
					test2 <- data.frame(test2, test1)
				}
				test2 <- test2[,-1]
				
				for(i in 1:(result$num_sets)) {
					#subset data for chosen value and store LL
					if(result$num_sets==1) {
						subset1 <- subset (mydata, as.numeric(test2) == 2)
						subset2 <- subset (mydata, as.numeric(test2) == 1)
						fa1 <- subset (fold_association, as.numeric(test2) == 2)
						fa2 <- subset (fold_association, as.numeric(test2) == 1)					
					}
					else {
						subset1 <- subset (mydata, as.numeric(test2[[i]]) == 2)	
						subset2 <- subset (mydata, as.numeric(test2[[i]]) == 1)
						fa1 <- subset (fold_association, as.numeric(test2[[i]]) == 2)
						fa2 <- subset (fold_association, as.numeric(test2[[i]]) == 1)	
					}
					if((nrow(subset1)>control$min.N)&(nrow(subset2)>control$min.N)){
					  #cat("Factors CV\n")
					  LL.temp <- cvLikelihood(model, subset1, subset2, fa1, fa2, control, invariance)
					}
					else{
					  LL.temp <- NA
					}
					#cat("LL.temp=",LL.temp,"for factors\n",sep=" ")
					#catch LLR for each comparison
					if(!is.na(LL.temp)){
					  LL.within <- cbind(LL.within, (LL.baseline-sum(LL.temp)))
					  within.split <- cbind(within.split, i)
					  cov.col <- cbind(cov.col, cur_col)
					  cov.name <- cbind(cov.name, colnames(mydata[cur_col]))
					  cov.type <- cbind(cov.type, var.type)
					  n.comp <- n.comp + 1
					}
				}
			}
			
			#LL.temp
			#LL.within
			#within.split
		}
		#ordered factors#########################################
		if(is.ordered(mydata[,cur_col])) {
			var.type = 2
			v <- as.numeric(as.character(mydata[,cur_col]))
			val.sets <- sort(union(v,v))
			if(length(val.sets) > 1) {
				for(i in 2:(length(val.sets))) {
					#subset data for chosen value and store LL
					subset1 <- subset (mydata, v > (val.sets[i]+val.sets[(i-1)])/2)					
					fa1 <- subset (fold_association, v > (val.sets[i]+val.sets[(i-1)])/2)
					fa2 <- subset (fold_association, v < (val.sets[i]+val.sets[(i-1)])/2)	
					subset2 <- subset (mydata, as.numeric(as.character(mydata[,cur_col])) < (val.sets[i]+val.sets[(i-1)])/2)
					if((nrow(subset1)>control$min.N)&(nrow(subset2)>control$min.N)){
					  LL.temp <- cvLikelihood(model, subset1, subset2, fa1, fa2, control, invariance)
					}
					else{
					  LL.temp <- NA
					}
					#cat("LL.temp=",LL.temp,"for ordered factors\n",sep=" ")
					#catch LLR for each comparison
					if(!is.na(LL.temp)){
					  LL.within <- cbind(LL.within, (LL.baseline-sum(LL.temp)))
					  within.split <- cbind(within.split, (val.sets[i]+val.sets[(i-1)])/2)
					  cov.col <- cbind(cov.col, cur_col)
					  cov.name <- cbind(cov.name, colnames(mydata[cur_col]))
					  cov.type <- cbind(cov.type, var.type)
					  n.comp <- n.comp + 1
					}
				}
			}
		}
	}
	
	#numeric (continuous) covariates################################
	if(is.numeric(mydata[,cur_col])) {
		var.type = 2
		v <- as.numeric(mydata[,cur_col])
		val.sets <- sort(union(v,v))
		#if(length(val.sets) < 30|!isTRUE(control$shortcut)){
		  if(length(val.sets) > 1) {
		    for(i in 2:(length(val.sets))) {
		      #subset data for chosen value and store LL
		      subset1 <- subset (mydata, as.numeric(mydata[,cur_col]) > (val.sets[i]+val.sets[(i-1)])/2)
		      subset2 <- subset (mydata, as.numeric(mydata[,cur_col]) < (val.sets[i]+val.sets[(i-1)])/2)
		      fa1 <- subset (fold_association, as.numeric(mydata[,cur_col]) > (val.sets[i]+val.sets[(i-1)])/2)
		      fa2 <- subset (fold_association, as.numeric(mydata[,cur_col]) < (val.sets[i]+val.sets[(i-1)])/2)
		      if((nrow(subset1)>control$min.N)&(nrow(subset2)>control$min.N)){
		        LL.temp <- cvLikelihood(model, subset1, subset2, fa1, fa2, control, invariance)
		      }
          else{
            LL.temp <- NA
          }
		      #cat("LL.temp=",LL.temp,"for cont. vars\n",sep=" ")
		      #catch LLR for each comparison
		      if(!is.na(LL.temp)){
		        LL.within <- cbind(LL.within, (LL.baseline-sum(LL.temp)))
		        within.split <- cbind(within.split, (val.sets[i]+val.sets[(i-1)])/2)
		        cov.col <- cbind(cov.col, cur_col)
		        cov.name <- cbind(cov.name, colnames(mydata[cur_col]))
		        cov.type <- cbind(cov.type, var.type)
		        n.comp <- n.comp + 1
		      }
		    }
		  }
		#}
	}
	
	#store the LL, split value and variable number for each cov that makes a possible split	
}

if(is.null(LL.within)){return(NULL)}

 btn.matrix <- rbind(LL.within,cov.name,cov.col,within.split)
 colnames(btn.matrix) <- c(paste("var",seq(1,ncol(btn.matrix)),sep=""))
 rownames(btn.matrix) <- c("LR","variable","column","split val")
 LL.max <- NA
 for(cur_col in 1:ncol(LL.within)) {
	if(cur_col == 1 | is.na(LL.max)) {
		LL.max <- LL.within[cur_col]
		split.max <- within.split[cur_col]
		name.max <- cov.name[cur_col]
		col.max <-cov.col[cur_col]
		type.max <- cov.type[cur_col]
	} else {
		if (LL.max < LL.within[cur_col]) {
			LL.max <- LL.within[cur_col]
			split.max <- within.split[cur_col]
			name.max <- cov.name[cur_col]
			col.max <-cov.col[cur_col]
			type.max <- cov.type[cur_col]
		}
	}
}

	return(list(LL.max=LL.max,split.max=split.max,name.max=name.max,
	col.max=col.max, type.max=type.max, n.comp=n.comp, btn.matrix=btn.matrix ))
}
