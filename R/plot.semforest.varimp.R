#' @exportS3Method plot semforest.varimp
plot.semforest.varimp <-
  function(x,
           sort.values = T,
           aggregate = "mean",
           horiz = T,
           las = 1,
           convergence = F,
           scale = "absolute",
           xlim = NULL,
           head = NULL,
           tail = NULL,
           na.omit = TRUE,
           ...)
  {

    vimp <- x
    
    if (utils::hasName(vimp,"boruta")) {
      filter_ids <- 1:(ncol(vimp$importance)/2)
      vimp$var.names <- vimp$var.names[filter_ids]
      vimp$importance <- vimp$importance[,filter_ids]
      vimp$ll.baselines <- vimp$ll.baselines[filter_ids]
      x <- vimp
    }
    
    if (convergence) {
      if (!is(vimp$importance, "matrix")) {
        stop("You must run varimp with return.vector set to true!")
        
      }
      varimpConvergencePlot(vimp, na.omit = na.omit, ...)
      return()
      
    }
    

    if (is(vimp$importance, "matrix")) {
      x <- aggregateVarimp(vimp, aggregate, scale, na.omit)
    } else {
      x <- vimp$importance
      
      if (!na.omit) {
        x[is.na(x)] <- 0
      }
    }
    
    if (!is.null(vimp$importance.level1)) {
      x.level1 <-
        aggregateVarimp(vimp$importance.level1, aggregate, scale, na.omit)
    }
    
    vnames <- vimp$var.names
    if (sort.values) {
      # replace NAs with low number
      low <- min(x, na.rm = T) - 1
      filt <- is.na(x)
      x[filt] <- low
      
      srt <- sort(x, index.return = T)
      x <- x[srt$ix]
      if (!is.null(vimp$importance.level1)) {
        x.level1 <- x.level1[srt$ix]#
      }
      
      vnames <- vnames[srt$ix]
      
      x[x <= (low + .5)] <- NA
    }
    
    selection <- 1:length(x)
    
    if (!is.null(tail) && !is.null(head)) {
      selection <- c(1:tail, (length(x) - head + 1):length(x))
      #x <- x[c(1:tail, (length(x)-head+1):length(x))]
      # vnames <- vnames[c(1:tail, (length(vnames)-head+1):length(vnames))]
    } else {
      if (!is.null(tail)) {
        selection <- 1:tail
        # x<- x[1:tail]
        # vnames <- vnames[1:tail]
      }
      
      if (!is.null(head)) {
        selection <- (length(x) - head + 1):length(x)
        # x<- x[(length(x)-head+1):length(x)]
        #  vnames <- vnames[(length(vnames)-head+1):length(vnames)]
      }
      
    }
    
    x <- x[selection]
    vnames <- vnames[selection]
    if (exists("x.level1")) {
      #  this makes x to be all effects other than the "main effect"
      x.level1 <- x.level1[selection]
      x <- x - x.level1
    }
    
    if (is.null(xlim)) {
      xlim <- c(min(min(x, na.rm = T), 0)    , max(max(x, na.rm = T), 0))
    }
    
    linch <-  max(strwidth(vnames, "inch") + 0.4, na.rm = TRUE)
    par(mai = c(1.02, linch, 0.82, 0.42))
    
    col <- NULL
    if (utils::hasName(vimp,"boruta")) {
      col <- ifelse(vimp$filter,"grey","white")
    }
    
    # if (!exists("x.level1")) {
    barplot(
      x,
      names.arg = vnames,
      horiz = horiz,
      las = las,
      xlim = xlim,
      col = NULL,
      ...
    )
    if (utils::hasName(vimp,"boruta")) {
      graphics::abline(v=vimp$boruta_threshold,lwd=2)
    }

  }
