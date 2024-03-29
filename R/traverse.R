traverse.rec <- function(row, tree)
{

  if (tree$caption == "TERMINAL")
    return(tree$node_id)
  
  
  value <- tryCatch({
    row[[tree$rule$name]]
  }, error = function(cond) {
    message("ERROR! Incomplete dataset!")
    stop()
    return(NA)
  })
  
  if (is.na(value)) {
    if (is.null(tree$missing.model)) {
      return(tree$node_id)
      
    } else {
      value = predict(tree$missing.model, newdata = row)
    }
    
    
    
  }
  
  log.val <- NA
  
 # if (tree$rule$relation == ">=") {
    #if (is.ordered(value)) { value <- as.numeric(as.character(value)) }
    # CJ: I think ordered factors are handled incorrectly,
    # because you can have:
    # ses <- ordered(c(1,2,2,3,1,2), labels = c("low", "middle", "high"))
    # as.numeric(as.character(ses))
    # [1] NA NA NA NA NA NA
    # But this works:
    # ses[2] >= "high"
    # [1] FALSE
    # ses[2] >= "low"
    # [1] TRUE
  #  log.val <- value >= tree$rule$value
  #} else if (tree$rule$relation == "%in%") {
  #  log.val <- value %in% tree$rule$value
  #} else {
  #  stop("Comparison not supported in traverse.rec():", tree$rule)
  #}
  log.val = do.call(tree$rule$relation, list(value, tree$rule$value))
  
  if (!log.val)
  {
    return(traverse.rec(row, tree$left_child))
    
  } else {
    return(traverse.rec(row, tree$right_child))
    
    
  }
  
}

traverse <- function(tree, dataset)
{
  if (!is.null(tree$traverse.fun)) {
    return(tree$traverse.fun(dataset))
  }
  
  if (is(dataset, "data.frame")) {
    result <- rep(NA, dim(dataset)[1])
    for (i in 1:dim(dataset)[1]) {
      result[i] <- traverse.rec(row = dataset[i, ], tree = tree)
    }
    return(result)
  } else {
    return(apply(
      X = dataset,
      MARGIN = 1,
      FUN = traverse.rec,
      tree
    ))
  }
}
