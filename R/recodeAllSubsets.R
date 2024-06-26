recodeAllSubsets <-
  function(v,
           name = "unknown",
           id = NA,
           growbool = F,
           use.levels = F) {
    if (use.levels & is.factor(v)) {
      values_set <- levels(v)
    } else {
      values_set <- sort(unique(v))
    }
    
    num_sets <- 2 ** (length(values_set) - 1)
    
    if (num_sets == 1) {
      return (NULL)
      
    }
    
    if (all(is.na(v))) {
      return(NULL)
    }
    
    names <- c()
    columns <- c()
    expressions <- c()
    
    for (i in 1:(num_sets - 1))
    {
      temp_set <- c()
      complement <- c()
      for (j in 1:length(values_set))
      {
        if (floor(i / 2^(j-1)) %% 2 > 0) {
          temp_set <- append(temp_set, values_set[j])
        }	else {
          complement <- append(complement, values_set[j])
        }
      }
      
      
      
      column <- v %in% temp_set
      columns <- cbind(columns, column)
      if (growbool) {
        names <- append(names, paste(name, " in [", toString(temp_set), "]"))
      }
      else {
        names <- append(names, paste(name, i, sep = ""))
      }
      
      exp <- list()
      exp$value <- temp_set
      exp$type <- "in"
      exp$id <- id
      expressions <- append(expressions, exp)
      
      
    }
    
    for (i in 1:length(column)) {
      if (num_sets < 3) {
        if (is.na(v[i])) {
          columns[i] <- NA
        }
      }
      else
        for (j in 1:(num_sets - 1)) {
          if (is.na(v[i])) {
            columns[i, j] <- NA
          }
        }
    }
    
    
    columns <- data.frame(columns)
    names(columns) <- names
    
    result <- c()
    result$values <- c(temp_set)
    result$columns <- columns
    result$expressions <- expressions
    result$num_sets <- (num_sets - 1)
    
    return(result)
    
  }
