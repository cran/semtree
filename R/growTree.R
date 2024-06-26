#################################################
# Tree Growing Process                         ##
#################################################
#
# growTree implements the basic recursive splitting logic
# and creates the tree data structure including all sorts
# of model- and meta-information. The actual evaluation
# of the best splits is outsourced to separate functions
# depending on what split method is chosen.
#
#
#
#
growTree <- function(model = NULL, mydata = NULL,
                     control = NULL, invariance = NULL, meta = NULL,
                     edgelabel = NULL, depth = 0, constraints = NULL, ...) {
  if (is.null(mydata)) {
    stop("There was no data for growing the tree")
  }

  if (is.null(meta)) {
    warning("SEM tree could not determine model variables and covariates in the dataset.")
    return(NULL)
  }

  if (control$verbose) {
    ui_message("Growing level ", depth, " n = ", nrow(mydata))
  }

  if (control$report.level > 0) {
    report(paste("Growing tree level", depth), 0)
  }



  # Node null settings in testing for significant splitting
  node <- list()
  node$left_child <- NULL
  node$right_child <- NULL
  node$caption <- "TERMINAL"
  node$N <- dim(mydata)[1]
  class(node) <- "semtree"

  # -- sample columns in case of SEM forest usage --
  fulldata <- mydata
  fullmeta <- meta
  if (control$mtry > 0) {
    # get names of model variables before sampling
    model.names <- names(mydata)[meta$model.ids]
    covariate.names <- names(mydata)[meta$covariate.ids]
    # perform sampling
    mydata <- sampleColumns(mydata, names(mydata)[meta$covariate.ids], control$mtry)
    # get new model ids after sampling by name
    meta$model.ids <- sapply(model.names, function(x) {
      which(x == names(mydata))
    })
    names(meta$model.ids) <- NULL
    meta$covariate.ids <- unlist(lapply(covariate.names, function(x) {
      which(x == names(mydata))
    }))

    node$colnames <- colnames(mydata)
    if (control$verbose) {
      ui_message("Subsampled predictors: ", paste(node$colnames[meta$covariate.ids]))
    }
  }

  # determine whether split evaluation can be done on p values
  node$p.values.valid <- control$method != "cv"

  # set some default values for the node object
  node$lr <- NA
  node$edge_label <- edgelabel

  # Estimate model on current data set
  ## 31.08.2022: Model in root is not refitted if refit is set to FALSE
  if (depth == 0 & !control$refit & length(constraints) == 0) { # do not fit model
    node$model <- model
  } else { # fit model
    # OpenMx
    if (control$sem.prog == "OpenMx") {
      full.model <- mxAddNewModelData(
        model = model, data = mydata,
        name = "BASE MODEL"
      )
      node$model <- try(
        suppressMessages(OpenMx::mxTryHard(
          model = full.model, paste = FALSE,
          silent = TRUE, bestInitsOutput = FALSE
        )),
        silent = TRUE
      )
    }
    # lavaan
    if (control$sem.prog == "lavaan") {
      ## 11.08.2022 Note: fits lavaan model on mydata
      node$model <- try(suppressWarnings(eval(
        parse(text = paste("lavaan::", model@Options$model.type,
          "(lavaan::parTable(model),data=mydata,missing='",
          model@Options$missing, "')",
          sep = ""
        ))
      )), silent = T)
    }
    ## 26.06.2022: Added code for ctsem models
    if (control$sem.prog == "ctsem") {
      full.model <- suppressMessages(try(
        ctsemOMX::ctFit(
          dat = mydata[, -meta$covariate.ids],
          ctmodelobj = model$ctmodelobj,
          dataform = "wide",
          objective = model$ctfitargs$objective,
          stationary = model$ctfitargs$stationary,
          optimizer = model$ctfitargs$optimizer,
          retryattempts = ifelse(model$ctfitargs$retryattempts >= 20,
            yes = model$ctfitargs$retryattempts,
            no = 20
          ),
          carefulFit = model$ctfitargs$carefulFit,
          showInits = model$ctfitargs$showInits,
          asymptotes = model$ctfitargs$asymptotes,
          meanIntervals = model$ctfitargs$meanIntervals,
          discreteTime = model$ctfitargs$discreteTime,
          verbose = model$ctfitargs$verbose,
          transformedParams = model$ctfitargs$transformedParams
        )
      ))
      full.model$mxobj@name <- "BASE MODEL"
      node$model <- full.model
    }
  }


  if (is(node$model, "try-error")) {
    ui_fail("Model had a run error.")
    node$term.reason <- node$model[[1]]
    node$model <- NULL
    return(node)
  }

  if (is.null(node$model)) {
    node$term.reason <- "Model was NULL! Model could not be estimated."
    return(node)
  }


  ###########################################################
  ###               OPENMX USED HERE                      ###
  ###########################################################
  if (control$sem.prog == "OpenMx") {
    # some export/namespace problem here with the generic
    # getS3method("summary","MxModel") gets me the right fun
    S3summary <- getS3method("summary", "MxModel")

    # list of point estimates, std.dev, and names of all freely estimated parameters
    node$params <- S3summary(node$model)$parameters[, 5]
    names(node$params) <- S3summary(node$model)$parameters[, 1]
    node$params_sd <- S3summary(node$model)$parameters[, 6]
    node$param_names <- S3summary(node$model)$parameters[, 1]
  }
  ###########################################################
  ###               lavaan USED HERE                      ###
  ###########################################################
  if (control$sem.prog == "lavaan") {
    node$params <- lavaan::coef(node$model) # put parameters into node
    names(node$params) <- names(lavaan::coef(node$model)) # parameter names are stored as well

    # read in estimated parameters (take only those that have non-NA z values)
    # parameters <- data.frame(
    #  lavaan::parameterEstimates(node$model))[!is.na(
    #    data.frame(lavaan::parameterEstimates(node$model))[,"z"]),]

    parameters <- lavaan::parameterEstimates(node$model)

    # if any labels are missing (some labels provided), then put default labels in the label col.
    for (i in 1:nrow(parameters)) {
      if (!is.null(parameters$label)) {
        if (parameters$label[i] == "") {
          parameters$label[i] <- paste(parameters$lhs[i], parameters$op[i], parameters$rhs[i], sep = "")
        }
      }
    }
    # if all labels are missing make a label column
    if (is.null(parameters$label)) {
      label <- paste(parameters$lhs, parameters$op, parameters$rhs, sep = "")
      parameters <- cbind(parameters, label)
    }

    # store the SE of the estimates
    se <- rep(NA, length(unique(parameters$se)))
    for (i in 1:length(unique(parameters$label))) {
      for (j in 1:nrow(parameters)) {
        if (unique(parameters$label)[i] == parameters$label[j]) {
          se[i] <- parameters$se[j]
        }
      }
    }

    # list of point estimates, std.dev, and names of all freely estimated parameters
    node$params_sd <- se
    node$param_names <- names(lavaan::coef(node$model))
  }
  ###########################################################
  ###               ctsemOMX USED HERE                    ###
  ###########################################################
  if (control$sem.prog == "ctsem") {
    # list of point estimates, std.dev, and names of all freely estimated parameters
    if (control$ctsem_sd) { # slower
      ctsem_summary <- summary(node$model) # this is very slow
      node$params <- ctsem_summary$ctparameters[, "Value"]
      names(node$params) <- rownames(ctsem_summary$ctparameters)
      node$params_sd <- ctsem_summary$ctparameters[, "StdError"]
      node$param_names <- rownames(ctsem_summary$ctparameters)
    } else { # faster
      ctsem_coef <- coef.ctsemFit(node$model)
      node$params <- ctsem_coef
      node$params_sd <- NA
      node$param_names <- names(ctsem_coef)
    }
  }

  # df

  if (!is.null(constraints$focus.parameters)) {
    # df's are equal to number of focus parameters
    node$df <- length(constraints$focus.parameters)
  } else {
    # focus.parameters=NULL is as if all parameters were focus parameters, that is,
    # df == num. parameters
    node$df <- length(node$param_names)
  }



  # add unique node id via global variable
  node$node_id <- getGlobal("global.node.id")
  setGlobal("global.node.id", node$node_id + 1)

  # determine whether we should skip splitting
  # 1. minimum cases in node
  if (!is.na(control$min.N)) {
    if (node$N <= control$min.N) {
      if (control$verbose) {
        ui_message("Minimum user defined N for leaf node.")
      }
      node$term.reason <- "Minimum number of cases in leaf node"
      return(node)
    }
  }
  # 2. maximum depth for tree reached
  if (!is.na(control$max.depth)) {
    if (depth >= control$max.depth) {
      if (control$verbose) {
        ui_message("Max user defined tree depth reached.")
      }
      node$term.reason <- "Maximum depth reached in leaf node"
      return(node)
    }
  }

  # determine best split based in chosen method (ml or score) and (naive, cv, fair)
  result <- NULL
  # 1. unbalanced selection method
  if (control$method == "naive") {
    #   if (control$test.type=="ml") {

    result <- tryCatch(
      ################################################
      naiveSplit(node$model, mydata, control, invariance, meta, constraints = constraints, ...)
      ################################################
      ,
      error = function(e) {
        cat(paste("Error occured!", e, sep = "\n"))
        traceback()
        return(NULL)
      }
    )
  } else if (control$method == "score") {
    result <- tryCatch(
      ################################################
      result <- ScoreSplit(node$model, mydata, control, invariance, meta, constraints = constraints, ...)
      ################################################
      ,
      error = function(e) {
        cat(paste("Error occured!", e, sep = "\n"))
        traceback()
        return(NULL)
      }
    )
  }
  # 2a. split half data to determine best split then use hold out set to compare one split per covariate
  else if (control$method == "fair") {
    control$fair3Step <- FALSE
    result <- tryCatch(
      ################################################
      fairSplit(node$model, mydata, control, invariance, meta, constraints = constraints, ...)
      ################################################
      ,
      error = function(e) {
        cat(paste("Error occured!", e, sep = "\n"))
        return(NULL)
      }
    )
  }
  # 2b. split half data to determine best split then use hold out set to compare one split per covariate, with step 3 all splits retest
  else if (control$method == "fair3") {
    control$fair3Step <- TRUE
    result <- tryCatch(
      ################################################
      fairSplit(node$model, mydata, control, invariance, meta, constraints = constraints, ...)
      ################################################
      ,
      error = function(e) {
        cat(paste("Error occured!", e, sep = "\n"))
        return(NULL)
      }
    )
  } else {
    ui_fail("Error. Unknown split method selected")
    stop()
  }

  # return values in result are:
  # LL.max		: numeric, log likelihood ratio of best split
  # split.max 	: numeric, split point; value to split best column on
  #               (for metric variables)
  # col.max		: index of best column
  # name.max	: name of best candidate
  # type.max  :
  # btn.matrix : a matrix, which contains test statistics and
  #              more information for
  #              the various split points evaluated
  # n.comp     : the (implicit) number of multiple tests evaluated for
  #               determining the best split

  # store the value of the selected test statistic
  node$lr <- NA
  if (!is.null(result)) {
    node$lr <- result$LL.max
    node$result <- result
  }

  # if no split found, exit node without continuing growth
  if (is.null(result) || is.null(result$LL.max)) {
    if (control$verbose) {
      ui_message("Best Likelihood Ratio was NULL. Stop splitting")
    }
    return(node)
  }

  # provide verbose output to the user about best split
  if (control$verbose) {
    ui_ok("Best split is  ", result$name.max, " with statistic = ", round(node$lr, 2))
  }

  # compute p value from chi2
  if (!is.null(result$p.max)) {
    node$p <- result$p.max
  } else {
    node$p <- pchisq(node$lr, df = node$df, lower.tail = F)

    if (control$use.maxlm) {
      # Borders for continuous covariates
      if (!is.factor(mydata[, result$name.max])) {
        props <- cumsum(table(mydata[, result$name.max])) / node$N
        split_val_lhs <- as.numeric(names(which(props >= control$strucchange.from)[1]))
        split_val_rhs <- as.numeric(names(which(props >= control$strucchange.to)[1]))

        btn_matrix_max <- result$btn.matrix[, result$btn.matrix["variable", ] ==
          result$name.max, drop = FALSE]

        num_split_val <- as.numeric(btn_matrix_max["split val", ])

        n1 <- which(num_split_val <= split_val_lhs)
        n1 <- n1[length(n1)]
        n2 <- which(num_split_val >= split_val_rhs)[1]

        if (length(n1) == 0) {
          n1 <- 1
        }
        if (is.na(n2)) {
          n2 <- length(num_split_val)
        }

        LR <- as.numeric(btn_matrix_max["LR", n1:n2])

        max_pos <- which.max(LR) + n1 - 1
        node$result$LL.max <- node$lr <- as.numeric(btn_matrix_max["LR", max_pos])
        node$result$split.max <- as.numeric(btn_matrix_max["split val", max_pos])
      }

      node$p <- computePval_maxLR(
        maxLR = node$lr, q = node$df,
        covariate = mydata[, result$col.max], from = control$strucchange.from,
        to = control$strucchange.to, nrep = control$strucchange.nrep
      )
    }
  }


  # ---------	determine whether to continue splitting	--------------
  if (is(control$custom.stopping.rule, "function")) {
    stopping.rule <- control$custom.stopping.rule
  } else {
    stopping.rule <- stoppingRuleDefault
  }
  # stoppingRuleDefault() is a function that gets inputs node, result, control
  # this function can be replaced by a user-specified function
  srule <- stopping.rule(node, result, control)

  # determine whether splitting should be continued depending on return state
  # of the function
  if (is(srule, "list")) {
    node <- srule$node
    cont.split <- !(srule$stop.rule)
  } else {
    cont.split <- !srule
    node$p.values.valid <- FALSE
  }

  # restore mydata here if (mtry was > 0)	-- for semforests
  if (control$mtry > 0) {
    # also need to remap col.max to original data!
    if (!is.null(result$col.max) && !is.na(result$col.max)) {
      col.max.name <- names(mydata)[result$col.max]
      result$col.max <- which(names(fulldata) == col.max.name)
    } else {
      col.max.name <- NULL
    }

    # restore data
    mydata <- fulldata
    meta <- fullmeta
  }

  if ((!is.null(cont.split)) && (!is.na(cont.split)) && (cont.split)) {
    if (control$report.level > 10) {
      report("Stop splitting based on stopping rule.", 1)
    }



    # store the split name (covariate name and split value) RHS is yes branch
    if (result$type.max == .SCALE_CATEGORICAL) {
      # unordered factor collating and splitting
      lvl <- (control$method == "fair")
      result1 <- recodeAllSubsets(mydata[, result$col.max], colnames(mydata)[result$col.max],
        growbool = T, use.levels = lvl
      )


      test2 <- rep(NA, nrow(mydata))
      if (!is.na(result1$num_sets) & !is.null(result1$num_sets)) {
        for (j in 1:result1$num_sets) {
          test1 <- rep(NA, nrow(mydata))
          for (i in 1:nrow(mydata)) {
            if (isTRUE(result1$columns[i, j])) {
              test1[i] <- 1
            } else if (!is.na(result1$columns[i, j])) {
              test1[i] <- 0
            } else {
              test1[i] <- NA
            }
          }
          test1 <- as.factor(test1)
          test2 <- data.frame(test2, test1)
        }
      }
      test2 <- test2[, -1]

      # if level is categorical, then split.max corresponds to the index of
      # the best column in the matrix that represents all subsets
      # make sure that this is not casted to a string if there
      # are predictors of other types (esp., factors)
      # browser()
      if (!all(is.na(result$btn.matrix))) {
        result$split.max <- as.integer(result$split.max)

        # named <- colnames(result1$columns)[result$split.max]
        #      node$caption <- paste(colnames(result1$columns)[result$split.max])
        best_subset_col_id <- result$split.max
        best_values <- result1$expressions[(best_subset_col_id - 1) * 3 + 1]$value
      } else {
        best_values <- result$split.max
      }
      node$rule <- list(
        variable = result$col.max, relation = "%in%",
        value = best_values,
        name = result$name.max
      )
      node$caption <- paste(result$name.max, " in [", paste0(best_values,
        collapse = " "
      ), " ]")

      if (result1$num_sets == 1) {
        sub1 <- subset(mydata, as.numeric(test2) == 2)
        sub2 <- subset(mydata, as.numeric(test2) == 1)
      } else {
        sub1 <- subset(mydata, as.numeric(test2[[result$split.max]]) == 2)
        sub2 <- subset(mydata, as.numeric(test2[[result$split.max]]) == 1)
      }
    } else if (result$type.max == .SCALE_METRIC) {
      # if var.type==2, then split.max corresponds to the split point value
      # make sure that this is not casted to a string if there
      # are predictors of other types (esp., factors)
      result$split.max <- as.numeric(result$split.max)

      # ordered factor splitting of data
      node$caption <- paste(result$name.max, ">=", signif(result$split.max, 6), sep = " ")
      node$rule <- list(variable = result$col.max, relation = ">=", value = c(result$split.max), name = result$name.max)
      sub1 <- subset(mydata, as.numeric(as.character(mydata[, (result$col.max)])) > result$split.max)
      sub2 <- subset(mydata, as.numeric(as.character(mydata[, (result$col.max)])) <= result$split.max)
    } else if (result$type.max == .SCALE_ORDINAL) {
      node$caption <- paste(result$name.max, ">", result$split.max, sep = " ")
      node$rule <- list(variable = result$col.max, relation = ">", value = c(result$split.max), name = result$name.max)
      sub1 <- subset(mydata, mydata[, (result$col.max)] > result$split.max)
      sub2 <- subset(mydata, mydata[, (result$col.max)] <= result$split.max)
    } else if (result$type.max == 99) {
      # this is an error code by score test implementation
      # return node and stop splitting
      # TODO (MA): Do we need to issue a warning?
      return(node)
    } else {
      # TODO: remove this bc this condition should be captured earlier in any case
      # continuous variables splitting
      # node$caption <- paste(result$name.max,">=", signif(result$split.max,3),sep=" ")
      # node$rule = list(variable=result$col.max, relation=">=", value=c(result$split.max), name = result$name.max)
      # sub1 <- subset( mydata, as.numeric(mydata[, (result$col.max)]) >result$split.max)
      # sub2 <- subset( mydata, as.numeric(mydata[, (result$col.max)])<=result$split.max)
      stop("An error occured!")
    }

    flush.console()

    ##########################################################
    ## NEW CODE TO INCLUDE CASES MISSING ON SPLITTING VARIABLE
    class(node) <- "semtree"
    if (control$use.all & (nrow(mydata) > (nrow(sub1) + nrow(sub2)))) {
      if (control$verbose) {
        message("Missing on splitting variable: ", result$name.max)
      }
      completeSplits <- calculateBestSubsets(model, mydata, sub1, sub2, result)
      sub1 <- completeSplits$sub1
      sub2 <- completeSplits$sub2
    }
    ##########################################################

    # build a model for missing data
    if (control$missing == "ctree") {
      ui_warn("Missing data treatment with ctree is not yet implemented.")
      # temp = mydata[!is.na(mydata[,result$name.max]),]
      # node$missing.model = party::ctree(
      #  data = temp,
      #  formula = as.formula(paste0(result$name.max,"~.")))
    } else if (control$missing == "rpart") {
      temp <- mydata[!is.na(mydata[, result$name.max]), ]
      node$missing.model <- rpart::rpart(
        data = temp,
        formula = as.formula(paste0(result$name.max, "~."))
      )
    }


    # recursively continue splitting
    # result1 - RHS; result2 - LHS
    result2 <- growTree(node$model, sub2, control, invariance, meta, edgelabel = 0, depth = depth + 1, constraints)
    result1 <- growTree(node$model, sub1, control, invariance, meta, edgelabel = 1, depth = depth + 1, constraints)

    # store results in recursive list structure
    node$left_child <- result2
    node$right_child <- result1

    return(node)
  } else {
    # if cont.split is F or NA or NULL then return node without further splitting
    return(node)
  }
}
