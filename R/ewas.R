###Continuous###
regress_cont <- function(d, fmla, variables, rtype, use_survey){
  n <- length(variables)
  df <- data.frame(Variable = character(n),
                  N = numeric(n),
                  Converged = logical(n),
                  Beta = numeric(n),
                  SE = numeric(n),
                  Variable_pvalue = numeric(n),
                  LRT_pvalue = numeric(n),
                  Diff_AIC = numeric(n),
                  pval = numeric(n),
                  stringsAsFactors = FALSE
  )
  i = 1
  for(var_name in variables){
    df$Variable[i] <- var_name
    # Update formula
    var_fmla <- gsub("~x", paste("~", var_name, sep=""), fmla)
    if(use_survey){
      var_result <- tryCatch(survey::svyglm(stats::as.formula(var_fmla), family=rtype, design=d), error=function(e) {NULL})
    } else {
      var_result <- tryCatch(glm(stats::as.formula(var_fmla), family=rtype, data=d), error=function(e) {NULL})
    }
    if (is.null(var_result)){
      # Return null results
      df$N[i] <- NA
      df$Converged[i] <- NA
      df$Beta[i] <- NA
      df$SE[i] <- NA
      df$Variable_pvalue[i] <- NA
      df$LRT_pvalue[i] <- NA
      df$Diff_AIC[i] <- NA
      df$pval[i] <- NA
    } else {
      # Get the summary
      var_summary <- summary(var_result)
      # Return processed summary results
      df$N[i] <- length(var_result$residuals)
      df$Converged[i] <- var_result$converged
      df$Beta[i] <- var_summary$coefficients[2,1]
      df$SE[i] <- var_summary$coefficients[2,2]
      df$Variable_pvalue[i] <- var_summary$coefficients[2,4]
      df$LRT_pvalue[i] <- NA
      df$Diff_AIC[i] <- NA
      df$pval[i] <- var_summary$coefficients[2,4]
    }
    i <- i + 1
  }
  return(df)
}

###Categorical###
regress_cat <- function(d, fmla, fmla_restricted, variables, rtype, use_survey){
  n <- length(variables)
  df <- data.frame(Variable = character(n),
                  N = numeric(n),
                  Converged = logical(n),
                  Beta = numeric(n),
                  SE = numeric(n),
                  Variable_pvalue = numeric(n),
                  LRT_pvalue = numeric(n),
                  Diff_AIC = numeric(n),
                  pval = numeric(n),
                  stringsAsFactors = FALSE
  )
  i = 1
  for(var_name in variables){
    df$Variable[i] <- var_name
    # Update formulas
    var_fmla <- gsub("~x", paste("~", var_name, sep=""), fmla)
    var_fmla_restricted <- gsub("~x", paste("~", var_name, sep=""), fmla_restricted)
    if(use_survey){
      var_result <- tryCatch(survey::svyglm(stats::as.formula(var_fmla), family=rtype, design=d), error=function(e) {NULL})
      restricted_result <- tryCatch(survey::svyglm(stats::as.formula(var_fmla_restricted), family=rtype, design=var_result$model), error=function(e) {NULL})
    } else {
      var_result <- tryCatch(glm(stats::as.formula(var_fmla), family=rtype, data=d), error=function(e) {NULL})
      restricted_result <- tryCatch(glm(stats::as.formula(var_fmla_restricted), family=rtype, data=var_result$model), error=function(e) {NULL})
    }
    if(!is.null(var_result) & !is.null(restricted_result)){
      # Get LRT result
      lrt <- stats::anova(var_result, restricted_result, test="LRT")
      # Return processed result
      df$N[i] <- length(var_result$residuals)
      df$Converged[i] <- var_result$converged
      df$Beta[i] <- NA
      df$SE[i] <- NA
      df$Variable_pvalue[i] <- NA
      df$LRT_pvalue[i] <- lrt$`Pr(>Chi)`[2]
      df$Diff_AIC[i] <- var_result$aic - restricted_result$aic
      df$pval[i] <- lrt$`Pr(>Chi)`[2]
    } else{
      # Return null results
      df$N[i] <- NA
      df$Converged[i] <- NA
      df$Beta[i] <- NA
      df$SE[i] <- NA
      df$Variable_pvalue[i] <- NA
      df$LRT_pvalue[i] <- NA
      df$Diff_AIC[i] <- NA
      df$pval[i] <- NA
    }
    i <- i+1
  }
  return(df)
}

#' ewas
#'
#' Run environment-wide association study
#' @param d data.frame or survey::svydesign object containing all of the data
#' @param cat_vars List of variables to regress that are categorical or binary
#' @param cont_vars  List of variables to regress that are continuous
#' @param y name(s) of response variable(s)
#' @param cat_covars List of covariates that are categorical or binary
#' @param cont_covars List of covariates that are continuous
#' @param regress family for the regression model as specified in glm, linear or logisitic
#' @return data frame containing following fields Variable, Sample Size, Converged, SE, Beta, Variable p-value, LRT, AIC, pval, Phenotype
#' @export
#' @family analysis functions
#' @examples
#' \dontrun{
#' ewas(d, cat_vars, cont_vars, y, cat_covars, cont_covars, regress)
#' }

ewas <- function(d, cat_vars=NULL, cont_vars=NULL, y, cat_covars=NULL, cont_covars=NULL, regress){
  t1 <- Sys.time()

  if(missing(y)){
    stop("Please specify either 'continuous' or 'categorical' type for predictor variables")
  }
  if(missing(regress)){
    stop("Please specify family type for glm()")
  }
  if(is.null(cat_vars)){
    cat_vars <- list()
  }
  if(is.null(cont_vars)){
    cont_vars <- list()
  }
  if(is.null(cat_covars)){
    cat_covars <- list()
  }
  if(is.null(cont_covars)){
    cont_covars <- list()
  }

  # Ignore the covariates, phenotype, and ID if they were included in the variable lists
  remove <- c(y, cat_covars, cont_covars, "ID")
  cat_vars <- setdiff(cat_vars, remove)
  cont_vars <- setdiff(cont_vars, remove)
  # Ignore the phenotype, and ID if they were included in the covariates lists
  remove <- c(y, "ID")
  cat_covars <- setdiff(cat_covars, remove)
  cont_covars <- setdiff(cont_covars, remove)

  # Ensure variables/covariates aren't listed as multiple different types
  both <- intersect(cat_covars, cont_covars)
  if (length(both) > 0){stop("Some covariates are listed as both categorical and continuous: ", paste(both, collapse=", "))}
  both <- intersect(cat_vars, cont_vars)
  if (length(both) > 0){stop("Some variables are listed as both categorical and continuous: ", paste(both, collapse=", "))}

  # Determine the type that was passed in
  if(class(d) == "data.frame"){
    use_survey <- FALSE
    print("Running using stats::glm")
  } else if(class(d)[2] == "survey.design") {
    use_survey <- TRUE
    print("Running using survey:svyglm")
  } else {
    stop("Data must be either a data.frame or a survey::design object")
  }

  # Ensure specified variables are all present
  if (use_survey){
    missing_cat_vars = setdiff(cat_vars, names(d$variables))
    missing_cont_vars = setdiff(cont_vars, names(d$variables))
    missing_phenotypes = setdiff(y, names(d$variables))
    missing_cat_covars = setdiff(cat_covars, names(d$variables))
    missing_cont_covars = setdiff(cont_covars, names(d$variables))
  } else {
    missing_cat_vars = setdiff(cat_vars, names(d))
    missing_cont_vars = setdiff(cont_vars, names(d))
    missing_phenotypes = setdiff(y, names(d))
    missing_cat_covars = setdiff(cat_covars, names(d))
    missing_cont_covars = setdiff(cont_covars, names(d))
  }
  if(length(missing_cat_vars) > 0) {
    stop("Some categorical variable(s) could not be found in the data: ", paste(missing_cat_vars, collapse=", "))
  }
  if(length(missing_cont_vars) > 0) {
    stop("Some continuous variable(s) could not be found in the data: ", paste(missing_cont_vars, collapse=", "))
  }
  if(length(missing_phenotypes) > 0) {
    stop("Phenotype(s) couldn't be found in the lists of variables: ", paste(missing_phenotypes, collapse=", "))
  }
  if(length(missing_cat_covars)>0) {
      stop("Some categorical covariate(s) couldn't be found in the data: ", paste(missing_cat_covars, collapse=", "))
  }
  if(length(missing_cont_covars)>0) {
      stop("Some continuous covariate(s) couldn't be found in the data: ", paste(missing_cont_covars, collapse=", "))
  }

  #Correct the types and check for IDs
  if (use_survey) {
    # ID
    if(is.element('ID', names(d$variables))==FALSE){stop("Please add ID to the data as column 1")}
    d$variables$ID <- factor(d$variables$ID)
    # Categorical
    d$variables[cat_vars] <- lapply(d$variables[cat_vars], factor)
    d$variables[cat_covars] <- lapply(d$variables[cat_covars], factor)
    # Continuous
    if(sum(sapply(d$variables[cont_vars], is.numeric))!=length(cont_vars)){
      non_numeric_vars <- names(d$variables[!sapply(d$variables[cont_vars], is.numeric)])
      stop("Some continuous variables are not numeric: ", paste(non_numeric_vars, collapse=", "))
    }
    if(sum(sapply(d$variables[cont_covars], is.numeric))!=length(cont_covars)){
      non_numeric_covars <- names(d$variables[!sapply(d$variables[cont_covars], is.numeric)])
      stop("Some continuous covariates are not numeric: ", paste(non_numeric_covars, collapse=", "))
    }
  } else {
    # ID
    if(is.element('ID', names(d))==FALSE){stop("Please add ID to the data as column 1")}
    d$ID <- factor(d$ID)
    # Categorical
    d[cat_vars] <- lapply(d[cat_vars], factor)
    d[cat_covars] <- lapply(d[cat_covars], factor)
    # Continuous
    if(sum(sapply(d[cont_vars], is.numeric))!=length(cont_vars)){
      non_numeric_vars <- names(d[!sapply(d[cont_vars], is.numeric)])
      stop("Some continuous variables are not numeric: ", paste(non_numeric_vars, collapse=", "))
    }
    if(sum(sapply(d[cont_covars], is.numeric))!=length(cont_covars)){
      non_numeric_covars <- names(d[!sapply(d[cont_covars], is.numeric)])
      stop("Some continuous covariates are not numeric: ", paste(non_numeric_covars, collapse=", "))
    }
  }

  #Specify regression forumulas and combined list of covariates
  cov <- c(cat_covars, cont_covars)
  if(length(cov)>0){
    fmla <- paste(y,"~x+", paste(cov, collapse="+"), sep="")
    fmla_restricted <- paste(y, "~", paste(cov, collapse="+"), sep="")
  } else {
    fmla <- paste(y,"~x", sep="")
    fmla_restricted <- paste(y, "~1", sep="")
  }

  #Run Regressions
  if(!is.null(cat_vars) & !is.null(cont_vars)){
    # Regress both kinds of variables and merge
    rcont <- regress_cont(d=d, fmla=fmla, variables=cont_vars, rtype=regress, use_survey=use_survey)
    rcat <- regress_cat(d=d, fmla=fmla, fmla_restricted=fmla_restricted, variables=cat_vars, rtype=regress, use_survey=use_survey)
    fres <- rbind(rcont, rcat)

  } else if(is.null(cat_vars) & !is.null(cont_vars)){
    # Regress continuous variables
    fres <- regress_cont(d=d, fmla=fmla, variables=cont_vars, rtype=regress, use_survey=use_survey)
  
  } else if(is.null(cont) & !is.null(cat)){
    # Regress categorical variables
    fres <- regress_cat(d=d, fmla=fmla, fmla_restricted=fmla_restricted, variables=cat_vars, rtype=regress, use_survey=use_survey)
  }

  # Create a dataframe, sort by pvalue, and add the tested phenotype as a column
  #fres <- as.data.frame(lapply(fres, unlist))
  fres <- fres[order(fres$pval),]
  fres$phenotype <- y

  t2 <- Sys.time()
  print(paste("Finished in", round(as.numeric(difftime(t2,t1, units="secs")), 6), "secs", sep=" "))

  return(fres)
}


