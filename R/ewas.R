#' ewas
#'
#' Run environment-wide association study
#' @param cat data frame containing categorical variables with first column as ID
#' @param cont data frame containing continuous variables with first column as ID
#' @param y name of response variable
#' @param cov vector containing names of covariates
#' @param regress family for the regression model as specified in glm, linear or logisitic
#' @param adjust p-value adjustment (bonferroni or fdr)
#' @return data frame containing following fields Variable, Sample Size, Converged, SE, Beta, Variable p-value, LRT, AIC, adjusted p-value
#' @export
#' @examples
#' ewas(cat, cont, y, cov, regress, adjust)

ewas <- function(cat=NULL, cont=NULL, y, cov=NULL, regress, adjust){
  t1 <- Sys.time()
  print("Running...")

  if(missing(y)){
    stop("Please specify either 'continuous' or 'categorical' type for predictor variables")
  }
  if(missing(regress)){
    stop("Please specify family type for glm()")
  }

  ###Continuous###
  #Regress over columns != ID, y, covariates, or categorical variables
  regress_cont <- function(d, fmla, cols, rtype){
    mco <- lapply(d[, !(colnames(d) %in% cols), drop=FALSE], function (x) return(tryCatch(do.call("glm", list(as.formula(fmla), family=as.name(rtype), data=as.name("d"))), error=function(e) NULL)))
    nmco<- mco[!sapply(mco, is.null)]
    sco <- lapply(nmco, function (x) summary(x))
    #Grab sample size, beta, se beta, and pvalue
    rco <- data.frame(t(as.data.frame(sapply(nmco, function(x) as.data.frame(length(x$residuals))))),
                      t(as.data.frame(sapply(nmco, function(x) as.data.frame(x$converged)))),
                      t(as.data.frame(sapply(sco, function(x) as.data.frame(cbind(x$coefficients[2,1],
                                                                                  x$coefficients[2,2],
                                                                                  x$coefficients[2,4]))))))
    prco <- data.frame(names = gsub("\\.length.x.residuals.","", row.names(rco)), rco, row.names = NULL)
    names(prco) <- c("Variable", "N", "Converged", "Beta", "SE", "Variable_pvalue")
    prco$Sort <- ifelse(prco$Converged==TRUE, prco$Variable_pvalue, NA)
    return(prco)
  }

  ###Categorical###
  #Regress over columns != ID, y, covariates, or continuous variables
  regress_cat <- function(d, fmla, cols, rtype, usenull=FALSE){
    mca <- lapply(d[, !(colnames(d) %in% cols), drop=FALSE], function (x) return(tryCatch(do.call("glm", list(as.formula(fmla), family=as.name(rtype), data=as.name("d"))), error=function(e) NULL)))
    if(usenull==FALSE){
      red <- lapply(d[,!(colnames(d) %in% cols), drop=FALSE], function(x) return(tryCatch(glm(as.formula(gsub("x\\+", "", fmla)), data=d[!is.na(x), ], family=rtype), error=function(e) NULL)))
    } else {
      red <- lapply(d[,!(colnames(d) %in% cols), drop=FALSE], function(x) return(tryCatch(glm(as.formula(gsub("\\~x", "~1", fmla)), data=d[!is.na(x), ], family=rtype), error=function(e) NULL)))
    }
    nmca<- mca[!sapply(mca, is.null)]
    nred<- red[!sapply(red, is.null)]
    lrt <- mapply(function (x,y) anova(x,y, test="LRT"), x=nmca, y=nred, SIMPLIFY = FALSE)
    #Grab sample size, beta, se beta, and pvalue
    rca <- data.frame(t(as.data.frame(sapply(nmca, function(x) as.data.frame(length(x$residuals))))),
                      t(as.data.frame(sapply(nmca, function(x) as.data.frame(x$converged)))),
                      "NA", "NA", "NA",
                      t(as.data.frame(sapply(lrt, function(x) as.data.frame(cbind(x$`Pr(>Chi)`[2]))))),
                      t(as.data.frame(mapply(function (x,y) x$aic-y$aic, x=nmca, y=nred, SIMPLIFY = FALSE))))
    prca <- data.frame(names = gsub("\\.length.x.residuals.","", row.names(rca)), rca, row.names = NULL)
    names(prca) <- c("Variable", "N", "Converged", "Beta", "SE", "Variable_pvalue", "LRT_pvalue", "Diff_AIC")
    prca$Sort <- ifelse(prca$Converged==TRUE, prca$LRT_pvalue, NA)
    return(prca)
  }

  #Specify regression forumula
  if(!is.null(cov)){
    fmla <- paste(y,"~x+", paste(cov, collapse="+"), sep="")
    usenull=FALSE
  } else {
    fmla <- paste(y,"~x", sep="")
    usenull<-TRUE
  }

  #Run Regressions
  if(!is.null(cat) & !is.null(cont)){
    if(is.element('ID', names(cat))==FALSE | is.element('ID', names(cont))==FALSE){
      stop("Please add ID to 'cat' and/or 'cont' as column 1")
    }
    cat <- as.data.frame(sapply(cat, factor))
    cont$ID <- factor(cont$ID)
    if(sum(sapply(cont[, -1, drop=FALSE],is.numeric))!=ncol(cont)-1){
      stop("Please make sure that all values in 'cont' are numeric")
    }
    d <- merge(cat, cont, by="ID", all=TRUE)

    if(dim(cont[, !(colnames(cont) %in% c("ID", cov, y)), drop=FALSE])[2]>0){
      rcont <- regress_cont(d=d, fmla=fmla, cols=c("ID", cov, y, names(cat)), rtype=regress)
      rcont$LRT_pvalue <- NA
      rcont$Diff_AIC <- NA
    } else {
      rcont <- NULL
      print("No continuous variables to run regressions on")
    }
    if(dim(cat[, !(colnames(cat) %in% c("ID", cov, y)), drop=FALSE])[2]>0){
      rcat <- regress_cat(d=d, fmla=fmla, cols=c("ID", cov, y, names(cont)), rtype=regress, usenull=usenull)
    } else {
      rcat <- NULL
      print("No categorical variables to run regressions on")
    }

    fres <- rbind(rcont, rcat)

  } else if(is.null(cat) & !is.null(cont)){
    if(is.element('ID', names(cont))==FALSE){
      stop("Please add ID 'cont' as column 1")
    }
    cont$ID <- factor(cont$ID)
    if(sum(sapply(cont[, -1],is.numeric))!=ncol(cont)-1){
      stop("Please make sure that all values in 'cont' are numeric")
    }
    fres <- regress_cont(d=cont, fmla=fmla, cols=c("ID", cov, y), rtype=regress)

  } else if(is.null(cont) & !is.null(cat)){
    if(is.element('ID', names(cat))==FALSE){
      stop("Please add ID to 'cat' as column 1")
    }
    d <- as.data.frame(sapply(cat, factor))
    fres <- regress_cat(d=d, fmla=fmla, cols=c("ID", cov, y, names(cont)), rtype=regress, usenull=usenull)
  }

  fres <- as.data.frame(lapply(fres, unlist))
  fres <- fres[order(fres$Sort),]

  #Optional multiple testing correction
  if(!missing(adjust)){
    if(length(grep("fdr", adjust))!=0){
      fres$pvalue_FDR <- p.adjust(fres$Sort, method="fdr")
      fres <- fres[order(fres$pvalue_FDR), ]
    }
    if(length(grep("bonferroni", adjust))!=0){
      fres$pvalue_Bonf <- p.adjust(fres$Sort, method="bonferroni")
      fres <- fres[order(fres$pvalue_Bonf), ]
    }
  }

  t2 <- Sys.time()
  print(paste("Finished in", round(as.numeric(difftime(t2,t1, units="secs")), 6), "secs", sep=" "))

  return(fres[, names(fres)!="Sort"])
}


