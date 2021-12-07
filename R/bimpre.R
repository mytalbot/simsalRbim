#' Data Preprocessing Function
#'
#' The \code{bimpre} function is a helper function for data preprocessing.
#' Missing item combinations are filled and marked as ties. Simulated data are
#' marked and the data are prepared in the correct order (pairings) according
#' to the ground truth.
#'
#' @param dat curated data.frame obtained from the import function (unified format)
#' @param GT item list with the ground truth (GT; letters are case sensitive!)
#' @param simOpt item to be checked (this can be an item from the GT or a new
#' one. Remove the item from GT if specified here!)
#' @param deviation how much deviation is allowed for marking ties (default is 0 which translates to 50%)
#' @param minQuantity minimum quantitiy to be recognized as difference (default is 0)
#' @param verbose show function results
#'
#' @import utils
#' @import rlang
#'
#' @return data.frame with the raw data
#'
#' @export

bimpre <- function(dat=NULL, GT=NULL, simOpt=NULL, deviation=0, minQuantity=0,
                   verbose=TRUE){

  # Helper function
  printf     <- function(...) cat(sprintf(...))

  # create the item list
  optionList <- c(GT, simOpt)

  if(sum(dat$optionA %in% simOpt) + sum(dat$optionB %in% simOpt)==0){
    warning(paste("simsalRbim: ","Hey, you forgot to provide data for ",
                  simOpt, "! Using the default model.\n", sep=""))
  }else{}



  #  Sort function ----------------------------------------------------------
  firstSortTable <- function(inData, order) {
    for (i in seq_len(nrow(inData))) {

      if (!any(which(order == inData$optionA[i])) |
          !any(which(order == inData$optionB[i]))) {
        inData$subjectID[i] <- NA
      } else if (which(order == inData$optionA[i]) >
                 which(order == inData$optionB[i])) {
          #printf('%d: Exchange A & B\n', i)
          temp <- inData$optionA[i]
          inData$optionA[i] <- inData$optionB[i]
          inData$optionB[i] <- temp
          temp <- inData$quantityA[i]
          inData$quantityA[i] <- inData$quantityB[i]
          inData$quantityB[i] <- temp

          if (!is_empty(dat$sideA[i])) {
            if (inData$sideA[i] == 'left')
              inData$sideA[i] <- 'right'
            else
              inData$sideA[i] <- 'left'
          }

        }
    }
    return(dat=subset(inData, !is.na(subjectID)))
  }




  dat <- firstSortTable(dat, c(GT, simOpt))



  # Mark Ties ---------------------------------------------------------------
  # prepare the test column for later analysis
  dat$test   <- NA
  dat$test   <- paste(dat$optionA, dat$optionB, sep="")
  dat$result <- NA

  # fill up quantities for ties if present
  dat[dat$tie %in% TRUE, c("quantityA", "quantityB")] <- c(50,50)


  ### Mark the ties
  markTies   <- function(inData=NULL, deviation = deviation, minQuantity = minQuantity) {

    for (i in seq_len(nrow(inData)) ) {
      sumQ <- inData$quantityA[i] + inData$quantityB[i]
      inData$qPercentA[i] <- (inData$quantityA[i]*100) / sumQ
      inData$qPercentB[i] <- (inData$quantityB[i]*100) / sumQ
      if (sumQ <= minQuantity) {
        inData[i, 'tie']  <- TRUE
      }
    }
    inData$tie[inData$qPercentA <= (50+deviation) &
                 inData$qPercentA >= (50-deviation)] <- TRUE
    inData[is.na(inData$tie), 'tie'] <- FALSE

    inData$result[inData$qPercentA >
                    inData$qPercentB & inData$tie == FALSE] <- 1
    inData$result[inData$tie == TRUE] <- 0
    inData$result[inData$qPercentA <
                    inData$qPercentB & inData$tie == FALSE] <- -1

    if(verbose==TRUE){
      printf ( paste("simsalRbim: ",sum(inData$tie==TRUE)," tie(s) marked.\n" ,sep="") )
    }else{}

    return(inData)
  }

  # mark the ties in the data & prepare the results column
  dat        <- markTies(dat, deviation = deviation, minQuantity = deviation)


  subjectList <- unique(dat$subjectID)
  dat$sim <- FALSE
  for (i in seq_along(GT) ) {
    for (j in seq_along(subjectList)) {
      if (nrow(subset(dat, subjectID == subjectList[j] & optionA == GT[i] &
                      optionB == simOpt)) < 1) {
        #printf('Erzeuge Eintrag...\n')
        tempRow                     <- nrow(dat) + 1
        dat[tempRow, 'subjectID'] <- subjectList[j]
        dat$optionA[tempRow]      <- GT[i]
        dat$optionB[tempRow]      <- simOpt
        dat$quantityA[tempRow]    <- 0
        dat$quantityB[tempRow]    <- 0
        dat$qPercentA[tempRow]    <- 50
        dat$qPercentB[tempRow]    <- 50
        dat$tie[tempRow]          <- TRUE
        dat$result[tempRow]       <- 0
        dat$sim[tempRow]          <- TRUE
      }
    }
  }
  #rm(subjectList, i, j, tempRow)

  dat$test <- paste(dat$optionA, dat$optionB, sep='')
  dat$optionFactorA <- as.factor(dat$optionA)
  dat$optionFactorB <- as.factor(dat$optionB)



  # Sort the data
  p1_num <- NULL
  p2_num <- NULL
  for (i in 1:(length(optionList)-1)) {
    p1_num <- c(p1_num, 1:i)
    p2_num <- c(p2_num, rep(i+1, i))
  }

  p1   <- paste0(optionList[p1_num], optionList[p2_num])
  p2   <- paste0(optionList[p2_num], optionList[p1_num])
  pairings <- c(p1,p2)

  ## and order them correctly
  ydata      <- dat
  ydata$test <- factor(dat$test, levels = pairings)
  ydata      <- with(ydata, ydata[order(test),])

  return(ydata)
}





