#' bimbalance - balanced item analysis
#'
#' The \code{bimbalance} function checks the imbalances of all present item
#' combinations in the data in terms of side (R: right, L:left). It counts the
#' cases on each side and also calculates a ratio (RL_ratio = R/L).
#'
#' @param dat raw data (e.g., from the \code{bimload} function)
#' @param sidevar the actual column name of the side variable in the raw data
#' (must be coded "right" or "left")
#'
#' @return data.frame R/L counts and the RL_ratio
#'
#' @export


bimbalance <- function(dat=NULL, sidevar="sideA"){

  if(sum(names(dat) == sidevar) == 1){
    # balancing
    combodat        <- dat
    combodat$actual <- paste(combodat$optionA, combodat$optionB, sep="")

    # test actual items
    combodat$test   <- paste(combodat$optionA, combodat$optionB, sep="")
    uniqueCombo     <- unique(combodat$actual)

    balance <- NULL
    ratio <- NULL
    for(i in 1:length(uniqueCombo)){
      counts_R    <- dim( combodat[combodat$test %in% uniqueCombo[i] & combodat$sideA %in% "left", ])[1]
      counts_L    <- dim( combodat[combodat$test %in% uniqueCombo[i] & combodat$sideA %in% "right", ])[1]
      pairdat_R   <- combodat[combodat$test %in% uniqueCombo[i] & combodat$sideA %in% "left", ]
      pairdat_L   <- combodat[combodat$test %in% uniqueCombo[i] & combodat$sideA %in% "right", ]
      ratio       <- rbind(ratio, data.frame(comb0    = uniqueCombo[i],
                                             counts_R = counts_R,
                                             counts_L = counts_L,
                                             RL_ratio = round(dim(pairdat_R)[1] / dim(pairdat_L)[1],3)))
    }


    balance <- ratio

    return(balance)

  }else{
    warning("You have no column with side information (left/right) in your data OR
         you might have given it another name (see sidevar='sideA')")
  }


}


