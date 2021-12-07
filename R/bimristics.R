#' bimristics - data characteristics
#'
#' The \code{bimristics} function shows general data characteristics like the
#' number of natural ties, total number of tests, etc. It requires curated data
#' from the \code{bimpre} function and has to be checked for each test item
#' individually (simOpt).
#'
#' @param predat curated data.frame obtained from the \code{bimpre} function
#' @param simOpt item to be checked
#'
#' @return data.frame with study characteristics
#'
#' @export

bimristics <- function(predat=NULL, simOpt=NULL){

  # How many Subjects are simulated?
  BB <- predat[grepl(simOpt, predat$test, fixed = TRUE),  ]
  subSimratio <- c()
  for(i in 1:length(unique(BB$subjectID))){
    sub            <- BB[BB$subjectID %in%  unique(BB$subjectID)[i],  ]
    sub_sim        <- sum(sub$sim)
    sub_full       <- sum(sub$sim==FALSE)
    subSimratio[i] <- sub_sim/(sub_full + sub_sim)
  }

  stats <- data.frame(simOptItem      = simOpt,
                      subjects        = length(unique(predat$subjectID)),
                      subjectCoverage = 1-mean(subSimratio),
                      naturalTies     = sum(predat$tie==TRUE & predat$sim ==FALSE),
                      NoOfSims        = sum(predat$tie==TRUE & predat$sim ==TRUE),
                      pref_A          = sum(predat$result ==  1),
                      pref_B          = sum(predat$result == -1),
                      totalTests      = dim(predat)[1] )
  return(stats)
}




