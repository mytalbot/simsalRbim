#' Worth Calculation
#'
#' The \code{bimworth} calculates the worth model using the curated preference
#' test data (e.g, from bimpre).
#'
#' @param ydata curated data.frame obtained from the bimpre function
#' @param GT item list with the ground truth (GT; letters are case sensitive!)
#' @param simOpt item to be checked (this can be an item from the GT or a new
#' one. Remove the item from GT if specified here!)
#' @param randOP randomize the test result for open/undecided pairs
#' (e.g., data marked as ties)
#' @param intrans calculate intransitivity during worth value estimation
#' (calculation intense! TRUE/FALSE)
#' @param showPlot plot the worth plot (default=FALSE)
#' @param ylim y limits of the worth plot (default c(0,0.7))
#' @param verbose show model outputs (default= FALSE)
#'
#' @import gnm
#' @import ggplot2
#' @importFrom reshape2 dcast
#' @import prefmod
#' @importFrom graphics plot
#' @importFrom stats anova as.formula poisson
#'
#' @return list with the worth values for each item and the total number of
#' simulated combinations based on simOption
#'
#' @export
#'


bimworth <- function(ydata=NULL, GT=NULL, simOpt=NULL,
                     randOP=FALSE, intrans=FALSE, showPlot=FALSE,
                     ylim=c(0,0.7), verbose=FALSE){

  # Decision Ratio (a quality estimatation)
  Dratio = sum(ydata$result==1)/sum(ydata$result==-1)


  # create the item list
  optionList <- c(GT, simOpt)

  # Randomize open pairs
  if(randOP==TRUE){
    nosim   <- ydata[ydata$sim==FALSE, ]
    simdat  <- ydata[ydata$sim==TRUE,  ]
    simdat$result  <- sample(c(0,1,-1), replace=TRUE, length(simdat$result))
    ydata   <- rbind(nosim, simdat)
  }else{}


  # calculate the intransitivity
  if(intrans==TRUE){
    I <- bimintrans(dat      = ydata,
                    idcolumn = "subjectID",
                    I2       = "optionB",
                    I1       = "optionA",
                    response = "result")
  }else{}


  ### Do the model
  modData    <- dcast(ydata, formula = subjectID ~ test,
                      fun.aggregate = sum, value.var = 'result')
  modData$subjectID <- NULL
  modelY     <- llbt.design(modData, nitems = length(optionList),
                            objnames = optionList   )

  formula    <- as.formula(paste("y~",
                                 paste( optionList[1:(length(optionList)-1) ] ,
                                        collapse="+")) )

  h1Y        <- gnm(formula, data = modelY, family = poisson  )
  #NOTE: last entry of optionList is the intercept


  # print the model summary
  if(verbose==TRUE){
    print(summary(h1Y))
  }else{}

  hworY      <- llbt.worth(h1Y, outmat ="worth")

  # show plot and adapt the error message
  if(showPlot==TRUE){
    plot( hworY, ylim=ylim, ylab="worth")
  }else{}

  if(intrans==TRUE){
    return(list(worth=hworY, I=I, Dratio=Dratio))
  }else{
    return(worth=hworY)
  }
}





