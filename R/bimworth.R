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
#' @param showPlot plot the worth plot (c("FALSE","worth","coef"))
#' @param ylim y limits of the worth plot (default c(0,0.7))
#' @param size point size in the worth plot (default = 7)
#' @param verbose show model outputs (default= FALSE)
#'
#' @import gnm
#' @import ggplot2
#' @importFrom reshape2 dcast
#' @import prefmod
#' @importFrom graphics plot
#' @importFrom stats anova as.formula poisson
#' @importFrom stats confint
#' @importFrom stats coef
#' @importFrom ggrepel geom_label_repel
#'
#' @return list with the worth values for each item and the total number of
#' simulated combinations based on simOption
#'
#' @export
#'


bimworth <- function(ydata=NULL, GT=NULL, simOpt=NULL,
                     randOP=FALSE, intrans=FALSE, showPlot=FALSE,
                     ylim=c(0,0.8), size = 5, verbose=FALSE){

  worth <- NULL

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



  # Show the model estimates with 95% CIs in a separate plot
  if(showPlot=="coef"){

    f <- function(d) {
      fit <- do.call("gnm", list(formula, data = d, family = poisson ))
      stats::confint(fit)
    }
    CI <- as.data.frame(f(as.data.frame(modelY)))
    rownames(CI)[1] <- simOpt


    #hworY           <- llbt.worth(h1Y, outmat ="est")
    hworY           <- as.matrix(coef(h1Y))
    colnames(hworY) <- "estimate"
    row.names(hworY)[1] <- simOpt


    new_df          <- cbind(hworY[row.names(CI), ], CI)
    colnames(new_df)<- c("estimate","lwr","upr")
    new_df$item     <- rownames(new_df)


    new_df$item <- factor(new_df$item,
                             levels = new_df$item[order(new_df$estimate)])


    if(verbose==TRUE){
      print(new_df)
    }else{}

    p <- ggplot(new_df, aes(x=item, y=estimate)) +
      geom_point(size=2) +
      geom_errorbar(aes(ymin=lwr, ymax=upr), width=.1 ) +
      # ylim(mYlim) +
      labs(title    = "Item comparisons (no simulation)",
           subtitle  = "Model coefficients with 95% confidence intervals")   +
      ylab("Estimate") +
      xlab("") +
      labs(colour="Item") +
      theme_bw()
    p <- p + coord_flip()
    print(p)

  }else{ }


  # print the model summary
  if(verbose==TRUE){
    print(summary(h1Y))
  }else{}

  hworY      <- llbt.worth(h1Y, outmat ="worth")

  # show plot and adapt the error message
  if(showPlot=="worth"){
    # plot( hworY, ylim=ylim, ylab="Worth value") # traditional worth plot


    # ze own worthplot --------------------------------------------------------

    # color function (standard colors)
    hcl <- NULL
    gg_color_hue <- function(n) {
      hues = seq(15, 375, length = n + 1)
      hcl(h = hues, l = 65, c = 100)[1:n]
    }

    n    = dim(hworY)[1]
    cols = gg_color_hue(n)

    # plotting the worth values
    p <- ggplot(data.frame(hworY), aes(x=rep(1,dim(hworY)[1]), y=worth) ) +
      geom_line() +
      geom_point(color="black", shape=21, size=size, fill= cols, stroke=1.2  ) +
      labs(title     = "Preferences") +
      ylab("Worth value") +
      xlab("Items")  +
      ylim(ylim) +
      theme_bw() +
      scale_x_discrete(limits = factor(1))

    p <- p + theme(legend.position         = "none")

    p <- p + geom_label_repel(aes(label     = rownames(data.frame(hworY))),
                              size          = 4,
                              box.padding   = unit(1.2, "lines"),
                              point.padding = unit(1.2, "lines"),
                              show.legend   = FALSE )

    p <- p + theme(axis.text.x = element_blank())

    p <- p +  theme(axis.line        = element_line(colour = "black"),
                      strip.background = element_rect(fill = "white", colour = "black", size = 0.8),
                      strip.text       = element_text(size = 12),
                      axis.text.x      = element_text(size = 12),
                      axis.title.x     = element_text(size = 13),
                      axis.text.y      = element_text(size = 13),
                      axis.title.y     = element_text(size = 14))
    p <- p + theme(axis.text.x = element_blank())
    print(p)

  }else{}

  if(intrans==TRUE){
    return(list(worth=hworY, I=I, Dratio=Dratio))
  }else{
    return(worth=hworY)
  }
}





