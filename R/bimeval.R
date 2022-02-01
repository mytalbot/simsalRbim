#' Worth Values Evaluation Function
#'
#' The \code{bimeval} will evaluate the worth values and will give an estimate
#' on the error of uncertainty (consensus error (CE) in the unsimulated data.
#'
#' @param ydata curated data.frame from the preprocessing function.
#' @param GT item list with the ground truth (GT; letters are case sensitive!)
#' @param simOpt item to be checked (this can be an item from the GT or a new
#' @param worth the worth matrix from the bimworth function. Note, that this can be a list when the intransitivity is calculated!
#' @param coverage the threshold for the ratio of tested subjects per total subjects (default=0.8)
#' @param showPlot plot the worth plot as a bubble plot with consensus errors
#' @param filtersim exclude the simulated items from the evaluation function (defaults to FALSE)
#' @param title plot title (default: "Consensus Analysis")
#' @param subtitle plot subtitle (if NULL, a default will be used)
#' @param ylim y limits of the worth plot (default c(0,0.7))
#'
#' @import ggplot2
#' @import viridis
#' @importFrom ggrepel geom_label_repel
#'
#' @return either a data.frame with the consensus errors and/or a plot object
#'
#' @export
#'
#'
bimeval <- function(ydata=NULL, GT=NULL, simOpt=NULL, worth= NULL, coverage=0.8,
                    showPlot=FALSE, filtersim=FALSE, title="Consensus Analysis",
                    subtitle=NULL, ylim=c(0,1)){

  predat     <- ydata
  optionlist <- c(simOpt, GT)

  # filter sim = TRUE; take only non randomized data
  if(filtersim==TRUE){
    predat     <- predat[predat$sim==FALSE, ]
  }else{}


  #### warnings #### -----------------------------------------------------------
  BB              <- predat[grepl(simOpt,predat$test, fixed = TRUE),  ]
  provided        <- length(unique(BB$subjectID [BB$sim == FALSE   ]))
  simulated       <- length( unique(BB$subjectID[BB$sim != FALSE   ]) )
  subjratio       <- provided/ (simulated )

  # item coverage
  II              <- predat[grepl(simOpt, predat$test, fixed = TRUE),]
  items_provided  <- length(unique(II$test [II$sim == FALSE   ]))
  items_simulated <- length(unique(II$test [II$sim != FALSE   ]))
  itemratio       <- items_provided/(items_simulated)

  # warning: too few subjects
  if((subjratio)<coverage){
    warning(paste("simsalRbim: ","No. of SUBJECTS WARNING!
                The number of subjects you have provided for testing the
                simOpt=", "'", simOpt,"'" ," item is probably insufficient!
                Try increasing the number of subjects.
                You are currently below ", coverage*100, "% data coverage for
                that item.
                The consensus error may be biased!
                Your provided-to-simulated subjects ratio is at: ",
                  round(subjratio,4) * 100, "%.\n", sep=""))

  }else{}

  # warning if too few item combinations
  if((itemratio)<coverage){
    warning(paste("simsalRbim: ","No. of ITEMS WARNING!
                The number of item tests you have provided for testing the
                simOpt=", "'", simOpt,"'" ," item is probably insufficient!
                Try increasing the number of item combinations.
                You are currently below ", coverage*100, "% item coverage for
                that item.
                The consensus error may be biased!
                Your provided-to-simulated items ratio is at: ",
                  round(itemratio,4) * 100, "%.\n", sep=""))
  }else{}
  #### warnings end #### -------------------------------------------------------





  # new CE loop
  items    <- unique(predat$test)
  preCE    <- NULL
  for(i in 1:length(items)){

    # select a unique testcombo
    seldat <- predat[predat$test %in% items[i], ]

    # how many subjects?
    # worth of each factor = 1/no of subjects
    wfact  <- 1/dim(seldat)[1] *100

    # Check if there are more subject entries than there should be
    if(length(unique(seldat$subjectID)) == dim(seldat)[1]){
    }else{
      warning("There are multiple subject-entries per item. Revise the subject
              names! The result will be wrong!")
    }

    B     <- sum(seldat$result == -1)
    A     <- sum(seldat$result ==  1)
    Tie   <- sum(seldat$result ==  0)

    ceA   <- (A*wfact  + (Tie*wfact/2))
    ceB   <- (B*wfact  + (Tie*wfact/2))

    preCE <- rbind(preCE, data.frame(test       = unique(seldat$test),
                                     n          = length(seldat$subjectID),
                                     no_of_ties = Tie,
                                     CE_A       = ceA,
                                     CE_B       = ceB))

  }


  # calculate the consensus error
  liste <- NULL
  for(i in 1:length(preCE$test  )){
    if( preCE[i, "CE_A" ] > preCE[i, "CE_B" ]  ){
      delta  <- (50 - preCE[i, "CE_B" ])
    }else{
      delta  <- (50 - preCE[i, "CE_A" ])
    }
    liste    <- rbind(liste, data.frame(item       = preCE$test[i],
                                        n          = preCE[i, "n"],
                                        no_of_ties = preCE[i, "no_of_ties"],
                                        CE_A       = preCE[i, "CE_A"],
                                        CE_B       = preCE[i, "CE_B"],
                                        delta      = delta,
                                        CEraw      = 50-delta,
                                        CE         = (50-delta)/50*100))

  }



  # calculate the mean performance for each of the items in the worth list
  querydat <- NULL
  inter    <- NULL
  errors   <- NULL
  for( j in 1:length(optionlist)){
    querydat <- liste[grep(optionlist[j], liste$item),]
    meanCE   <- mean(querydat$CE, na.rm=TRUE)

    inter    <-  data.frame(item   = optionlist[j],
                            n      = dim(querydat)[1],
                            meanCE = meanCE,
                            SD     = sd  (querydat$CE, na.rm=TRUE) )
    #lwr    = meanCE - qt(0.975, df= length(querydat)-1) * sd(querydat$CE, na.rm = TRUE)/sqrt(length(querydat$CE)),
    #upr    = meanCE + qt(0.975, df= length(querydat)-1) * sd(querydat$CE, na.rm = TRUE)/sqrt(length(querydat$CE)))

    errors   <- rbind(errors, data.frame(item    = optionlist[j],
                                         n       = inter$n,
                                         worth   = worth[grep(optionlist[j],  rownames(worth) ) ],
                                         CE      = round(inter$meanCE,2)))
    #lwr     = round(inter$lwr,2),
    #upr     = round(inter$upr,2)))



  }


  # order like the worth value & label the data
  W <- errors[match( rownames(worth), errors$item),]
  W$label        <- paste(W$item," (CE=",  W$CE , "%)",sep="")



  # Plot
  if(showPlot==TRUE){

    # subtitle changes at user input
    if(length(subtitle)!=0){
      subtitle <- paste("Bubble sizes indicate uncertainty in item choice
                        (total=",round(mean(W$CE),2),"%)",sep="")
    }else{
      subtitle <- subtitle
    }

    # setting the bubble color palette
    cols <- viridis_pal(option = "D")(length(W$item))

    p <- ggplot(W, aes(x=rep(1,dim(W)[1]), y=worth) ) +
      geom_point(W,mapping=aes(size = CE), color="black",
                 shape=21, fill=cols )  +
      geom_line() +


      labs(title     = title,
           subtitle  = subtitle,
           size      = "Consensus error (%)") +
      ylab("Worth value") +
      xlab("Items")  +
      ylim(ylim) +
      theme_bw() +
      scale_x_discrete(limits = factor(1 ))
    #p <- p + scale_size(range = c(0, 10))
    p <- p +  theme(legend.position         = "top")
    p <- p + geom_label_repel(aes(label     = label),
                              size          = 4,
                              box.padding   = unit(0.6, "lines"),
                              point.padding = unit(0.6, "lines"),
                              show.legend   = FALSE )

    p <- p +  theme(axis.line        = element_line(colour = "black"),
                    strip.background = element_rect(fill = "white",
                                                    colour = "black",size=0.8),
                    strip.text       = element_text(size = 12),
                    axis.text.x      = element_text(size = 12),
                    axis.title.x     = element_text(size = 13),
                    axis.text.y      = element_text(size = 13),
                    axis.title.y     = element_text(size = 14))
    p <- p + theme(axis.text.x = element_blank())
    print(p)

    return(list(predat=predat, preCE=preCE, detailed=liste, errors=W, p=p))

  }else{
    return(list(predat=predat, preCE=preCE, detailed=liste, errors=W ))
  }

}

