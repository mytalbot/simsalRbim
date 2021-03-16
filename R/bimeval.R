#' Worth Values Evaluation Function
#'
#' The \code{bimeval} will evaluate the worth values and will give an estimate
#' on the error of uncertainty in the unsimulated data.
#'
#' @param ydata curated data.frame from the preprocessing function.
#' @param GT item list with the ground truth (GT; letters are case sensitive!)
#' @param simOpt item to be checked (this can be an item from the GT or a new
#' @param coverage the threshold for the ratio of tested subjects per total subjects (default=0.8)
#' @param worth the worth matrix from the bimworth function. Note, that this can be a list when the intransitivity is calculated!
#' @param showPlot plot the worth plot as a bubble plot with uncertainty errors
#' @param title plot title (default: "Consensus Analysis")
#' @param subtitle plot subtitle (if NULL, a default will be used)
#'
#' @import ggplot2
#' @import viridis
#' @import ggrepel
#'
#' @return either a data.frame with the consensus errors and/or a plot object
#'
#' @export
#'
#'
bimeval  <- function(ydata=NULL, worth= NULL, GT=NULL, simOpt=NULL,
                     coverage=0.8, showPlot=TRUE,
                     title="Consensus Analysis", subtitle=NULL){

  predat     <- ydata
  optionlist <- c(simOpt, GT)

  # compute the decision matrix for all data combinations in the predat object
  TT <- NULL
  for(i in 1:length(unique(predat$test))){
    tt <- data.frame(test   = as.character(unique(predat[predat$test==unique(predat$test)[i], "test"])),
                     pct_first      = sum(predat[predat$test==unique(predat$test)[i], ]$result== 1) / length(unique(predat$subjectID))*100,
                     pct_second     = 100 - (sum(predat[predat$test==unique(predat$test)[i], ]$result== 1) / length(unique(predat$subjectID))*100))

    TT <- rbind(TT, tt)
  }


  # check subjects data coverage - results may be underrepresented!
  BB        <- predat[grepl(simOpt,predat$test, fixed = TRUE),  ]
  provided  <- length(unique(BB$subjectID[BB$tie == FALSE   ]))
  simulated <- length(unique(BB$subjectID[BB$tie != FALSE   ]))
  subjratio <- provided/(simulated + provided)

  # item coverage
  II              <- predat[grepl(simOpt, predat$test, fixed = TRUE),]
  items_provided  <- length(unique(II$test [II$tie == FALSE   ]))
  items_simulated <- length(unique(II$test [II$tie != FALSE   ]))
  itemratio       <- items_provided/(items_simulated + items_provided)




  # warning: too few subjects
  if((subjratio)<coverage){
    warning(paste("simsalRbim: ","No. of SUBJECTS WARNING!
                The number of subjects you have provided for testing the simOpt=", "'", simOpt,"'" ," item is probably insufficient!
                Try increasing the number of subjects.
                You are currently below ", coverage*100, "% data coverage for that item.
                Your provided-to-simulated subjects ratio is at: ",
                  round(subjratio,4) * 100, "%.\n", sep=""))

  }else{}

  # warning if too few item combinations
  if((itemratio)<coverage){
  warning(paste("simsalRbim: ","No. of ITEMS WARNING!
                The number of item tests you have provided for testing the simOpt=", "'", simOpt,"'" ," item is probably insufficient!
                Try increasing the number of item combinations.
                You are currently below ", coverage*100, "% item coverage for that item.
                Your provided-to-simulated items ratio is at: ",
                round(itemratio,4) * 100, "%.\n", sep=""))
  }else{}





  # calculate the mean deviation weights
  weights    <- NULL
  for(j in 1:length(optionlist)){

    item  <- TT[grepl(optionlist[j], TT$test, fixed = TRUE),]
    liste <- NULL
    for(i in 1:dim(item)[1]){
      if( item[i, "pct_first" ] > item[i, "pct_second" ]  ){
        delta  <- (50 - item[i, "pct_second" ])
      }else{
        delta <- (50 - item[i, "pct_first" ])
      }
      liste    <- rbind(liste, data.frame(item =optionlist[j], delta = delta))
    }

    mean_delta <- mean(liste$delta, na.rm = TRUE)
    sd_delta   <- sd  (liste$delta, na.rm = TRUE)
    n_delta    <- length(liste$delta)
    lwr        <- mean_delta - qt(0.975,df=length(liste$delta)-1)*sd  (liste$delta, na.rm = TRUE)/sqrt(length(liste$delta))
    upr        <- mean_delta + qt(0.975,df=length(liste$delta)-1)*sd  (liste$delta, na.rm = TRUE)/sqrt(length(liste$delta))

    weights    <- rbind(weights, data.frame(item       = optionlist[j],
                                            mean_delta = mean_delta))#,
                                            #sd_delta   = sd_delta,
                                            #n          = n_delta,
                                            #lwr        = lwr,
                                            #upr        = upr ))
  }



  weights.sorted <- weights[order(rownames(weights)),]
  W              <- cbind(weights.sorted ,
                          worth = worth[match(weights.sorted$item, rownames(worth)),])

  #W <- label <- NULL
  W$uncert       <- round((50-W$mean_delta)/50*100,2)
  W$label        <- paste(W$item," ", round((50-W$mean_delta)/50*100,2), "%",sep="")




  # Plot
  if(showPlot==TRUE){

    # subtitle changes at user input
    if(length(subtitle)==0){
      subtitle <- paste("Bubble sizes indicate uncertainty in item choice (total=",round(mean(W$uncert),2),"%)",sep="")
    }else{
      subtitle <- subtitle
    }

    # setting the bubble color palette
    cols <- viridis_pal(option = "D")(length(W$item))

    p <- ggplot(W, aes(x=rep(1,dim(W)[1]), y=worth) ) +
      geom_point( aes(size = (50-mean_delta)/50*100), color="black", shape=21, fill=cols ) +
      geom_line() +
      labs(title     = title,
           subtitle  = subtitle,
           size      = "Consensus error (%)") +
      ylab("Mean Worth Value") +
      xlab("worth")  +
      theme_bw() +
      scale_x_discrete(limits = factor(1 ))
    #p <- p + scale_size(range = c(0, 10))
    p <- p +  theme(legend.position         = "top")
    p <- p + geom_label_repel(aes(label     = label),
                              size          = 4,
                              box.padding   = unit(0.6, "lines"),
                              point.padding = unit(0.6, "lines"),
                              show.legend   = FALSE )

    print(p)

    return(list(errors=W[,-5],p=p))

  }else{
    return(errors=W[,-5])
  }

}
