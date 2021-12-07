#' Integrated Position Simulation Function
#'
#' The \code{bimposim} function estimates the position of an item for which
#' limited data are available by testing it against 2 other items from the fully
#' available data. The open item positions are randomly filled and simulated.
#' The postion estimation is derived from the best simulated transitivity -
#' therefore, the outcome is dependent on the number of runs, relative to the
#' totally available item combinations and data completeness. The transitivity
#' cutoff is set to be no less than 10% than the transitivity in the worth model
#' without the tested item. The user can, however, specify other cutoffs.
#'
#' @param dat raw data from the bim_load function
#' @param testitem item of interest (to determine the hypothetical position of the item)
#' @param GT item list with the ground truth (GT; letters are case sensitive!)
#' @param tested_1 testitem is tested against this item
#' @param tested_2 testitem is tested against this item as well
#' @param threshold the default cutoff for the lower end of the Iratio is 10 (%).
#' @param runs the number of simulation runs (default is 50)
#' @param tcut the transitivity ratio cut (default is NULL) - at the default level
#' the function uses the threshold parameter as cutoff.
#' @param ylim y limits of the worth plot (default c(0,1))
#'
#' @import gnm
#' @import ggplot2
#' @importFrom reshape2 dcast
#' @import prefmod
#' @importFrom graphics plot
#' @importFrom ggrepel geom_label_repel
#'
#' @return list with the Iratio cutoff recommendation, the simulated data per run,
#' the frequency distribution of the simulated positions with the corresponding
#' Iratios, the simulated position plot (a bubbleplot), the optimal run with the
#' best simulated transitivity as well as the reconstructed worth plot, including
#' the simulated item at the optimally found position
#'
#' @export
#'

bimposim <- function(dat=NULL, testitem=NULL, GT=NULL, tested_1=NULL,
                     tested_2=NULL, threshold=10, runs=50, tcut=NULL, seed=FALSE,
                     ylim=c(0,1)){


  ### generate test and default data
  testset    <- dat[ (dat$optionA==testitem | dat$optionB==testitem),]
  default    <- dat[!(dat$optionA==testitem | dat$optionB==testitem),]

  ### Determine Transitivity Cutoff for the rest of the data
  simOpt1    <- GT [1]
  GT1        <- GT[-1]
  predat1    <- bimpre (dat=default, GT=GT1, simOpt=simOpt1, deviation=0, minQuantity=0)
  transcut   <- round(bimintrans(predat1)$Iratio - (bimintrans(predat1)$Iratio/100 * threshold),4)

  print(paste("The Iratio cutoff should at least be higher than: ", transcut, sep=""))


  ### Build a test set
  sim_func   <- function (A, B){
    testset[((  testset$optionA==testitem & testset$optionB == A) | (testset$optionA == A & testset$optionB==testitem) |
               (   testset$optionA==testitem & testset$optionB == B) | (testset$optionA == B & testset$optionB==testitem) ),]
  }
  test       <- sim_func(tested_1,tested_2)
  dat_sim    <- rbind(default,test)


  ### simulate
  frqnc      <- bimsim(rawdat      = dat_sim,
                       GT          = GT,
                       simOpt      = testitem,
                       limitToRun  = runs,
                       seed        = seed,
                       showPlot    = FALSE,
                       tcut        = if(is.null(tcut)){transcut=transcut } else{ transcut=tcut},
                       ylim        = ylim,
                       filter.crit = "Iratio" )

  simplot    <- frqnc$p
  simdat     <- frqnc$D
  simfreq    <- frqnc$frq

  # Reconstruct the worth plot from the best result -------------------------
  best       <- frqnc$D
  optimal    <- best[best$trRatio == max(best$trRatio)[1], ]
  optimal    <- optimal[ optimal$run == min(optimal$run)[1], ]


  # Result of the best simulated position
  print(paste("Best simulated transitivity for ", testitem,": ",
              round(unique(optimal$trRatio),2), " at position ",
              optimal[optimal$item == testitem, "pos"],sep=""))

  ### color function (standard colors) for the reconstructed Worth plot
  hcl <- NULL
  gg_color_hue <- function(n) {
    hues = seq(15, 375, length = n + 1)
    hcl(h = hues, l = 65, c = 100)[1:n]
  }
  n    = length(c(testitem, GT))
  cols = factor(gg_color_hue(n), levels=gg_color_hue(n))

  # press into order and attribute colors - otherwise messed up!
  df <- data.frame(optimal)
  df <- df[order(df$worth, decreasing = TRUE), ,drop=F ]
  cbind(df, cols=cols)


  ### Do ze worth plot
  p <- ggplot(optimal, aes(x=rep(1,dim(optimal)[1]), y=worth) ) +
    geom_line() +
    geom_point(color="black", shape=21, size=5, fill= cols, stroke=1.2  ) +
    labs(title     = paste(testitem, " simulated against ", tested_1, " and ", tested_2, sep=""),
         subtitle  = paste("Best simulated transitivity for ", testitem,": ",
                           round(unique(optimal$trRatio),2), " at position ",optimal[optimal$item == testitem, "pos"],sep="")) +
    ylab("Worth value") +
    xlab("Items")  +
    ylim(ylim) +
    theme_bw() +
    scale_x_discrete(limits = factor(1))

  p <- p + theme(legend.position         = "none")

  p <- p + geom_label_repel(aes(label     = label),
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


  return(list(transcut=transcut, simdat=simdat, simfreq=simfreq, simplot=simplot,
              optimal=optimal, simworthplot=p))

}




