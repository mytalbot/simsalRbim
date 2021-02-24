#' Uninformed positioning and cutoff determination
#'
#' The \code{bimUninformed} estimates reasonable randomization cutoffs for
#' simOption items. The cutoff is determined by monitoring the adjusted p-value
#' development in post-hoc ANOVA tests. The optimal cutoff is reached when
#' the adjusted p-values show no overlap, or zero deviation in the plot.
#'
#' @param ydata curated data.frame from the preprocessing function.
#' @param GT item list with the ground truth (GT; letters are case sensitive!)
#' @param simOpt item to be checked (this can be an item from the GT or a new
#' one. Remove the item from GT if specified here!)
#' @param limitToRun limit to this number of repetitions for the randomizations
#' (default 5, you'll probably need more)
#' @param seed use constant seeding for stable plot results (default=TRUE)
#' @param showPlot show stability plot
#' @param ylim y-limits of the output plot (default c(-0.5,1.5))
#'
#' @import ggplot2
#' @import dplyr
#' @importFrom stringr str_split_fixed
#' @importFrom stats aov anova sd TukeyHSD qt
#'
#' @return list with the randomized worth values (W), the result of the posthoc
#' tests (posthocs), the cutoff values (cutoffs) and
#' the conditional plot output (p).
#'
#' @export
#'

bimUninformed <- function(ydata=NULL, GT=NULL, simOpt=NULL, limitToRun=5,
                          seed=TRUE, showPlot=TRUE, ylim=c(-0.5,1.5) ){


  # if there is variance, continue, else break
  if( sum( ydata$sim ) != 0){


  # Helper function
  printf     <- function(...) cat(sprintf(...))

  # set variable bindings to avoid global bining errors
  W      <- run <- item <- NULL
  itemCI <- `p adj` <- norma <- NULL

  # use seeding?
  if(seed==TRUE){
    set.seed(55)
  }else{}

  # randomization procedure
  reps    <- limitToRun
  deviate <- 0
  W       <- NULL
  for(i in 1:reps){

    w       <- bimworth(ydata             = ydata,
                        GT                = GT,
                        simOpt            = simOpt,
                        randOP            = TRUE,
                        showPlot          = FALSE)

    W       <- rbind(W, data.frame(run    = i,
                                   item   = rownames(w),
                                   worth  = as.numeric(w)))


  }


    # calculate posthoc tests for every run from the randomization function
    ph      <- NULL
    sumPadj <- c()

    for(i in 2:length(unique(W$run))){

      adat  <- W[W$run %in% 1:i, ]
      res   <- summary(fit   <- aov(worth ~ item, data=adat))

      posthoc                <- cbind(
        str_split_fixed(rownames(TukeyHSD(fit)$item), "-", 2),
        as.data.frame(round(TukeyHSD(fit)$item,4)))
      posthoc$run            <- i
      colnames(posthoc)[1:2] <- c("item1", "item2")
      rownames(posthoc)      <- NULL
      posthoc$item1          <- as.character(posthoc$item1)
      posthoc$item2          <- as.character(posthoc$item2)

      ph                     <- rbind(ph, posthoc)

      # asses p-values at each run!
      sumPadj[i]             <- sum( posthoc[ posthoc$item2 == simOpt |
                                                posthoc$item1 == simOpt ,  6 ] )
    }


    # sum up the adjusted pvalues from the ANOVA calculations
    itemCI <- ph %>%
      group_by(run)   %>%
      dplyr:: filter(item1 == simOpt | item2 == simOpt) %>%
      summarise(meanItem = mean(`p adj`, na.rm = TRUE)  ,
                SD       = sd(`p adj`,   na.rm = TRUE),
                n.W      = n(), .groups = 'drop') %>%
      as.data.frame() %>%
      mutate(SE  = SD / sqrt(n.W),
             lwr = meanItem - qt(1 - (0.05 / 2), n.W - 1) * SE,
             upr = meanItem + qt(1 - (0.05 / 2), n.W - 1) * SE,
             delta = upr-lwr)

    # introduce threshold-colored points
    itemCI$col   <- cut(itemCI$upr,
                        breaks = c(-Inf, 0.05, Inf),
                        labels = c("<=0.05", ">0.05"))


    # Do ze plotting
    p1 <- ggplot(itemCI, aes(x=run, y=meanItem, color=col  )) +
      geom_point(size=2) +
      geom_errorbar(aes(ymin=lwr, ymax=upr), width=.3) +

      labs(title    = bquote(paste("Stability development (",
                                   p[adj], "+95% CIs)")), #expression(p[adj]),
           subtitle = paste("Test item: ", simOpt, sep=""),
           color    = "lower CI threshold") +
      ylim(ylim) +
      ylab("Mean adjusted p-values") +
      xlab("Randomization run") +
      geom_hline(yintercept = 0.05, color="red",  linetype="dashed")   +
      geom_hline(yintercept = 0,    color="gray", linetype="solid")   +
      theme_bw() +
      scale_color_manual(values = c("darkgreen", "darkred"))

    p1 <- p1 +  theme(legend.position  = "top",
                      # panel.border     = element_blank(),
                      #  panel.grid.major = element_blank(),
                      # panel.grid.minor = element_blank(),
                      axis.line        = element_line(colour = "black"),
                      strip.background = element_rect(fill = "white",
                                                      colour = "black",
                                                      size = 0.8),
                      strip.text       = element_text(size = 12),
                      axis.text.x      = element_text(size = 12),
                      axis.title.x     = element_text(size = 13),
                      axis.text.y      = element_text(size = 13),
                      axis.title.y     = element_text(size = 14))

    if(showPlot==TRUE){
      print(p1)
    }else{}


    cutoff <- itemCI[itemCI$delta ==0,][1,]$run

    if(is.na(cutoff)==TRUE){
      printf("simsalRbim: Random fluctuation is still high -
                   try increasing the number of runs.\n")
    }else{
      cutoff
    }

    return(list(cutoff=cutoff, p=p1))

  }else{
    warning("There is no variation in the data. Cutoff cannot be calculated.")
  }

}




