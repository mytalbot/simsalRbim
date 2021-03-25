#' Cutoff Determination
#'
#' The \code{bimpos} estimates a reasonable position of an incomplete item by
#' using an uninformed simulation of missing data.
#'
#' @param ydata curated data.frame from the preprocessing function.
#' @param GT item list with the ground truth (GT; letters are case sensitive!)
#' @param simOpt item to be checked (this can be an item from the GT or a new
#' one. Remove the item from GT if specified here!)
#' @param limitToRun limit to this number of repetitions for the
#' randomizations (default 5, you'll probably need more)
#' @param showPlot plot the worth plot (default=FALSE)
#'
#' @import ggplot2
#' @import dplyr
#' @import reshape2
#' @importFrom stringr str_split_fixed
#' @importFrom stats aov anova sd TukeyHSD qt
#'
#' @return list with the simulation errors (simerrors) and the plot object (p).
#'
#' @export
#'

bimpos <- function(ydata=NULL, GT=NULL, simOpt=NULL, limitToRun=5,
                   showPlot=TRUE ){

  # repeat the worth calculation n-times
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


  # melt to long format
  mW  <- melt(W, id.vars = c("run", "item","worth"))


  # calculate the simulation errors for all items
  simerrors  <- worth <- SD.W <- n.W <- Mean.W <- se.W <-NULL
  lower.ci.W <- upper.ci.W <- NULL
  simerrors <- mW %>%
    group_by(item) %>%
    summarise(Mean.W = mean(worth, na.rm = TRUE),
              SD.W   = sd(worth, na.rm = TRUE),
              n.W    = n()) %>%
    mutate(se.W = SD.W / sqrt(n.W),
           lower.ci.W = Mean.W - qt(1 - (0.05 / 2), n.W - 1) * se.W,
           upper.ci.W = Mean.W + qt(1 - (0.05 / 2), n.W - 1) * se.W) %>%
    arrange(desc(Mean.W)) %>%
    mutate(pos = 1:n()) %>%
    as.data.frame()

  # Colorize simOpt Item in the plot
  highlight_df <- simerrors %>%
    filter(item %in% simOpt)
  stitle       <- paste("No. of Randomizations: ", limitToRun,
                        " (tested item: ", simOpt, ")",sep="")


  # Do ze plotting
  simerrors      <- simerrors[order(factor(simerrors$pos)), ]
  simerrors$item <- factor(simerrors$item,
                           levels = simerrors$item[order(simerrors$pos)])


  psim <- ggplot(simerrors, aes(x=item, y=Mean.W  )) +
    geom_point() +
    geom_errorbar(aes(ymin=lower.ci.W, ymax=upper.ci.W), width=.1 ) +
    # ylim(mYlim) +
    labs(title    = "Optimized item position",
         subtitle  = stitle)   +
    ylab("Mean Worth Value") +
    xlab("") +
    labs(colour="Item") +
    theme_bw()
  psim <- psim + geom_point   (data    = highlight_df,
                               aes(x   = item,y=Mean.W),
                               color   = 'red', size=2)
  psim <- psim + geom_errorbar(data    = highlight_df,
                               aes(ymin=lower.ci.W,
                                   ymax=upper.ci.W),
                               width   =.1,
                               color   = 'red')
  psim <- psim +  theme(axis.line        = element_line(colour = "black"),
                        strip.background = element_rect(fill   = "white",
                                                        colour = "black",
                                                        size = 0.8),
                        strip.text       = element_text(size = 12),
                        axis.text.x      = element_text(size = 12),
                        axis.title.x     = element_text(size = 13),
                        axis.text.y      = element_text(size = 13),
                        axis.title.y     = element_text(size = 14))


  # show plot or not...
  if(showPlot==TRUE){
   print(psim)
  }else{}


  return(list(simerrors=simerrors, p=psim))
}



