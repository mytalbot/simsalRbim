#' Preference Test Simulation with Variable Ground Truth
#'
#' The \code{bimsim} function covers the informed simulation of the preference
#' test data, e.g., when an additional item was tested but has ties and/or
#' missing item combinations according to the ground truth. With the help of
#' intransitivity calculations a cutoff can be set to exclude any simulation
#' results that do not show transitivity.
#'
#' @param ydata curated data.frame from the preprocessing function.
#' @param GT item list with the ground truth (GT; letters are case sensitive!)
#' @param simOpt item to be checked (this can be an item from the GT or a new
#' one. Remove the item from GT if specified here!)
#' @param limitToRun limit to this number of repetitions for the randomizations
#'  (default 5, you'll probably need more)
#' @param seed use constant seeding for stable plot results (default=TRUE)
#' @param fval filter value for informed transitivity checks (default = 0.9 for
#'  90% of all possible values), the lower the value the more informed the
#'  positioning becomes
#' @param showPlot show the bubble plot with the intransitivity value vs item
#' position (bubble size=worth-value)
#' @param ylim scale y axis of the worth plot (default: c(0,0.7))
#'
#' @import ggplot2
#' @importFrom dplyr filter '%>%'
#'
#' @return list with the position frequencies and the plot object
#'
#' @export
#'

bimsim <- function(ydata = NULL, GT=NULL, simOpt=NULL, limitToRun=10,
                   seed=TRUE, fval= 0.9, showPlot=TRUE, ylim=c(0,0.7)){

  # Helperfunction
  printf <- function(...) cat(sprintf(...))

  # variable definition
  worth      <- pos <- NULL

  # use seeding?
  if(seed==TRUE){
    set.seed(123)
  }else{}

  # do the simulation
  reps    <- limitToRun
  deviate <- 0
  W       <- NULL
  D       <- NULL
  for(i in 1:reps){
    D       <- NULL
    w       <- bimworth(ydata           = ydata,
                        GT              = GT,
                        simOpt          = simOpt,
                        randOP          = TRUE,
                        showPlot        = FALSE,
                        intrans         = TRUE)

    D <- rbind(D, data.frame(run        = i,
                             Icounts    = w$I$intranscount,
                             triplets   = w$I$no_tripl,
                             Iratio     = w$I$Iratio,
                             item       = rownames(w$worth),
                             worth      = as.numeric(w$worth)))


    d <- D %>%
      arrange(desc(worth)) %>%
      mutate(pos = 1:n()) %>%
      as.data.frame()

    W <- rbind(W, d)
  }


  # add to X-scale for better optics
  L    <- length(GT) + 1

  # Plot
  p  <- W %>%
    filter(item %in% simOpt) %>%
    filter(Iratio <= fval) %>%
    ggplot(aes(x=factor(pos), y=worth)) +
    geom_jitter( aes(size = Iratio, fill =  Iratio),  shape = 21, alpha = 0.7,
                 width=0.2) +
    ylim(0,0.7) +
    scale_fill_viridis_c(guide = "legend") +
    scale_size_continuous(range = c(0, L+1  )) + # adjust this
    xlab("Position") +
    ylab("Mean Worth Value")   +
    labs(title    = "Informed position simulation",
         subtitle = paste("Item: ",simOpt, " at ", limitToRun,
                          " randomizations (Iratio cutoff=",fval,").", sep=""),
         legend   = "Intransitivity Ratio") +
    theme_bw() +
    theme(axis.title.x = element_text(hjust= 0.5)) +
    theme(axis.title.y = element_text(hjust= 0.5)) +
    scale_x_discrete(limits = factor(1:L ))
  p <- p +  theme(legend.position  = "top")

  if(showPlot==TRUE){
    print(p)
  }else{}

  fltrd <- W %>%
    filter(item %in% simOpt)%>%
    filter(Iratio <= fval)

  frq   <- round(table(fltrd$pos) / sum(table(fltrd$pos)),4)

  return(list(frq=frq, p=p))
}




