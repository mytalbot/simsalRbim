#' Preference Test Simulation with Variable Ground Truth
#'
#' The \code{bimsim} function covers the informed simulation of the preference
#' test data, e.g., when an additional item was tested but has ties and/or
#' missing item combinations according to the ground truth. With the help of
#' intransitivity calculations a cutoff can be set to exclude any simulation
#' results that do not show transitivity.
#'
#' @param rawdat rawdata from the bimload function
#' @param GT item list with the ground truth (GT; letters are case sensitive!)
#' @param simOpt item to be checked (this can be an item from the GT or a new
#' one. Remove the item from GT if specified here!)
#' @param filter.crit the filter criterion for the plot (defaults to "CE":
#' consensus error; alternatively the intransitivity ratio (Iratio) can be
#' shown)
#' @param limitToRun limit to this number of repetitions for the randomizations
#' (default 5, you'll probably need more)
#' @param tcut threshold (cutoff) for the filter.crit variable (default = 1 for
#' 100% of the values); i.e., when filter.crit="CE" and tcut=0.9, the plot will
#' show 90% of the consensus errors. When filter.crit="Iratio" and tcut =0.9,
#' the plot will show items with transitivities > 90%.
#' @param deviation how much deviation is allowed for marking ties
#' (default is 0 which translates to 50%; deviation =5 adds 5% to 50% = 55%)
#' @param minQuantity minimum quantity to be recognized as difference
#' (default is 0)
#' @param seed use constant seeding for stable plot results (default=TRUE)
#' @param showPlot show simulation runs with consensus error bubbles
#' @param ylim scale y axis of the worth plot (default: c(0,0.7))
#'
#' @import ggplot2
#' @import viridis
#' @importFrom dplyr filter '%>%'
#'
#' @return list with the position frequencies and the plot object
#'
#' @export
#'


bimsim <- function(rawdat=NULL, GT=GT, simOpt=simOpt, filter.crit="CE",
                   limitToRun=5, tcut=0.9, deviation=0, minQuantity=0,
                   seed=TRUE, showPlot=TRUE,ylim=c(0,0.7)){

  # Helperfunction
  printf <- function(...) cat(sprintf(...))

  # use seeding?
  if(seed==TRUE){
    set.seed(123)
  }else{}


  # do the simulation
  reps    <- limitToRun
  deviate <- 0
  W       <- NULL
  D       <- NULL
  D       <- pos <- CE <- NULL
  for(i in 1:reps){

    predat     <- bimpre (dat=rawdat, GT=GT, simOpt=simOpt, deviation=deviation,
                          minQuantity=minQuantity, verbose=FALSE)

    # Randomize open pairs
    nosim   <- predat[predat$sim==FALSE, ]
    simdat  <- predat[predat$sim==TRUE,  ]
    simdat$result  <- sample(c(0,1,-1), replace=TRUE, length(simdat$result))
    ydata          <- rbind(nosim, simdat)

    worth      <- bimworth(ydata    = ydata,
                           GT       = GT,
                           simOpt   = simOpt,
                           randOP   = FALSE,
                           intrans  = ifelse(filter.crit=="CE",FALSE,TRUE))

    # adapt to worth matrix change when CE is used
    if(filter.crit=="CE"){
      options(warn=-1)
      w_errors   <- bimeval(ydata     = ydata,
                            worth     = worth,
                            GT        = GT,
                            simOpt    = simOpt,
                            filtersim = FALSE)
      options(warn=0)

      # add the intransitivity ratio
      w_errors$Iratio <- NULL
      w_errors$Iratio <- NA

    }else{
      options(warn=-1)
      w_errors   <- bimeval(ydata     = ydata,
                            worth     = worth$worth,
                            GT        = GT,
                            simOpt    = simOpt,
                            filtersim = FALSE)
      options(warn=0)

      # add the intransitivity ratio
      w_errors$Iratio <- NULL
      w_errors$Iratio <- worth$I$Iratio

    }





    # arrange all
    d <- w_errors %>%
      arrange(desc(worth)) %>%
      mutate(pos = 1:n()) %>%
      as.data.frame()

    D <- rbind(D, data.frame(run        = i, d) )
  }

  # add to X-scale for better optics
  L    <- length(GT) + 1


  # adapt the CE scale
  D$CE <- D$CE / 100


  # adapt the legend label
  if (filter.crit == "CE") {
    legend.label  = "Consensus Error (CE)"

    # plot CR
    p  <- D %>%
      filter(item %in% simOpt) %>%
      filter(get(filter.crit)  <= tcut) %>%
      ggplot(aes(x=factor(pos), y=worth)) +
      geom_jitter( aes(size =  get(filter.crit), fill=get(filter.crit) ),
                   shape = 21, alpha = 0.7, width=0.2) +
      ylim(ylim) +
      scale_fill_viridis_c(guide = "legend") +
      scale_size_continuous(range = c(0, L+1  )) + # adjust this
      xlab("Position") +
      ylab("Mean Worth Value")   +
      labs(title    = "Informed position simulation",
           subtitle = paste("Item: ", simOpt, " at ", limitToRun,
                            " randomizations (", filter.crit," cutoff=",
                            tcut*100,"%).", sep="")) +
      theme_bw() +
      theme(axis.title.x = element_text(hjust= 0.5)) +
      theme(axis.title.y = element_text(hjust= 0.5)) +
      scale_x_discrete(limits = factor(1:L ))
    p <- p +  theme(legend.position  = "top")
    p <- p + labs(fill = legend.label, size=legend.label)


  } else if (filter.crit == "Iratio") {
    legend.label  = "Transitivity Ratio (1-Iratio)"

    # plot IRatio
    D <- D %>%
      mutate(D, trRatio= 1-Iratio)

    p  <- D %>%
      filter(item %in% simOpt) %>%
      filter(trRatio  >= tcut) %>%
      ggplot(aes(x=factor(pos), y=worth)) +
      geom_jitter( aes(size = trRatio, fill=trRatio ),
                   shape = 21, alpha = 0.7, width=0.2) +
      ylim(ylim) +

      scale_fill_viridis_c(guide = "legend" ) +
         scale_size_continuous(range = c(1, L +1 )) + # adjust this
      xlab("Position") +
      ylab("Mean Worth Value")   +
      labs(title    = "Informed position simulation",
           subtitle = paste("Item: ", simOpt, " at ", limitToRun,
                            " randomizations (1-", filter.crit," cutoff=",
                            tcut*100,"%).", sep="")) +
      theme_bw() +
      theme(axis.title.x = element_text(hjust= 0.5)) +
      theme(axis.title.y = element_text(hjust= 0.5)) +
      scale_x_discrete(limits = factor(1:L ))
    p <- p +  theme(legend.position  = "top")
    p <- p + labs(fill = legend.label, size=legend.label)

  }


  if(showPlot==TRUE){
    print(p)
  }else{}


  # filter differently!
  if (filter.crit == "CE") {
  fltrd <- D %>%
    filter(item %in% simOpt) %>%
    filter(CE <= tcut)

    frq   <-  table(fltrd$pos) / sum(table(fltrd$pos))

  }else{
    fltrd <- D %>%
      filter(item %in% simOpt) %>%
      filter(trRatio >= tcut)

    frq   <-  table(fltrd$pos) / sum(table(fltrd$pos))
  }




  return(list(D=D, frq=frq, p=p))
}

