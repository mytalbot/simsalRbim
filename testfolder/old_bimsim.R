
bimsim <- function(rawdat=NULL, GT=GT, simOpt=simOpt,
                   limitToRun=5, tcut=100, deviation=0, minQuantity=0,
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
    ydata   <- rbind(nosim, simdat)
    #ydata

    worth      <- bimworth(ydata    = ydata,
                           GT       = GT,
                           simOpt   = simOpt,
                           randOP   = FALSE,
                           intrans  = FALSE)
    options(warn=-1)
    w_errors   <- bimeval(ydata     = ydata,
                          worth     = worth,
                          GT        = GT,
                          simOpt    = simOpt,
                          filtersim = FALSE)
    options(warn=0)

    d <- w_errors %>%
      arrange(desc(worth)) %>%
      mutate(pos = 1:n()) %>%
      as.data.frame()

    D <- rbind(D, data.frame(run        = i, d) )
  }

  # add to X-scale for better optics
  L    <- length(GT) + 1

  # plot
  p  <- D %>%
    filter(item %in% simOpt) %>%
    filter(CE <= tcut) %>%
    ggplot(aes(x=factor(pos), y=worth)) +
    geom_jitter( aes(size =  CE, fill=CE ),  shape = 21, alpha = 0.7,
                 width=0.2) +
    ylim(ylim) +
    scale_fill_viridis_c(guide = "legend") +
    scale_size_continuous(range = c(0, L+1  )) + # adjust this
    xlab("Position") +
    ylab("Mean Worth Value")   +
    labs(title    = "Informed position simulation",
         subtitle = paste("Item: ",simOpt, " at ", limitToRun,
                          " randomizations (CE cutoff=",tcut,"%).", sep=""),
         legend   = "Consensus Error") +
    theme_bw() +
    theme(axis.title.x = element_text(hjust= 0.5)) +
    theme(axis.title.y = element_text(hjust= 0.5)) +
    scale_x_discrete(limits = factor(1:L ))
  p <- p +  theme(legend.position  = "top")

  if(showPlot==TRUE){
    print(p)
  }else{}

  fltrd <- D %>%
    filter(item %in% simOpt) %>%
    filter(CE <= tcut)

  frq   <- round(table(fltrd$pos) / sum(table(fltrd$pos)),4)

  return(list(frq=frq, p=p))
}

