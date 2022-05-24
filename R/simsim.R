#' Item Position Simulation in a Preference Test
#'
#' With \code{simsim} function the putative position of new items in a
#' preference test can be simulated based on its Worth Value. This requires a
#' minimum set of tested items and the remaining combinations with the ground
#' truth (GT) will be simulated. The performance of the simulation can be
#' assessed with the amount of intransitive results (expressed as a ratio:
#' Iratio) and the overall consensus error (CE). The more experimentally tested
#' item combinations enter the simulation, the more accurate the simulated
#' position will be.
#'
#' @param data rawdata from the bimload function
#' @param GT item list with the ground truth (GT; letters are case sensitive!)
#' @param simOpt item to be checked (this can be an item from the GT or a new
#' one. Remove the item from GT if specified here!)
#' @param filter.crit the filter criterion for the plot (defaults to "CE":
#' consensus error; alternatively the intransitivity ratio (Iratio) can be
#' shown)
#' @param runs the number of simulation runs (e.g., 200)
#' @param seeding use constant seeding for stable simulation results
#' (default=TRUE)
#' @param truepos the true position of the item (most likely unknown); used for
#' performance testing
#' @param verbose FALSE silences the bimpre output on the number of simulated
#' ties
#' @param path path where the results of the simulations are saved as *.txt
#' files. If left empty (NULL), the results will only be stored in memory.
#'
#' @importFrom dplyr filter '%>%'
#'
#' @return list with the position frequencies and the plot object
#'
#' @export


# the simulation function and param extraction ----------------------------
simsim <- function(data=NULL, simOpt=NULL, GT=NULL, seeding=TRUE, runs=NULL,
                    truepos=NULL, verbose=FALSE, path=NULL ){

  # calculate all possible combinations and label them after the GT
  combis           <- expand.grid(rep(list(0:1), length(GT)))
  colnames(combis) <- GT
  combis           <- combis[-1,]
  combis           <- combis[order(apply(combis,1,sum)),]

  # set the seeding constant or not...
  if(seeding == TRUE){set.seed(123)}else{}

  # do the simulation function
  all <- NULL
  for(i in 1:dim(combis)[1]){
    CO       <- as.data.frame(combis[i,])
    againsts <- names(CO)[CO==1]

    res      <- nusim(data    = data,
                      simOpt  = simOpt,
                      GT      = GT,
                      against = as.character(againsts),
                      verbose = verbose,
                      runs    = runs)

    # select the best result
    ordres   <- NULL
    ordres   <- res[  order( res$Iratio, res$CE) ,] [1,]

    # if the truth is not known, the tp_frq and tp cannot be calculated
    if(is.null(truepos)){
      all      <- rbind(all, data.frame(item    = simOpt,
                                        pos     = ordres$pos,
                                        worth   = round(ordres$worth,2),
                                        Iratio  = round(ordres$Iratio,2),
                                        CE      = ordres$CE,
                                        total   = length(res$pos),
                                        against = paste(againsts,collapse=",")))
    }else{
      all      <- rbind(all, data.frame(item    = simOpt,
                                        pos     = ordres$pos,
                                        worth   = round(ordres$worth,2),
                                        Iratio  = round(ordres$Iratio,2),
                                        CE      = ordres$CE,
                                        tp_frq  = round(sum(res$pos == truepos) / length(res$pos),2),
                                        tp      = sum(res$pos == truepos),
                                        total   = length(res$pos),
                                        against = paste(againsts,collapse=",")))
    }


  # save when path is given
  if(is.null(path)){
  }else{
    sink(paste(path,simOpt, " ",runs, ".txt",sep=""))
    print(all)
    sink()
  }

  }
  return(all)
}




# Heart of the simulation function (this is an internal function of the wrapper)
nusim <- function(data=NULL, simOpt=NULL, GT=NULL, against=NULL, remove=simOpt,
                  filter.crit = "Iratio", runs=10, verbose=NULL ){

  simulations <- NULL

  # Datensatz erschaffen
  default  <- data[!(data$optionA==remove | data$optionB==remove),]

  #create dataframe with only Lake
  testset  <- data[(data$optionA==remove| data$optionB==remove),]

  combiagainst <- against
  test     <- testset[(( testset$optionA %in% remove & testset$optionB %in% combiagainst) | (testset$optionA %in% combiagainst & testset$optionB %in% remove) |
                         ( testset$optionA %in% remove & testset$optionB %in% combiagainst) | (testset$optionA %in% combiagainst & testset$optionB %in% remove) ),]

  dat_sim  <- NULL
  dat_sim  <- rbind(default, test)


  D <- NULL
  W <- NULL
  for(i in 1:runs){

    predat     <- bimpre (dat=dat_sim, GT=GT, simOpt=simOpt, deviation=0, minQuantity=0, verbose=verbose)

    nosim   <- predat[predat$sim==FALSE, ]
    simdat  <- predat[predat$sim==TRUE,  ]
    simdat$result  <- sample(c(1, -1), replace=TRUE, length(simdat$result)) # keine NULLEN!
    ydata          <- rbind(nosim, simdat)

    worth      <- bimworth(ydata    = ydata,
                           GT       = GT,
                           simOpt   = simOpt,
                           randOP   = FALSE,
                           intrans  = TRUE)

    # W <- cbind(W, worth$worth)

    w_errors   <- bimeval(ydata     = ydata,
                          worth     = worth$worth,
                          GT        = GT,
                          simOpt    = simOpt,
                          filtersim = FALSE)
    w_errors$errors$Iratio <- NULL
    w_errors$errors$Iratio <- worth$I$Iratio

    d <- w_errors$errors %>%
      arrange(desc(worth)) %>%
      mutate(run = i) %>%
      mutate(pos = 1:n()) %>%
      as.data.frame()

    D <- rbind(D, d)
  }

  simulations <- D[D$item %in% simOpt, ] %>%
    arrange((Iratio ))

  #return(list(simulations,W))
  return(simulations)
}

