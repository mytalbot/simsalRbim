



# check if data for simOpt was provided
if(sum(dat$optionA %in% simOpt) + sum(dat$optionB %in% simOpt)==0){
  warning(paste("simsalRbim: ","Hey, you forgot to provide data for ", simOpt, "! Using the default model.\n", sep=""))
}else{}

### this function checks the data for missing mirrored items and adds them if TRUE
check_mirrored <- function(dat=NULL){
  # test reversed item combination presence per Animal!?
  itemlist <- paste(dat$optionA, dat$optionB, sep="")
  new      <- NULL
  query    <- NULL
  alles    <- NULL
  filled   <- c()
  for(i in 1:length(unique(dat$subjectID))){
    query    <- dat[dat$subjectID==unique(dat$subjectID)[i], ]
    add      <- NULL
    new      <- NULL

    qlist          <- paste(query$optionB, query$optionA, sep="") %in% itemlist

    add            <- query[qlist==FALSE,]
    add$optionA    <- query[qlist==FALSE,"optionB"]
    add$optionB    <- query[qlist==FALSE,"optionA"]
    add$quantityA  <- query[qlist==FALSE,"quantityB"]
    add$quantityB  <- query[qlist==FALSE,"quantityA"]

    new            <- rbind(query, add)
    alles          <- rbind(alles, new)

    filled         <- append(filled, sum(qlist==FALSE))
  }
  row.names(alles) <- NULL

  if(verbose==TRUE){
    printf(paste("simsalRbim: ", sum(filled), " mirrored item combinations were filled.\n", sep=""))
  }else{}

  return(alles)
}

# put data together again
alles            <- check_mirrored(dat)
dat              <- alles



# now we check for missing data and introduce the simulated column
check_missing <- function(dat=NULL){

  subjectList <- unique(dat$subjectID)
  dat$sim     <- F
  for (i in 1:length(GT)) {
    for (j in 1:length(subjectList)) {
      if (nrow(subset(dat, subjectID == subjectList[j] & optionA == GT[i] & optionB == simOpt)) < 1) {
        tempRow                     <- nrow(dat) + 1
        dat[tempRow, 'subjectID'] <- subjectList[j]
        dat$optionA[tempRow]      <- GT[i]
        dat$optionB[tempRow]      <- simOpt
        dat$quantityA[tempRow]    <- NA
        dat$quantityB[tempRow]    <- NA
        dat$tie[tempRow]          <- TRUE
        dat$sim[tempRow]          <- TRUE
      }
    }
  }

  # how many ties?
  if(verbose==TRUE){
    printf(paste("simsalRbim: ", sum(dat$tie, na.rm=TRUE), " items were filled up as ties.\n", sep=""))
  }else{}

  return(dat)
}

# check actually missing values, and fill them up with ties - if present
dat              <- check_missing(dat)








# prepare the test column for later analysis
dat$test   <- NA
dat$test   <- paste(dat$optionA, dat$optionB, sep="")
dat$result <- NA

# fill up quantities for ties if present
dat[dat$tie %in% TRUE, c("quantityA", "quantityB")] <- c(50,50)


### Mark the ties
markTies   <- function(inData=NULL, deviation = 0, minQuantity = 0) {

  for (i in 1:nrow(inData)) {
    sumQ <- inData$quantityA[i] + inData$quantityB[i]
    inData$qPercentA[i] = (inData$quantityA[i]*100) / sumQ
    inData$qPercentB[i] = (inData$quantityB[i]*100) / sumQ
    if (sumQ <= minQuantity) {
      inData[i, 'tie'] <- TRUE
    }
  }
  inData$tie[inData$qPercentA <= (50+deviation) & inData$qPercentA >= (50-deviation)] <- TRUE
  inData[is.na(inData$tie), 'tie'] <- FALSE

  inData$result[inData$qPercentA > inData$qPercentB & inData$tie == F] <- 1
  inData$result[inData$tie == T] <- 0
  inData$result[inData$qPercentA < inData$qPercentB & inData$tie == F] <- -1

  if(verbose==TRUE){
    printf(paste("simsalRbim: ", sum(inData$tie), " ties were marked.\n", sep=""))
  }else{}

  return(inData)
}

# mark the ties in the data & prepare the results column
dat        <- markTies(dat, deviation = deviation, minQuantity = deviation)



# Sort the data
p1_num <- NULL
p2_num <- NULL
for (i in 1:(length(optionList)-1)) {
  p1_num <- c(p1_num, 1:i)
  p2_num <- c(p2_num, rep(i+1, i))
}

p1   <- paste0(optionList[p1_num], optionList[p2_num])
p2   <- paste0(optionList[p2_num], optionList[p1_num])
pairings <- c(p1,p2)

## and order them correctly
ydata      <- dat
ydata$test <- factor(dat$test, levels = pairings)
ydata      <- with(ydata, ydata[order(test),])




