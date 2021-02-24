#' Intransitivity Calculation
#'
#' The \code{bimintrans} calculates the intransitivity of items.
#'
#' @param dat uses sorted pairings of input data from a preference test.
#' @param idcolumn variable name for the ID column (default is subjectID)
#' @param I2 item 2 - optionA
#' @param I1 item 1 - optionB
#' @param response response column
#'
#' @return list with the total number of intransitivity counts and number of triplets
#'
#' @export
#'


# Intransitivity Function -------------------------------------------------
bimintrans <- function(dat=NULL, idcolumn="subjectID", I2="optionA", I1="optionB", response="result"){

  # make global settings local
  vIMG1       <- which(names(dat)==I1)
  vIMG2       <- which(names(dat)==I2)

  # prepare data
  dat$img1 <- as.character(dat[,vIMG1])
  dat$img2 <- as.character(dat[,vIMG2])

  resVar      <- which(names(dat)==response)
  intIDcol    <- which(names(dat)==idcolumn)

  uIDs        <- unique( dat[,intIDcol])
  uItems      <- unique( c(dat[,vIMG1], dat[,vIMG2]) )
  count       <- 0

  #length(uIDs)
  #length(uItems)

  # calculate item intransitivity
  i1<-1
  i2<-2
  i3<-3
  intranscount<-0
  tripletcount<-0

  for(thisID in uIDs)
  {
    thisData   <- subset(dat, dat[names(dat)==idcolumn]==thisID)
    lenDataset <- length(thisData[[1]])

    for(i1 in 1:(length(uItems)-2))
    {
      for(i2 in (i1+1):(length(uItems)-1))
      {
        for(i3 in (i2+1):length(uItems))
        {
          firstI  <- uItems[i1]
          secondI <- uItems[i2]
          thirdI  <- uItems[i3]

          for(count in 1:lenDataset)
          {

            #find i1 vs i2
            if (thisData[count,vIMG1]==firstI & thisData[count, vIMG2]==secondI) {
            }else if (thisData[count, resVar]==1)  {
              AoverB <- 1
            }else {AoverB<-0}

            if (thisData[count,vIMG1]==secondI & thisData[count, vIMG2]==firstI) {
            }else if (thisData[count, resVar]==0)  {
              AoverB<-1
            }else {AoverB<-0}

            #find i2 vs i3
            if (thisData[count,vIMG1]==secondI & thisData[count, vIMG2]==thirdI) {
            }else if (thisData[count, resVar]==1)  {
              BoverC<-1
            }else {BoverC<-0}

            if (thisData[count,vIMG1]==thirdI & thisData[count, vIMG2]==secondI) {
            }else if (thisData[count, resVar]==0)  {
              BoverC<-1
            }else {BoverC<-0}

            #find i3 vs i1
            if (thisData[count,vIMG1]==thirdI & thisData[count, vIMG2]==firstI) {
            }else if (thisData[count, resVar]==1)  {
              CoverA<-1
            }else {CoverA<-0}

            if (thisData[count,vIMG1]==firstI & thisData[count, vIMG2]==thirdI) {
            }else if (thisData[count, resVar]==1)  {
              CoverA<-1
            }else {CoverA<-0}

          }

          #Invalid cases (A>B)&(B>C)&(C>A) OR (B>A)&(C>B)&(A>C)
          if(AoverB & BoverC & CoverA)
          {
            #print(paste(thisID,":",firstI,">",secondI,">",thirdI,">",firstI, "(clockwise A>B>C>A)"))
            intranscount<-intranscount+1
          }
          if(!AoverB & !BoverC & !CoverA)
          {
            #print(paste(thisID,":",firstI,">",thirdI,">",secondI,">",firstI, "(counterclockwise A>C>B>A)"))
            intranscount<-intranscount+1
          }
          tripletcount  <-tripletcount+1
        }
      }
    }
  }
  # print(paste(intranscount, "intransitive triplets from ", tripletcount, "total triplets"))
  return(list(intranscount =intranscount,
              no_tripl     =tripletcount,
              Iratio       =intranscount/tripletcount ))#intranscount/tripletcount))
}


