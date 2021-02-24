# Read raw data
tdata <- read.table(file=filename, header=F, sep="\t", fill=T)


# Reading the two line input format
if (length(tdata) < 5) {

  printf('simsalRbim: Inputformat 2 lines per test.\n')
  if (!is.numeric(tdata$V3)) {
    printf('simsalRbim: Table header found.\n')
    # remove table header (and add it later with the correct names)
    tdata <- read.table(file=filename, header=T, sep="\t", fill=T, stringsAsFactors=F)
  }

  printf('simsalRbim: Read %d lines in total.\n', nrow(tdata))
  if (length(tdata) == 3) {
    names(tdata) <- c('subjectID', 'option', 'quantity')
  } else if (length(tdata) == 4) {
    names(tdata) <- c('subjectID', 'option', 'quantity', 'side')
    printf('simsalRbim: \"side\" as a parameter is included.\n')

  } else {
    stop('simsalRbim: Expect 3 or 4 columns as an input: subjectID, option, quantity and (optional) side!\n')
  }

  # Check des zweizeiligen Eingabeformates

  # Test: Grade Anzahl von Zeilen
  if (nrow(tdata)%%2 != 0) {
    stop('simsalRbim: Your number of lines are uneven but you use a 2-line input format. There is for sure one line missing.')
  }

  tdata$subjectID <- as.character(tdata$subjectID)
  tdata$option    <- as.character(tdata$option)

  # Test: subjectID, options and side
  for (i in 1:(nrow(tdata)/2)) {
    if (tdata$subjectID[(i-1)*2+1] != tdata$subjectID[(i-1)*2+2]) {
      stop('simsalRbim: Error: Line ', (i-1)*2+1,' and ', (i-1)*2+2, ': different subjectID: ', tdata$subjectID[(i-1)*2+1], ' != ', tdata$subjectID[(i-1)*2+2], '.\n')
    }
    if (tdata$option[(i-1)*2+1] == tdata$option[(i-1)*2+2]) {
      stop('simsalRbim: Error: Line ', (i-1)*2+1,' and ', (i-1)*2+2, ': same option: ', tdata$option[(i-1)*2+1], ' != ', tdata$option[(i-1)*2+2], '.\n')
    }
    if (ncol(tdata) == 4) {
      if (tdata$side[(i-1)*2+1] == tdata$side[(i-1)*2+2]) {
        stop('tieFighter: Error: Line ', (i-1)*2+1,' and ', (i-1)*2+2, ': same side: ', tdata$side[(i-1)*2+1], ' != ', tdata$side[(i-1)*2+2], '.\n')
      }
    }
  }

  # Convert dual line format to single line format
  printf('Convert dual line data to single line data...\n')
  sData <- data.frame()
  for (i in 1:(nrow(tdata)/2)) {
    sData[i, 'subjectID'] <- tdata$subjectID[(i-1)*2+1]
    sData[i, 'optionA'] <- tdata$option[(i-1)*2+1]
    sData[i, 'optionB'] <- tdata$option[(i-1)*2+2]
    sData[i, 'quantityA'] <- tdata$quantity[(i-1)*2+1]
    sData[i, 'quantityB'] <- tdata$quantity[(i-1)*2+2]
    if ('sideA' %in% colnames(tdata)) sData[i, 'side'] <- tdata$side[(i-1)*2+1]
  }
  tdata <- sData
  rm(sData)

  #
  # Reading the single line input format
  #
} else {
  printf('simsalRbim: Inputformat 1 line per test.\n')
  if (!is.numeric(tdata$V4) & !is.numeric(tdata$V5)) {
    printf('simsalRbim: Table header found.\n')
    tdata <- read.table(file=filename, header=T, sep="\t", fill=T, stringsAsFactors=F)
  }

  printf('simsalRbim: Read %d lines in total.\n', nrow(tdata))
  if (length(tdata) == 5) {
    names(tdata) <- c('subjectID', 'optionA', 'optionB', 'quantityA', 'quantityB')
  } else if (length(tdata) == 6) {
    names(tdata) <- c('subjectID', 'optionA', 'optionB', 'quantityA', 'quantityB', 'sideA')
    printf('simsalRbim: \"side\" as a parameter is included.\n')
  } else {
    stop('simsalRbim: Expect 5 or 6 columns as an input: subjectID, optionA, optionB, quantityA, quantityB and (optional) sideA!\n')
  }
}

# check for different type of errors in the file
tdata$subjectID <- as.character(tdata$subjectID)

# optionA and optionB: remove white spaces and add 'x' in the begin
#tdata$optionA <- as.character(sprintf('%s', str_remove_all(as.character(tdata$optionA), '[ +-:.*=/]')))
#tdata$optionB <- as.character(sprintf('%s', str_remove_all(as.character(tdata$optionB), '[ +-:.*=/]')))

#RABRABRAB -> Wenn option mit Zahl oder Sonderzeichen anfängt -> Warnung

# Check if quantityA and quantityB are numeric
if (!is.numeric(tdata$quantityA) || !is.numeric(tdata$quantityB)) {
  stop('Found an error in the data file: quantityA and/or quantityB must be numbers.')

  # Check if side is left or right, l or r, or in capital letters
} else if('sideA' %in% colnames(tdata)) {
  tdata$sideA <- as.character(tdata$sideA)
  if (!((tdata$sideA=='left') || (tdata$sideA=='right') || (tdata$sideA=='L') ||
        (tdata$sideA=='R') || (tdata$sideA=='l') || (tdata$sideA=='r') ||
        (tdata$sideA=='LEFT') || (tdata$sideA=='RIGHT'))) {
    stop('Found an error in the data file: side must be left or right (l or r).')
  } else {
    for (i in 1:nrow(tdata)) {
      ifelse((tdata$sideA[i]=='left') || (tdata$sideA[i]=='l') || (tdata$sideA[i]=='L') || (tdata$sideA[i]=='LEFT'), tdata$sideA[i] <- 'left', tdata$sideA[i] <- 'right')
    }
  }
}

return(tdata)
