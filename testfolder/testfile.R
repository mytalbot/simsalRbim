
library(simsalRbim)

#  Perfect use case with no randomized items ------------------------------
dat        <- bimload("C:/MHH Bleich/Aktuelles/PrePrefPackage 2020/data/ZickeZackelinear.txt")


# dat <- dat[dat$subjectID=="eins", ]

simOpt     <- "Zicke"
GT         <- c("Zacke", "Huehner", "Kacke" )

predat     <- bimpre (dat=dat, GT=GT, simOpt=simOpt, deviation=0, minQuantity=0)

worth      <- bimworth(ydata    = predat,
                       GT       = GT,
                       simOpt   = simOpt,
                       intrans  = TRUE,
                       verbose  = TRUE,
                       showPlot = "worth")
worth

w_errors   <- bimeval(ydata=predat, worth= worth$worth, GT=GT, simOpt=simOpt, showPlot=TRUE  )
w_errors




# Introduction of a new item with restricted measurements -----------------
# Import from a fresh file w/ Coke items
# and 1 Tie
library(simsalRbim)


dat        <- ZickeZacke

simOpt     <- "HoiHoiHoi"
GT         <- c( "Huehner", "Kacke",  "Zicke","Zacke" )

predat     <- bimpre (dat=dat, GT=GT, simOpt=simOpt, deviation=0, minQuantity=0)

worth      <- bimworth(ydata    = predat,
                       GT       = GT,
                       simOpt   = simOpt,
                       randOP   = TRUE,
                       intrans  = TRUE,
                       showPlot = "worth")
worth

w_errors   <- bimeval(ydata=predat, worth= worth$worth, GT=GT, simOpt=simOpt, filtersim=FALSE, showPlot=TRUE )
w_errors




ydata=predat
worth= worth$worth
GT=GT
simOpt=simOpt
showPlot=TRUE

\begin{align*}
CE_{HoiHoiHoi} = (50-((25+25+0+0)/4))/50*100=25%\\
\end{align*}

### Now we do an uninformed item positioning, also to obtain a reasonable threshold for the no. of randomizations
cutoff     <- bimUninformed(ydata=predat, GT=GT, simOpt=simOpt, limitToRun=100, ylim=c(-0.6,1.1) )
cutoff$cutoff

### now the randomized worth calculation is reapeated n-times
# Note: as long as CIs show an overlap, the positioning is not secure.
pos        <- bimpos(ydata=predat, GT=GT, simOpt=simOpt, limitToRun=78, showPlot=TRUE )
pos$simerrors

# now the informed simulation
frqnc      <- bimsim(rawdat=dat, GT=GT, simOpt=simOpt, limitToRun=78, fval=100,
                     deviation=0, minQuantity=0, seed=TRUE,
                     showPlot=TRUE, ylim=c(0,0.45))
frqnc$frq

















ydata = predat
GT=GT
simOpt=simOpt
limitToRun=10
seed=TRUE
fval= 1
showPlot=TRUE
ylim=c(0,0.7)






ydata = predat
GT=GT
simOpt=simOpt
limitToRun=78
fval= 0.25
showPlot=TRUE
ylim=c(0,0.7)

# Wie einig sind sich die Probanden OHNE Simulation und wo überlappt es?
# Dabei die n-Zahl berücksichtigen.
# Spuckt das GNM model ein Konfidenzinterval aus?
# Poweranalyse? Wie viele Items braucht man?









# Human data 1 --------------------------------------------------------------
dat        <- bimload ("C:/MHH Bleich/Aktuelles/PrePrefPackage 2020/data/human_LagreValenceRange_SpringSchool.txt")
simOpt     <- "Lake"
GT         <- c("Frustrated","Crow","War","Cat","Doctor", "Fire")

predat     <- bimpre (dat=dat, GT=GT, simOpt=simOpt, deviation=0, minQuantity=0)

worth      <- bimworth(ydata    = predat,
                       GT       = GT,
                       simOpt   = simOpt,
                       intrans  = TRUE,
                       showPlot = "worth")
worth

w_errors   <- bimeval(ydata=predat, worth= worth$worth, GT=GT, simOpt=simOpt, verbose=TRUE )
w_errors




dat        <- bimload ("C:/MHH Bleich/Aktuelles/PrePrefPackage 2020/data/human_LowValenceRange.txt")
simOpt     <- "Sunset"
GT         <- c("House","Grass","Food","Monkey","Timber","Flowers" )

predat     <- bimpre (dat=dat, GT=GT, simOpt=simOpt, deviation=0, minQuantity=0)

worth      <- bimworth(ydata    = predat,
                       GT       = GT,
                       simOpt   = simOpt,
                       intrans  = FALSE,
                       showPlot = "worth")
worth

w_errors   <- bimeval(ydata=predat, worth= worth, GT=GT, simOpt=simOpt )
w_errors




dat        <- bimload ("C:/MHH Bleich/Aktuelles/PrePrefPackage 2020/data/human_LargeValanceRange.txt")
simOpt     <- "Lake"
GT         <- c("Crow","War","Cat","Doctor", "Fire", "Frustrated")
predat     <- bimpre (dat=dat, GT=GT, simOpt=simOpt, deviation=0, minQuantity=0)

worth      <- bimworth(ydata    = predat,
                       GT       = GT,
                       simOpt   = simOpt,
                       intrans  = FALSE,
                       randOP   = TRUE,
                       showPlot = "worth")
worth

w_errors   <- bimeval(ydata=predat, worth= worth, GT=GT, simOpt=simOpt )
w_errors






dat        <- bimload ("C:/MHH Bleich/Aktuelles/PrePrefPackage 2020/data/upload data GitHub/human_LowValenceRange.txt")

simOpt     <- "Flowers"
GT         <- c( "Grass",  "House",  "Sunset", "Food",   "Monkey", "Timber")
predat     <- bimpre (dat=dat, GT=GT, simOpt=simOpt, deviation=0, minQuantity=0)

worth      <- bimworth(ydata    = predat,
                       GT       = GT,
                       simOpt   = simOpt,
                       intrans  = FALSE,
                       randOP   = FALSE,
                       showPlot = "worth")
worth

w_errors   <- bimeval(ydata=predat, worth= worth , GT=GT, simOpt=simOpt, showPlot=TRUE  )
w_errors

ydata=predat
worth= worth
GT=GT
simOpt=simOpt
showPlot=TRUE



# Human data test------------------------------------------------------------
# Introduction of the Nackmull
dat        <- bimload ("C:/MHH Bleich/Aktuelles/PrePrefPackage 2020/data/human_LagreValenceRange_SpringSchool.txt")

dat        <- rbind(dat, data.frame(subjectID = c("16de27e0","16de27e0","1cdf6341","36f1fbb9"),
                                    optionA   = c("Lake","Doctor","Frustrated","Crow"),
                                    optionB   = c("Nacktmull","Nacktmull","Nacktmull", "Nacktmull"),
                                    quantityA = c(220,190,50,120),
                                    quantityB = c(40,170,22,114),
                                    sideA     = c("left", "left", "left", "left")))

simOpt     <- "Nacktmull"
GT         <- c("Lake","Crow","War","Cat","Doctor", "Fire", "Frustrated")

predat     <- bimpre (dat=dat, GT=GT, simOpt=simOpt, deviation=0, minQuantity=0)

worth      <- bimworth(ydata    = predat,
                       GT       = GT,
                       simOpt   = simOpt,
                       intrans  = TRUE ,
                       showPlot = TRUE)
worth

w_errors   <- bimeval(ydata=predat, worth= worth$worth, GT=GT, simOpt=simOpt )
w_errors







# binary ------------------------------------------------------------------

# bimeval does not work when side is included
library(simsalRbim)

dat        <- bimload ("C:/MHH Bleich/Aktuelles/PrePrefPackage 2020/data/Rhesus_oneLine_20201116DP.txt")

simOpt     <- "water"
GT         <- c("banana", "grape", "NaCl", "quinine")

predat     <- bimpre (dat=dat, GT=GT, simOpt=simOpt, deviation=0, minQuantity=0)

worth      <- bimworth(ydata    = predat,
                       GT       = GT,
                       simOpt   = simOpt,
                       intrans  = TRUE ,
                       showPlot = "worth")
worth

w_errors   <- bimeval(ydata=predat, worth= worth$worth, GT=GT, simOpt=simOpt )
w_errors


ydata=predat
worth= worth$worth
GT=GT
simOpt=simOpt




# binary
library(simsalRbim)

dat        <- bimload ("C:/MHH Bleich/Aktuelles/PrePrefPackage 2020/data/Mice_oneLineTest1_20201102DP.txt")

simOpt     <- "water"
GT         <- c("HCl","m5MSac",  "m10MSac", "NaCl"   )

predat     <- bimpre (dat=dat, GT=GT, simOpt=simOpt, deviation=0, minQuantity=0)

worth      <- bimworth(ydata    = predat,
                       GT       = GT,
                       simOpt   = simOpt,
                       intrans  = TRUE ,
                       showPlot = "worth")
worth

w_errors   <- bimeval(ydata=predat, worth= worth$worth, GT=GT, simOpt=simOpt, filtersim=FALSE, showPlot=TRUE )
w_errors


ydata=predat
worth= worth$worth
GT=GT
simOpt=simOpt
filtersim=FALSE
showPlot=TRUE





ydata=predat
worth= worth$worth
GT=GT
simOpt=simOpt



cutoff     <- bimUninformed(ydat=predat, GT=GT, simOpt=simOpt, limitToRun=50, ylim=c(-1,2) )
cutoff$cutoff

pos        <- bimpos(ydata=predat, GT=GT, simOpt=simOpt, limitToRun=40, showPlot=TRUE )
pos$simerrors

# takes some time!
frqnc      <- bimsim(ydata = predat, GT=GT, simOpt=simOpt, limitToRun=20, fval= 1, showPlot=TRUE, ylim=c(0,0.7))
frqnc$frq




