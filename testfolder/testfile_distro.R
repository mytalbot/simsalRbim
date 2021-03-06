library(simsalRbim)

#  Perfect use case with no randomized items ------------------------------
dat        <- bimload(".../ZickeZackelinear.txt")
simOpt     <- "Zicke"
GT         <- c("Zacke", "Huehner", "Kacke")

predat     <- bimpre (dat=dat, GT=GT, simOpt=simOpt )

worth      <- bimworth(ydata    = predat,
                       GT       = GT,
                       simOpt   = simOpt,
                       intrans  = TRUE,
                       verbose  = TRUE,
                       showPlot = "worth") # or "coef"
worth

w_errors   <- bimeval(ydata     = predat,
                      worth     = worth$worth,
                      GT        = GT,
                      simOpt    = simOpt,
                      showPlot  = TRUE  )

# remember: we want mean_delta to be high (close to 50%)
w_errors



# ZickeZacke w/ HoiHoiHoi item  -------------------------------------------
dat        <- ZickeZacke
simOpt     <- "HoiHoiHoi"
GT         <- c("Huehner", "Kacke", "Zacke","Zicke")

predat     <- bimpre (dat=dat, GT=GT, simOpt=simOpt)

worth      <- bimworth(ydata    = predat,
                       GT       = GT,
                       simOpt   = simOpt,
                       randOP   = TRUE,
                       intrans  = TRUE,
                       showPlot = "worth")
worth

w_errors   <- bimeval(ydata     = predat,
                      worth     = worth$worth,
                      GT        = GT,
                      simOpt    = simOpt,
                      showPlot  = TRUE )
w_errors


# uninformed simulation  - cutoff determination
cutoff     <- bimUninformed(ydat       = predat,
                            GT         = GT,
                            simOpt     = simOpt,
                            limitToRun = 50,
                            ylim       = c(-1,2) )
cutoff$cutoff

# uninformed simulation  - Use the cutoff in the item positioning
pos        <- bimpos(ydata=predat, GT=GT, simOpt=simOpt, limitToRun=cutoff$cutoff, showPlot=TRUE )
pos$simerrors


# INFORMED Simulation - INTRANSITIVITY RUN
frqnc      <- bimsim(rawdat      = dat,
                     GT          = GT,
                     simOpt      = simOpt,
                     limitToRun  = 50,
                     tcut        = 0.9,
                     filter.crit ="Iratio",
                     ylim        = c(0,0.45))
frqnc$frq


# INFORMED Simulation - CONSENSUS ERROR RUN
frqnc      <- bimsim(rawdat      = dat,
                     GT          = GT,
                     simOpt      = simOpt,
                     limitToRun  = 50,
                     tcut        = 0.9,
                     filter.crit ="CE",
                     ylim        = c(0,0.45))
frqnc$frq




# Human Large Valence (Spring School) -------------------------------------
dat        <- bimload(".../human_LagreValenceRange_SpringSchool.txt")
simOpt     <- "Lake"
GT         <- c("Frustrated","Crow","War","Cat","Doctor", "Fire")

predat     <- bimpre (dat=dat, GT=GT, simOpt=simOpt)

worth      <- bimworth(ydata    = predat,
                       GT       = GT,
                       simOpt   = simOpt,
                       intrans  = TRUE,
                       showPlot = "worth")
worth

w_errors   <- bimeval(ydata     = predat,
                      worth     = worth$worth,
                      GT        = GT,
                      simOpt    = simOpt,
                      showPlot  = TRUE,
                      ylim      = c(0, 0.45))
w_errors




# Maus - Mice_oneLineTest1_20201102DP -------------------------------------
dat        <- bimload (".../Mice_oneLineTest1_20201102DP.txt")
simOpt     <- "water"
GT         <- c("HCl","m5MSac",  "m10MSac", "NaCl"   )

predat     <- bimpre (dat=dat, GT=GT, simOpt=simOpt, deviation=0, minQuantity=0)

worth      <- bimworth(ydata    = predat,
                       GT       = GT,
                       simOpt   = simOpt,
                       intrans  = TRUE ,
                       showPlot = "worth")
worth

w_errors   <- bimeval(ydata     = predat,
                      worth     = worth$worth,
                      GT        = GT,
                      simOpt    = simOpt,
                      showPlot  = TRUE,
                      ylim      = c(0, 0.45))
w_errors




# Monkey ------------------------------------------------------------------
# Note: Banana and Grape are equal when deviation = 0. Change deviation to 10! ;-)
dat        <- bimload (".../Rhesus_oneLine_20201116DP.txt")
simOpt     <- "water"
GT         <- c("banana", "grape", "NaCl", "quinine")

predat     <- bimpre (dat=dat, GT=GT, simOpt=simOpt, deviation=0 )

worth      <- bimworth(ydata    = predat,
                       GT       = GT,
                       simOpt   = simOpt,
                       intrans  = TRUE ,
                       showPlot = "worth")
worth

w_errors   <- bimeval(ydata     = predat,
                      worth     = worth$worth,
                      GT        = GT,
                      simOpt    = simOpt,
                      showPlot  = TRUE)
w_errors




# Was von Ute -------------------------------------------------------------

# Vorgewese
library(dplyr)
library(tidyr)
data = read.delim(".../place_preference_30daysL_active time.txt", header= T, sep = "\t", dec= ".")
data = t(data)
data = as.data.frame(data)
data = mutate(data, compiled_studies = c( rep("structural",10), rep("active",10), rep("housing",10) ) ) #column with experiment (sructural, active, housing)
data = mutate(data, experiment_number = c(1:30) ) # column with experiment_number (day)
data = mutate(data, optionA = c("rope","clip_papertube","mouseswing","clip_papertube","second_plane","clip_plastictube","clip_plastictube","clip_plastictube","clip_plastictube","second_plane","treatball","tube_stones","flappuzzle","tube_stones","tube_stones","tube_stones","latticeball","treatball","flappuzzle","latticeball","floorhouse","woodenangle_with_hole","woodenangle_with_hole","woodenangle","paperhouse","floorhouse","paperhouse","houseball","woodenangle","houseball"))#f?gt ENR1 hinzu
data = mutate(data, optionB = c("mouseswing","rope","clip_papertube","second_plane","rope","rope","second_plane","clip_papertube","mouseswing","mouseswing","latticeball","flappuzzle","treatball","latticeball","slidingpuzzle","treatball","slidingpuzzle","slidingpuzzle","slidingpuzzle","flappuzzle","woodenangle","houseball","woodenangle","paperhouse","floorhouse","woodenangle_with_hole","woodenangle_with_hole","floorhouse","houseball","paperhouse"))#f?gt ENR2 hinzu
data = gather(data, key = subjectID, value = "quantityA", -compiled_studies, -experiment_number, -optionA, -optionB) #verwandelt mausspalten mit pr?ferenz in spalten
data = mutate(data, quantityB = 100- quantityA)
data = mutate(data, quantityA = ifelse(data[,6] > 50, 1, 0 ))
data = mutate(data, quantityB = ifelse(data[,7] > 50, 1, 0 ))

# hier beginnt die Rechnerey
simOpt     <- "second_plane"
GT         <- c("mouseswing", "clip_papertube",  "rope", "clip_plastictube")

predat     <- bimpre (dat=data, GT=GT, simOpt=simOpt, deviation=0, minQuantity=0)

worth      <- bimworth(ydata    = predat,
                       GT       = GT,
                       simOpt   = simOpt,
                       intrans  = TRUE,
                       showPlot = "worth",
                       ylim     = c(0.1,0.3))
worth

w_errors   <- bimeval(ydata     = predat,
                      worth     = worth$worth,
                      GT        = GT,
                      simOpt    = simOpt,
                      showPlot  = TRUE,
                      ylim     = c(0.1,0.3))
w_errors
# der Consensus Error ist relativ groß. Das liegt am niedrigen mean_delta. So
# sind sich die Subjects z.B. bei second_plane im Mittel sehr uneins
# (50-12.5) = 37.5 .... 37.5/50*100= 75% Consensus Error.



