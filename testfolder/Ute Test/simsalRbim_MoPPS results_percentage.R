library(dplyr)
library(ggplot2)
library(gnm)
library(prefmod)
library(reshape2)
library(rlang)
library(stringr)
library(forestmangr)
library(viridis)
library(ggrepel)

library(simsalRbim)

#-----------------------------example for simsalRbim-------------------------------------------------------------------------


dat        <- ZickeZacke

simOpt     <- "HoiHoiHoi"
GT         <- c("Zacke", "Huehner", "Kacke",  "Zicke" )

predat     <- bimpre(dat=dat, GT=GT, simOpt=simOpt)
#> simsalRbim: 1 tie(s) marked.

worth      <- bimworth(ydata    = predat,
                       GT       = GT,
                       simOpt   = simOpt,
                       showPlot = "worth")

w_errors   <- bimeval(ydata     = predat, 
                      worth     = worth, 
                      GT        = GT, 
                      simOpt    = simOpt,
                      filtersim = FALSE,
                      showPlot  = TRUE,
                      ylim      = c(0,0.8))

#-------------------------------MoPSS results active time----------------------------------------------------------------------------

data = read.delim("C:/Users/Ute/Backup D Daten/MoPSS Daten und Fotos/#MoPPs script etc/preference ranking/simsalRbim ranking/MOPPS data prepared for simsalRbim_active time.txt", header= T, sep = "\t", dec= ".")

#structural enrichments
data_structural = filter(data, compiled_studies == "structural")
data_structural = select(data_structural, subjectID, optionA, optionB, quantityA, quantityB)

dat        <- data_structural
simOpt     <- "second_plane"

GT         <- c("mouseswing", "clip_papertube",  "rope", "clip_plastictube" )

predat     <- bimpre(dat=dat, GT=GT, simOpt=simOpt)

worth      <- bimworth(ydata    = predat,
                       GT       = GT,
                       simOpt   = simOpt,
                       showPlot = "worth",
                       ylim     = c(0.1,0.3) )

w_errors   <- bimeval(ydata     = predat, 
                      worth     = worth, 
                      GT        = GT, 
                      simOpt    = simOpt,
                      filtersim = FALSE,
                      showPlot  = TRUE,
                      ylim      = c(0.1,0.3))

#active enrichments
data_active = filter(data, compiled_studies == "active")
data_active = select(data_active, subjectID, optionA, optionB, quantityA, quantityB)

dat        <- data_active
simOpt     <- "latticeball"

GT         <- c("flappuzzle", "treatball",  "slidingpuzzle", "tube_stones" )

predat     <- bimpre(dat=dat, GT=GT, simOpt=simOpt)

worth      <- bimworth(ydata    = predat,
                       GT       = GT,
                       simOpt   = simOpt,
                       showPlot = "worth",
                       ylim     = c(0,0.6))

w_errors   <- bimeval(ydata     = predat, 
                      worth     = worth, 
                      GT        = GT, 
                      simOpt    = simOpt,
                      filtersim = FALSE,
                      showPlot  = TRUE,
                      ylim      = c(0,0.6))


#housing enrichments
data_housing = filter(data, compiled_studies == "housing")
data_housing = select(data_housing, subjectID, optionA, optionB, quantityA, quantityB)

dat        <- data_housing
simOpt     <- "floorhouse"

GT         <- c("paperhouse", "woodenangle",  "woodenangle_with_hole", "houseball" )

predat     <- bimpre(dat=dat, GT=GT, simOpt=simOpt)

worth      <- bimworth(ydata    = predat,
                       GT       = GT,
                       simOpt   = simOpt,
                       showPlot = "worth",
                       ylim     = c(0.0,0.4))

w_errors   <- bimeval(ydata     = predat, 
                      worth     = worth, 
                      GT        = GT, 
                      simOpt    = simOpt,
                      filtersim = FALSE,
                      showPlot  = TRUE,
                      ylim      = c(0.0,0.4))

#-------------------------------MoPSS results inactive time----------------------------------------------------------------------------

data = read.delim("C:/Users/Ute/Backup D Daten/MoPSS Daten und Fotos/#MoPPs script etc/preference ranking/simsalRbim ranking/MOPPS data prepared for simsalRbim_inactive time.txt", header= T, sep = "\t", dec= ".")

#structural enrichments
data_structural = filter(data, compiled_studies == "structural")
data_structural = select(data_structural, subjectID, optionA, optionB, quantityA, quantityB)

dat        <- data_structural
simOpt     <- "rope"

GT         <- c("mouseswing", "clip_papertube",  "second_plane", "clip_plastictube" )

predat     <- bimpre(dat=dat, GT=GT, simOpt=simOpt)


worth      <- bimworth(ydata    = predat,
                       GT       = GT,
                       simOpt   = simOpt,
                       showPlot = "worth",
                       ylim     = c(0.1,0.3))

w_errors   <- bimeval(ydata     = predat, 
                      worth     = worth, 
                      GT        = GT, 
                      simOpt    = simOpt,
                      filtersim = FALSE,
                      showPlot  = TRUE,
                      ylim      = c(0.1,0.3))


#active enrichments
data_active = filter(data, compiled_studies == "active")
data_active = select(data_active, subjectID, optionA, optionB, quantityA, quantityB)

dat        <- data_active
simOpt     <- "latticeball"

GT         <- c("flappuzzle", "treatball",  "slidingpuzzle", "tube_stones" )

predat     <- bimpre(dat=dat, GT=GT, simOpt=simOpt)


worth      <- bimworth(ydata    = predat,
                       GT       = GT,
                       simOpt   = simOpt,
                       showPlot = "worth",
                       ylim     = c(0,0.6))

w_errors   <- bimeval(ydata     = predat, 
                      worth     = worth, 
                      GT        = GT, 
                      simOpt    = simOpt,
                      filtersim = FALSE,
                      showPlot  = TRUE,
                      ylim      = c(0,0.6))


#housing enrichments
data_housing = filter(data, compiled_studies == "housing")
data_housing = select(data_housing, subjectID, optionA, optionB, quantityA, quantityB)

dat        <- data_housing
simOpt     <- "paperhouse"

GT         <- c("floorhouse", "woodenangle",  "woodenangle_with_hole", "houseball" )

predat     <- bimpre(dat=dat, GT=GT, simOpt=simOpt)

worth      <- bimworth(ydata    = predat,
                       GT       = GT,
                       simOpt   = simOpt,
                       showPlot = "worth",
                       ylim     =c(0.18,0.22) )

w_errors   <- bimeval(ydata     = predat, 
                      worth     = worth, 
                      GT        = GT, 
                      simOpt    = simOpt,
                      filtersim = FALSE,
                      showPlot  = TRUE,
                      ylim      = c(0.18,0.22))



