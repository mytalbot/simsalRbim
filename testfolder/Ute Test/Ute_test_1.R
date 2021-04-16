

library(simsalRbim)
library(dplyr)
library(tidyr)


data = read.delim("./testfolder/Ute Test/place_preference_30daysL_active time.txt", header= T, sep = "\t", dec= ".")

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






simOpt     <- "second_plane"
GT         <- c("mouseswing", "clip_papertube",  "rope", "clip_plastictube")

predat     <- bimpre (dat=data, GT=GT, simOpt=simOpt, deviation=0, minQuantity=0)

worth      <- bimworth(ydata    = predat,
                       GT       = GT,
                       simOpt   = simOpt,
                       intrans  = TRUE,
                       verbose  = TRUE,
                       showPlot = "worth")
worth

w_errors   <- bimeval(ydata=predat, worth= worth$worth, GT=GT, simOpt=simOpt, showPlot=TRUE  )
w_errors


ydata=predat
worth= worth$worth
GT=GT
simOpt=simOpt
coverage=0.8
showPlot=FALSE
filtersim=FALSE
title="Consensus Analysis"
subtitle=NULL
verbose=FALSE
ylim=c(0,1)



