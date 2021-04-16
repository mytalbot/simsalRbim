library(dplyr)
library(tidyr)

#read/input data (Linkes Enrichment)

data = read.delim("C:/Users/Ute/Backup D Daten/MoPSS Daten und Fotos/#MoPPs script etc/preference ranking/R Ranking Lars/MoPPS Versuch/place_preference_30daysL_active time.txt", header= T, sep = "\t", dec= ".")


#"V01" = "rope vs. mouse swing",
#"V02" = "clip+paper tube vs. rope",
#"V03" = "mouse swing vs. clip+paper tube", 
#"V04" = "clip+paper tube vs. 2nd plane", 
#"V05" = "2nd plane vs. rope", 
#"V06" = "clip+plastic tube vs. rope", 
#"V07" = "clip+plastic tube vs. 2nd plane", 
#"V08" = "clip+plastic tube vs. clip+paper tube", 
#"V09" = "clip+plastic tube vs. mouse swing", 
#"V10" = "2nd plane vs. mouse swing",
#"V11" = "treat ball vs. lattice ball",
#"V12" = "tube+stones vs. flap puzzle",
#"V13" = "flap puzzle vs. treat ball", 
#"V14" = "tube+stones vs. lattice ball", 
#"V15" = "tube+stones vs. sliding puzzle", 
#"V16" = "tube+stones vs. treat ball", 
#"V17" = "lattice ball vs. sliding puzzle", 
#"V18" = "treat ball vs. sliding puzzle", 
#"V19" = "flap puzzle vs. sliding puzzle", 
#"V20" = "lattice ball vs. flap puzzle",
#"V21" = "floor house vs. wooden angle",
#"V22" = "wooden angle with hole vs. house ball",
#"V23" = "wooden angle with hole vs. wooden angle", 
#"V24" = "wooden angle vs. paper house", 
#"V25" = "paper house vs. floor house", 
#"V26" = "floor house vs. wooden angle with hole", 
#"V27" = "paper house vs. wooden angle with hole", 
#"V28" = "house ball vs. floor house", 
#"V29" = "wooden angle vs. house ball", 
#"V30" = "house ball vs. paper house"

#Spalten hinzufügen (mouse no., group, experiment (Structural, active, housing, ENR1, ENR2, group, preference enr 1))

data = t(data)

data = as.data.frame(data)

data = mutate(data, compiled_studies = c( rep("structural",10), rep("active",10), rep("housing",10) ) ) #column with experiment (structural, active, housing)

data = mutate(data, experiment_number = c(1:30) ) # column with experiment_number (day)

data = mutate(data, optionA = c("rope","clip_papertube","mouseswing","clip_papertube","second_plane","clip_plastictube","clip_plastictube","clip_plastictube","clip_plastictube","second_plane","treatball","tube_stones","flappuzzle","tube_stones","tube_stones","tube_stones","latticeball","treatball","flappuzzle","latticeball","floorhouse","woodenangle_with_hole","woodenangle_with_hole","woodenangle","paperhouse","floorhouse","paperhouse","houseball","woodenangle","houseball"))#fügt ENR1 hinzu

data = mutate(data, optionB = c("mouseswing","rope","clip_papertube","second_plane","rope","rope","second_plane","clip_papertube","mouseswing","mouseswing","latticeball","flappuzzle","treatball","latticeball","slidingpuzzle","treatball","slidingpuzzle","slidingpuzzle","slidingpuzzle","flappuzzle","woodenangle","houseball","woodenangle","paperhouse","floorhouse","woodenangle_with_hole","woodenangle_with_hole","floorhouse","houseball","paperhouse"))#fügt ENR2 hinzu

data = gather(data, key = subjectID, value = "quantityA", -compiled_studies, -experiment_number, -optionA, -optionB) #verwandelt mausspalten mit präferenz in spalten

data = mutate(data, quantityB = 100- quantityA)

#data = mutate( data, group = c(rep("ENR1", 120), rep("ENR2", 120), rep("ENR3", 120)) )#fügt Spalte mit Gruppe hinzu


#für simsalRbim benötigte Spalten filtern (subjectID, optionA, optionB, quantityA, quantityB)

data = select(data, compiled_studies, subjectID, optionA, optionB, quantityA, quantityB)


##data$ENR1 = as.factor(data$ENR1)
##data$ENR2 = as.factor(data$ENR2)
##data$ID = as.factor(data$ID)
##data$side = as.factor(data$side)
##data$compiled_studies = as.factor(data$compiled_studies)

#erstelltes dataframe speichern

write.table(data, "MOPPS data prepared for simsalRbim_active time.txt", sep = "\t", row.names = T)


###################################################data preparation mit binaren ergebnis 0/1 für simsalRbim

data = read.delim("C:/Users/Ute/Backup D Daten/MoPSS Daten und Fotos/#MoPPs script etc/preference ranking/R Ranking Lars/MoPPS Versuch/place_preference_30daysL_active time.txt", header= T, sep = "\t", dec= ".")


#"V01" = "rope vs. mouse swing",
#"V02" = "clip+paper tube vs. rope",
#"V03" = "mouse swing vs. clip+paper tube", 
#"V04" = "clip+paper tube vs. 2nd plane", 
#"V05" = "2nd plane vs. rope", 
#"V06" = "clip+plastic tube vs. rope", 
#"V07" = "clip+plastic tube vs. 2nd plane", 
#"V08" = "clip+plastic tube vs. clip+paper tube", 
#"V09" = "clip+plastic tube vs. mouse swing", 
#"V10" = "2nd plane vs. mouse swing",
#"V11" = "treat ball vs. lattice ball",
#"V12" = "tube+stones vs. flap puzzle",
#"V13" = "flap puzzle vs. treat ball", 
#"V14" = "tube+stones vs. lattice ball", 
#"V15" = "tube+stones vs. sliding puzzle", 
#"V16" = "tube+stones vs. treat ball", 
#"V17" = "lattice ball vs. sliding puzzle", 
#"V18" = "treat ball vs. sliding puzzle", 
#"V19" = "flap puzzle vs. sliding puzzle", 
#"V20" = "lattice ball vs. flap puzzle",
#"V21" = "floor house vs. wooden angle",
#"V22" = "wooden angle with hole vs. house ball",
#"V23" = "wooden angle with hole vs. wooden angle", 
#"V24" = "wooden angle vs. paper house", 
#"V25" = "paper house vs. floor house", 
#"V26" = "floor house vs. wooden angle with hole", 
#"V27" = "paper house vs. wooden angle with hole", 
#"V28" = "house ball vs. floor house", 
#"V29" = "wooden angle vs. house ball", 
#"V30" = "house ball vs. paper house"

#Spalten hinzufügen (mouse no., group, experiment (Structural, active, housing, ENR1, ENR2, group, preference enr 1))

data = t(data)

data = as.data.frame(data)

data = mutate(data, compiled_studies = c( rep("structural",10), rep("active",10), rep("housing",10) ) ) #column with experiment (sructural, active, housing)

data = mutate(data, experiment_number = c(1:30) ) # column with experiment_number (day)

data = mutate(data, optionA = c("rope","clip_papertube","mouseswing","clip_papertube","second_plane","clip_plastictube","clip_plastictube","clip_plastictube","clip_plastictube","second_plane","treatball","tube_stones","flappuzzle","tube_stones","tube_stones","tube_stones","latticeball","treatball","flappuzzle","latticeball","floorhouse","woodenangle_with_hole","woodenangle_with_hole","woodenangle","paperhouse","floorhouse","paperhouse","houseball","woodenangle","houseball"))#fügt ENR1 hinzu

data = mutate(data, optionB = c("mouseswing","rope","clip_papertube","second_plane","rope","rope","second_plane","clip_papertube","mouseswing","mouseswing","latticeball","flappuzzle","treatball","latticeball","slidingpuzzle","treatball","slidingpuzzle","slidingpuzzle","slidingpuzzle","flappuzzle","woodenangle","houseball","woodenangle","paperhouse","floorhouse","woodenangle_with_hole","woodenangle_with_hole","floorhouse","houseball","paperhouse"))#fügt ENR2 hinzu

data = gather(data, key = subjectID, value = "quantityA", -compiled_studies, -experiment_number, -optionA, -optionB) #verwandelt mausspalten mit präferenz in spalten

data = mutate(data, quantityB = 100- quantityA)

data = mutate(data, quantityA = ifelse(data[,6] > 50, 1, 0 ))

data = mutate(data, quantityB = ifelse(data[,7] > 50, 1, 0 ))

#SimsalRbim structural enrichments
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

#structural
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
                       ylim     = c(0,0.4) )

w_errors   <- bimeval(ydata     = predat, 
                      worth     = worth, 
                      GT        = GT, 
                      simOpt    = simOpt,
                      filtersim = FALSE,
                      showPlot  = TRUE,
                      ylim      = c(0,0.4))

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

