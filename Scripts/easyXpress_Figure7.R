
rm(list = ls())

#install.packages("easyXpress")
#install.packages("devtools")
#devtools::install_github("AndersenLab/easyXpress")
#install.packages("tidyverse")
#install.packages("jquerylib")
library(easyXpress)
library(tidyverse)
#library(jquerylib)
#install.packages("ggplot2")
library(ggplot2)

ex.Dir="/vast/eande106/projects/Nikita/Filtration/Experiment_3"

filedirs=c(paste0(ex.Dir, '/20240906-NJ-R2-drugresponse/20240906-NJ-R2-p009-m2x/Analysis-20240912'),
          paste0(ex.Dir, '/20240920-NJR3-drugresponse/20240920-NJR3-drugresponse-p009-m2x/Analysis-20240927'),
          paste0(ex.Dir, '/20240926-NJR4-drugresponse/20240926-NJR4-drugresponse-p009-m2x/Analysis-20240927'))

rdafiles = c('20240906-NJ-R2-p009-m2x_Analysis-20240912.RData',
             '20240920-NJR3-drugresponse_1_Analysis-20240927.RData',
             'NA_Nikita_project6.RData')


#Read in the data
dr <- easyXpress::readXpress(filedir = filedirs,
                             rdafile = rdafiles,
                             design = T,doseR = T)


#modelSelection
ms <- easyXpress::modelSelection(dr$raw_data)

#Flag Objects
ef <- edgeOF(data =ms)
  
cf <- clusterOF(data = ef)

#checkOF function to see how the object flags look across the desired grouping variables
c1 <- checkOF (data = cf, drug, strain, bleach, concentration_um)
c1$p

#checkObjs function to visualize the size distributions of the objects bygrouping variables
c2 <- checkObjs(data =cf, OF = 'filter', drug, strain, concentration_um, bleach)
c2

#checkModels function to see if small objects are debris
#Add variables that describe the PATH to processed images and well labels
cm <- cf %>%
  #add img dir var
  dplyr::mutate(i.dir =
                  dplyr::case_when(Metadata_Experiment == "NJR2"~
                                             paste0(ex.Dir, "/20240906-NJ-R2-drugresponse/20240906-NJ-R2-p009-m2x_/Analysis-20240912/processed_images/"),
                                  Metadata_Experiment == "R3"~
                                      paste0(ex.Dir, "/20240920-NJR3-drugresponse/20240920-NJR3-drugresponse-p009-m2x/Analysis-20240927/processed_images"),
                                    Metadata_Experiment == "R4"~
                                       paste0(ex.Dir, "/20240926-NJR4-drugresponse/20240926-NJR4-drugresponse-p009-m2x/Analysis-20240927/processed_images"),
                                             TRUE ~ NA_character_),
                #add well label var
                w.lab = paste(drug, concentration_um, strain, sep ="_"))
                
#use the checkModels function##Function is not working. Why??
cm.out <- checkModels(data = cm,
                      # the grouping vars (...), make a plot for each.
                      Metadata_Experiment, drug,
                      proc.img.dir = "i.dir",
                      well.label = "w.lab",
                      #save in the repo you cloned
                      out.dir = paste0(ex.Dir, "/eXDR/checkModels/out"))

#add the user variable that will be converted to an object flag
u = cm %>%
  dplyr::mutate(user = dplyr::case_when(
                                        drug == "Albendazole_B" &
                                          model == "MDHD" ~ "junk",
                                       drug == "Albendazole_B" &
                                          worm_length_um < 165 ~ "junk",
                                      
                                       drug == "Albendazole_F" &
                                         model == "MDHD" ~ "junk",
                                       drug == "Albendazole_F" &
                                         worm_length_um < 165 ~ "junk",
                                          TRUE ~ NA_character_)) 

#Run the userOF function and specify user variable as the flag
uf <- easyXpress::userOF(data =u, user)

#Check the object data again to see if the bimodal distributions are resolved
checkObjs(data = uf, OF = "filter", drug, concentration_um, strain, bleach)

#apply the classifierOF function to flag objects that are likely debris or improperly segmented worms
cl <- classifierOF(data = uf)

#apply outlierOF to flag objects that have extreme worm_length_um values relative to other objects in the same well
o <- easyXpress::outlierOF(data = cl)

#Use checkObjs function again to check the effect of filtering all the object flags.
#check objects again, notice how there are 5 ObjectFlags detected now.
checkObjs(data = o, OF = 'filter', drug, concentration_um, strain, bleach)

#Use checkOF function again to see how much data is being flagged.
co2 <- checkOF(data = o, drug, concentration_um, strain)
co2$p

# #Use the viewOverlay function to check the these wells and see if the flag are working properly whether the data are worth keeping
# #Important! Set the seed to ensure the 8 random wells
set.seed(99)
 
 #Set the flags and filter data
 o2 <- easyXpress::setOF(data = o) %>% #set the flags
  dplyr::filter (drug == "Albendazole_B"
                 
                  ) %>%

#   #randomly sample 8 wells
  dplyr::filter(well.id %in% sample(well.id, size = 8))

# #use the viewOverlay function ###Not working
 vo1 <- easyXpress::viewOverlay(data = o2,             
                               proc.img.dir = "i.dir",
                               well.label = "w.lab",
                               obj.label = "model",
                               text.anno = "objectFlag",
                              #save to example dir
                                file = paste0(ex.Dir, "/eXDR/viewOverlay/overlay.png"))

#use filterOF function to remove all the flagged objects from the data after the flags have been checked
#finalise the object data made with outlierOF function above
proc.objs <- easyXpress::filterOF(o, rmVars = T)

#use summarizeWells function to remove flagged objects, summarize data within each well and drop all object related variables from the data
#remove all flags, summarize wells, and drop object vars all in one function
raw.wells <- easyXpress::summarizeWells(data = o)


##use the titerWF to check for improperly titered bleaches 
tf <- easyXpress::titerWF(data = raw.wells,
                          Metadata_Experiment, bleach, strain)
tf$p

#use the nWF function to flag wells with too many or too few objects in them. 
n <-easyXpress::nWF(data =tf$d, drug, concentration_um, strain, max = 30, min = 5)
n$p

#use the outlierWF function to flag outliers within groups
#then use dplyr::mutate to add a variable to indicate independent bleaches.
ow <- easyXpress::outlierWF(data = n$d,
                            Metadata_Experiment, bleach, drug,
                             strain) %>%
      dplyr::mutate(assay_bleach = paste(Metadata_Experiment, bleach, sep = "_"))  

#use checkWF() function to see how many wells are being flagged across grouping variable specified with ...
cw1 <- easyXpress::checkWF(data = ow, drug, concentration_um, strain)
cw1$p

#use filterWF() function to remove all flagged wells from the data 
#use the filterWF() function and drop the flagging variables afterward
fw <- filterWF(data = ow, rmVars = TRUE)
#dr$design$Metadata_Experiment <- "JB"
#use checkBalance() function to see the fraction of data retained after the flags are filtered and add assay_bleach var to design
#fw <- na.omit(fw)

cb <- checkBalance(data = fw, drug, concentration_um,strain,
   design = dr$design %>%
     dplyr::mutate(assay_bleach =
                    paste0(Metadata_Experiment, bleach)),
  x = assay_bleach)
cb$p

#Look at the plot and add a nicer x-axis with ggplot2 package
cb$p +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5)) +
  ggplot2::geom_hline(yintercept = 0.75, linetype = 2, color = "red")

##Filter the unbalanced bleaches - Not running this code as the above code is not running##

#use ckheckEff function is used to look at the data after problematic bleaches have been removed in the above step. 
ce1 <- easyXpress::checkEff(data = fw,
                            drug, strain,
                            x = concentration_um,
                            y = median_wormlength_um,
                            fill = drug,
                            scales = "free_x")
ce1                           

# Regress the effect of independent bleaches for each drug using regEff()
reg <- easyXpress::regEff(data = fw,
                          drug,
                          d.var = median_wormlength_um,
                          c.var = assay_bleach)

# Look at the regression coefficients in the diagnostic plot p2
reg$p2

#fw <- fw %>%
#  mutate(drug = case_when(
 #   drug == "Albendazole_F" ~ "Albendazole_F",   # Rename DMSO_F to Control_F
 #   drug == "Albendazole_B" ~ "Albendazole_B",   # Rename DMSO_B to Control_B
 #   TRUE ~ drug                        # Keep other values unchanged
 # ))

#fw <- fw %>% dplyr::filter(concentration_um %in% c(0,30))
#Use delta() function to control innate sifferences among strains. This function will calculate the difference in well
#summary statistics between the expeimental condition and the median control condition within a group.
# use the delta() function
##This function is also not working
del <- easyXpress::delta(data = fw,
                         assay_bleach, drug, strain, # group with ...
                         WF = "filter",
                         doseR = TRUE,
                         vars = "median_wormlength_um")

# check the finalized data
checkEff(data = del, drug, strain, x = concentration_um,
         y = median_wormlength_um_delta,
         fill = assay_bleach,
         scales = "free_x")

write.csv(del, "/vast/eande106/projects/Nikita/Filtration/Filtrationmanuscript/Processed_data/Figure7_processed_data.csv", row.names = FALSE)

