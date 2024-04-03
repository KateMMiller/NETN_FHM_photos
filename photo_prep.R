#----------------------------------
# Script pulls in forest photopoints from server, resizes, adds border and title, and saves them as jpegs
#----------------------------------
# Note that because this is pulling 1,500+ images from the Z drive, this can be slow. If you have all
# of the most recent cycle of photopoints locally on your machine, it can go a lot faster.

#----- Libraries -----
library(tidyverse)
library(magick)
library(stringi)
library(forestNETN)
importData()

#----- Compile data for the popups -----
NHPs <- c("MABI", "MIMA", "MORR", "ROVA", "SAGA", "SARA", "WEFA")
from_ACAD = 2021
to_ACAD = 2023
from_NHP = 2019
to_NHP = 2023

plots1 <- rbind(joinLocEvent(park = "ACAD", from = from_ACAD, to = to_ACAD, output = 'verbose', locType = "all"),
                joinLocEvent(park = NHPs, from = from_NHP, to = to_NHP, output = 'verbose', locType = "all")) |> 
  select(Plot_Name, Unit_Code = ParkUnit, Panel = PanelCode, Physio = PhysiographyLabel,  
         Directions, Location_Notes = PlotNotes, SampleYear, Lat, Long, IsStuntedWoodland)

trees <- rbind(joinTreeData(park = "ACAD", from = from_ACAD, to = to_ACAD, status = 'active', locType = "all"),
               joinTreeData(park = NHPs, from = from_NHP, to = to_NHP, status = 'active', locType = "all"))

table(trees$TreeStatusCode)
live = c("AB", "AF", "AL", "AM", "AS", "RB", "RF", "RL", "RS")
dead = c("DB", "DL", "DM", "DS")

trees_sum <- trees |> 
  mutate(status = case_when(TreeStatusCode %in% live ~ "live",
                            TreeStatusCode %in% dead ~ "dead",
                            TRUE ~ "X")) |> 
  filter(!status %in% "X") |>
  group_by(Plot_Name, SampleYear, status) |> 
  summarize(Num_Stems = sum(num_stems), 
            .groups = 'drop') |> 
  pivot_wider(names_from = status, values_from = Num_Stems, values_fill = 0) |> 
  select(Plot_Name, Num_Live_Trees = live, Num_Dead_Trees = dead)

inv_shrubs1 <- rbind(sumQuadGuilds(park = "ACAD", from = from_ACAD, to = to_ACAD, speciesType = "invasive", locType = "all"),
                     sumQuadGuilds(park = NHPs, from = from_NHP, to = to_NHP, speciesType = "invasive", locType = "all"))

inv_shrubs <- inv_shrubs1 |> filter(Group %in% "Tree") |> 
  group_by(Plot_Name) |> summarize(Inv_Shrub_Cov = sum(quad_pct_cover))

regen1 <- rbind(joinRegenData(park = "ACAD", from = from_ACAD, to = to_ACAD, locType = "all"),
               joinRegenData(park = NHPs, from = from_NHP, to = to_NHP, locType = "all"))

regen <- regen1 |> group_by(Plot_Name) |> summarize(Num_Seedlings = sum(seed_den)*3,
                                                    Num_Saplings = sum(sap_den)*3
                                                    )
numspp1 <- rbind(sumSpeciesList(park = "ACAD", from = from_ACAD, to = to_ACAD, locType = "all"),
                sumSpeciesList(park = NHPs, from = from_NHP, to = to_NHP, locType = "all")) 

numspp <- numspp1 |> group_by(Plot_Name) |> summarize(Num_Quad_Species = sum(quad_avg_cov > 0),
                                                      Num_Add_Species = sum(addspp_present > 0))

dfs <- list(plots1, trees_sum, inv_shrubs, regen, numspp)
comb <- reduce(dfs, left_join, by = "Plot_Name")
comb[,12:ncol(comb)][is.na(comb[,12:ncol(comb)])] <- 0

#-----comb#----- Join sample data with photo info -----

#----- Uncomment next 3 lines to remove previous set of photos -----
# old_photos <- list.files("./www", pattern = "JPG$", full.names = T)
# old_photos <- old_photos[!grepl("AH_small", old_photos)] 
# file.remove(old_photos)
table(plots1$Unit_Code, plots1$Panel) # correct number of plots

path <- c("Z:/PROJECTS/MONITORING/Forest_Health/5_Data/Photos/Photopoints/")

path19 <- paste0(path, 2019)
path21 <- paste0(path, 2021)
path22 <- paste0(path, 2022)
path23 <- paste0(path, 2023)

photo_name1 <- list.files(c(path19, path21, path22, path23), 
                            pattern = 'JPG$', full.names = F)

full_names1 <- list.files(c(path19, path21, path22, path23), 
                          pattern = 'JPG$', full.names = T)

drops <- c("ID", "RN", "UC")
photo_name <- photo_name1[!grepl("ID|RN|UC|QAQC", photo_name1)] #drop ID photos
full_names <- full_names1[!grepl("ID|RN|UC|QAQC", full_names1)]

photo_name_df <- data.frame(photo_name)  |>  
                 mutate(plot_name = substr(photo_name, 1, 8),
                        scene = case_when(grepl("UR", photo_name) ~ "UR", 
                                          grepl("BR", photo_name) ~ "BR",
                                          grepl("BL", photo_name) ~ "BL",
                                          grepl("UL", photo_name) ~ "UL"))  |>  
                 arrange(plot_name, photo_name) |>  
                 group_by(plot_name, scene) |>  
                 summarise(photo_name = first(photo_name),
                           .groups = 'drop')

head(photo_name_df)

photo_name_wide <- photo_name_df %>% spread(scene, photo_name) %>% select(-`<NA>`)
photo_name_wide$Plot_Name <- sub("_", "-", photo_name_wide$plot_name)
head(photo_name_wide)
plots <- left_join(comb, photo_name_wide[,-1], by = "Plot_Name")
head(plots)

write.csv(plots, "./data/Plots.csv", row.names = FALSE)

# Check for duplicate plot records (ie QAQC photopoints missing _QAQC in the file name)
dups <- plots$Plot_Name[duplicated(plots$Plot_Name)]

if(length(dups) > 0){
  warning(paste0("There are ", length(unique(dups)), " duplicates in the plots data frame. Check plots: ", 
                 paste0(unique(dups), collapse = ",")))}

# Function to rename each photo based on its file name
view_namer <- function(pname){
  ifelse(grepl("UR", pname), paste0(" Upper Right "),
  ifelse(grepl("BR", pname), paste0(" Bottom Right "),
  ifelse(grepl("BL", pname), paste0(" Bottom Left "),
  ifelse(grepl("UL", pname), paste0(" Upper Left "),
  paste("unknown"))
         )))
}

# Function to resize and label each photo
process_image <- function(import_name, export_name){
  title <- view_namer(pname = export_name)
  
  img <- image_read(import_name)
  img2 <- image_border(img, 'black','6x6')
  img3 <- image_scale(img2, 'X600')
  img4 <- image_annotate(img3, 
                         paste(title), 
                         size = 16, 
                         color = 'black',
                         boxcolor = 'white', 
                         location = "+10+10",
                         degrees = 0)
  image_write(img4, format='jpeg', paste0("./www/", export_name))
}

# test on 1 photo
# process_image(import_name = full_names[40], export_name = photo_name[40])

# Run through all photos. Can break into even smaller chunks of bogs down computer too much
num_photos <- length(full_names) #1587
map2(full_names[1:500], photo_name[1:500], ~process_image(.x,.y))
map2(full_names[501:1000], photo_name[501:1000], ~process_image(.x,.y))
map2(full_names[1001:num_photos], photo_name[1001:num_photos], ~process_image(.x,.y))
