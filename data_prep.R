library(forestNETN)
library(tidyverse)

plots <- read.csv("./data/Plots.csv")[,c(1:22)]

importData()

live <- c("1", "AB", "AF", "AL", "AM", "AS", "RB", "RF", "RL", "RS")
dead <- c("2", "DB", "DL", "DM", "DS")

trees <- joinTreeData(from = 2016, to = 2019) %>% select(Plot_Name, TagCode, TreeStatusCode, DBHcm) %>% 
         mutate(status = case_when(TreeStatusCode %in% live ~ "live",
                                   TreeStatusCode %in% dead ~ "dead",
                                   TRUE ~ "unk"),
                stems_live = ifelse(status == "live", 1, 0),
                stems_dead = ifelse(status == 'dead', 1, 0)) %>% 
         group_by(Plot_Name) %>% 
         summarize(Num_Live_Trees = sum(stems_live, na.rm = T),
                   Num_Dead_Trees = sum(stems_dead, na.rm = T))

spp_list <- sumSpeciesList(from = 2016, to = 2019) %>% select(Plot_Name, ScientificName) %>% 
            group_by(Plot_Name) %>% summarize(Num_Species = n()) 

invshrubs <- joinMicroShrubData(from = 2016, to = 2019, speciesType = 'invasive') %>% 
             group_by(Plot_Name) %>% summarize(Inv_Shrub_Cover = sum(shrub_avg_cov, na.rm = T)) %>% 
             arrange(Plot_Name)

regen <- joinRegenData(from = 2016, to = 2019) %>% select(Plot_Name, seed_den, sap_den) %>% 
         group_by(Plot_Name) %>% summarize(Num_Seedlings = sum(seed_den, na.rm = T) * 3,
                                           Num_Saplings = sum(sap_den, na.rm = T) * 3)

dfs <- list(plots, trees, spp_list, invshrubs, regen)

plots2 <- Reduce(left_join, dfs)

write.csv(plots2, "./data/Plots.csv", row.names = FALSE)
