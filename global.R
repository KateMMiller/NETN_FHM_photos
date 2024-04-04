#-------------------------
# Global files to populate map 
#-------------------------
plots <- read.csv("./data/Plots.csv")
plots$plot_num <- substr(plots$Plot_Name, 6, 8)

bboxes <- read.csv("./data/boundboxes.csv")

meanLat <- (bboxes[bboxes$ParkCode == "NETN", c("LatS")] + 
              bboxes[bboxes$ParkCode == "NETN", c("LatN")])/2

meanLong <- (bboxes[bboxes$ParkCode == "NETN", c("LongE")] + 
               bboxes[bboxes$ParkCode == "NETN", c("LongW")])/2

