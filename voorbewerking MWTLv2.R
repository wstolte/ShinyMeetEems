#######################################################################
####                                                               ####
####          Script by willem.stolte@deltares.nl                  ####
####    Reads MWTL csv and saves file for presentation purpose     ####
####                                                               ####
####                copyright Deltares                             ####
####                                                               ####
#######################################################################

require(scales)
require(dplyr)
require(tidyr)
require(ggplot2)

submap <- read.csv2("d:/Tools_Scripts/Mapping tables/RWS2DELWAQ2names.csv", header = T, stringsAsFactors=FALSE)
locmap <- read.csv2("d:/Tools_Scripts/Mapping tables/RWS2DELWAQ2locations.csv", header = T, stringsAsFactors=FALSE)
locmod = c("Huibertgat_oost", "IMARES_st_2", "IMARES_st_3b", "Bocht_van_Watum", "IMARES_st_4b", "IMARES_st_5", "Groote_Gat_noord")

# rws_dat           <- read.csv2("d:\\GIS-DATA\\Nederland\\EemsDoll\\naarPostGis\\RWS\\MWTL_all.csv",dec = ".")
rws_dat           <- read.csv2("d:/GIS-DATA/Nederland/EemsDoll/RWS/levering2015/bewerkt/surface_all.csv", header = T)
# rws_dat$wrd       <- as.numeric(rws_dat$wrd)
rws_dat$datetime   <- as.POSIXct(rws_dat$datetime, format = "%Y-%m-%d %H:%M:%S")

# moet worden select_ or rename_
rws_dat$variable   <- mapvalues(as.character(rws_dat$parhdhcod), from = submap$RWS_DONAR_parcod_hdh2, to = submap$NL_name, warn_missing = F)
# rws_dat$variable   <- rename_(rws_dat, submap$NL_name = submap$RWS_DONAR_parcod_hdh2 submap$NL_name, warn_missing = F)

# rws_dat$location   <- mapvalues(as.character(rws_dat$locoms), from = locmap$locoms, to = locmap$Delwaq_ED, warn_missing = F)
# rws_dat$variable   <- mapvalues(as.character(rws_dat$variable), from = submap$Delwaq, to = submap$Delwaq_long_name, warn_missing = F)
rws_dat$month      <- format(rws_dat$datetime, format = "%m")
#filter for high PO4 and NH4 measurements
rws_dat <- subset(rws_dat, rws_dat$wrd < 0.6 | rws_dat$variable != "phosphate")
rws_dat <- subset(rws_dat, rws_dat$wrd < 1 | rws_dat$variable != "P nf")
rws_dat <- subset(rws_dat, rws_dat$wrd < 1 | rws_dat$variable != "TotP")
rws_dat <- subset(rws_dat, rws_dat$wrd < 0.6 | rws_dat$variable != "ammonium")
rws_dat <- subset(rws_dat, rws_dat$wrd < 75 | rws_dat$variable != "chlorophyll-a")
rws_dat <- subset(rws_dat, rws_dat$wrd < 3 | rws_dat$variable != "N pg")
rws_dat <- subset(rws_dat, rws_dat$wrd < 30 | rws_dat$variable != "dissolved org C")
rws_dat <- subset(rws_dat, rws_dat$wrd < 500 | rws_dat$variable != "suspended solids")
rws_dat <- subset(rws_dat, rws_dat$wrd < 1000 )
rws_dat <- subset(rws_dat, rws_dat$wrd >= 0 )
rws_dat <- subset(rws_dat, rws_dat$kwccod == 0 )
rws_dat$year <- as.numeric(format(rws_dat$datetime, "%Y"))
rws_dat$season <- ifelse(rws_dat$month %in% c("10", "11", "12", "01", "02"), "winter", "summer")

save(rws_dat, file = "d:/Tools_Scripts/R/ShinyMeetEems/data/MWTL_Eems_bewerkt.Rdata")

imares_dat <- read.csv("d:/GIS-DATA/Nederland/EemsDoll/naarPostGis/IMARES/all_nutrients_long.csv", sep = ";")
imares_dat$datetime   <- as.POSIXct(imares_dat$datetime, format = "%d-%m-%y %H:%M")
imares_dat$variable   <- mapvalues(as.character(imares_dat$variable), from = submap$short_name, to = submap$NL_name, warn_missing = F)
imares_dat$location   <- mapvalues(as.character(imares_dat$rws_loccode), from = locmap$loccod, to = locmap$locomsch, warn_missing = F)
# imares_dat$variable   <- mapvalues(as.character(imares_dat$variable), from = submap$Delwaq, to = submap$Delwaq_long_name, warn_missing = F)
imares_dat$month      <- format(imares_dat$datetime, format = "%m")
imares_dat$year <- as.numeric(format(imares_dat$datetime, "%Y"))
imares_dat$season <- ifelse(imares_dat$month %in% c("10", "11", "12", "01", "02"), "winter", "summer")
colnames(imares_dat) <- mapvalues(colnames(imares_dat), from = c("value", "location"), to = c("wrd", "locoms"))

save(imares_dat, file = "d:/Tools_Scripts/R/ShinyMeetEems/data/IMARES_Eems_bewerkt.Rdata")

# p <- ggplot(aes(datetime, wrd), data = imares_dat)
# p + geom_line(aes(color = locoms)) +
#   geom_point(aes(datetime, wrd, color = locoms), data = rws_dat) +
#   facet_wrap(~ variable, scales = "free") 
