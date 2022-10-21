# Globe animation data:
library(readr)
library(countrycode)

# Load war data:
wars <- read_csv('source-data/Inter-StateWarData_v4.0.csv')
wars <- data.frame(wars)

# Split World War II in Asia/Pacific out to reflect countries fighting there:
wars$WarName[wars$WarName == 'World War II' & wars$StateName %in% c('Mongolia', 'Australia', 'Japan', 'New Zealand', 'China')] <- 'World War II in Asia (approx)'

# Split US casualties by war:
ratio <- 57137/177100	# Source: https://www.ibiblio.org/hyperwar/USA/ref/Casualties/Casualties-1.html - This gives a different source for the total, as do many others. For consistency, we use COW numbers for the total
US_losses_in_world_war_2 <- wars$BatDeath[wars$WarName == 'World War II' & wars$StateName == 'United States of America']
US_losses_in_world_war_2_europe <- US_losses_in_world_war_2*(1-ratio)
US_losses_in_world_war_2_asia <- US_losses_in_world_war_2*(ratio)

# Generate total battle deaths for conflict (estimated)
wars$total_battle_deaths <- ave(wars$BatDeath, wars$WarName, FUN = function(x){sum(ifelse(x < 0, 0, x))})
wars$total_battle_deaths[wars$WarName == 'World War II'] <- unique(wars$total_battle_deaths[wars$WarName == 'World War II']) - US_losses_in_world_war_2_asia
wars$total_battle_deaths[wars$WarName == 'World War II in Asia (approx)'] <- unique( wars$total_battle_deaths[wars$WarName == 'World War II in Asia (approx)']) + US_losses_in_world_war_2_asia

# Generate iso3c code
wars$iso3c <- countrycode(wars$ccode, 'cown', 'iso3c')

# Get country centroids:
library(rgeos)
library(rworldmap)

# get world map
wmap <- getMap(resolution="high")

# get centroids
centroids <- gCentroid(wmap, byid=TRUE)

# get a data.frame with centroids
df <- as.data.frame(centroids)
colnames(df) <- c('lng_country', 'lat_country')
df$ccode <- countrycode(rownames(df), 'country.name', 'cown')
wars <- merge(wars, df, by='ccode', all.x = T)

# Get location of war (using manual matching of wars to mids, and some manual collection of conflict location data)
merge_table <- read_csv('source-data/WarNum to DispNum matching (Correlates of War data) and locations if missing.csv')
midl <- read_csv('source-data/MIDLOCA_2.1.csv')

colnames(merge_table)[3] <- 'dispnum'
war_locations <- merge(merge_table, midl, by = 'dispnum', all.x=T)
war_locations$lat[is.na(war_locations$lat)] <- war_locations$midloc2_ylatitude[is.na(war_locations$lat)]
war_locations$lng[is.na(war_locations$lng)] <- war_locations$midloc2_xlongitude[is.na(war_locations$lng)]
war_locations <- data.frame(war_locations)

wars <- merge(wars, war_locations[!duplicated(war_locations$WarNum), c('WarNum', 'lat', 'lng')], all.x = T)

# Add Aremenia v Azerbaijan
wars <- rbind(wars, rep(NA, ncol(wars)))
wars$WarNum[nrow(wars)] <- 99999
wars$WarName[nrow(wars)] <- "Armenia v Azerbaijan"
wars$total_battle_deaths[nrow(wars)] <- 7000
wars$StartYear1[nrow(wars)] <- 2020
wars$EndYear1[nrow(wars)] <- 2020
wars$StartYear2[nrow(wars)] <- 2020
wars$EndYear2[nrow(wars)] <- 2020
wars$lat[nrow(wars)] <- 39.869473
wars$lng[nrow(wars)] <- 46.255298

# Add Russo v Ukraine
wars <- rbind(wars, rep(NA, ncol(wars)))
wars$WarNum[nrow(wars)] <- 999999
wars$WarName[nrow(wars)] <- "2022 Russo-Ukraine war"
wars$total_battle_deaths[nrow(wars)] <- 50000
wars$StartYear1[nrow(wars)] <- 2022
wars$EndYear1[nrow(wars)] <- 2022
wars$StartYear2[nrow(wars)] <- 2022
wars$EndYear2[nrow(wars)] <- 2022
wars$lat[nrow(wars)] <- 36.305
wars$lng[nrow(wars)] <- 50.251

# Fix location of second part of Vietnam war, putting it at the then-border between North and South Vietnam (previously overlapped with other conflict)
wars[wars$WarNum == 163, c('lat')] <- c(17.01)
wars[wars$WarNum == 163, c('lng')] <- c(106.82)

# Fix location of the Second Schleswig-Holstein wars
wars[wars$WarNum %in% c(46) , c('lat')] <- c(54.51)
wars[wars$WarNum %in% c(46) , c('lng')] <- c(9.72)

# Fix location of the Naval War (also known as the Chincha War or Guano war)
wars[wars$WarNum %in% c(52) , c('lat')] <- c(-13.64)
wars[wars$WarNum %in% c(52) , c('lng')] <- c(-76.40)

# Fix location of the Italian-Roman war
wars[wars$WarNum %in% c(34) , c('lat')] <- c(45.47)
wars[wars$WarNum %in% c(34) , c('lng')] <- c(8.88)

# Fix location of the Neapolitan war
wars[wars$WarNum %in% c(37) , c('lat')] <- c(37.77)
wars[wars$WarNum %in% c(37) , c('lng')] <- c(12.45)

# Fix location of the Crimean War (acknowledging that fighting took place elsewhere too)
wars[wars$WarNum %in% c(22) , c('lat')] <- c(33.66)
wars[wars$WarNum %in% c(22) , c('lng')] <- c(44.83)

# Fix location of World War II in Asia
wars[wars$WarName == 'World War II in Asia (approx)', c('lat')] <- c(14.62)
wars[wars$WarName == 'World War II in Asia (approx)', c('lng')] <- c(121.11)

# Give this war a separate new
wars$WarNum[wars$WarNam =='World War II in Asia (approx)'] <- max(wars$WarNum, na.rm=2)*10
wars$WarNum[wars$WarNam =='World War II in Asia (approx)'] <- max(wars$WarNum, na.rm=2)*10

# Write export
write_csv(wars, 'output-data/war_list_1816_2010.csv')
write_csv(wars, 'output-data/wars_with_approximate_location.csv')

# Generate same in long format
wars_long <- data.frame()
for(i in 1:nrow(wars)){
  temp <-  wars[rep(i, length(wars$StartYear1[i]:min(max(wars$EndYear1[i], wars$EndYear2[i]), max(wars$EndYear1, na.rm = T)))), ]
  temp$year <- wars$StartYear1[i]:min(max(wars$EndYear1[i], wars$EndYear2[i]), max(wars$EndYear1, na.rm = T))
  wars_long <- rbind(wars_long, temp)
}

write_csv(wars, 'output-data/war_list_1816_2010_long_format.csv')
