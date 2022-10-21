# War over time chart:

library(readr)
library(readxl)
library(ggplot2)

# Load OWID data:
deaths <- read_csv('source-data/deaths-in-state-based-conflicts-per-100000.csv')
deaths$type <- 'Other conflicts'
cols <- colnames(deaths)
deaths <- data.frame(deaths[deaths$Entity == 'World', ])
deaths <- rbind.data.frame(deaths, c('World', 'OWID_WRL', 2021, NA, NA,  (3825 + 187 + 2906)*100000/7874965825, NA, 'Other conflicts')) # Nagarno-Karabakh war
deaths <- rbind.data.frame(deaths, c('World', 'OWID_WRL', 2022, NA, NA,  (9000 + 7500)*100000/7874965825, NA, 'Ukraine war, low estimate (16000 killed)')) # Ukraine
deaths <- rbind.data.frame(deaths, c('World', 'OWID_WRL', 2022, NA, NA,  (25000 + 25000)*100000/7874965825, NA, 'Ukraine war, high estimate (50000 killed)')) # Ukraine
for(i in 3:7){
  deaths[,i] <- as.numeric(deaths[, i])
}
colnames(deaths) <- cols

# Generate plot
ggplot(deaths[deaths$Entity == 'World' & deaths$Year < 2022, ], aes(x=Year, y=`Deaths in conflicts between states per 100,000`, fill=type, col=type))+geom_bar(stat="identity")+xlab('')+geom_point(data=deaths[deaths$Year == 2022, ])+ylab('')+ggtitle('Deaths in conflicts between states per 100,000')+theme_minimal()+theme(legend.title = element_blank())
ggsave('plots/battle_deaths_over_time.png')
