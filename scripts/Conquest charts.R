# This script replicates the conquest charts
# Conquests per year chart:

# Loading data (COW)
df <- read_csv("source-data/terr-changes-v6/tc2018.csv")

# Loading data (OWID):
worldpop <- read_csv('source-data/owid_population.csv')

# More descriptive names:
df$type <- df$procedur
df$type[df$procedur == 1] <- "Conquest\n(armed force)"
df$type[df$procedur == 2] <- "Annexation\n(unilateral)"
df$type[df$procedur == 3] <- "Cession\n(part yielded to other entity)"
df$type[df$procedur == 4] <- "Secession\n(establish new indep. entities)"
df$type[df$procedur == 5] <- "Unification\n(new entity from 2+ pre-existing)"
df$type[df$procedur == 6] <- "Mandated territory\n(by League of Nations or UN)"
df$type[df$procedur == -9] <- "Unknown process\n(often independences)"

# Get names of countries based on correlates of war code
library(countrycode)
df$country_gaining <- countrycode(df$gainer, "cown", "country.name")
df$country_losing <- countrycode(df$loser, "cown", "country.name")

# Get these by decade surrounding:
df$decade <- round((df$year)/10, 0)*10

# Next get gaining and losing country
df$iso3c_gaining <- countrycode(df$country_gaining, 'country.name', 'iso3c')
df$iso3c_losing <- countrycode(df$country_losing, 'country.name', 'iso3c')

# Export these as data frames:
write_csv(df[df$type == 'Conquest\n(armed force)' & df$year > 1945, c('year', 'country_gaining', 'iso3c_gaining', 'country_losing', 'iso3c_losing', 'type', 'pop', 'area')], 'output-data/conquests_since_1945.csv')

write_csv(df[df$type == 'Conquest\n(armed force)' , c('year', 'country_gaining', 'iso3c_gaining', 'country_losing', 'iso3c_losing', 'type', 'pop', 'area')], 'output-data/conquests_since_1816.csv')

write_csv(df[df$year > 1945, c('year', 'country_gaining', 'iso3c_gaining', 'country_losing', 'iso3c_losing', 'type', 'pop', 'area')], 'output-data/territorial_changes_since_1945.csv')

write_csv(df[, c('year', 'country_gaining', 'iso3c_gaining', 'country_losing', 'iso3c_losing', 'type', 'pop', 'area')], 'output-data/territorial_changes_since_1816.csv')


# Draw up the charts:
# Chart 1:
ggplot(df, aes(x=decade, fill= type=='Conquest\n(armed force)'))+geom_histogram(bins = length(unique(df$decade)))+xlab('')+theme_minimal()+ggtitle('Territorial Changes (1816-2018)')
ggsave('plots/conquests_by_decade.png')

# Chart 2:
df$area <- as.numeric(df$area)
df$year <- as.numeric(df$year)
df$country_group <- df$country_gaining
df$country_group[!df$country_gaining %in% c('Russia', 'Japan',
                                    'Italy', 'France',
                                    'United Kingdom',
                                    'Turkey')] <- "Other"
ggplot(df[df$type == 'Conquest\n(armed force)', ], aes(x=year, size=area, y=country_group))+geom_jitter(alpha = 0.25, height = 0.2)+theme_minimal()+xlab('')+ylab('')+geom_point(aes(x=2022, y='Russia', size = 28400 + 27200 + 26500 + 26700)) # Kherson +  Zaporizhzhia + Donetsk + Luhansk
ggsave('plots/conquests_by_country.png')


