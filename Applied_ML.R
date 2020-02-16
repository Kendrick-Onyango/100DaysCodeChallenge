
# Applied Machine Learning

devtools::install_github("gadenbuie/countdown")


library(tidymodels)
library(kableExtra)
#library(countdown)
library(ggthemes)


#set theme

thm <- theme_bw() + 
  theme(
    panel.background = element_rect(fill = 'transparent', colour = NA),
    plot.background = element_rect(fill = 'transparent', colour = NA),
    legend.position = 'top',
    legend.background = element_rect(fill = 'transparent', colour = NA),
    legend.key = element_rect(fill = 'transparent', colour = NA)
  )
theme_set(thm)



 # location
 # house components
 # number of bedrooms
 # general assessments like quality and condition 

install.packages("AmesHousing")

library(AmesHousing) #for the Housing Data
library(leaflet)
library(htmltools)
library(Cairo)


#Data for Ames Housing
ames <- make_ames()

col_key <- c(
      'NAmes',     '#0000FF',
      'CollgCr',   '#FF0000',
      'OldTown',   '#FFFFFF',
      'Edwards',   '#FF00B6',
      'Somerst',   '#FF3030',
      'NridgHt',   '#009FFF',
      'Gilbert',   '#DD00FF',
      'Sawyer',    '#9A4D42',
      'NWAmes',    '#00FFBE',
      'SawyerW',   '#1F9698',
      'Mitchel',   '#FFACFD',
      'BrkSide',   '#720055',
      'Crawfor',   '#F1085C',
      'IDOTRR',    '#FE8F42',
      'Timber',    '#004CFF',
      'NoRidge',   '#ffff00',
      'StoneBr',   '#B1CC71',
      'SWISU',     '#02AD24',
      'ClearCr',   '#FFD300',
      'MeadowV',   '#886C00',
      'BrDale',    '#FFB79F',
      'Blmngtn',   '#858567',
      'Veenker',   '#A10300',
      'NPkVill',   '#00479E',
      'Blueste',   '#DC5E93',
      'Greens',    '#93D4FF',
      'GreenHills', '#e5f2e5', 
      'Landmrk',   '#C8FF00'
)

col_key <- as.data.frame(matrix(col_key, byrow = TRUE, ncol = 2))
names(col_key) <- c('Neighborhood', 'color')

col_key <- col_key %>% 
          mutate(
            Neighborhood =
              dplyr::recode(
                Neighborhood,
                'Blmngtn' = 'Bloomington_Heights',
                'Bluestem' = 'Bluestem',
                "BrDale" = "Briardale",
                "BrkSide" = "Brookside",
                "ClearCr" = "Clear_Creek",
                "CollgCr" = "College_Creek",
                "Crawfor" = "Crawford",
                "Edwards" = "Edwards",
                "Gilbert" = "Gilbert",
                "Greens" = "Greens",
                "GreenHills" = "Green_Hills",
                "IDOTRR" = "Iowa_DOT_and_Rail_Road",
                "Landmrk" = "Landmark",
                "MeadowV" = "Meadow_Village",
                "Mitchel" = "Mitchell",
                "NAmes" = "North_Ames",
                "NoRidge" = "Northridge",
                "NPkVill" = "Northpark_Villa",
                "NridgHt" = "Northridge_Heights",
                "NWAmes" = "Northwest_Ames",
                "OldTown" = "Old_Town",
                "SWISU" = "South_and_West_of_Iowa_State_University",
                "Sawyer" = "Sawyer",
                "SawyerW" = "Sawyer_West",
                "Somerst" = "Somerset",
                "StoneBr" = "Stone_Brook",
                "Timber" = "Timberland",
                "Veenker" = "Veenker"
              )
      )


#Housing data loaded
# Perhaps a change of theme would be nice!!


lon_rnd <- range(ames$Longitude)
lat_rnd <- range(ames$Latitude)

ia_map <- leaflet(width = '100%') %>% 
  addProviderTiles(providers$Stamen.Toner)

for(i in 1:nrow(col_key)) {
  ia_map <- ia_map %>% 
    addCircles(
      data = subset(ames, Neighborhood == col_key$Neighborhood[i]),
      lng = ~Longitude, lat = ~Latitude,
      color = col_key$color[i],
      fill = TRUE,
      fillColor = col_key$color[i],
      radius = 6,
      popup = htmlEscape(col_key$Neighborhood[i]),
      opacity = .25
    )
}

ia_map




#Data Manipulation 

library(tidyverse)


ames_prices <- "http://bit.ly/2whgsQM" %>%
    read_delim(delim = "\t", guess_max = 2000) %>%
    rename_at(vars(contains(' ')), list(~gsub(' ', '_', .))) %>%
    dplyr::rename(Sale_Price = SalePrice) %>%
    dplyr::filter(!is.na(Electrical)) %>%
    dplyr::select(-Order, -PID, -Garage_Yr_Blt)



ames_prices %>% 
  group_by(Alley) %>% 
  summarise(
    mean_price = mean(Sale_Price / 1000),
    n = sum(!is.na(Sale_Price))
  )

mini_ames <- ames_prices %>% 
  dplyr::select(Alley, Sale_Price, Yr_Sold) %>% 
  dplyr::filter(!is.na(Alley))


head(mini_ames, n = 10)


mini_ames1 <- ames_prices %>% 
  dplyr::select(Alley, Sale_Price, Yr_Sold) %>% 
  dplyr::filter(is.na(Alley))


head(mini_ames1, n = 10)


#split the data in parts -- alley
by_alley <- split(mini_ames, mini_ames$Alley)
map(by_alley, head, n = 10)
map(by_alley, nrow)
map_int(by_alley, nrow)


map(by_alley,
    ~summarise(.x, max_price = max(Sale_Price)))


map(by_alley,
    ~summarise(.x, max_price = mean(Sale_Price)))

map(by_alley,
    ~summarise(.x, min_price = min(Sale_Price),max_price = max(Sale_Price),mean_price = mean(Sale_Price)))


#Nest
ames_lts_col <- nest(mini_ames, -Alley)
ames_lts_col


#Quick Data Investigation

library(AmesHousing)
ames <- make_ames()
names(ames)

ames_summ <- ames %>% 
  mutate(Mean = mean(Sale_Price),
         per_cent = (Sale_Price) / (2930),
         )

head(ames_summ, 10)














