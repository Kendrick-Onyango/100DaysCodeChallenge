

# Need to load this package first to prevent it
# from masking the 'select' command in dplyr (tidyverse)

install.packages(c('PASWR','formattable','wesanderson','ggrepel','viridis','gapminder','gridExtra'))
library(PASWR) #titanic3 dataset (for age-sex population pyramid)

## General:
library(tidyverse) # dplyr, data manipulation
library(formattable) # percent format
library(wesanderson) # Color Palettes from Wes Anderson Movies

## For specific plots:
library(ggrepel) # text labels
library(viridis) # colors
library(gapminder) # gdp life expectancy data
library(gridExtra) 

# Set default ggplot theme
theme_set(theme_bw()+
            theme(legend.position = "top",
                  plot.subtitle= element_text(face="bold",hjust=0.5),
                  plot.title = element_text(lineheight=1, face="bold",hjust = 0.5)))

# Color blind friendly palette from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", 
               "#D55E00", "#CC79A7")




ggplot(data=gapminder %>% filter(year==2007),
       aes(x = gdpPercap, y = lifeExp, color = continent,size=pop,group=1)) +
  geom_point() +
  # remove legend margins
  theme( legend.margin=margin(0,0,0,0),
         legend.box.margin=margin(0,0,0,0),
         legend.pos='right') +
  scale_x_continuous(labels=scales::dollar) +
  geom_smooth(method="loess",show.legend=F,size=0.5,alpha=0.25) + # Regression line
  #scale_color_manual(values=wes_palette('Royal2')) +
  labs(title='The Wealth of Nations - GDP v. Life Expectancy',
       caption='Data is for 2007. 95% confidence interval is shaded.') +
  xlab('GDP Per Capita (USD, inflation-adjusted)') +
  ylab('Life Expectancy (at birth)') +
  guides(color=guide_legend(title='Continent',override.aes = list(size=2.5)),
         size=guide_legend(title='Population'))