library('readxl')
library('ggplot2')
library("gganimate")
library("dplyr")
library("ggthemes")
library("gapminder")
library('gifski')
library("png")
library('extrafont')
library("tidyverse")
library("lubridate")
library(gridExtra)




# 1st Animated plot: Life Expectancy vs GDP per capita. 
graph1 = gapminder %>%
  ggplot(aes(x = gdpPercap, y = lifeExp, colour = continent, size = pop))+
  geom_point(alpha = 0.7, stroke = 0)+
  theme_fivethirtyeight()+
  labs(x = "GDP per Capita", 
       y = "Life Expectancy", 
       title = "GDP per Capita VS Life Expectancy" )+
  scale_x_log10()+
  theme(panel.background = element_rect(fill = 'white'),
        plot.background = element_rect(fill = 'white'),
        legend.text = element_text(size = 15),
        axis.title.x = element_text(margin = margin(t = 40)),
        axis.title.y = element_text(margin = margin(r = 40)),
        legend.background = element_rect("white"),
        axis.title = element_text(),
        plot.title = element_text(hjust = 0.5))+
  scale_size_continuous(name = "population")

graph1


graph1.animation = graph1 +
  transition_time(year)+
  labs(subtitle = "year : {frame_time}")

animate(graph1.animation, width = 1200, height = 800, fps = 20, duration = 10)


# ADMIE data imported manually into a dataframe. The data concerns the rate of electricity production in Greece (not entire) per source,
# for the first five months of 2021 .
elec_prod_per_source = data.frame("MWh" = c((1632/4565),(582/4565),(1197/4565),(742/4565),(410/4565),0,1/4565,
                                                    861/4031,517/4031, 894/4031, 864/4031,462/4031,431/4031, 1/4031,
                                                    1341/4216,644/4216,859/4216,275/4216,557/4216,538/4216,1/4216,
                                                    1615/3802, 412/3802, 775/3802,218/3802,590/3802,191/3802,1/3802,
                                                    1221/3766, 363/3766, 713/3766,328/3766,681/3766,459/3766,2/3766)*100,
                                  "Type" = rep(c("Natural Gas","Coal", "Renewable Energy", "Hydroelectrical",
                                                 "Network Production","Equilibrium", "Else"),5),
                                  
                                  "Month" = c(rep(1,7),rep(2,7),rep(3,7),rep(4,7),rep(5,7)))


df <- elec_prod_per_source %>% 
  group_by(Type) %>% # Variable to be transformed
  count() %>% 
  ungroup() %>% 
  mutate(perc = `n` / sum(`n`)) %>% 
  arrange(perc) %>%
  mutate(labels = scales::percent(perc))



graph2 = elec_prod_per_source %>%
  ggplot(aes(x = "", y = MWh, fill = Type))+
  geom_col()+
  geom_text(aes(label = round(MWh, digits = 2)),
            position = position_stack(vjust = 0.5),
            )+
  labs(title = "Greece:\nContribution in electricity generation\nper source in Jan-May 2021 (in MWh)", x = "", y = "Mwh(%)")+
  theme(panel.background = element_rect(fill = 'transparent'),
        title = element_text(size = 15, hjust = 0.5, face = 'bold'),
        legend.text = element_text(size = 13))+
  coord_polar(theta = "y")+
  scale_fill_discrete("")
  
  

graph2.animation = graph2 +
  transition_time(Month)+
  labs(subtitle = "Month :{frame_time}")


animate(graph2.animation, nframes = 5, fps = 1)




# Denmark: Average temperature evolution throughout the 20th and 21th century
cool = read.csv(file.choose())

cool %>%
  mutate(dansk_kurac$co2_per_capita[2:nrow(dansk_kurac)], .before_col = Annual.Mean)


cool

graph5 =
cool %>% 
  ggplot(aes(x = Category, y = Annual.Mean))+
  geom_line(size = 0.8, colour = "red")+
  geom_point(size = 3)+
  geom_hline(yintercept = mean(cool$Annual.Mean), lty = "dashed", size = 0.6)+
  labs(title = "Average Temperature in Denmark", y = "Temperature(Celsius)", subtitle = "between 1901 - 2020", x = "Year")+
  theme_fivethirtyeight()+
  theme(plot.title = element_text(size = 20, hjust = 0.5, face = "bold", colour = "black", margin = margin(b=20)),
        axis.title.y = element_text(size = 13, margin = margin(r=40)),
        axis.title.x = element_text(size = 13, margin = margin(t=40)),
        axis.line.y = element_line(size = 0.5, colour = "black", linetype = "dotted"),
        axis.ticks.y = element_line(size = 0.5, colour = "black", linetype = "solid"),
        plot.subtitle = element_text(hjust = 0.5,colour = "black", face = 'italic', margin = margin(b = 60, t = 0)),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background =  element_blank())+
  scale_x_continuous(breaks = seq(1900,2020,20))


graph5.animation = graph5 +
  transition_reveal(Category)

animate(graph5.animation, width = 1800, height = 400, fps = 30, duration = 15)



# Data about the Co2 Emissions in Denmark throughout a specific  time span 
emit = read_xlsx(file.choose(), col_names  = FALSE)

emit
dansk_emit = emit[emit$country == "Denmark",]# Specify Country
dansk_emit = dansk_emit[dansk_emit$year >= 1990,]# Specify Time span

names(emit)
# We create the TS plot
graph6 = 
  dansk_emit %>%
  ggplot(aes(x = year, y = co2_per_capita))+
  geom_line(size = 0.5, colour = "black")+
  geom_point(size = 1)+
  labs(title = "Denmark's Co2 Emissions per capita", y = "Metric Tonnes", subtitle = "between 1901 - 2020")+
  theme_fivethirtyeight()+
  theme(plot.title = element_text(size = 20, hjust = 0.5, face = "bold", colour = "black", margin = margin(b=20)),
        axis.title.y = element_text(size = 13, margin = margin(r=40)),
        axis.title.x = element_blank(),
        axis.line.y = element_line(size = 0.5, colour = "black", linetype = "dotted"),
        axis.ticks.y = element_line(size = 0.5, colour = "black", linetype = "solid"),
        plot.subtitle = element_text(hjust = 0.5,colour = "black", face = 'italic', margin = margin(b = 60, t = 0)),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background =  element_blank())



# Now we aim to plot the amount of Co2 emissions in Denmark including biomass, since 1990. There is a mismatching between the two 
# datasets of Co2 emissions.


incl_biomass_co2 = read_xlsx(file.choose())
excl_biomass_co2 = read_xlsx(file.choose())

incl_biomass_co2 = incl_biomass_co2[3,3:length(incl_biomass_co2)]# Select the needed single row and the respective columns
excl_biomass_co2 = excl_biomass_co2[3,3:length(excl_biomass_co2)]


excl_biomass_co2 = as.integer(excl_biomass_co2)# We turn Characters into integers for visualising them
incl_biomass_co2 = as.integer(incl_biomass_co2)

year = seq(1990,2020,1)# For convenience, we create a list with the years under investigation

real_df = data.frame('ExcludingBiomass' = excl_biomass_co2,
                     'IncludingBiomass' = incl_biomass_co2,
                     'year' = year)# We collect everything into a dataframe


graph7 = 
  real_df %>%
  ggplot()+
  geom_line(aes(x = year, y = IncludingBiomass, color = "w/biomass"))+
  geom_point(aes(x = year, y = IncludingBiomass), size = 3)+
  geom_line(aes(x = year, y = ExcludingBiomass, color = "without biomass"))+
  geom_point(aes(x = year, y = ExcludingBiomass))+
  labs(title = "Denmark's Co2 Emissions", y = "Metric Tonnes", subtitle = "between 1990 - 2020", caption = "Source : STATISTICS DENMARK")+
  theme_fivethirtyeight()+
  theme(plot.title = element_text(size = 20, hjust = 0.5, face = "bold", colour = "black", margin = margin(b=20)),
        axis.title.y = element_text(size = 13, margin = margin(r=40)),
        axis.title.x = element_blank(),
        axis.ticks.y = element_line(size = 0.5, colour = "black", linetype = "solid"),
        plot.subtitle = element_text(hjust = 0.5,colour = "black", face = 'italic', margin = margin(b = 60, t = 0)),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(),
        panel.grid.minor = element_blank(),
        plot.background =  element_blank(),
        legend.title = element_blank(),
        legend.background = element_blank(),
        plot.caption = element_text(face = "italic", size = 7))+
  scale_x_continuous(breaks = seq(1990,2020,5))



graph7.animation = graph7 +
  transition_reveal(year)



animate(graph7.animation, width = 1600, height = 400, fps = 20, duration = 10)







scale_size






