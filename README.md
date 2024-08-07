# Environmental Animated Visuals

<br/>

## Table of Contents

- [Project Overview](#project-overview)
- [Data Sources](#data-sources)
- [Tools](#tools)
- [Questions to be answered](#questions-to-be-answered)
- [Illustrating the Answers](#illustrating-the-answers)
- [Plus one](#plus-one)
##

<br/>

### Project Overview

In this R-Script, we create 3+1 animated graphs on the basis of climate change concerns.
Firstly, we collect data from the Greek Ministry of Energy about the amount of electricity produced by different sources during the first five months of 2021.
Then, we move our focus to Denmark, where we firstly create an animated graph for the Average Temperature of Denmark since 1900. Furthermore, we examine
the CO2 Emissions of Denmark separately in two cases. Initially, we observe the CO2 Emissions without counting the emissions derived from Biomass, and then we
aggregate Biomass impact in order to examine any discrepancy in emissions, finally emphasizing that Biomass constitute a significant polluter. Finally, slightly 
out of context, we present a very interesting animated graph comparing GDP vs Life Expectancy in various countries worldwide.

### Data Sources

The information about the energy sources of electricity production in Greece are collected individually from the Greek Ministry of Energy and incorporated directly to the R script.
Moreover, the data concerning Denmark's average temperature and Co2 emissions was retrieved from the relevant official State's website and are stored in the files 
"observed-average-annual-mean-temperature-of-denmark-for-1901-2020.csv", "Co2_with_biomass.xml", "Co2_without_biomass.xml".

### Tools

The visualisation libraries ggplot and gganimate and the wrangling library dplyr. Also, readxl and gapminder libraries are used for loading and acquiring data, respectively.
Everything is developed within the R framework.




### Questions to be answered

1. How much renewable the electricity generation was in the beginning of 2021 in Greece?
2. Is there an increase in Denmarks' average temperature througout the 20th, and so far in the 21st century? 
3. What are Denmarks' Co2 emissions during the same period of time?
4. Is biomass a significant polluter concerning Denmarks' electricity production?


### Illustrating the Answers

### 1.


For answering the first question, we initially create the required datframe.

```R
elec_prod_per_source = data.frame("MWh" = c((1632/4565),(582/4565),(1197/4565),(742/4565),(410/4565),0,1/4565,
                                                    861/4031,517/4031, 894/4031, 864/4031,462/4031,431/4031, 1/4031,
                                                    1341/4216,644/4216,859/4216,275/4216,557/4216,538/4216,1/4216,
                                                    1615/3802, 412/3802, 775/3802,218/3802,590/3802,191/3802,1/3802,
                                                    1221/3766, 363/3766, 713/3766,328/3766,681/3766,459/3766,2/3766)*100,
                                  "Type" = rep(c("Natural Gas","Coal", "Renewable Energy", "Hydroelectrical",
                                                 "Network Production","Equilibrium", "Else"),5),
                                  
                                  "Month" = c(rep(1,7),rep(2,7),rep(3,7),rep(4,7),rep(5,7)))
```

Then, we plot the information in a form of a pie plot.

```R
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
```
And we turn the pie chart animated on the grounds of the different months.

```R
graph2.animation = graph2 +
  transition_time(Month)+
  labs(subtitle = "Month :{frame_time}")
```
We finally run it.

```R
animate(graph2.animation, nframes = 5, fps = 1)
```
![EnergyPie2](https://github.com/user-attachments/assets/673e41bb-25c2-476a-b886-3a5e6efa0192)

From the pie chart we see that electricity from coal is still a substantial part of the energy production in Greece in 2021, that 
the relevant figure for renewable energy is fluctuating significantly, implying inadequate stability of green energy production, while,
natural gas holds the first place in terms of where most electricity produced in Greece between January and May, 2021. 

<br/><br/>
### 2.

We now move forward to explore the evolution of Denmarks' average temperature througout the 20th, and so far in the 21st century.

Let us first load the relevant data from the "observed-average-annual-mean-temperature-of-denmark-for-1901-2020.xml"

```R
cool = read.csv(file.choose())
```

Then, we create the line plot object.

```R
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
```

We now tranform the plot into an animated line graph based on the year.

```R
graph5.animation = graph5 +
  transition_reveal(Category)
```

Run it.

```R
animate(graph5.animation, width = 1800, height = 400, fps = 30, duration = 15)
```
*The dashed horizontal line represents the aggregate average temperature in Denmark throughout the period under investigation.

![Avg_Temp](https://github.com/user-attachments/assets/d37dc545-7ebe-41d9-8763-4f7b9dcf8370)

The observation that provides insights into the question whether Denmarks' Temperature has increased in this period of time is the trend of the line after 2000
where it almost always (except one year) lies over the total average, signifying a considerable increase in the 21st century so far. 

<br/><br/>

### 3 & 4.

Next, we examine the Co2 emissions in the country of Denmark. In fact, we aim to answer both last questions with one plot. 

We start by introducing the datasets "Co2_with_biomass" and "Co2_without_biomass".

```R
incl_biomass_co2 = read_xlsx(file.choose())
excl_biomass_co2 = read_xlsx(file.choose())
```

We need to tranform the structure of the datasets and turn the values into the approriate form, that we do below.
```R
incl_biomass_co2 = incl_biomass_co2[3,3:length(incl_biomass_co2)]# Select the needed single row and the respective columns
excl_biomass_co2 = excl_biomass_co2[3,3:length(excl_biomass_co2)]


excl_biomass_co2 = as.integer(excl_biomass_co2)# We turn Characters into integers for visualising them
incl_biomass_co2 = as.integer(incl_biomass_co2)

year = seq(1990,2020,1)# For convenience, we create a list with the years under investigation
```
Collecting all the information into one dataframe.

```R
real_df = data.frame('ExcludingBiomass' = excl_biomass_co2,
                     'IncludingBiomass' = incl_biomass_co2,
                     'year' = year)
```

We are now ready to create the plot object as follows.

```R
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
```

We animate on the variable of year that we created above.

```R
graph7.animation = graph7 +
  transition_reveal(year)
```

Run.

```R
animate(graph7.animation, width = 1600, height = 400, fps = 20, duration = 10)
```

![Co2_Den](https://github.com/user-attachments/assets/fee89489-e8a5-4f04-b7b1-a9b7a647b8e0)

The critical information to extract from the plot above is that through years the discrepancy between the included and not-included biomass Co2 emissions
is increased. Thus, more and more biomass is deployed as a source of electricity production. Yet, this comes as no suprise, as Danish state has designeted
biomass as a green, renewable form of electricity production hence not counting the emissions from it. However, as the plot reveals, there is significant
pollution owing to the use of biomass, and evading this crucial observation does not help the mission towards a zero-Co2 emissions planet.


<br/><br/>

### Plus one.

We end this short visual report on climate topics with a universal plot of Gross Domestic Product against Life expectancy. We find it an interesting addition 
to this project serving as a reminder about the state, growth, needs and prospects globally.

The code is as follows.

```R
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
```



![GDPvslongeivity](https://github.com/user-attachments/assets/e49ecba8-7f05-4a48-94bd-963a7e91676a)

