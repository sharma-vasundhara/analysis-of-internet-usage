---
title: "analysis_of_internet_usage"
author: "Vasundhara Sharma"
date: "1/22/2022"
output: github_document
---

knitr::opts_chunk$set(echo = TRUE)
library(tidyr)
library(dplyr)  
library(magrittr)
library(tidyr)
library(stringr)
library(lubridate)
library(stringr)
library(lemon)
library(knitr)
library(ggplot2)
library(gridExtra)
library(maps)
library(ggmap)
library(kableExtra)
library(ggrepel)
library(directlabels)


# Reading the data files

# reading data files
df_broadband_penetration <- read.csv("data_files/broadband-penetration-by-country.csv", na.string = "")
df_mobile_subscription <- read.csv("data_files/mobile-cellular-subscriptions-per-100-people.csv", na.string = "")
df_internet_users <- read.csv("data_files/number-of-internet-users-by-country.csv")
df_share_indviduals_using_internet <- read.csv("data_files/share-of-individuals-using-the-internet.csv")
df_continents <- read.csv("data_files/continents2.csv")
df_political_indexes <- read.csv("data_files/country-index.csv")


# Understanding the data

str(df_broadband_penetration)

knitr::kable(df_broadband_penetration %>%
               select(everything()) %>% 
               summarise_all(list(~sum(is.na(.)))),
             caption = 'Number of NULL values in each column')



str(df_mobile_subscription)

knitr::kable(df_mobile_subscription %>%
               select(everything()) %>% 
               summarise_all(list(~sum(is.na(.)))),
             caption = 'Number of NULL values in each column')



str(df_internet_users)

knitr::kable(df_internet_users %>%
               select(everything()) %>% 
               summarise_all(list(~sum(is.na(.)))),
             caption = 'Number of NULL values in each column')



str(df_share_indviduals_using_internet)

knitr::kable(df_share_indviduals_using_internet %>%
               select(everything()) %>% 
               summarise_all(list(~sum(is.na(.)))),
             caption = 'Number of NULL values in each column')



str(df_continents)

knitr::kable(df_continents %>%
               select(everything()) %>% 
               summarise_all(list(~sum(is.na(.)))),
             caption = 'Number of NULL values in each column')



str(df_political_indexes)

knitr::kable(df_political_indexes %>%
               select(everything()) %>% 
               summarise_all(list(~sum(is.na(.)))),
             caption = 'Number of NULL values in each column')


# Renaming columns and fixing the political indexes

# renaming column names
colnames(df_broadband_penetration) <- c("Country", "Code", "Year", "Fixed_Broadband_Subscription_Per_100_People")
colnames(df_mobile_subscription) <- c("Country", "Code", "Year", "Mobile_Cellular_Subscription_Per_100_People")
colnames(df_internet_users) <- c("Country", "Code", "Year", "Number_Of_Internet_Users")
colnames(df_share_indviduals_using_internet) <- c("Country", "Code", "Year", "Percent_Of_People_Using_Internet")
colnames(df_political_indexes) <- c("Code", "Country", "Year", "Democracy_Index", "Electoral_Process_And_Pluralism", "Functioning_Of_Government", "Political_Participation", "Political_Culture", "Civil_Libterties", "Change_In_Democracy_Index")
colnames(df_continents) <- c("Country",	"Alpha-2",	"Alpha-3",	"Country-code",	"ISO_3166-2",	"Region",	"Sub-region", "Intermediate-region",	"Region-code",	"Sub-region-code", "Intermediate-region-code")

# fixing data
df_political_indexes$Democracy_Index = (as.numeric(df_political_indexes$Democracy_Index) / 10)



# Exploring and analyzing the data

knitr::kable(df_mobile_subscription %>%
             filter(Code != "") %>% 
             filter(Year == 2019) %>%
             select(Country, Code, Mobile_Cellular_Subscription_Per_100_People)%>%
             arrange(desc(Mobile_Cellular_Subscription_Per_100_People)) %>% top_n(5), 
             caption = "Countries With Maximum Mobile Subscription in 2019")

knitr::kable(df_broadband_penetration %>%
             filter(Code != "") %>% 
             filter(Year == 2019) %>%
             select(Country, Code, Fixed_Broadband_Subscription_Per_100_People)%>%
             arrange(desc(Fixed_Broadband_Subscription_Per_100_People)) %>% top_n(5),
             caption = "Countries With Maximum Fixed Broadband Subscription in 2019")

knitr::kable(df_internet_users %>%
             filter(Code != "") %>% 
             filter(Year == 2016) %>%
             select(Country, Code, Number_Of_Internet_Users)%>%
             arrange(desc(Number_Of_Internet_Users)) %>% top_n(5),
             caption = "Countries With Maximum Internet users in 2016")

knitr::kable(head(df_political_indexes %>%
             filter(Year == "2006") %>%
             select(Country, Democracy_Index) %>%
             arrange(desc(Democracy_Index)), 5),
             caption = "Countries With Maximum Democracy Indexes in 2006")

knitr::kable(head(df_political_indexes %>%
             filter(Year == "2006") %>%
             select(Country, Democracy_Index) %>%
             arrange(Democracy_Index), 5),
             caption = "Countries With Minimum Democracy Indexes in 2006")

knitr::kable(head(df_political_indexes %>% 
             filter(Year == "2020") %>%
             select(Country, Democracy_Index) %>%
             arrange(desc(Democracy_Index)), 5),
             caption = "Countries With Maximum Democracy Indexes in 2020")

knitr::kable(head(df_political_indexes %>%
             filter(Year == "2020") %>%
             select(Country, Democracy_Index) %>%
             arrange(Democracy_Index), 5),
             caption = "Countries With Minimum Democracy Indexes in 2020")



options(repr.plot.width = 12, repr.plot.height = 8)
#Comparing the growth of Mobile Subscriptions from 2009 to 2019

#Removing the regions from the Country column to obtain only the countries
df_mobile_subscription_onlycountries <- df_mobile_subscription %>% filter(Code !=  "")

#The records for the year 2009
df_mobile_subscription_onlycountries_2009 <- df_mobile_subscription_onlycountries %>% filter(Year == 2009)

#The records for the year 2019
df_mobile_subscription_onlycountries_2019 <- df_mobile_subscription_onlycountries %>% filter(Year == 2019)

#Joining the tables to get the records for only 2009 and 2019
df_mobile_subscription_combined_2009_2019 <- inner_join(df_mobile_subscription_onlycountries_2009,
                                                        df_mobile_subscription_onlycountries_2019, 
                                                        by = c("Country", "Code"))

df_mobile_subscription_combined_change <- mutate(df_mobile_subscription_combined_2009_2019, 
                                                 Percentage_Change = ((Mobile_Cellular_Subscription_Per_100_People.y - Mobile_Cellular_Subscription_Per_100_People.x)))

#Figuring out the countries which have improved the most in a decade
df_mobile_subscription_combined_change_ordered <- df_mobile_subscription_combined_change %>%
                                                  arrange(desc(df_mobile_subscription_combined_change$Percentage_Change))

#Obtaining the top 10 countries to have improved the most
df_mobile_subscription_combined_change_ordered_top10 <- df_mobile_subscription_combined_change_ordered %>% head(20)

# renaming the columns
colnames(df_mobile_subscription_combined_change_ordered_top10)[4] <- "Mobile_Cellular_Subscription_Per_100_People_2009"
colnames(df_mobile_subscription_combined_change_ordered_top10)[6] <- "Mobile_Cellular_Subscription_Per_100_People_2019"

# pivoting the 2009 and 2019 columns 
df_mobile_subscription_for_plot <- pivot_longer(df_mobile_subscription_combined_change_ordered_top10,
                                                cols =c("Mobile_Cellular_Subscription_Per_100_People_2019",
                                                        "Mobile_Cellular_Subscription_Per_100_People_2009"),
                                                names_to = "Mobile_cellular_Subscriptions_per_100_people", 
                                                values_to = "Subscriptions_per_100_people")

# bar plot to visualize the top-10 countries
ggplot(df_mobile_subscription_for_plot, 
       aes(fill = Mobile_cellular_Subscriptions_per_100_people,
           x = Country,
           y = Subscriptions_per_100_people,
           group = Mobile_cellular_Subscriptions_per_100_people)) + 
  geom_bar(position = "dodge",
           stat = "identity") + 
  ggtitle("Countries with Highest Change of Mobile subscriptions per 100 people") + 
  theme(legend.box.background = element_rect(color = "black",
                                             size=0.5),
        legend.box.margin = margin(25, 6, 6, 6)) + 
  theme(axis.text.x = element_text(size=10, 
                                   angle=90,
                                   hjust = 0.95,
                                   vjust = 0.2)) 

# figuring out the countries which have improved the lowest
df_mobile_subscription_combined_change_ordered_asc <- df_mobile_subscription_combined_change %>%
                                                      arrange(df_mobile_subscription_combined_change$Percentage_Change)

df_mobile_subscription_combined_change_ordered_bottom10 <- df_mobile_subscription_combined_change_ordered_asc %>% head(20)

colnames(df_mobile_subscription_combined_change_ordered_bottom10)[4] <- "Mobile_Cellular_Subscription_Per_100_People_2009"
colnames(df_mobile_subscription_combined_change_ordered_bottom10)[6] <- "Mobile_Cellular_Subscription_Per_100_People_2019"

df_mobile_subscription_for_plot_bottom10 <- pivot_longer(df_mobile_subscription_combined_change_ordered_bottom10,
                                                         cols = c("Mobile_Cellular_Subscription_Per_100_People_2019",
                                                                  "Mobile_Cellular_Subscription_Per_100_People_2009"),
                                                         names_to = "Mobile_cellular_Subscriptions_per_100_people", 
                                                         values_to = "Subscriptions_per_100_people")

# plotting a stacked Bar-Plot to visualize the bottom 10 countries
ggplot(df_mobile_subscription_for_plot_bottom10,
       aes(fill = Mobile_cellular_Subscriptions_per_100_people,
           x = Subscriptions_per_100_people,
           y = Country,
           group = Mobile_cellular_Subscriptions_per_100_people)) +
  geom_bar(position = "stack",
           stat="identity") + 
  ggtitle("Countries with Lowest Mobile subscriptions per 100 people") +
  theme(legend.box.background = element_rect(color="black", 
                                             size=0.5),
        legend.box.margin = margin(25, 6, 6, 6))



# comparing the growth of broadband from 2009 to 2019

# removing the regions from the Entity column to obtain only the countries
df_broadband_penetration_onlycountries <- df_broadband_penetration %>% filter(Code !=  "")

# the records for the year 2009
df_broadband_penetration_onlycountries_2009 <- df_broadband_penetration_onlycountries %>% filter(Year == 2009)

# the records for the year 2019
df_broadband_penetration_onlycountries_2019 <- df_broadband_penetration_onlycountries %>% filter(Year == 2019)

# joining the tables to get the records for only 2009 and 2019
df_broadband_penetration_combined_2009_2019 <- inner_join(df_broadband_penetration_onlycountries_2009,
                                                          df_broadband_penetration_onlycountries_2019, 
                                                          by = c("Country", "Code"))

df_broadband_penetration_combined_change <- mutate(df_broadband_penetration_combined_2009_2019, 
                                                   Percentage_Change = ((Fixed_Broadband_Subscription_Per_100_People.y - Fixed_Broadband_Subscription_Per_100_People.x)))

# figuring out the countries which have improved the most in a decade
df_broadband_penetration_combined_change_ordered <- df_broadband_penetration_combined_change %>%
                                                    arrange(desc(df_broadband_penetration_combined_change$Percentage_Change))

# obtaining the top 10 countries to have improved the most
df_broadband_penetration_combined_change_ordered_top10 <- df_broadband_penetration_combined_change_ordered %>% head(20)

# renaming the columns
colnames(df_broadband_penetration_combined_change_ordered_top10)[4] <- "Fixed_Broadband_Subscription_Per_100_People_2009"
colnames(df_broadband_penetration_combined_change_ordered_top10)[6] <- "Fixed_Broadband_Subscription_Per_100_People_2019"

# pivoting the 2009 and 2019 columns 
df_broadband_penetration_for_plot <- pivot_longer(df_broadband_penetration_combined_change_ordered_top10,
                                                  cols = c("Fixed_Broadband_Subscription_Per_100_People_2019",
                                                           "Fixed_Broadband_Subscription_Per_100_People_2009"),
                                                  names_to = "Fixed_Broadband_Subscriptions_per_100_people", 
                                                  values_to = "Subscriptions_per_100_people")

# bar plot to visualize the top-10 countries
ggplot(df_broadband_penetration_for_plot,
       aes(fill = Fixed_Broadband_Subscriptions_per_100_people,
           x = Country,
           y = Subscriptions_per_100_people,
           group = Fixed_Broadband_Subscriptions_per_100_people)) +
  geom_bar(position = "dodge",
           stat = "identity") + 
  ggtitle("Countries with Highest Change of Broadband subscriptions per 100 people") +
  theme(legend.box.background = element_rect(color = "black",
                                             size = 0.5),
        legend.box.margin = margin(25, 6, 6, 6)) + 
  theme(axis.text.x = element_text(size = 10, 
                                   angle = 90,
                                   hjust = 0.95,
                                   vjust = 0.2)) 

# figuring out the countries which have improved the lowest
df_broadband_penetration_combined_change_ordered_asc <- df_broadband_penetration_combined_change %>%
                                                        arrange(df_broadband_penetration_combined_change$Percentage_Change)

df_broadband_penetration_combined_change_ordered_bottom10 <- df_broadband_penetration_combined_change_ordered_asc %>% head(20)

colnames(df_broadband_penetration_combined_change_ordered_bottom10)[4] <- "Fixed_Broadband_Subscription_Per_100_People_2009"
colnames(df_broadband_penetration_combined_change_ordered_bottom10)[6] <- "Fixed_Broadband_Subscription_Per_100_People_2019"

df_broadband_penetration_for_plot_bottom10 <- pivot_longer(df_broadband_penetration_combined_change_ordered_bottom10,
                                                           cols = c("Fixed_Broadband_Subscription_Per_100_People_2019",
                                                                    "Fixed_Broadband_Subscription_Per_100_People_2009"),
                                                           names_to = "Fixed_Broadband_Subscriptions_per_100_people", 
                                                           values_to = "Subscriptions_per_100_people")

# plotting a stacked Bar-Plot to visualize the bottom 10 countries
ggplot(df_broadband_penetration_for_plot_bottom10,
       aes(fill = Fixed_Broadband_Subscriptions_per_100_people,
           x = Subscriptions_per_100_people,
           y = Country,
           group = Fixed_Broadband_Subscriptions_per_100_people))+
  geom_bar(position = "stack",
           stat = "identity") + 
  ggtitle("Countries with Lowest Broadband subscriptions per 100 people") +
  theme(legend.box.background = element_rect(color = "black", 
                                             size = 0.5),
        legend.box.margin = margin(25, 6, 6, 6))


options(repr.plot.width = 25, repr.plot.height = 5)
# heatmap of df_mobile_subscription for 2009

# importing the world map
world_map <- map_data("world")

# merging the world map with the mobile subscriptions by country dataset
world_map = merge(world_map,
                  df_mobile_subscription_onlycountries_2009,
                  by.x = "region",
                  by.y = "Country")

# arranging the table on the basis of Mobile subscriptions per 100 people
df_mobile_subscription_onlycountries_top10 <- df_mobile_subscription_onlycountries %>% arrange(desc(Mobile_Cellular_Subscription_Per_100_People)) %>% head(10)

# plotting the heatmap of the world for the year 2009
ggplot(world_map, aes(map_id = region, 
                      fill = Mobile_Cellular_Subscription_Per_100_People)) + 
  geom_map(map = world_map, 
             size = 0.1) + 
  scale_fill_gradient(low = "#fff7bc", 
                      high = "#cc4c02", 
                      name = "Mobile Cellular Subscriptions Per 100 People") + 
  expand_limits(x = world_map$long, 
                y = world_map$lat) + 
  ggtitle("Mobile Cellular Subscriptions Per 100 People Throughout The World - 2009") +
  theme(legend.box.background = element_rect(color = "black", 
                                             size = 0.5),
        legend.box.margin = margin(25, 6, 6, 6))

# heatmap of df_mobile_subscription for 2019
world_map <- map_data("world")
world_map = merge(world_map,
                  df_mobile_subscription_onlycountries_2019,
                  by.x = "region",
                  by.y = "Country")

df_mobile_subscription_onlycountries_top10 <- df_mobile_subscription_onlycountries %>% 
                                              arrange(desc(Mobile_Cellular_Subscription_Per_100_People)) %>% 
                                              head(10)

# plotting the heatmap of the world for the year 2019
ggplot(world_map, 
       aes(map_id = region, 
           fill = Mobile_Cellular_Subscription_Per_100_People)) + 
  geom_map(map = world_map, 
           size = 0.25) + 
  scale_fill_gradient(low = "#fff7bc", 
                      high = "#cc4c02", 
                      name = "Mobile Cellular Subscriptions Per 100 People") + 
  expand_limits(x = world_map$long, 
                y = world_map$lat) + 
  ggtitle("Mobile Cellular Subscriptions Per 100 People Throughout The World - 2019") +
  theme(legend.box.background = element_rect(color = "black", 
                                             size = 0.5),
        legend.box.margin = margin(25, 6, 6, 6))


knitr::opts_chunk$set(warning = FALSE, message = FALSE) 

# heatmap of df_broadband_penetration for 2009

# importing the world map
world_map <- map_data("world")

# merging the world map with the broadband penetration by country dataset
world_map = merge(world_map,df_broadband_penetration_onlycountries_2009,
                  by.x = "region",
                  by.y = "Country")

# arranging the table on the basis of Fixed Broadband subscriptions per 100 people
df_broadband_penetration_onlycountries_top10 <- df_broadband_penetration_onlycountries %>%
                                                arrange(desc(Fixed_Broadband_Subscription_Per_100_People)) %>% head(10)

# plotting the heatmap of the world for the year 2009
ggplot(world_map, 
       aes(map_id = region, 
           fill = Fixed_Broadband_Subscription_Per_100_People)) + 
  geom_map(map = world_map, 
           size = 0.1) + 
  scale_fill_gradient(low = "#fff7bc", 
                      high = "#cc4c02", 
                      name = "Fixed Broadband Subscriptions Per 100 People") + 
  expand_limits(x = world_map$long, 
                y = world_map$lat) + 
  ggtitle("Broadband Subscriptions Per 100 People Throughout the World – 2009") +
  theme(legend.box.background = element_rect(color = "black", 
                                             size = 0.5),
        legend.box.margin = margin(25, 6, 6, 6))

# heatmap of df_broadband_penetration for 2019
world_map <- map_data("world")
world_map = merge(world_map,df_broadband_penetration_onlycountries_2019,
                  by.x = "region",
                  by.y = "Country")
df_broadband_penetration_onlycountries_top10 <- df_broadband_penetration_onlycountries %>%
                                                arrange(desc(Fixed_Broadband_Subscription_Per_100_People)) %>% head(10)

# plotting the heatmap of the world for the year 2019
ggplot(world_map, 
       aes(map_id = region, 
           fill = Fixed_Broadband_Subscription_Per_100_People)) + 
  geom_map(map = world_map, 
           size = 0.1) + 
  scale_fill_gradient(low = "#fff7bc", 
                      high = "#cc4c02", 
                      name = "Fixed Broadband Subscriptions Per 100 People") +
  expand_limits(x = world_map$long, 
                y = world_map$lat) + 
  ggtitle("Broadband Subscriptions Per 100 People Throughout the World – 2019") +
  theme(legend.box.background = element_rect(color = "black", 
                                             size=0.5),
        legend.box.margin = margin(6, 6, 6, 6))



options(repr.plot.width = 12, repr.plot.height = 8)

# filtering the Broadband Penetration Dataset for the year 2019
df_broadband_penetration_2019 <- df_broadband_penetration %>% filter(Year == 2019)

#Filtering the Mobile Cellular Dataset for the year 2019
df_mobile_subscription_2019 <- df_mobile_subscription %>% filter(Code !=  "") %>% filter(Year == 2019)

# df_broadband_penetration and Continents
# joining the Broadband Penetration and Continents dataset using Country as the primary Key
df_broadband_penetration_continents <- inner_join(df_broadband_penetration_2019,
                                                  df_continents,
                                                  by = "Country")

# grouping the dataset on the basis of region
df_broadband_penetration_continents_grouped <- df_broadband_penetration_continents %>% 
                                               group_by(Region) %>% 
                                              summarise(Mean_Broadband_Subscriptions_Per_100_people =
                                                          mean(Fixed_Broadband_Subscription_Per_100_People))

head(df_broadband_penetration_continents_grouped)

# plotting the Pie-Chart for df_broadband_penetration for different regions
ggplot(df_broadband_penetration_continents_grouped,
       aes(x = "",
           y = Mean_Broadband_Subscriptions_Per_100_people,
           fill = Region)) + 
  geom_bar(stat = "identity", 
           width=1) +
  coord_polar("y", 
              start=0) + 
  theme_void() +
  ggtitle("Mean Broadband Subscriptions Per 100 People \nAcross different Regions of the World") + 
  theme(plot.title = element_text(hjust = 0.3,
                                  size = 15,
                                  face = "bold")) +
  guides(fill  = guide_legend(reverse = TRUE)) +
  scale_fill_brewer(name = "Regions") + 
  theme(legend.text = element_text(size=10),
        legend.title = element_text(hjust = 0.5,
                                    size = 10,
                                    face = "bold")) + 
    geom_text(aes(label = paste0(round(df_broadband_penetration_continents_grouped$Mean_Broadband_Subscriptions_Per_100_people,
                                       digits = 2))),
              position = position_stack(vjust = 0.5),
              size = 3,
              face = "bold") + 
    theme(legend.box.background = element_rect(color = "black", 
                                               size = 1),
          legend.box.margin = margin(25, 6, 6, 6))

# df_mobile_subscription and Democracy of Countries
df_mobile_subscription_continents <- inner_join(df_mobile_subscription_2019,df_continents,by="Country")

df_mobile_subscription_continents_grouped <- df_mobile_subscription_continents %>% 
                                             group_by(Region) %>% 
                                             summarise(Mean_cellular_Subscriptions_Per_100_people = 
                                                         mean(Mobile_Cellular_Subscription_Per_100_People))

head(df_mobile_subscription_continents_grouped)

# plotting the Pie-Chart for df_mobile_subscription for different regions
ggplot(df_mobile_subscription_continents_grouped,
       aes(x = "",
           y = Mean_cellular_Subscriptions_Per_100_people,
           fill = Region)) + 
  geom_bar(stat = "identity", 
           width = 1) +
  coord_polar("y", 
              start = 0) + 
  theme_void() + 
  ggtitle("Mean Cellular Subscriptions Per 100 People \nAcross different Regions of the World") + 
  theme(plot.title = element_text(hjust = 0.3,
                                  size = 15,
                                  face = "bold")) +
  guides(fill  = guide_legend(reverse = TRUE))+
  scale_fill_brewer(name = "Regions") + 
  theme(legend.text = element_text(size=10),
        legend.title = element_text(hjust = 0.5,
                                    size = 10,
                                    face = "bold")) + 
    geom_text(aes(label = paste0(round(df_mobile_subscription_continents_grouped$Mean_cellular_Subscriptions_Per_100_people,
                                       digits = 2))),
              position = position_stack(vjust = 0.5),
              size = 3,
              face = "bold") + 
    theme(legend.box.background = element_rect(color="black", size=1),
          legend.box.margin = margin(25, 6, 6, 6))


# options(repr.plot.width = 18, repr.plot.height = 20)

# joining data set
df_political_index_broadband_penetration <- merge(x = df_broadband_penetration, y = df_political_indexes, by = c("Country", "Year"))


# Democracy Index VS Fixed Broadband Subscription Per 100 People
df_democracy_index_broadband_penetration <- df_political_index_broadband_penetration %>% select(Country, Year, Democracy_Index, Fixed_Broadband_Subscription_Per_100_People)

plt_demo_indx_2006 <- df_democracy_index_broadband_penetration %>% filter(Year == "2006") %>%
  ggplot(aes(x = Democracy_Index, y = Fixed_Broadband_Subscription_Per_100_People)) + 
  geom_point(color = "deepskyblue") + 
  xlim(1,11) +
  ylim(0,50) +
  scale_x_continuous(breaks = c(2,4,6,8,10)) +
  theme_minimal() + 
  geom_smooth(method = "lm", formula = y ~ poly(x, 3), se = FALSE, color = "deeppink") +
  ggtitle("Democracy Index VS Fixed Broadband Subscription Per 100 People \n(2006)") + 
  labs(x = "Democracy Index", y = "Fixed Broadbad Subscription \nPer 100 People") + 
  theme(plot.title = element_text(size = 10, hjust = 0.5, face="italic"), legend.position = "none", axis.title = element_text(size = 8), axis.text = element_text(size = 5)) +
  geom_vline(xintercept = 1) +
  geom_hline(yintercept = 0) +
  annotate("rect", xmin=7.5, xmax=10, ymin=20, ymax= 32, fill=NA, color="grey40") +
  annotate("text", color="grey40", x=5.6, y=30, label="Countries with high democracy index \nand boradband subscriptions", size = 4, face = "bold")

plt_demo_indx_2008 <- df_democracy_index_broadband_penetration %>% filter(Year == "2008") %>%
  ggplot(aes(x = Democracy_Index, y = Fixed_Broadband_Subscription_Per_100_People)) + 
  geom_point(color = "deepskyblue") + 
  xlim(1,11) +
  ylim(0,50) +
  scale_x_continuous(breaks = c(2,4,6,8,10)) +
  theme_minimal() + 
  geom_smooth(method = "lm", formula = y ~ poly(x, 3), se = FALSE, color = "deeppink") +
  ggtitle("Democracy Index VS Fixed Broadband Subscription Per 100 People \n(2008)") + 
  labs(x = "Democracy Index", y = "Fixed Broadbad Subscription \nPer 100 People") + 
  theme(plot.title = element_text(size = 10, hjust = 0.5, face="italic"), legend.position = "none", axis.title = element_text(size = 8), axis.text = element_text(size = 5)) +      
  geom_vline(xintercept = 1) +
  geom_hline(yintercept = 0)


plt_demo_indx_2010 <- df_democracy_index_broadband_penetration %>% filter(Year == "2010") %>%
  ggplot(aes(x = Democracy_Index, y = Fixed_Broadband_Subscription_Per_100_People)) + 
  geom_point(color = "deepskyblue") + 
  xlim(1,11) +
  ylim(0,50) +
  scale_x_continuous(breaks = c(2,4,6,8,10)) +
  theme_minimal() + 
  geom_smooth(method = "lm", formula = y ~ poly(x, 3), se = FALSE, color = "deeppink") +
  ggtitle("Democracy Index VS Fixed Broadband Subscription Per 100 People \n(2010)") + 
  labs(x = "Democracy Index", y = "Fixed Broadbad Subscription \nPer 100 People") + 
  theme(plot.title = element_text(size = 10, hjust = 0.5, face="italic"), legend.position = "none", axis.title = element_text(size = 8), axis.text = element_text(size = 5)) +
  geom_vline(xintercept = 1) +
  geom_hline(yintercept = 0)

plt_demo_indx_2012 <- df_democracy_index_broadband_penetration %>% filter(Year == "2012") %>%
  ggplot(aes(x = Democracy_Index, y = Fixed_Broadband_Subscription_Per_100_People)) + 
  geom_point(color = "deepskyblue") +
  xlim(1,11) +
  ylim(0,50) +
  scale_x_continuous(breaks = c(2,4,6,8,10)) +
  theme_minimal() + 
  geom_smooth(method = "lm", formula = y ~ poly(x, 3), se = FALSE, color = "deeppink") +
  ggtitle("Democracy Index VS Fixed Broadband Subscription Per 100 People \n(2012)") + 
  labs(x = "Democracy Index", y = "Fixed Broadbad Subscription \nPer 100 People") + 
  theme(plot.title = element_text(size = 10, hjust = 0.5, face="italic"), legend.position = "none", axis.title = element_text(size = 8), axis.text = element_text(size = 5)) +
  geom_vline(xintercept = 1) +
  geom_hline(yintercept = 0)

plt_demo_indx_2014 <- df_democracy_index_broadband_penetration %>% filter(Year == "2014") %>%
  ggplot(aes(x = Democracy_Index, y = Fixed_Broadband_Subscription_Per_100_People)) + 
  geom_point(color = "deepskyblue") + 
  xlim(1,11) +
  ylim(0,50) +
  scale_x_continuous(breaks = c(2,4,6,8,10)) +
  theme_minimal() + 
  geom_smooth(method = "lm", formula = y ~ poly(x, 3), se = FALSE, color = "deeppink") +
  ggtitle("Democracy Index VS Fixed Broadband Subscription Per 100 People \n(2014)") + 
  labs(x = "Democracy Index", y = "Fixed Broadbad Subscription \nPer 100 People") + 
  theme(plot.title = element_text(size = 10, hjust = 0.5, face="italic"), legend.position = "none", axis.title = element_text(size = 8), axis.text = element_text(size = 5)) +
  geom_vline(xintercept = 1) +
  geom_hline(yintercept = 0)

plt_demo_indx_2018 <- df_democracy_index_broadband_penetration %>% filter(Year == "2018") %>%
  ggplot(aes(x = Democracy_Index, y = Fixed_Broadband_Subscription_Per_100_People)) + 
  geom_point(color = "deepskyblue") + 
  xlim(1,11) +
  ylim(0,50) +
  scale_x_continuous(breaks = c(2,4,6,8,10)) +
  theme_minimal() + 
  geom_smooth(method = "lm", formula = y ~ poly(x, 3), se = FALSE, color = "deeppink") +
  ggtitle("Democracy Index VS Fixed Broadband Subscription Per 100 People \n(2018)") + 
  labs(x = "Democracy Index", y = "Fixed Broadbad Subscription \nPer 100 People") + 
  theme(plot.title = element_text(size = 10, hjust = 0.5, face="italic"), legend.position = "none", axis.title = element_text(size = 8), axis.text = element_text(size = 5)) +
  geom_vline(xintercept = 1) +
  geom_hline(yintercept = 0)

grid.arrange(plt_demo_indx_2006, plt_demo_indx_2008, plt_demo_indx_2010, plt_demo_indx_2012, plt_demo_indx_2014, plt_demo_indx_2018, nrow = 3)



# ![](https://i.ibb.co/XjNzyvR/a.gif)



# calculating correlation coefficient 
print(paste("Correlation Coefficiant:", 
            cor(df_democracy_index_broadband_penetration$Democracy_Index, df_democracy_index_broadband_penetration$Fixed_Broadband_Subscription_Per_100_People)))



# Functioning of Government VS Fixed Broadband Subscription Per 100 People
df_func_govt_broadband_penetration <- df_political_index_broadband_penetration %>% select(Country, Year, Functioning_Of_Government, Fixed_Broadband_Subscription_Per_100_People)

plt_func_govt_2006 <- df_func_govt_broadband_penetration %>% filter(Year == "2006") %>%
  ggplot(aes(x = Functioning_Of_Government, y = Fixed_Broadband_Subscription_Per_100_People)) + 
  geom_point(color = "limegreen") + 
  xlim(0,100) +
  ylim(0,50) +
  theme_minimal() + 
  geom_smooth(method = "lm", formula = y ~ poly(x, 3), se = FALSE, color = "palevioletred") +
  ggtitle("Functioning Of Government VS Fixed Broadband Subscription \nPer 100 People (2006)") + 
  labs(x = "Functioning Of Government", y = "Fixed Broadbad Subscription \nPer 100 People") + 
  theme(plot.title = element_text(size = 10, hjust = 0.5, face="italic"), legend.position = "none", axis.title = element_text(size = 8), axis.text = element_text(size = 5)) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0)


plt_func_govt_2008 <- df_func_govt_broadband_penetration %>% filter(Year == "2008") %>%
  ggplot(aes(x = Functioning_Of_Government, y = Fixed_Broadband_Subscription_Per_100_People)) + 
  geom_point(color = "limegreen") + 
  xlim(0,100) +
  ylim(0,50) +
  theme_minimal() + 
  geom_smooth(method = "lm", formula = y ~ poly(x, 3), se = FALSE, color = "palevioletred") +
  ggtitle("Functioning Of Government VS Fixed Broadband Subscription \nPer 100 People (2008)") + 
  labs(x = "Functioning Of Government", y = "Fixed Broadbad Subscription \nPer 100 People") + 
  theme(plot.title = element_text(size = 10, hjust = 0.5, face="italic"), legend.position = "none", axis.title = element_text(size = 8), axis.text = element_text(size = 5)) +       
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0)


plt_func_govt_2010 <- df_func_govt_broadband_penetration %>% filter(Year == "2010") %>%
  ggplot(aes(x = Functioning_Of_Government, y = Fixed_Broadband_Subscription_Per_100_People)) + 
  geom_point(color = "limegreen") + 
  xlim(0,100) +
  ylim(0,50) +
  theme_minimal() + 
  geom_smooth(method = "lm", formula = y ~ poly(x, 3), se = FALSE, color = "palevioletred") +
  ggtitle("Functioning Of Government VS Fixed Broadband Subscription \nPer 100 People (2010)") + 
  labs(x = "Functioning Of Government", y = "Fixed Broadbad Subscription \nPer 100 People") + 
  theme(plot.title = element_text(size = 10, hjust = 0.5, face="italic"), legend.position = "none", axis.title = element_text(size = 8), axis.text = element_text(size = 5)) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0)


plt_func_govt_2012 <- df_func_govt_broadband_penetration %>% filter(Year == "2012") %>%
  ggplot(aes(x = Functioning_Of_Government, y = Fixed_Broadband_Subscription_Per_100_People)) + 
  geom_point(color = "limegreen") + 
  xlim(0,100) +
  ylim(0,50) +
  theme_minimal() + 
  geom_smooth(method = "lm", formula = y ~ poly(x, 3), se = FALSE, color = "palevioletred") +
  ggtitle("Functioning Of Government VS Fixed Broadband Subscription \nPer 100 People (2012)") + 
  labs(x = "Functioning Of Government", y = "Fixed Broadbad Subscription \nPer 100 People") + 
  theme(plot.title = element_text(size = 10, hjust = 0.5, face="italic"), legend.position = "none", axis.title = element_text(size = 8), axis.text = element_text(size = 5)) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0)


plt_func_govt_2014 <- df_func_govt_broadband_penetration %>% filter(Year == "2014") %>%
  ggplot(aes(x = Functioning_Of_Government, y = Fixed_Broadband_Subscription_Per_100_People)) + 
  geom_point(color = "limegreen") + 
  xlim(0,100) +
  ylim(0,50) +
  theme_minimal() + 
  geom_smooth(method = "lm", formula = y ~ poly(x, 3), se = FALSE, color = "palevioletred") +
  ggtitle("Functioning Of Government VS Fixed Broadband Subscription \nPer 100 People (2014)") + 
  labs(x = "Functioning Of Government", y = "Fixed Broadbad Subscription \nPer 100 People") + 
  theme(plot.title = element_text(size = 10, hjust = 0.5, face="italic"), legend.position = "none", axis.title = element_text(size = 8), axis.text = element_text(size = 5)) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0)

plt_func_govt_2018 <- df_func_govt_broadband_penetration %>% filter(Year == "2018") %>%
  ggplot(aes(x = Functioning_Of_Government, y = Fixed_Broadband_Subscription_Per_100_People)) + 
  geom_point(color = "limegreen") + 
  xlim(0,100) +
  ylim(0,50) +
  theme_minimal() + 
  geom_smooth(method = "lm", formula = y ~ poly(x, 3), se = FALSE, color = "palevioletred") +
  ggtitle("Functioning Of Government VS Fixed Broadband Subscription \nPer 100 People (2018)") + 
  labs(x = "Functioning Of Government", y = "Fixed Broadbad Subscription \nPer 100 People") + 
  theme(plot.title = element_text(size = 10, hjust = 0.5, face="italic"), legend.position = "none", axis.title = element_text(size = 8), axis.text = element_text(size = 5)) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0)


grid.arrange(plt_func_govt_2006, plt_func_govt_2008, plt_func_govt_2010, plt_func_govt_2012, plt_func_govt_2014, plt_func_govt_2018, nrow = 3)


#![](https://i.ibb.co/L1kcRjY/b.gif)




# calculating correlation coefficient
print(paste("Correlation Coefficiant:", 
            cor(df_political_index_broadband_penetration$Functioning_Of_Government, df_democracy_index_broadband_penetration$Fixed_Broadband_Subscription_Per_100_People)))


