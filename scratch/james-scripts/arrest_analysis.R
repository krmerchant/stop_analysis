pacman::p_load(dplyr,tidycensus,stringi,stringr,reshape2,ggsci,tidyr,ggplot2,mapview,ggmap,RColorBrewer)

arrests = read.csv("MPD_Adult_Arrests.csv") %>% mutate(OC = grepl("Open Container",DESCRIPTION))

a = arrests %>% filter(OC) %>% count(DESCRIPTION)

oc_by_year = arrests %>% filter(OC) %>% group_by(YEAR) %>% summarize(n_OC = sum(OC))
oc_by_year %>% ggplot(aes(x = YEAR,y = n_OC))+ 
  geom_line()+
  scale_x_continuous(breaks=min(oc_by_year$YEAR):max(oc_by_year$YEAR))+
  geom_point()+
  xlab("")+
  ylab("# of Open Container Arrests in DC")+
  theme_minimal(base_size = 20)

arrests_by_year = arrests %>% group_by(YEAR) %>% summarize(n_a = n(),
                                                           percent_oc = sum(OC)/n_a)
arrests_by_year %>% ggplot(aes(x = YEAR,y = n_a))+ 
  geom_line()+
  scale_x_continuous(breaks=min(oc_by_year$YEAR):max(oc_by_year$YEAR))+
  geom_point()+
  xlab("")+
  ylab("# of Total Arrests in DC")+
  theme_minimal(base_size = 20)+
  ylim(0,35000)

arrests_by_year %>% ggplot(aes(x = YEAR,y = percent_oc))+ 
  geom_line()+
  scale_x_continuous(breaks=min(oc_by_year$YEAR):max(oc_by_year$YEAR))+
  geom_point()+
  xlab("")+
  ylab("% of Arrests that are Open Container in DC")+
  theme_minimal(base_size = 20)+
  scale_y_continuous(labels = scales::percent)


oc_by_race = arrests %>% filter(OC) %>% group_by(RACE) %>% summarize(n_OC = sum(OC)) %>% ungroup() %>%
  mutate(prop_OC = n_OC/sum(n_OC))

oc_by_race %>% ggplot(aes(x = RACE,y = prop_OC))+
  geom_bar(stat = "identity", position = "dodge")+
  xlab("")+
  ylab("Proportion of Open Container Arrests")+
  theme_minimal(base_size = 20)


oc_by_ethnicity = arrests %>% filter(OC) %>% group_by(ETHNICITY) %>% summarize(n_OC = sum(OC)) %>% ungroup() %>%
  mutate(prop_OC = n_OC/sum(n_OC))

oc_by_ethnicity %>% ggplot(aes(x = ETHNICITY,y = prop_OC))+
  geom_bar(stat = "identity", position = "dodge")+
  xlab("")+
  ylab("Proportion of Open Container Arrests")+
  theme_minimal(base_size = 20)

oc_by_district = arrests %>% filter(OC) %>% group_by(DEFENDANT_DISTRICT) %>% summarize(n_OC = sum(OC)) %>% ungroup() %>%
  mutate(prop_OC = n_OC/sum(n_OC))

arrests = arrests %>% mutate(day_of_year = substr(DATE_,1,10),
                             general_day = substr(DATE_,6,10),
                             month = substr(DATE_,6,7))

day_of_year_count = arrests %>% filter(OC) %>% count(day_of_year)
general_day_count = arrests %>% filter(OC) %>% count(general_day)

arrests_2025 = arrests %>% filter(YEAR >= 2024) %>% group_by(month,YEAR) %>% summarize(n = n())

arrests_2025 %>% ggplot(aes(x = month,y= n, fill = as.factor(YEAR) ))+
  geom_bar(stat = "identity",position = "dodge")+
  scale_fill_nejm(name = "Year")+
  theme_minimal(base_size = 20)+
  xlab("Month")+
  ylab("Number of Open Container Arrests")

month_breakdown = arrests %>% 
  #filter(OC) %>%
  group_by(month) %>% summarize(n = n()) %>%
  ungroup() %>% mutate(prop = n/sum(n))

month_breakdown %>% ggplot(aes(x = month, y = prop))+
  geom_bar(stat = "identity")+
  geom_hline(yintercept = 1/12, color = "red")+
  xlab("Month")+
  ylab("Proportion of Open Container Arrests")+
  theme_minimal(base_size = 20)

api_key = "90adec3d-2af9-4a04-a061-f0791ce183a0"
register_stadiamaps(api_key) 


map_bounds = c(left = -77.06, bottom = 38.85,right = -76.97,top=38.95)
ggmap(get_stadiamap(bbox = map_bounds, zoom = 14, maptype = "stamen_toner_lite"))+
  stat_density2d(data = arrests %>% filter(OC),
                 aes(x = ARREST_LONGITUDE, y = ARREST_LATITUDE,fill = ..level.., alpha = ..level..), 
                 geom = "polygon")+
  scale_fill_gradientn(colours=rev(brewer.pal(7, "Spectral"))) + 
  scale_alpha(range = c(0, 0.5), guide = FALSE)+
  ggtitle("Open Container Arrest Heat Map 2013-2025")

map_bounds = c(left = -77.05, bottom = 38.85,right = -76.95,top=38.95)
ggmap(get_stadiamap(bbox = map_bounds, zoom = 13, maptype = "stamen_toner_lite"))+
  stat_density2d(data = arrests %>% filter(OC & YEAR == 2025),
                 aes(x = ARREST_LONGITUDE, y = ARREST_LATITUDE,fill = ..level.., alpha = ..level..), 
                 geom = "polygon")+
  scale_fill_gradientn(colours=rev(brewer.pal(7, "Spectral"))) + 
  scale_alpha(range = c(0, 0.5), guide = FALSE)+
  ggtitle("Open Container Arrest Heat Map 2025")
