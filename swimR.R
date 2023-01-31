#https://pilgrim.netlify.app/

install.packages("flextable")
install.packages("SwimmeR")
devtools::install_github("gpilgrim2670/JumpeR")
install.packages("transformr")

library(SwimmeR)
library(tidyverse)
library(JumpeR)
library(flextable)
library(gganimate)
library(transformr)
library(ggridges)
file <- "jrnats22.pdf"


#can import df_long instead of reading pdf - import the csv, then start at line 68
df <- file %>% 
  SwimmeR::read_results() %>% 
  swim_parse(splits = TRUE, split_length = 50)

#df2 <- df - why?
cols <- c(3,5,6, 11:41) #might need to adjust
df[, cols] <- lapply(df[, cols], as.numeric)

df$ave_split <- rowMeans(df[c(11:26)])

hist(df$Split_50)



df_long <- gather(df, lap, split_time, Split_50:Split_1500, factor_key=TRUE) #make a long dataframe

df_long %>% ggplot(aes(x = split_time, y = lap)) + geom_boxplot()

df_long$lapnum <- case_when(df_long$lap == "Split_50" ~ 1,
                            df_long$lap == "Split_100" ~ 2,
                            df_long$lap == "Split_150" ~ 3,
                            df_long$lap == "Split_200" ~ 4,
                            df_long$lap == "Split_250" ~ 5,
                            df_long$lap == "Split_300" ~ 6,
                            df_long$lap == "Split_350" ~ 7,
                            df_long$lap == "Split_400" ~ 8,
                            df_long$lap == "Split_450" ~ 9,
                            df_long$lap == "Split_500" ~ 10,
                            df_long$lap == "Split_550" ~ 11,
                            df_long$lap == "Split_600" ~ 12,
                            df_long$lap == "Split_650" ~ 13,
                            df_long$lap == "Split_700" ~ 14,
                            df_long$lap == "Split_750" ~ 15,
                            df_long$lap == "Split_800" ~ 16,
                            df_long$lap == "Split_850" ~ 17,
                            df_long$lap == "Split_900" ~ 18,
                            df_long$lap == "Split_950" ~ 19,
                            df_long$lap == "Split_1000" ~ 20,
                            df_long$lap == "Split_1050" ~ 21,
                            df_long$lap == "Split_1100" ~ 22,
                            df_long$lap == "Split_1150" ~ 23,
                            df_long$lap == "Split_1200" ~ 24,
                            df_long$lap == "Split_1250" ~ 25,
                            df_long$lap == "Split_1300" ~ 26,
                            df_long$lap == "Split_1350" ~ 27,
                            df_long$lap == "Split_1400" ~ 28,
                            df_long$lap == "Split_1450" ~ 29,
                            df_long$lap == "Split_1500" ~ 30)


w800f22 <- df_long %>% 
  filter(grepl("Women 800", Event))

w1500f22 <- df_long %>% 
  filter(grepl("Women 1500", Event))


#take out laps beyond race length
w800f22 <- w800f22 %>% filter(lapnum < 17)

#line plot colored by finish place
w800f22 %>% ggplot(aes(x = lapnum, 
                     y = split_time, 
                     group = Name,
                     color = Place)) +
  geom_line() + 
  geom_label(aes(label = Name, x = 8, y = 30)) +
  facet_wrap(~Place)

w1500f22 %>% ggplot(aes(x = lapnum, 
                       y = split_time, 
                       group = Name,
                       color = Place)) +
  geom_line() + 
  geom_label(aes(label = Name, x = 15, y = 30)) +
  facet_wrap(~Place)

#box plots of lap times
df_long %>% 
  filter(Event == "Women 800 LC Meter Freestyle" | Event == "Women 1500 LC Meter Freestyle" ) %>% 
  ggplot(aes(x = split_time, y = lap)) + 
  geom_boxplot() +
  facet_wrap(~ Event)


#z-scores by swimmer
w1500f22aves <- w1500f22 %>%
  filter(lapnum > 1 & lapnum < 30) %>% 
  group_by(Name) %>% summarise(ave_time = mean(split_time),
                                          deviation = sd(split_time),
                                          place = last(Place)) 

w1500f22aves %>% ggplot(aes(x = place, y = deviation)) + 
  geom_point() +
  geom_smooth(method = "lm", se = F)

w1500f22aves %>% ggplot(aes(x = place, y = ave_time)) + 
  geom_point() +
  geom_smooth(method = "lm", se = F)

w1500f22 %>%
  ggplot(aes(x = lapnum, 
             y = split_time, 
             group = Name, 
             color = Place)) + 
  geom_line() + 
  geom_point()



#work on below later
w800f$elapsed_time <- ave(w800f$split_time, w800f$Name, FUN = cumsum)

w800f <- w800f %>% arrange(elapsed_time, lapnum)

w800f$lap_place <- rep(1:10, length(w800f$lap)/ 10)

#assign colors to places - need same number of colors as places
my_colors <- c("goldenrod", "azure3", "darkgoldenrod4", "grey92", "grey92", "grey92", "grey92", "grey92", "grey92", "grey92")            
my_colors2 <- c("goldenrod", "azure3", "darkgoldenrod4", "aquamarine4", "aquamarine2", "aquamarine", "grey50", "indianred1", "indianred2", "indianred3")            
names(my_colors) <- levels(factor(w800f$Place))
#colors to fill
my_scale <- scale_fill_manual(name = "Place", values = my_colors)    
my_scale_c <- scale_color_manual(name = "Place", values = my_colors)    
#colors for lines
my_scale2 <- scale_color_manual(name = "Place", values = my_colors2)    

#line plot with lap ranks
w800f %>%  
  ggplot(aes(x = lapnum,
             y = split_time, 
             group = Name,
             color = as.factor(Place)
                     )) +
  geom_line(linewidth = 1.25) +
  my_scale2 +
  theme_minimal() +
  geom_text(aes(x = lapnum, 
                y = split_time, 
                label = lap_place)) 

#ridge plot?
w800f %>% ggplot(aes(x = split_time, 
                     y = reorder(Name, -Place), 
                     fill = as.factor(Place))) +
  geom_density_ridges() + 
  my_scale + 
  theme_light() +
  labs(title = "Women's 800 Free",
       subtitle = "First to Tenth",
       x = "Distribution of Lap Times",
       y = "") +
  theme(legend.position = "none")

#overall position by lap  
w800f %>% ggplot(aes(x = lapnum, 
                     y = lap_place, 
                     group = Name, 
                     color = as.factor(Place))) + 
  geom_line(linewidth = 1.5) + 
  my_scale2 +
  theme_light() +
  scale_y_discrete(name ="Place by Lap", 
                   limits=c(0,1, 2, 3, 4, 5, 6, 7, 8, 9, 10))

#get leader by lap
ltime <- w800f %>% 
  group_by(lapnum) %>% 
  summarise(leader = min(elapsed_time))
#put it back on whole df
w800f <- left_join(w800f, ltime, b = "lapnum")
#calculate time behind leader by lap
wsub$time_behind = wsub$elapsed_time - wsub$leader

#graph time behind leader by lap
wsub %>% ggplot(aes(x = lapnum, 
                    y = time_behind, 
                    group = Name, 
                    color = as.factor(Place))) + 
  geom_line(linewidth = 1) + 
  geom_point() + 
  my_scale2

wsub %>% ggplot() + 
  geom_segment(aes(x = lapnum, y = 0,
                   xend = lapnum, yend = time_behind)) +
  facet_wrap(~ Name) + theme_light() +
  labs(title = "Women's 800 Free",
       subtitle = "2020 US Olympic Trials",
       x = "Lap",
       y = "Time Behind Lap Leader")


w200f %>% ggplot(aes(x = split_time, 
                     y = -lapnum, 
                     group = lapnum)) +
  geom_boxplot() +
  labs(title = "Women's 200 Free",
       subtitle = "2020 US Olympic Trials",
       x = "Split Time",
       y = "Lap Number",
       caption = "by...")

w200f %>% filter(Place == 1 & lapnum < 5) %>% 
  ggplot(aes(x = lapnum, 
             y = split_time, 
             group = Name,
             color = as.factor(Place))) + 
  geom_line() + 
  geom_point() + 
  geom_label(aes(x = lapnum, y = split_time, label = Place))

df_long %>% ggplot(aes(x = lapnum, y = split_time, 
                       group = Name,
                       color = as.factor(Place))) +
  geom_line()


df_long %>% filter(Place < 6) %>% 
  ggplot(aes(x = lapnum, y = split_time, 
                       group = Name,
                       color = as.factor(Place))) +
  geom_line() +
  geom_point() +
  scale_x_continuous(limits = c(1, 16), breaks = c(1:16)) +
  geom_text(data = filter(df_long, lapnum == 16, Place < 6), 
            aes(x = lapnum, y = split_time, label = Place )) +
  geom_hline(aes(yintercept = ave_split) ) +
  labs(title = "Women's 800m Lap Times",
       subtitle = "Katie Ledeky always fastest",
       x = "Lap",
       y = "Lap Time",
       caption = "2022 USA Nationals",
       color = "Place") + facet_wrap(~ Name)

# + transition_reveal(lapnum) animates


df_long %>% group_by(lapnum) %>% 
  summarise(ave_lap = mean(split_time, na.rm = T)) %>% 
  ggplot(aes(x = lapnum, y = ave_lap)) + 
  geom_line() + 
  geom_point() +
  geom_text(aes(x = lapnum, 
                y = ave_lap, 
                label = lapnum),
            nudge_y = 0.1 
            )

df_long %>% ggplot(aes(x = lapnum, 
                       y = split_time, 
                       group = lapnum)
                   ) +
  geom_boxplot() + geom_line(data = filter(df_long, Place < 6),
                             aes(x = lapnum, 
                                 y = split_time, 
                                 group = Place, 
                                 color = as.factor(Place))
                             )


df_long$tot_time <-ave(df_long$split_time,df_long$Name,FUN=cumsum) #calculates cumulative time by lap

df_long %>% filter(Place < 6) %>% 
  ggplot(aes(x = tot_time, y = Place, group = Place)) + 
  geom_line() + geom_point() 

df_long %>% filter(Place < 6) %>% 
  ggplot(aes(x = lapnum, y = split_time, 
             group = Place, color = as.factor(Place),
             fill = as.factor(Place))) + 
  geom_col(position = "dodge") +
  labs(color = "Place",
       fill = "Place")

#### Not sure anything below works anymore- but might need for coding examples ####
#time behind Ledeky?

ledeky <-  df_long %>% filter(Name == "LEDECKY Katie")
ledeky <- ledeky %>% select(lapnum, split_time, tot_time)
ledeky <- ledeky %>% rename(split_ledeky = split_time, tot_ledeky = tot_time)



df_long <- left_join(df_long, ledeky)

#graph of differences by lap
df_long %>% filter(Place < 6) %>% 
  ggplot(aes(x = lapnum, y = split_time - split_ledeky, 
             group = Name,
             color = as.factor(Place))) +
  geom_line() +
  geom_point() +
  scale_x_continuous(limits = c(1, 16), breaks = c(1:16)) +
  geom_text(data = filter(df_long, lapnum == 16, Place < 6), 
            aes(x = lapnum, y = split_time - split_ledeky, label = Place )) +
  #geom_hline(aes(yintercept = ave_split) ) +
  labs(title = "Women's 800m Lap Times (behind Katie L)",
       subtitle = "Katie Ledeky always fastest",
       x = "Lap",
       y = "Time Behind Katie, by lap",
       caption = "2022 USA Nationals",
       color = "Place") #+ facet_wrap(~ Name)

#graph of total time behind (by lap)
p <- df_long %>% filter(Place < 6) %>% 
  ggplot(aes(x = lapnum, 
             y = tot_time - tot_ledeky, 
             group = Name,
             color = as.factor(Place))
         ) +
  geom_line() +
  geom_text(data = filter(df_long, Place < 6), 
            aes(x = lapnum, 
                y = tot_time - tot_ledeky, 
                label = round(tot_time - tot_ledeky, 1))
            ) +
  scale_x_continuous(limits = c(1, 16), breaks = c(1:16)) +
  geom_text(data = filter(df_long, lapnum == 16, Place < 6), 
            aes(x = lapnum, 
                y = tot_time - tot_ledeky, 
                label = Place )
            ) +
  #geom_hline(aes(yintercept = ave_split) ) +
  labs(title = "Women's 800m",
       subtitle = "Time behind Katie L",
       x = "Lap",
       y = "Time Behind Katie, by lap",
       caption = "2022 USA Nationals",
       color = "Place") #+ transition_reveal(lapnum, keep_last = T)

p 
#animate(p, fps = 2)

df_long %>% filter(Place < 6) %>% 
  ggplot(aes(y = reorder(Name, -Place), x = split_time, group = Name)) +
  geom_boxplot() +
  labs(title = "2022 USA Nats, Women's 800",
       subtitle = "Lap times, top 5",
       x = "Split Times",
       y = "1st (top) to 5th (bottom)"
  )

df %>% ggplot(aes(x = Reaction_Time, y = Place)) + geom_point()



free200w <- JuniorEast21 %>% 
  filter(Event == "Women 200 Yard Freestyle")
JuniorEast21 %>% mutate_at(c(10,40), as.numeric)


free200w %>% ggplot(aes(x = as.numeric(Split_50))) + 
  geom_boxplot()

