library(readxl)
library(tidyverse)


read_excel("/home/stenzy/Downloads/Accra Rainfall redefined.xlsx") %>% 
  setNames(
    c("Year", "Month", paste0("Day",1:31))
  ) -> a

pivot_longer(a, 3:33,
             names_to = "Day",
             values_to = "Rainfall") %>% 
  mutate(Month = month.abb[as.numeric(Month)]) %>% 
  mutate(Day = as.integer(gsub("Day", "", Day))) -> zz

#### Annual evaluation
zz <- zz %>% 
  group_by(Year) %>% 
  mutate(Month = factor(Month, levels = month.abb)) %>% 
  summarize( Total_Rainfall = sum(Rainfall, na.rm = TRUE)
  )

ggplot(data = zz,
       aes(x = Year, y = Total_Rainfall)) +
  geom_line() +
  geom_smooth(method = "lm", se = F, col = "red") 


##
bb <- read.csv("/home/stenzy/Downloads/04-tamsatMonthly.v3.1-410227200-1698796800_5.6_-0.168.csv")


b1 <- ymd(bb$time)
year <- year(b1)
month <- month(b1)
day <- day(b1)

bb %>% 
  mutate(year = year) %>% 
  mutate(month = month) %>% 
  mutate(day = day) %>% 
  select(year,month, day,rfe) -> bb

#### Annual Evaluation
bb <- bb %>% 
  group_by(year) %>% 
  summarize( Total_Rainfall = sum(rfe, na.rm = TRUE)
  )

ggplot(data = bb,
       aes(x = year, y = Total_Rainfall)) +
  geom_line() +
  geom_smooth(method = "lm", se = F, col = "blue") 

#GridExtra 
gridExtra::grid.arrange(
  ggplot(data = zz,
         aes(x = Year, y = Total_Rainfall)) +
    geom_line() +
    geom_smooth(method = "lm", se = F, col = "red") +
    labs(title = "Ground rainfall data (1983-2023)"),
  
    ggplot(data = bb,
         aes(x = year, y = Total_Rainfall)) +
    geom_line() +
    geom_smooth(method = "lm", se = F, col = "blue")+
    labs(title = "Satellite rainfall data (1983-2023)")
  
)

####Overlaying a map
ggplot(data = zz) +
  geom_line(aes(x = Year, y = Total_Rainfall, color = "Ground")) +
  geom_line(data = bb, aes(x = year, y = Total_Rainfall, color = "Satellite")) +
  scale_color_manual(values = c("Ground" = "red", "Satellite" = "blue"))


####monthly Validation
zz <- zz %>% 
  subset(Year >= 1983) %>% 
  group_by(Year, Month) %>% 
  mutate(Month = factor(Month, levels = month.abb)) %>% 
  summarize( Total_Rainfall = sum(Rainfall, na.rm = TRUE)
  )

ggplot(data = zz,
       aes(x = Month, y = Total_Rainfall)) + 
  geom_line(col = "red")


 bb <- bb %>% 
  group_by(year, month) %>% 
  mutate(month = month.abb[as.numeric(month)]) %>% 
  mutate(month = factor(month, levels = month.abb)) %>% 
  summarize( Total_Rainfall = sum(rfe, na.rm = TRUE)
  )

bb <- bb %>% 
  mutate(Total_Rainfall = ifelse(Total_Rainfall >= -999.9 & Total_Rainfall <= -1, NA, Total_Rainfall))

ggplot(data = bb,
       aes(x = month, y = Total_Rainfall)) + 
  geom_line(col = "blue")

####overlay for monthly
ggplot(data = zz) +
  geom_line(aes(x = Month, y = Total_Rainfall, color = "Ground")) +
  geom_line(data = bb, aes(x = month, y = Total_Rainfall, color = "Satellite")) +
  scale_color_manual(values = c("Ground" = "red", "Satellite" = "blue"))

#ggplot(data = zz,
 #      aes(x = Month, y = Total_Rainfall)) + 
#  geom_bar(col = "red")


#### finding the residual difference
zz$Total_Rainfall - bb$Total_Rainfall -> rf_difference

bb$rfd <- rf_difference 

bb %>% 
 select(year, month, rfd) -> rf_difference

####  faceting 
plot1 <- ggplot(rf_difference, mapping = aes(x = year, y = rfd, colour = month)) +
  geom_point()
plot1 + facet_wrap(~month, ncol = 4)



# Create a histogram
 bb %>%
  ggplot(aes(x = rfd)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black", alpha = 0.7) +
  geom_density() +
  facet_wrap(~month, scales = "free_y", ncol = 4) +
  labs(title = "Residual Differences Distribution Across Months (1983-2023)",
       x = "Residual Difference",
       y = "Frequency") +
  stat_function(fun = dnorm, args = list(mean = mean(bb$rfd), sd = sd(bb$rfd)), color = "red", size = 1)

  
  
stat_function(fun = dnorm, args  = list(mean = mean(bb$rfd), sd = sd(bb$rfd)))



p + stat_function(fun = function(x) {
  mu <- mean(x)
  sigma <- sd(x)
  dnorm(x, mean = mu, sd = sigma) * length(x) * diff(range(x))
}, color = "red", size = 1)


sd()
