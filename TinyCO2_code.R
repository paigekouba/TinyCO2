# Monday 5/5/25
# TinyCO2: High-performance, low-cost CO2 enrichment for field-grown plants

# The following code demonstrates the performance of the TinyCO2 eCO2 system for short-stature plants.
# Tests were conducted on bare soil except where otherwise noted.

library(tidyverse)
library(interp)

# Load the data: CO2 performance, light penetration data, temperature data
# testing <- read_csv([INSERT FILE LOCATION]"TinyCO2_performance.csv")
# PAR_df <- read.csv([INSERT FILE LOCATION]"tinyCO2_PAR.csv")
# temp_df <- read.csv([INSERT FILE LOCATION]"tinyCO2_T.csv")

attr(testing$TIMESTAMP, "tzone") # starts out as UTC, though clock time is correct
# force_tz keeps the clock time but reassigns the tz to be accurate
testing$TIMESTAMP <- force_tz(testing$TIMESTAMP, "America/Los_Angeles")

testing_daytime <- testing %>% 
  filter(PARuE > 50) # CO2 is programmed to shut off if PARuE < 50

mean(testing_daytime$DeltaObs) # 196.8225
sd(testing_daytime$DeltaObs) # 63.57922
sum(testing_daytime$CO2elev < (testing_daytime$CO2ref+200)*1.2 & testing_daytime$CO2elev > (testing_daytime$CO2ref+200)*0.8)/nrow(testing_daytime) # 95.1% of the time within 20% of the set point (ambient + 200 ppm)

# Fig 3: Density plot of CO2 elevation over 10 days of testing in April, 2024 
ggplot(testing_daytime, aes(x=DeltaObs))+
  geom_density(size=2)+
  geom_vline(aes(xintercept = mean(DeltaObs)),color = "red", linetype="dashed", size=1.5)+
  labs(title="Daytime CO2 Elevation: 4/9-4/19/24", 
       x= "∆ CO2 (ppm)",
       y= "Density") +
  scale_y_continuous(expand = c(0,0), limits=c(0,0.0087)) +
  theme_classic(base_size=20)

# Spatial Performance
testing2 <- testing %>%  # subset to just the 2 days of spatial testing in April 2024
  filter(TIMESTAMP < "2024-04-10 17:10:00")

# Between-Plot Testing
# at 7:25, 20m timesteps, plot 1-16

between_4.10.24 <- testing2 %>%
  filter(TIMESTAMP > "2024-04-10 07:25:00") %>%
  filter(TIMESTAMP < "2024-04-10 12:45:00") %>%
  select(TIMESTAMP, CO2ref, CO2elev, CO2test, DeltaObs) %>%
  mutate(DeltaTest = CO2test - CO2ref) %>%
  mutate(timestep = floor_date(TIMESTAMP - 5*60, unit = "20 minutes"),
         position = factor(c(1:16)[factor(timestep)])) %>% 
  # we need a 1-min buffer for the between-times when sampling tube is being moved to the next plot
  filter(TIMESTAMP > timestep + 6*60) %>% # # this provides a one-minute buffer (the TIMESTAMP lags the timestep by 5 min to begin with, so if it's > 6 min after start of "timestep" then it has a 1-min buffer)
  mutate(eCO2 = as.numeric(position %in% c(2,3,6,7,9,11,13,15)))

mean(between_4.10.24[between_4.10.24$position %in% c(1,4,5,8,10,12,14,16),]$DeltaTest) # 4.089264 = avg for aCO2 plots
sd(between_4.10.24[between_4.10.24$position %in% c(1,4,5,8,10,12,14,16),]$DeltaTest) # 9.684221 = sd for aCO2 plots

mean(between_4.10.24[between_4.10.24$position %in% c(2,3,6,7,9,11,13,15),]$DeltaTest) # 218.2669 = avg for eCO2 plots
sd(between_4.10.24[between_4.10.24$position %in% c(2,3,6,7,9,11,13,15),]$DeltaTest) # 39.1122 = sd for eCO2 plots

# Temporal Control during 1-day between-plot testing
between_4.10.24 %>% 
  filter(eCO2 == 1) %>% 
  mutate(within10 = (CO2elev < (CO2ref+200)*1.1 & CO2elev > (CO2ref+200)*0.9)) %>% 
  mutate(within20 = (CO2elev < (CO2ref+200)*1.2 & CO2elev > (CO2ref+200)*0.8)) %>% 
  # summarise(sum20 = sum(within20)) # 456 obs within 20% of target
  summarise(sum10 = sum(within10)) # 442 obs within 10% of target

456/nrow(between_4.10.24[between_4.10.24$eCO2==1,]) # = 100% of measurements within 20% of set point
442/nrow(between_4.10.24[between_4.10.24$eCO2==1,]) # = 96.9% within 10%

# Calculating per-plot average CO2 levels
avg_between_4.10.24 <- between_4.10.24 %>% 
  group_by(position, eCO2) %>% 
  summarise(mean = mean(DeltaTest), sd = sd(DeltaTest)) %>% 
  ungroup()

min(avg_between_4.10.24[avg_between_4.10.24$eCO2 == 1,]$mean) 
# 175.3 ppm = min eCO2 plot avg (plot # 15)
avg_between_4.10.24[avg_between_4.10.24$eCO2 == 1,][which.min(avg_between_4.10.24[avg_between_4.10.24$eCO2 == 1,]$mean),4]
# 52.9 = sd for plot # 15

max(avg_between_4.10.24[avg_between_4.10.24$eCO2 == 1,]$mean) 
# 251.0 ppm = max eCO2 plot avg (plot # 13)
avg_between_4.10.24[avg_between_4.10.24$eCO2 == 1,][which.max(avg_between_4.10.24[avg_between_4.10.24$eCO2 == 1,]$mean),4]
# 33.8 = sd for plot # 13

# Fig 4: Mean and sd of CO2 elevation achieved for each of 16 treatment plots
avg_between_4.10.24 %>% 
  ggplot(aes(x=factor(position), y=mean)) +
  geom_pointrange(data = avg_between_4.10.24, aes(ymin=(mean - sd), ymax = (mean + sd), color = as.factor(eCO2)), size=1, linewidth=1) +
  scale_color_discrete(type=c("darkgray", "black")) +
  labs(title = "∆CO2 Across All Plots, April 10, 2024") +
  xlab("Plot") + ylab("∆CO2 (ppm)") +
  labs(color="CO2 Treatment") +
  theme_classic(base_size=20)


# Horizontal variation within a plot
# not including a buffer period this time since all measurements are within one eCO2 screen

# within-plot testing, MFCBase = 1000
# starting at 11:40, changing every 20m
# spots in this order: A, B, F, G, K, L, C, D, H, I, E, J
position <- c("A", "B", "F", "G", "K", "L", "C", "D", "H", "I", "E", "J")
x_within <-  c(-13, 13, -13, 13, -13, 13, -26, 0, -26, 0, 26, 26)
y_within <- c(26, 26, 0, 0, -26, -26, 13, 13, -13, -13, 13, -13)
points_within <- data.frame(x_within, y_within, position)
#ggplot(points_within, aes(x=x_within, y=y_within, label = position)) + geom_text()
#(shows the layout of sampling points)

within_4.9.24 <- testing %>% 
  filter(TIMESTAMP > "2024-04-09 11:40:00") %>% 
  filter(TIMESTAMP < "2024-04-09 15:40:00") %>% # subset to within-plot spatial test on 4/9/24
  select(TIMESTAMP, CO2ref, CO2elev, CO2test, DeltaObs) %>% 
  mutate(DeltaTest = CO2test - CO2ref) %>% 
  mutate(timestep = floor_date(TIMESTAMP, unit = "20 minutes"), # assigns a time-group ID
         position = position[factor(timestep)]) # turns time-group into corresponding position ID
#summarize(mDeltaTest = mean(DeltaTest), sd = sd(DeltaTest))
#group_by(TIMESTAMP = cut(TIMESTAMP, breaks = "20 min")) %>% 
mean(within_4.9.24$DeltaTest) # 198.9011
sd(within_4.9.24$DeltaTest) # 53.36563

within_4.9.24$position <- factor(within_4.9.24$position, levels = position)

# assign spots to points in a grid, then use "interp" package to make figure
# get x and y associated with CO2 table
within_4.9.24 <- left_join(within_4.9.24, points_within, by = "position")
within_4.9.24$position <- factor(within_4.9.24$position, levels = position)

means_within <- within_4.9.24 %>% 
  group_by(position) %>% 
  summarise(mean = mean(DeltaTest), sd = sd(DeltaTest)) 
means_within <- left_join(means_within, points_within, by = "position")

min(means_within$mean) # 160.9 ppm is min avg for a single sampling point
max(means_within$mean) # 256.1 ppm is max avg for a single sampling point

# find out average difference from sampling point to center
# do sampling point (by row) - same row's value for stationary eCO2 line
within_4.9.24 %>% 
  mutate(diff_center = (CO2test - CO2elev)*100/CO2elev) %>% 
  group_by(position) %>% 
  summarize(mean = mean(diff_center)) %>% 
  summarise(mean(mean)) # mean = -3.35%
#  summarise(sd(mean)) # sd = 4.35%

# CO2 levels outside the screen; see below (l. 262)

# gridded bicubic spline interpolation from akima
interpolated <- interp(x = means_within$x_within, y = means_within$y_within, z = means_within$mean, linear=FALSE, extrap = FALSE)
#convert this to a long form dataframe
interp_df <- expand_grid(i = seq_along(interpolated$x), 
                         j = seq_along(interpolated$y)) %>% 
  mutate(x = interpolated$x[i],
         y = interpolated$y[j],
         DeltaTest = map2_dbl(i, j, ~interpolated$z[.x,.y])) %>% 
  select(-i, -j)

# Next, tested points outside the screen boundary, to test CO2 attenuation:
# compass points in 10cm increments, 20m timesteps, starting at 15:40
# N, E, S (replace), W, N+20, E+20, S+20, W+20(replace), 
# @ 18:24 N+30, E+30, S+30, W+30, S redo, W+20 redo
# 4/10, 14:25 @ 25s, N+40, E+40, S+40, W+40; 15:50 N+50, W+50, S+50, E+50

# adding outside-screen points to inside-screen points: (1) 15:40-18:00, (2) 18:24-2024 on 4/9; 
# (3) 14:25-15:45, (4) 15:50-17:10 on 4/10

# developing list of position IDs in the order they were measured
pos_out <- c("No+10", "Ea+10", "So+10", "We+10", "No+20", "Ea+20", "So+20", "No+30", "Ea+30", "So+30", "We+30", "So+10.1","We+20","No+40", "Ea+40", "So+40", "We+40", "No+50", "Ea+50", "So+50", "We+50")
pos_out <- factor(pos_out, levels = unique(pos_out))

# get x and y coordinates for outside-screen positions
x_out <- vector()
for(i in 1:5){
  x_i <- c(0,39+(i*10),0,-39-(i*10))
  x_out <- append(x_out, x_i)
}
x_out <- c(0,  49,   0, -49,   0,  59,   0,   0,  69,   0, -69, 0, -59,   0,  79,   0, -79,   0,  89,   0, -89)
y_out <- vector()
for(i in 1:5){
  y_i <- c(39+(i*10),0,-39-(i*10),0)
  y_out <- append(y_out, y_i)
}
y_out <- c(49,   0, -49,   0,  59,   0, -59, 69,   0, -69,   0, -49, 0,  79,   0, -79,   0,  89,   0, -89,   0)
points_out <- data.frame(x_out, y_out, pos_out)
points_out$pos_out <- factor(pos_out, levels = unique(pos_out))

# broken into four chunks due to timestep errors or dropped points
CO2_out1 <- testing %>% # first 7
  filter(TIMESTAMP > "2024-04-09 15:40:00") %>% 
  filter(TIMESTAMP < "2024-04-09 18:00:00") %>% 
  select(TIMESTAMP, CO2ref, CO2elev, CO2test, DeltaObs) %>% 
  mutate(DeltaTest = CO2test - CO2ref) %>% 
  mutate(timestep = floor_date(TIMESTAMP, unit = "20 minutes"), 
         position = pos_out[factor(timestep)])

CO2_out2 <- testing %>% # 8-13
  filter(TIMESTAMP > "2024-04-09 18:24:00") %>% 
  filter(TIMESTAMP < "2024-04-09 20:24:00") %>% 
  select(TIMESTAMP, CO2ref, CO2elev, CO2test, DeltaObs) %>% 
  mutate(DeltaTest = CO2test - CO2ref) %>% 
  mutate(timestep = floor_date(TIMESTAMP-(4*60), unit = "20 minutes"), 
         position = pos_out[8:13][factor(timestep)]) 

CO2_out3 <- testing %>% # 14-17
  filter(TIMESTAMP > "2024-04-10 14:25:00") %>% 
  filter(TIMESTAMP < "2024-04-10 15:45:00") %>% 
  select(TIMESTAMP, CO2ref, CO2elev, CO2test, DeltaObs) %>% 
  mutate(DeltaTest = CO2test - CO2ref) %>% 
  mutate(timestep = floor_date(TIMESTAMP-(5*60), unit = "20 minutes"), 
         position = pos_out[14:17][factor(timestep)]) 

CO2_out4 <- testing %>% # 18-21
  filter(TIMESTAMP > "2024-04-10 15:50:00") %>% 
  filter(TIMESTAMP < "2024-04-10 17:10:00") %>% 
  select(TIMESTAMP, CO2ref, CO2elev, CO2test, DeltaObs) %>% 
  mutate(DeltaTest = CO2test - CO2ref) %>% 
  mutate(timestep = floor_date(TIMESTAMP+(10*60), unit = "20 minutes"), 
         position = pos_out[18:21][factor(timestep)]) 

CO2_out <- rbind(CO2_out1, CO2_out2, CO2_out3, CO2_out4)
# filter out S+10, and corresponding point, since PAR dropped below threshold during that step
CO2_out <- CO2_out %>% 
  filter(position != "So+10")
points_out <- points_out[-3,]

# get x y data into CO2 df
# CO2_out <- left_join(CO2_out, points_out, by="position")
CO2_out <- left_join(CO2_out, points_out, by=c("position"="pos_out"))

# join outside screen data with inside screen data
# first fix naming conventions
names(within_4.9.24)[9] <- "x"
names(within_4.9.24)[10] <- "y"
names(CO2_out)[9] <- "x"
names(CO2_out)[10] <- "y"

# join inside-screen and outside-screen data:
in.out_4.9.24 <- rbind(within_4.9.24, CO2_out) # this is the measured CO2 values over several hours

# need same colnames for rbind
names(points_out) <- c("x", "y", "position")
names(points_within) <- c("x", "y", "position")
allpoints <- rbind(points_out, points_within) # this is the name and x,y coord of each of the 12 points measured

in.out_means <- in.out_4.9.24 %>% # df with per-point means
  group_by(position) %>% 
  summarise(mean = mean(DeltaTest), sd = sd(DeltaTest)) 
in.out_means <- left_join(in.out_means, allpoints, by = "position") # adds position name and coords

# interpolation function for data including outside-screen points
interpolated2 <- interp(x = in.out_means$x, y = in.out_means$y, z = in.out_means$mean, linear=TRUE, extrap = TRUE)
#convert this to a long form dataframe
interp_df2 <- expand_grid(i = seq_along(interpolated2$x), 
                          j = seq_along(interpolated2$y)) %>% 
  mutate(x = interpolated2$x[i],
         y = interpolated2$y[j],
         DeltaTest = map2_dbl(i, j, ~interpolated2$z[.x,.y])) %>% 
  select(-i, -j)

# Figure 5: measured and interpolated CO2 concentrations for a single plot, inside and outside the screen
interpolated_CO2 <- ggplot() + # interpolated points plus measured values  (means) plus circle showing screen
  geom_point(data = interp_df2, aes(x = x, y = y, color = DeltaTest), size=5, shape=15) +
  scale_color_continuous(type = "viridis") + theme_classic() +
  geom_text(data = in.out_means, aes(x=x, y=y, label = round(mean)), size = 5) +
  ggforce::geom_circle(aes(x0=0,y0=0,r=39)) +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0)) +
  labs(color="∆CO2")


# Vertical variation above a plot
# above-plot testing, MFCBase = 1100
# starting at 8:10 (10cm), until 11:10 (100cm) above, by 20m
pos_above <- c(paste0(seq(10,100,by=10),"cm"))
above_4.9.24 <- testing %>% 
  filter(TIMESTAMP < "2024-04-09 11:30:00") %>% 
  select(TIMESTAMP, CO2ref, CO2elev, CO2test, DeltaObs) %>% 
  mutate(DeltaTest = CO2test - CO2ref) %>% 
  mutate(timestep = floor_date(TIMESTAMP - 10*60, unit = "20 minutes"), 
         position = pos_above[factor(timestep)]) 
above_4.9.24$position <- factor(above_4.9.24$position, levels = unique(above_4.9.24$position))

above_means <- above_4.9.24 %>% 
  group_by(position) %>% 
  summarise(mean = mean(DeltaTest), sd = sd(DeltaTest)) %>% 
  ungroup()
colMeans(above_means[1:4,2]) # 198.3428 is the mean within the screen

# Figure 6: mean and sd of CO2 concentration at 10cm intervals above the plot center
above_pointrange <- above_means %>% # box plots
  ggplot() +
  annotate("rect", xmin=1, xmax=4.3, ymin=0, ymax=Inf, alpha=0.5) +
  geom_pointrange(data=above_means, aes(x= position, y=mean, ymin=mean-sd, ymax=mean+sd), size=1, linewidth=1) + coord_flip() + 
  geom_hline(yintercept=200,color = "red", linetype="dashed", size=1) +
  ylab("∆CO2") + xlab("Height Above Plot Center") +   theme_classic(base_size = 20)

# Next, testing the effects of vegetation height
# 12:45 short veg, canopy height
# 13:05 medium veg, canopy height
# 13:25 tall veg, canopy height
# 13:45 medium, 10cm
# 14:05 tall, 10cm

veg_4.10.24 <- testing %>%
  filter(TIMESTAMP > "2024-04-10 12:45:00") %>%
  filter(TIMESTAMP < "2024-04-10 14:25:00") %>%
  select(TIMESTAMP, CO2ref, CO2elev, CO2test, DeltaObs) %>%
  mutate(DeltaTest = CO2test - CO2ref) %>%
  # mutate(timestep = floor_date(TIMESTAMP - 5*60, unit = "20 minutes"),
  #        position = c("short_10cm@canopy","med_25-35cm@canopy","tall_50-60cm@canopy","med_@10cm","tall_@10cm")[factor(timestep)])
  mutate(timestep = floor_date(TIMESTAMP - 5*60, unit = "20 minutes"),
         position = c("Short (canopy)","Medium (canopy)","Tall (canopy)","Medium (at 10cm)","Tall (at 10cm)")[factor(timestep)])

veg_4.10.24$position <- factor(veg_4.10.24$position, levels = unique(veg_4.10.24$position))

avg_veg_4.10.24 <- veg_4.10.24 %>% 
  group_by(position) %>% 
  summarise(mean = mean(DeltaTest), sd = sd(DeltaTest)) %>% 
  ungroup()

# Fig S2: mean and sd of CO2 concentration with varying heights of elevation: short (10 cm), medium (25–35 cm), and tall (50–60 cm).
avg_veg_4.10.24 %>% 
  ggplot(aes(x=factor(position), y=mean)) +
  geom_pointrange(data = avg_veg_4.10.24, aes(ymin=(mean - sd), ymax = (mean + sd)), size=1, linewidth=1) +
  geom_hline(yintercept=200,color = "red", linetype="dashed", size=1) +
  labs(title = "∆CO2 for Varying Vegetation Height") +
  xlab("Vegetation Height & Sampling Height") + ylab("∆CO2 (ppm)") +
  labs(color="CO2 Treatment") +
  theme_classic(base_size = 19) +
  theme(axis.text.x = element_text(angle = 45, hjust=1))

# CO2 consumption per area, when PAR > 50
20*mean(testing_daytime$FlowMFC)/5000 # 20L/min max * FlowMFC (mV) / 5000 mV max = 3.968844 L/min average
3.968844*1440 # L/min * min/d = 5715.135 L/d when PAR > 50
5715.135*0.001836 # L/d * kg/L = 10.49299 kg/d
10.49299/4.96 # kg/d / 4.96m2 plot area = 2.115522 kg/m2/d when PAR > 50
# cf Leadley, 6.10 kg/d/m2 (24h) ~3.05 kg/d/m2 during daylight
# nrow(post4.9.24_daytime)*20/60 # = 7146.7 min PAR > 50
# nrow(post4.9.24)*20/60 # = 14889.67 min total

# CO2 consumption per area, 24h
20*mean(testing$FlowMFC)/5000 # 20L/min max * FlowMFC (mV) / 5000 mV max = 2.479395 L/min average
2.479395*1440 # L/min * min/d = 3570.329 L/d 
3570.329*0.001836 # L/d * kg/L = 6.555124 kg/d
6.555124/4.96 # kg/d / 4.96m2 plot area = 1.321598 kg/m2/d 
# cf Leadley, 6.10 kg/d/m2 (24h) 

# Finally, testing microclimate effects

temp_df2 <- temp_df %>% # data manipulation
  pivot_longer(A:L, names_to = "spot", values_to = "temperature") %>% 
  mutate(time = hms::parse_hm(time)) %>% 
  mutate(sampletime = format(as.POSIXlt(as.POSIXct('2000-1-1', "UTC") + 
                                          round(as.numeric(time)/3600)*3600),
                             format = "%H:%M:%S")) %>% 
  mutate(sample = rep(1:8,each=96)) %>% 
  pivot_wider(names_from = "screen", values_from = "temperature") %>% 
  rename(inside = i, outside = o)

PAR_df2 <- PAR_df %>% 
  pivot_longer(A:L, names_to = "spot", values_to = "PAR") %>% 
  mutate(time = hms::parse_hm(time)) %>% 
  mutate(sampletime = format(as.POSIXlt(as.POSIXct('2000-1-1', "UTC") + 
                                          round(as.numeric(time)/3600)*3600),
                             format = "%H:%M:%S")) %>% 
  mutate(sample = rep(1:8,each=96)) %>% 
  pivot_wider(names_from = "screen", values_from = "PAR") %>% 
  rename(inside = i, outside = o)

# bootstrap mean ±se of [inside] - [outside] (lower T, shading), for each of 8 timesteps
Tmean <- data.frame("mean.diff" = c(1:8)) # initialize results df
for(i in c(1:8)){Tmean[(i),1] <-          # this is saying each of the 8 sampletimes gets 1 row, and this is defining the first column in the results df
  mean(
    do.call(c,lapply(1:1000, function(boot){
      outside <- sample(unlist(temp_df2[temp_df2$sample==i,]$outside)[!is.na(unlist(temp_df2[temp_df2$sample==i,]$outside))], replace = T)
      inside <- sample(unlist(temp_df2[temp_df2$sample==i,]$inside)[!is.na(unlist(temp_df2[temp_df2$sample==i,]$inside))], replace = T)
      mean(inside) - mean(outside)
    })))
}

# then mean - sd
Tmin <- data.frame("min.diff" = c(1:8))  # initialize results df
for(i in c(1:8)){ x <- do.call(c,lapply(1:1000, function(boot){
  outside <- sample(unlist(temp_df2[temp_df2$sample==i,]$outside)[!is.na(unlist(temp_df2[temp_df2$sample==i,]$outside))], replace = T)
  inside <- sample(unlist(temp_df2[temp_df2$sample==i,]$inside)[!is.na(unlist(temp_df2[temp_df2$sample==i,]$inside))], replace = T)
  mean(inside) - mean(outside) 
}))
Tmin[i,1] <- mean(x) - sd(x)
}

# then mean + sd
Tmax <- data.frame("max.diff" = c(1:8))  # initialize results df
for(i in c(1:8)){ x <- do.call(c,lapply(1:1000, function(boot){
  outside <- sample(unlist(temp_df2[temp_df2$sample==i,]$outside)[!is.na(unlist(temp_df2[temp_df2$sample==i,]$outside))], replace = T)
  inside <- sample(unlist(temp_df2[temp_df2$sample==i,]$inside)[!is.na(unlist(temp_df2[temp_df2$sample==i,]$inside))], replace = T)
  mean(inside) - mean(outside) 
}))
Tmax[i,1] <- mean(x) + sd(x)
}

Tboot <- cbind(Tmean, Tmin, Tmax, "sample"=c(1:8))

Tboot2 <- right_join(Tboot, temp_df2[,c("sample","sampletime")], by = "sample")[!duplicated(right_join(Tboot, temp_df2[,c("sample","sampletime")], by = "sample")),]


# for PAR now
PARmean <- data.frame("mean.diff" = c(1:8)) # initialize results df
for(i in c(1:8)){PARmean[(i),1] <-          # this is saying each of the 8 sampletimes gets 1 row, and this is defining the first column in the results df
  mean(
    do.call(c,lapply(1:1000, function(boot){
      outside <- sample(unlist(PAR_df2[PAR_df2$sample==i,]$outside)[!is.na(unlist(PAR_df2[PAR_df2$sample==i,]$outside))], replace = T)
      inside <- sample(unlist(PAR_df2[PAR_df2$sample==i,]$inside)[!is.na(unlist(PAR_df2[PAR_df2$sample==i,]$inside))], replace = T)
      mean(inside) - mean(outside)
    })))
}

# then mean - sd
PARmin <- data.frame("min.diff" = c(1:8))  # initialize results df
for(i in c(1:8)){ x <- do.call(c,lapply(1:1000, function(boot){
  outside <- sample(unlist(PAR_df2[PAR_df2$sample==i,]$outside)[!is.na(unlist(PAR_df2[PAR_df2$sample==i,]$outside))], replace = T)
  inside <- sample(unlist(PAR_df2[PAR_df2$sample==i,]$inside)[!is.na(unlist(PAR_df2[PAR_df2$sample==i,]$inside))], replace = T)
  mean(inside) - mean(outside) 
}))
PARmin[i,1] <- mean(x) - sd(x)
}

# then mean + sd
PARmax <- data.frame("max.diff" = c(1:8))  # initialize results df
for(i in c(1:8)){ x <- do.call(c,lapply(1:1000, function(boot){
  outside <- sample(unlist(PAR_df2[PAR_df2$sample==i,]$outside)[!is.na(unlist(PAR_df2[PAR_df2$sample==i,]$outside))], replace = T)
  inside <- sample(unlist(PAR_df2[PAR_df2$sample==i,]$inside)[!is.na(unlist(PAR_df2[PAR_df2$sample==i,]$inside))], replace = T)
  mean(inside) - mean(outside) 
}))
PARmax[i,1] <- mean(x) + sd(x)
}

PARboot <- cbind(PARmean, PARmin, PARmax, "sample"=c(1:8))

PARboot2 <- right_join(PARboot, PAR_df2[,c("sample","sampletime")], by = "sample")[!duplicated(right_join(PARboot, PAR_df2[,c("sample","sampletime")], by = "sample")),]

mean(Tboot2$mean.diff) # -2.9ºC difference inside vs outside screen
max(abs(Tboot2$mean.diff)) # 8.3 max deviation (14:00)

mean(PARboot2$mean.diff)
# [1] -96.9 --> average PAR was 96.9 μmol m−2 s−1 lower inside screen
mean(PARboot2$mean.diff)/mean(PAR_df2$outside, na.rm=T)
# [1] -0.1269136 --> 12.7% lower PAR 
# these results may vary by a few decimal places from those in the paper, due to the bootstrap method used above.
