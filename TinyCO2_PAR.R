# Making heatmaps of T and PAR microclimate data

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

# t-tests to determine A and E plots can be pooled
t.test(temp_df2[temp_df2$CO2Tmt=="A","inside"], temp_df2[temp_df2$CO2Tmt=="E","inside"]) # t = -0.83382, df = 377.36, p-value = 0.4049
t.test(temp_df2[temp_df2$CO2Tmt=="A","outside"], temp_df2[temp_df2$CO2Tmt=="E","outside"]) # t = -0.056883, df = 378.72, p-value = 0.9547

t.test(PAR_df2[PAR_df2$CO2Tmt=="A","inside"], PAR_df2[PAR_df2$CO2Tmt=="E","inside"]) # t = -0.6593, df = 381.98, p-value = 0.5101
t.test(PAR_df2[PAR_df2$CO2Tmt=="E","inside"], PAR_df2[PAR_df2$CO2Tmt=="E","inside"]) # t = 0, df = 382, p-value = 1


spot <- c("A", "B", "F", "G", "K", "L", "C", "D", "H", "I", "E", "J")
x_within <-  c(-13, 13, -13, 13, -13, 13, -26, 0, -26, 0, 26, 26)
y_within <- c(26, 26, 0, 0, -26, -26, 13, 13, -13, -13, 13, -13)
points_within <- data.frame(x_within, y_within, spot)

PAR_df3 <- PAR_df2 %>% # take average, per-spot, for all INSIDE and OUTSIDE (n = 4 each) x each sample time
  # inside and outside should have their own column
  pivot_longer(c(inside,outside), names_to = "in.out", values_to = "PAR") %>% 
  group_by(spot, sample, in.out) %>% 
  summarise(meanPAR = mean(PAR, na.rm = T)) %>% 
  left_join(points_within, by = "spot") 

# now make a plot as practice for looping: all 12 spots for Sample 1, inside the screen

ggplot() + # interpolated points plus measured values  (means) plus circle showing screen
  geom_point(data = filter(PAR_df3, sample == 3, in.out == "outside"), aes(x = x_within, y = y_within, color = meanPAR), size=5, shape=15) +
  scale_color_gradient(low = "black", high="yellow") + theme_classic() +
  geom_text(data = filter(PAR_df3, sample == 3, in.out == "outside"), aes(x=x_within, y=y_within, label = round(meanPAR)), size = 5) +
  ggforce::geom_circle(aes(x0=0,y0=0,r=39)) +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0)) +
  labs(color="meanPAR")

# Make a function that takes sample # and in.out and creates an interpolated df
PAR_interp_fn <- function(sample_num, in.out_num) {
PAR_means <- filter(PAR_df3, sample == sample_num, in.out == in.out_num)
PARinterp <- interp(x = PAR_means$x_within, y = PAR_means$y_within, z = PAR_means$meanPAR, linear=FALSE, extrap=TRUE, xo=-39:39, yo=-39:39)
#convert this to a long form dataframe
PARinterp_df <- expand_grid(i = seq_along(PARinterp$x), 
                          j = seq_along(PARinterp$y)) %>% 
  mutate(x = PARinterp$x[i],
         y = PARinterp$y[j],
         PAR = map2_dbl(i, j, ~PARinterp$z[.x,.y])) %>% 
  select(-i, -j)
return(PARinterp_df)
}
PAR_interp_fn(3,"outside")

PAR_plots <- vector("list", length=8)
for(i in c(1:8)){
  PAR_plots[[i]] <-
    ggplot() + # interpolated points plus measured values  (means) plus circle showing screen
    geom_point(data = PAR_interp_fn(i,"outside"), aes(x = x, y = y, color = PAR), size=1, shape=15) +
    # scale_color_gradient(low = "black", high="yellow") + theme_classic() +
    scale_color_continuous(limits = c(14, 1850)) + theme_classic() +
    geom_text(data = filter(PAR_df3, sample == i, in.out == "outside"), aes(x=x_within, y=y_within, label = round(meanPAR)), size = 3, color="gray") +
 #   ggforce::geom_circle(aes(x0=0,y0=0,r=39)) +
    scale_y_continuous(expand = c(0,0)) +
    scale_x_continuous(expand = c(0,0)) +
    labs(color="PAR")
}
#PAR_plots[[8]]

PAR_plots_in <- vector("list", length=8)
for(i in c(1:8)){
  PAR_plots_in[[i]] <-
    ggplot() + # interpolated points plus measured values  (means) plus circle showing screen
    geom_point(data = PAR_interp_fn(i,"inside"), aes(x = x, y = y, color = PAR), size=1, shape=15) +
    # scale_color_gradient(low = "black", high="yellow") + theme_classic() +
    scale_color_continuous(limits = c(14, 1850)) + theme_classic() +
    geom_text(data = filter(PAR_df3, sample == i, in.out == "inside"), aes(x=x_within, y=y_within, label = round(meanPAR)), size = 3, color="gray") +
    ggforce::geom_circle(aes(x0=0,y0=0,r=39)) +
    scale_y_continuous(expand = c(0,0)) +
    scale_x_continuous(expand = c(0,0)) +
    labs(color="PAR")
}
library(gridExtra)
grid.arrange(PAR_plots_in[[1]],PAR_plots_in[[1]],PAR_plots_in[[1]],PAR_plots_in[[1]],PAR_plots_in[[1]],PAR_plots_in[[1]],PAR_plots_in[[1]],PAR_plots_in[[1]], nrow=1)
library(ggpubr)
ggarrange(PAR_plots_in[[1]],PAR_plots_in[[2]],PAR_plots_in[[3]],PAR_plots_in[[4]],PAR_plots_in[[5]],PAR_plots_in[[6]],PAR_plots_in[[7]],PAR_plots_in[[8]],
          PAR_plots[[1]],PAR_plots[[2]],PAR_plots[[3]],PAR_plots[[4]],PAR_plots[[5]],PAR_plots[[6]],PAR_plots[[7]],PAR_plots[[8]],
          nrow=2, ncol=8, common.legend = T)


