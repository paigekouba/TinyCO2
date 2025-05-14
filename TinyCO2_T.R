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


spot <- c("A", "B", "F", "G", "K", "L", "C", "D", "H", "I", "E", "J")
x_within <-  c(-13, 13, -13, 13, -13, 13, -26, 0, -26, 0, 26, 26)
y_within <- c(26, 26, 0, 0, -26, -26, 13, 13, -13, -13, 13, -13)
points_within <- data.frame(x_within, y_within, spot)

temp_df3 <- temp_df2 %>% # take average, per-spot, for all INSIDE and OUTSIDE (n = 4 each) x each sample time
  # inside and outside should have their own column
  pivot_longer(c(inside,outside), names_to = "in.out", values_to = "temp") %>% 
  group_by(spot, sample, in.out) %>% 
  summarise(meanT = mean(temp, na.rm = T)) %>% 
  left_join(points_within, by = "spot") 

# now make a plot as practice for looping: all 12 spots for Sample 1, inside the screen

ggplot() + # interpolated points plus measured values  (means) plus circle showing screen
  geom_point(data = filter(temp_df3, sample == 3, in.out == "outside"), aes(x = x_within, y = y_within, color = meanT), size=5, shape=15) +
  # scale_color_gradient(low = "black", high="yellow") + theme_classic() +
  scale_color_viridis_c(option="magma", limits = c(17, 68)) + theme_classic() +
  geom_text(data = filter(temp_df3, sample == 3, in.out == "outside"), aes(x=x_within, y=y_within, label = round(meanT)), size = 5) +
  ggforce::geom_circle(aes(x0=0,y0=0,r=39)) +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0)) +
  labs(color="meanT")

# Make a function that takes sample # and in.out and creates an interpolated df
library(interp)
library(akima)
T_interp_fn <- function(sample_num, in.out_num) {
  T_means <- filter(temp_df3, sample == sample_num, in.out == in.out_num)
  Tinterp <- interp(x = T_means$x_within, y = T_means$y_within, z = T_means$meanT, linear=FALSE, extrap=TRUE, xo=-39:39, yo=-39:39)
                    #linear=TRUE, extrap = TRUE, remove = FALSE) # duplicate = "mean")
  #convert this to a long form dataframe
  Tinterp_df <- expand_grid(i = seq_along(Tinterp$x),
                              j = seq_along(Tinterp$y)) %>%
    mutate(x = Tinterp$x[i],
           y = Tinterp$y[j],
           temp = map2_dbl(i, j, ~Tinterp$z[.x,.y])) %>%
    select(-i, -j)
  return(Tinterp_df)
}
# #T_interp_fn(3,"outside")

ggplot() + # interpolated points plus measured values  (means) plus circle showing screen
  geom_point(data = T_interp_fn(3,"outside"), aes(x = x, y = y, color = temp), size=1, shape=15) +
  scale_color_viridis_c(option="magma", limits = c(17, 68)) + theme_classic() +
  geom_text(data = filter(temp_df3, sample == 3, in.out == "outside"), aes(x=x_within, y=y_within, label = round(meanT)), size = 3) +
  ggforce::geom_circle(aes(x0=0,y0=0,r=39)) +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0)) +
  labs(color="temp")

T_plots <- vector("list", length=8)
for(i in c(1:8)){
  T_plots[[i]] <-
    ggplot() + # interpolated points plus measured values  (means) plus circle showing screen
    geom_point(data = T_interp_fn(i,"outside"), aes(x = x, y = y, color = temp), size=1, shape=15) +
    scale_color_viridis_c(option="magma", limits = c(17, 68)) + theme_classic() +
    geom_text(data = filter(temp_df3, sample == i, in.out == "outside"), aes(x=x_within, y=y_within, label = round(meanT)), size = 3, color="gray") +
   # ggforce::geom_circle(aes(x0=0,y0=0,r=39), color="white") +
    scale_y_continuous(expand = c(0,0)) +
    scale_x_continuous(expand = c(0,0)) +
    labs(color="temp")
}
#T_plots[[8]]

T_plots_in <- vector("list", length=8)
for(i in c(1:8)){
  T_plots_in[[i]] <-
    ggplot() + # interpolated points plus measured values  (means) plus circle showing screen
    geom_point(data = T_interp_fn(i,"inside"), aes(x = x, y = y, color = temp), size=1, shape=15) +
    scale_color_viridis_c(option="magma", limits = c(17, 68)) + theme_classic() +
    geom_text(data = filter(temp_df3, sample == i, in.out == "inside"), aes(x=x_within, y=y_within, label = round(meanT)), size = 3, color="gray") +
    ggforce::geom_circle(aes(x0=0,y0=0,r=39)) +
    scale_y_continuous(expand = c(0,0)) +
    scale_x_continuous(expand = c(0,0)) +
    labs(color="temp")
}

library(ggpubr)
ggarrange(T_plots_in[[1]],T_plots_in[[2]],T_plots_in[[3]],T_plots_in[[4]],T_plots_in[[5]],T_plots_in[[6]],T_plots_in[[7]],T_plots_in[[8]],
          T_plots[[1]],T_plots[[2]],T_plots[[3]],T_plots[[4]],T_plots[[5]],T_plots[[6]],T_plots[[7]],T_plots[[8]],
          nrow=2, ncol=8, common.legend = T)


