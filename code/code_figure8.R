# code figure 8

data <- read.csv("data_journal/data_figure8.csv")

# The palette with grey:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# New facet label names for dose variable
vehicle_label_labs <- c("Feed", "Pigs-to-Market", "Pigs-to-Farms")
names(vehicle_label_labs) <- c("feed", "market", "pig")


fig <- data %>% 
  filter(work %in% c("pig", "market", "feed")) %>%
  filter(!is.na(sum_distances_obs) & !is.na(sum_distances_sim)) %>% 
  pivot_longer(cols = c("sum_distances_obs", "sum_distances_sim"), 
               names_to = "distance_type", 
               values_to = "distance_value") %>% 
  ggplot(aes(y = distance_value/1000, x = NULL, fill = distance_type)) +
  geom_boxplot() +
  ylab("Total distance traveled (km)")+
  scale_fill_manual(values = c("sum_distances_obs" = cbPalette[2], "sum_distances_sim" = cbPalette[3]),
                    labels = c("Observed", "Rerouted"),
                    name = "Vehicle")+
  theme_bw() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y = element_text(colour="black", face="bold"),
        text = element_text(size = 26, face="bold"),
        legend.title = element_text(colour="black", size=26, face="bold"),
        legend.text = element_text(colour="black", size=26, face="bold")) +
  facet_wrap(~work,
             labeller = labeller(work = vehicle_label_labs))

fig

# significance test
library(nortest)
library(car)
t_test_results3 <- data.frame()
for (i in c("pig", "market", "feed")) {
  
  aux_ttest <- data %>%
    filter(work %in% c("pig", "market", "feed")) %>%
    filter(!is.na(sum_distances_obs) & !is.na(sum_distances_sim)) %>% 
    pivot_longer(cols = c("sum_distances_obs", "sum_distances_sim"), 
                 names_to = "distance_type", 
                 values_to = "distance_value") %>% 
    filter(work == i)
  
  ks_test_data1 <- ad.test(aux_ttest$distance_value[aux_ttest$distance_type == "sum_distances_obs"])
  ks_test_data2 <- ad.test(aux_ttest$distance_value[aux_ttest$distance_type == "sum_distances_sim"])
  
  if(round(ks_test_data1$p.value, 6) > 0.05 & round(ks_test_data2$p.value, 6) > 0.05){
    t_test_result <- t.test(aux_ttest$distance_value[aux_ttest$distance_type == "sum_distances_obs"],
                            aux_ttest$distance_value[aux_ttest$distance_type == "sum_distances_sim"],
                            alternative = "two.sided")
    test <- "t.test"
  } else {
    
    t_test_result <- wilcox.test(aux_ttest$distance_value[aux_ttest$distance_type == "sum_distances_obs"],
                                 aux_ttest$distance_value[aux_ttest$distance_type == "sum_distances_sim"],
                                 alternative = "less")
    test <- "wilcox.test"
  }
  
  print(test)
  
  t_test_results2 <- data.frame(work = i,
                                norm_obs = round(ks_test_data1$p.value, 6),
                                norm_sim = round(ks_test_data2$p.value, 6),
                                test = test,
                                av_obs = median(aux_ttest$distance_value[aux_ttest$distance_type == "sum_distances_obs"]),
                                av_sim = median(aux_ttest$distance_value[aux_ttest$distance_type == "sum_distances_sim"]),
                                p_comparison = round(t_test_result$p.value, 6)
  )
  
  t_test_results3 <- rbind(t_test_results3, t_test_results2)
}

t_test_results3$p_comparison2 <- NA
t_test_results3$p_comparison2[t_test_results3$p_comparison >= 0.05] <- "ns"
t_test_results3$p_comparison2[t_test_results3$p_comparison < 0.05] <- "*"
t_test_results3$p_comparison2[t_test_results3$p_comparison < 0.01] <- "**"
t_test_results3$p_comparison2[t_test_results3$p_comparison < 0.001] <- "***"

fig <- fig +
  geom_text(data = t_test_results3, aes(x= 0, y = 20000, label = p_comparison2),
            size = 6, color = "red", inherit.aes = FALSE)

fig
