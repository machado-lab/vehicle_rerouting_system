# code figure 3 and test
data <- read.csv("data_journal/data_figure3.csv")

# The palette with grey:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# New facet label names for dose variable
vehicle_label_labs <- c("Feed", "Pigs-to-Market", "Pigs-to-Farms")
names(vehicle_label_labs) <- c("feed", "market", "pig")


fig <- data %>% 
  ggplot(aes(y = rank, x = as.character(day), fill = scenario)) + 
  geom_boxplot() +
  ylab("Score")+
  xlab("Day") +
  scale_y_continuous(breaks = 1:19, limits = c(0, 19.7))+
  scale_fill_manual(values = c("Observed" = cbPalette[2], "Rerouting" = cbPalette[3]),
                    labels = c("Observed", "Rerouted"),
                    name = "Vehicle")+
  geom_hline(yintercept = 19, linetype = "dashed", color = "blue") +
  annotate("text", x = 3, y = 19.7, label = "Best vehicle", color = "black", vjust = -0.5, size = 6) +
  annotate("text", x = 3, y = 0, label = "Worst vehicle", color = "black", vjust = -0.5, size = 6) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  # geom_vline(xintercept = "32", linetype="solid", color = "gray", size=1)+
  # geom_vline(xintercept = c(1.5, 2.5, 3.5, 4.5, 5.5, 6.5),
  #            linetype="solid", color = "gray", size=1)+
  annotate("segment",
           x = c(1.5, 2.5, 3.5, 4.5, 5.5, 6.5),
           xend = c(1.5, 2.5, 3.5, 4.5, 5.5, 6.5),
           y = 1,
           yend = 19,
           linetype = "solid",
           color = "gray",
           size = 1) +
  theme_bw() +
  theme(axis.text.x = element_text(colour="black", face="bold"),
        axis.text.y = element_text(colour="black", face="bold"),
        text = element_text(size = 26, face="bold"),
        legend.title = element_text(colour="black", size=26, face="bold"),
        legend.text = element_text(colour="black", size=26, face="bold")) +
  facet_wrap(~type,
             labeller = labeller(type = vehicle_label_labs))


fig


# significance test
t_test_results3 <- data.frame()
for (i in unique(data$type)) {
  for (j in unique(unique(data$day))) {
    
    aux_ttest <- data %>%
      filter(type == i) %>% 
      filter(day == j)
    
    if(nrow(aux_ttest) > 1){
      summary(aux_ttest$rank[aux_ttest$scenario == "Observed"])
      summary(aux_ttest$rank[aux_ttest$scenario == "Rerouting"])
      
      t_test_result <- wilcox.test(aux_ttest$rank[aux_ttest$scenario == "Observed"],
                                   aux_ttest$rank[aux_ttest$scenario == "Rerouting"],
                                   alternative = "less")
      test <- "wilcox.test"
      
      
      t_test_results2 <- data.frame(type = i,
                                    day = j,
                                    test = test,
                                    av_obs = median(aux_ttest$rank[aux_ttest$scenario == "Observed"]),
                                    av_sim = median(aux_ttest$rank[aux_ttest$scenario == "Rerouting"]),
                                    p_comparison = round(t_test_result$p.value, 6)
      )
      
      
    } else{
      t_test_results2 <- data.frame(type = i,
                                    day = j,
                                    test = NA,
                                    av_obs = NA,
                                    av_sim = NA,
                                    p_comparison = NA
      )
    }
    
    
    t_test_results3 <- rbind(t_test_results3, t_test_results2)
  }  
}

t_test_results3$p_comparison2 <- NA # days without movements
t_test_results3$p_comparison2[t_test_results3$p_comparison >= 0.05] <- "ns"
t_test_results3$p_comparison2[t_test_results3$p_comparison < 0.05] <- "*"
t_test_results3$p_comparison2[t_test_results3$p_comparison < 0.01] <- "**"
t_test_results3$p_comparison2[t_test_results3$p_comparison < 0.001] <- "***"


fig <- fig +
  geom_text(data = t_test_results3, aes(x= as.character(day), y = 19.5, label = p_comparison2),
            size = 6, color = "red", inherit.aes = FALSE)

fig
