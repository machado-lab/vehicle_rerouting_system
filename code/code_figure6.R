# code figure 6 ####
data <- read.csv("data_journal/data_figure6.csv")

# The palette with grey:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# New facet label names for dose variable
vehicle_label_labs <- c("Feed", "Pigs-to-Market", "Pigs-to-Farms")
names(vehicle_label_labs) <- c("feed", "market", "pig")


fig <- data %>%
  ggplot(aes(y = n, x = NULL, fill = scenario)) + 
  geom_boxplot() +
  ylab("Number of C&D visits")+
  scale_fill_manual(values = c("Observed" = cbPalette[2], "Rerouting" = cbPalette[3]),
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
  facet_wrap(~vehicle_label,
             labeller = labeller(vehicle_label = vehicle_label_labs))

fig


# significance test
t_test_results3 <- data.frame()
for (i in unique(data$vehicle_label)) {
  
  aux_ttest <- data %>%
    filter(vehicle_label == i)
  
  # Add a Small Random Noise to avoid ties
  obs <- aux_ttest$n[aux_ttest$scenario == "Observed"] + runif(length(aux_ttest$n[aux_ttest$scenario == "Observed"]), -0.00001, 0.00001)
  est <- aux_ttest$n[aux_ttest$scenario == "Rerouting"] + runif(length(aux_ttest$n[aux_ttest$scenario == "Rerouting"]), -0.00001, 0.00001)
  
  ks_test_data1 <- ks.test(obs,
                           "pnorm",
                           mean = mean(aux_ttest$n[aux_ttest$scenario == "Observed"]),
                           sd = sd(aux_ttest$n[aux_ttest$scenario == "Observed"]))
  
  ks_test_data2 <- ks.test(est,
                           "pnorm",
                           mean = mean(aux_ttest$n[aux_ttest$scenario == "Rerouting"]),
                           sd = sd(aux_ttest$n[aux_ttest$scenario == "Rerouting"]))
  
  if(round(ks_test_data1$p.value, 6) > 0.05 & round(ks_test_data2$p.value, 6) > 0.05){
    t_test_result <- t.test(aux_ttest$n[aux_ttest$scenario == "Observed"],
                            aux_ttest$n[aux_ttest$scenario == "Rerouting"],
                            alternative = "two.sided")
    test <- "t.test"
  } else {
    
    t_test_result <- wilcox.test(aux_ttest$n[aux_ttest$scenario == "Observed"],
                                 aux_ttest$n[aux_ttest$scenario == "Rerouting"],
                                 alternative = "less")
    test <- "wilcox.test"
  }
  
  print(test)
  
  t_test_results2 <- data.frame(vehicle_label = i,
                                norm_obs = round(ks_test_data1$p.value, 6),
                                norm_sim = round(ks_test_data2$p.value, 6),
                                test = test,
                                av_obs = median(aux_ttest$n[aux_ttest$network == "Observed"]),
                                av_sim = median(aux_ttest$n[aux_ttest$network == "Rerouting"]),
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
  geom_text(data = t_test_results3, aes(x= 0, y = 33, label = p_comparison2),
            size = 6, color = "red", inherit.aes = FALSE)

fig
