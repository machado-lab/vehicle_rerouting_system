# code figure 4
data <- read.csv("data_journal/data_figure4.csv")

# significance test
t_test_results3 <- data.frame()
for (i in unique(data$vehicle_label)) {
  for (j in unique(data$clean_rate)) {
    
    aux_ttest <- data %>%
      mutate(clean_rate = as.character(clean_rate)) %>%
      filter(clean_rate == j) %>% 
      filter(vehicle_label == i)
    
    if(length(aux_ttest$n[aux_ttest$network == "simulated"]) > 1){
      summary(aux_ttest$n[aux_ttest$network == "observed"])
      summary(aux_ttest$n[aux_ttest$network == "simulated"])
      
      ks_test_data1 <- ks.test(aux_ttest$n[aux_ttest$network == "observed"],
                               "pnorm",
                               mean = mean(aux_ttest$n[aux_ttest$network == "observed"]),
                               sd = sd(aux_ttest$n[aux_ttest$network == "observed"]))
      
      ks_test_data2 <- ks.test(aux_ttest$n[aux_ttest$network == "simulated"],
                               "pnorm",
                               mean = mean(aux_ttest$n[aux_ttest$network == "simulated"]),
                               sd = sd(aux_ttest$n[aux_ttest$network == "simulated"]))
      
      if(round(ks_test_data1$p.value, 6) > 0.05 & round(ks_test_data2$p.value, 6) > 0.05){
        t_test_result <- t.test(aux_ttest$n[aux_ttest$network == "observed"],
                                aux_ttest$n[aux_ttest$network == "simulated"],
                                alternative = "greater")
        test <- "t.test"
      } else {
        
        t_test_result <- wilcox.test(aux_ttest$n[aux_ttest$network == "observed"],
                                     aux_ttest$n[aux_ttest$network == "simulated"],
                                     alternative = "greater")
        test <- "wilcox.test"
      }
      
      print(test)
      
      t_test_results2 <- data.frame(vehicle_label = i,
                                    clean_rate = j,
                                    norm_obs = round(ks_test_data1$p.value, 6),
                                    norm_sim = round(ks_test_data2$p.value, 6),
                                    test = test,
                                    av_obs = median(aux_ttest$n[aux_ttest$network == "observed"]),
                                    av_sim = median(aux_ttest$n[aux_ttest$network == "simulated"]),
                                    p_comparison = round(t_test_result$p.value, 6)
      )
      
      
    } else{
      t_test_results2 <- data.frame(vehicle_label = i,
                                    clean_rate = j,
                                    norm_obs = NA,
                                    norm_sim = NA,
                                    test = NA,
                                    av_obs = NA,
                                    av_sim = NA,
                                    p_comparison = NA
      )
    }
    
    
    t_test_results3 <- rbind(t_test_results3, t_test_results2)
  }
}

t_test_results3$p_comparison2 <- NA
t_test_results3$p_comparison2[t_test_results3$p_comparison >= 0.05] <- "ns"
t_test_results3$p_comparison2[t_test_results3$p_comparison < 0.05] <- "*"
t_test_results3$p_comparison2[t_test_results3$p_comparison < 0.01] <- "**"
t_test_results3$p_comparison2[t_test_results3$p_comparison < 0.001] <- "***"


t_test_results3$clean_rate <- as.character(t_test_results3$clean_rate)



# The palette with grey:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# New facet label names for dose variable
vehicle_label_labs <- c("Feed", "Pigs-to-Market", "Pigs-to-Farms")
names(vehicle_label_labs) <- c("feed", "market", "pig")

# New facet label names for supp variable
clean_rate_labs <- c(paste("C&D = 0%"),
                     "C&D = 10%",
                     "C&D = 50%",
                     "C&D = 80%",
                     "C&D = 90%",
                     "C&D = 100%")
names(clean_rate_labs) <- c("0", "0.1", "0.5", "0.8", "0.9", "1")


# plot
data %>%
  mutate(clean_rate = as.character(clean_rate)) %>%
  ggplot(aes(y = n, x = network, fill = network)) + 
  geom_boxplot() +
  scale_fill_manual(values = c("observed" = cbPalette[2], "simulated" = cbPalette[3]),
                    labels = c("Observed", "Rerouted"),
                    name = "Vehicle")+
  ylab("Number of infectious edges")+
  xlab("Days") +
  theme_bw() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y = element_text(colour="black", face="bold"),
        text = element_text(size = 21, face="bold"),
        legend.title = element_text(colour="black", size=21, face="bold"),
        legend.text = element_text(colour="black", size=21, face="bold")) +
  facet_grid(vehicle_label~clean_rate, 
             labeller = labeller(vehicle_label = vehicle_label_labs, clean_rate = clean_rate_labs)) +
  geom_text(data = t_test_results3, aes(x= 1.5, y = 80, label = p_comparison2),
            vjust = 1.5, size = 6, color = "red", inherit.aes = FALSE)
