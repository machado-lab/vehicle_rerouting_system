# code figure 5
data <- read.csv("data_journal/data_figure5.csv")

# The palette with grey:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

fig <- data %>% 
  filter(type == "total") %>% 
  mutate(max = 25453) %>% 
  mutate(prop = n/max) %>%
  mutate(prop_reduced = round((prop - 1)*100, +1)) %>%
  mutate(prop_reduced = ifelse(prop_reduced > 0, 0, prop_reduced)) %>%
  mutate(prop_reduced = paste0(prop_reduced, "%")) %>%
  mutate(prop_reduced = ifelse(cd == 0 & network == "observed", "Base", prop_reduced)) %>%
  mutate(cd =  as.character(cd)) %>% 
  ggplot(aes(x = cd, y = n, fill = network)) +
  geom_bar(stat = "identity", position="dodge") +
  geom_text(aes(label = prop_reduced), vjust = -0.5, hjust = 0.4, colour = "black", position = position_dodge(.9)) +
  scale_fill_manual(values = c("observed" = cbPalette[2], "simulated" = cbPalette[3]),
                    labels = c("Observed", "Rerouted"),
                    name = "Vehicle") +
  scale_y_continuous(breaks = seq(0, 25000, 5000)) +
  scale_x_discrete(labels = c(
    "0" = "0%",
    "0.1" = "10%",
    "0.5" = "50%",
    "0.8" = "80%",
    "0.9" = "90%",
    "1" = "100%"
  )) +
  xlab("C&D effectiveness") +
  ylab("Number of contacts\namong different communities") +
  theme_bw() +
  theme(axis.text.x = element_text(colour="black", face="bold"),
        axis.text.y = element_text(colour="black", face="bold"),
        text = element_text(size = 21, face="bold"),
        legend.title = element_text(colour="black", size=21, face="bold"),
        legend.text = element_text(colour="black", size=21, face="bold")) 

fig
