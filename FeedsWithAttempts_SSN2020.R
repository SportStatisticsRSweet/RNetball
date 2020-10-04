
# Load packages
library(ggridges)
library(ggdark)
library(gghalves)
library(ggtext)
library(here)
library(viridis)
library(cowplot)
library(extrafont)
library(gganimate)
library(ggraph)
library(tidygraph)

# A little look at who feeds who?
FeedAttempts <- MatchOfInterest %>% 
  select(Period, PeriodSeconds, PlayerName, StatDescription, TeamName) %>% 
  filter((StatDescription %in% c("Feed Attempt", "Shooter Attempts From Feeds"))) %>% 
  pivot_wider(names_from = "StatDescription", values_from = "PlayerName") %>% 
  group_by(`Feed Attempt`, `Shooter Attempts From Feeds`, TeamName) %>% 
  summarise(Count = n()) %>% 
  arrange(desc(Count))
# Rename
colnames(FeedAttempts) <- c("From", "To", "Team", "Total")
# Plot
FeverFeedAttempts <- FeedAttempts %>% 
  filter(Team == "Fever") 
SwiftsFeedAttempts <- FeedAttempts %>% 
  filter(Team == "Swifts") 

# Make graph
graph <- as_tbl_graph(FeverFeedAttempts)
# Plot
ggraph(graph, layout = 'linear', circular = TRUE) +
  geom_edge_arc(
    aes(alpha = Total),
    show.legend = F,
    arrow = arrow(length = unit(4, 'mm')),
    end_cap = circle(12, 'mm'),
    color = "white") +
  geom_node_label(aes(label = name, color = name),
                  show.legend = F, label.size = 1, fill = "#291720", size = 2.5) +
  labs(x = NULL, y = NULL,
       title = "\n Who fed to who (with an attempt) at the Fever in Rd12 v Swifts? \n",
       caption = NULL) +
  dark_theme_classic() +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5, colour = "grey"),
        plot.background = element_rect(fill = "black"),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_blank(),
        text = element_text(family = "Lucida Sans"),
        axis.line.x =  element_blank(),
        axis.line.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks =  element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_blank(),
        legend.position = "none") + 
  ggsave(here(paste0(format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), type = 'cairo') 

# Make graph
graph2 <- as_tbl_graph(SwiftsFeedAttempts)
# Plot
ggraph(graph2, layout = 'linear', circular = TRUE) +
  geom_edge_arc(
    aes(alpha = Total),
    show.legend = F,
    arrow = arrow(length = unit(4, 'mm')),
    end_cap = circle(12, 'mm'),
    color = "white") +
  geom_node_label(aes(label = name, color = name),
                  show.legend = F, label.size = 1, fill = "#291720", size = 2.5) +
  labs(x = NULL, y = NULL,
       title = "\n Who fed to who (with an attempt) at the Swifts in Rd12 v Fever? \n",
       caption = NULL) +
  dark_theme_classic() +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5, colour = "grey"),
        plot.background = element_rect(fill = "black"),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_blank(),
        text = element_text(family = "Lucida Sans"),
        axis.line.x =  element_blank(),
        axis.line.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks =  element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_blank(),
        legend.position = "none") + 
  ggsave(here(paste0(format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), type = 'cairo') 