#plots ms

## wild vs acclimation alpha div ####

###Comparison of both populations together
alpha_div %>%
  pivot_longer(-sample, names_to = "metric", values_to = "value") %>%
  left_join(., sample_metadata, by = join_by(sample == Tube_code)) %>%
  filter(time_point == "Acclimation" | time_point == "Wild") %>%
  mutate(metric = factor(metric, levels = c("richness", "neutral", "phylogenetic"))) %>%
  ggplot(aes(y = value, x = interaction(time_point, Population), group = interaction(time_point, Population), color = Population, fill = Population)) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8), alpha = 0.5) +
  scale_color_manual(name="Population",
                     breaks=c("Cold_wet","Hot_dry"),
                     labels=c("Cold","Hot"),
                     values=c('#008080', "#d57d2c")) +
  scale_fill_manual(name="Population",
                    breaks=c("Cold_wet","Hot_dry"),
                    labels=c("Cold","Hot"),
                    values=c('#00808050', "#d57d2c50")) +
  theme_minimal() +
  facet_grid(metric ~ Population, scales = "free_y")+
  stat_compare_means(size=3) +
  geom_line(aes(group=individual))+
  labs(x = "Time Point", y = "value", color = "Population", fill = "Population")


alpha_div %>%
  pivot_longer(-sample, names_to = "metric", values_to = "value") %>%
  left_join(., sample_metadata, by = join_by(sample == Tube_code)) %>%
  filter(time_point == "Acclimation" | time_point == "Wild") %>%
  mutate(metric = factor(metric, levels = c("richness", "neutral", "phylogenetic"))) %>%
  ggplot(aes(y = value, x = interaction(time_point, Population), group = interaction(time_point, Population), color = Population, fill = Population)) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8), alpha = 0.5) +
  facet_grid(metric ~ Population, scales = "free_y")+
  scale_color_manual(name="Population",
                     breaks=c("Cold_wet","Hot_dry"),
                     labels=c("Cold","Hot"),
                     values=c('#008080', "#d57d2c")) +
  scale_fill_manual(name="Population",
                    breaks=c("Cold_wet","Hot_dry"),
                    labels=c("Cold","Hot"),
                    values=c('#00808050', "#d57d2c50")) +
  theme_minimal() +
  theme(axis.text.x=element_text(size=10))+
  labs(x = "Time Point", y = "value")



#antibiotics
## Antibiotics vs acclimation alpha div ####

###Comparison of both populations together
p1<-alpha_div %>%
  pivot_longer(-sample, names_to = "metric", values_to = "value") %>%
  left_join(., sample_metadata, by = "sample") %>%
  mutate(metric=factor(metric,levels=c("richness","neutral"))) %>%
  filter(metric!="phylogenetic") %>%
  filter(time_point == "Antibiotics" ) %>% 
  ggplot(aes(y = value, x = Population, color=Population, fill=Population)) +
  geom_jitter(width = 0.2, show.legend = FALSE) +
  geom_boxplot(width = 0.5, alpha=0.5, outlier.shape = NA, show.legend = FALSE) +
  scale_color_manual(values=treatment_colors)+
  scale_fill_manual(values=treatment_colors) +
  facet_wrap(. ~ metric, scales = "free") +
  stat_compare_means(method = "wilcox.test", show.legend = F, size = 3, label.x = c(1.5))+
  theme_classic() +
  theme(
    strip.background = element_blank(),
    panel.grid.minor.x = element_line(size = .1, color = "grey"),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size=10),
    axis.text.x = element_text(angle = 45, hjust = 1),
    # Increase plot size
    plot.title = element_text(size = 10),
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 8)
  ) +
  ylab("Alpha diversity")

p2<-alpha_div %>%
  pivot_longer(-sample, names_to = "metric", values_to = "value") %>%
  left_join(., sample_metadata, by = "sample") %>%
  mutate(metric=factor(metric,levels=c("phylogenetic"))) %>%
  filter(time_point == "Antibiotics" ) %>% 
  ggplot(aes(y = value, x = Population, color=Population, fill=Population)) +
  geom_jitter(width = 0.2, show.legend = FALSE) +
  geom_boxplot(width = 0.5, alpha=0.5, outlier.shape = NA, show.legend = FALSE) +
  scale_color_manual(values=treatment_colors)+
  scale_fill_manual(values=treatment_colors) +
  stat_compare_means(method = "t.test", show.legend = F, size = 3, label.x = c(1.5))+
  theme_classic() +
  theme(
    strip.background = element_blank(),
    panel.grid.minor.x = element_line(size = .1, color = "grey"),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size=10),
    axis.text.x = element_text(angle = 45, hjust = 1),
    # Increase plot size
    plot.title = element_text(size = 10),
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 8)
  ) +
  ylab("Alpha diversity")

ggarrange(p1,p2, col=2, row=1, label=TRUE)

#antibiotics effect on cold-adapted lizards

###Comparison of both populations together
alpha_div %>%
  pivot_longer(-sample, names_to = "metric", values_to = "value") %>%
  left_join(., sample_metadata, by = join_by(sample == Tube_code)) %>%
  filter(time_point == "Acclimation" | time_point == "Antibiotics") %>%
  mutate(metric = factor(metric, levels = c("richness", "neutral", "phylogenetic"))) %>%
  ggplot(aes(y = value, x = time_point, color = time_point, fill = time_point)) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8), alpha = 0.5) +
  theme_minimal() +
  facet_wrap (~ metric , scales = "free_y")+
  stat_compare_means(size=3) +
  geom_line(aes(group=individual))+
  labs(x = "Time Point", y = "value", color = "time_point", fill = "time_point")

alpha_div %>%
  pivot_longer(-sample, names_to = "metric", values_to = "value") %>%
  left_join(., sample_metadata, by = "sample") %>%
  mutate(metric=factor(metric,levels=c("richness","neutral","phylogenetic"))) %>%
  filter(Population == "Cold_wet" & time_point %in% c("Acclimation", "Antibiotics") ) %>% 
  ggplot(aes(y = value, x = time_point, color=time_point, fill=time_point)) +
  geom_jitter(width = 0.2, show.legend = FALSE) +
  geom_boxplot(width = 0.5, alpha=0.5, outlier.shape = NA, show.legend = FALSE) +
  scale_color_manual(values=treatment_colors)+
  scale_fill_manual(values=treatment_colors) +
  facet_wrap(. ~ metric, scales = "free") +
  theme_classic() +
  theme(
    strip.background = element_blank(),
    panel.grid.minor.x = element_line(size = .1, color = "grey"),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size=10),
    axis.text.x = element_text(angle = 45, hjust = 1),
    # Increase plot size
    plot.title = element_text(size = 10),
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 8)
  ) +
  geom_line(aes(group=individual))+
  ylab("Alpha diversity")


#donor samples vs acclimation

p1<-richness %>%
  vegan::metaMDS(., trymax = 500, k = 2, trace=0) %>%
  vegan::scores() %>%
  as_tibble(., rownames = "sample") %>%
  dplyr::left_join(subset_meta, by = join_by(sample == sample)) %>%
  group_by(treatment) %>%
  mutate(x_cen = mean(NMDS1, na.rm = TRUE)) %>%
  mutate(y_cen = mean(NMDS2, na.rm = TRUE)) %>%
  ungroup() %>%
  ggplot(aes(x = NMDS1, y = NMDS2, color = treatment)) +
  geom_point(size = 4) +
  # scale_color_manual(values = treatment_colors,labels=c("Cold_wet" = "Cold wet", "Hot_dry" = "Hot dry")) +
  geom_segment(aes(x = x_cen, y = y_cen, xend = NMDS1, yend = NMDS2), alpha = 0.9, show.legend = FALSE) +
  theme(
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    panel.background = element_blank(),
    axis.line = element_line(size = 0.5, linetype = "solid", colour = "black"),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 12),
    legend.position = "right", legend.box = "vertical"
  )

p2<-neutral %>%
  vegan::metaMDS(., trymax = 500, k = 2, trace=0) %>%
  vegan::scores() %>%
  as_tibble(., rownames = "sample") %>%
  dplyr::left_join(subset_meta, by = join_by(sample == sample)) %>%
  group_by(treatment) %>%
  mutate(x_cen = mean(NMDS1, na.rm = TRUE)) %>%
  mutate(y_cen = mean(NMDS2, na.rm = TRUE)) %>%
  ungroup() %>%
  ggplot(aes(x = NMDS1, y = NMDS2, color = treatment)) +
  geom_point(size = 4) +
  # scale_color_manual(values = treatment_colors,labels=c("Cold_wet" = "Cold wet", "Hot_dry" = "Hot dry")) +
  geom_segment(aes(x = x_cen, y = y_cen, xend = NMDS1, yend = NMDS2), alpha = 0.9, show.legend = FALSE) +
  theme(
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    panel.background = element_blank(),
    axis.line = element_line(size = 0.5, linetype = "solid", colour = "black"),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 12),
    legend.position = "right", legend.box = "vertical"
  )

p3<-phylo %>%
  vegan::metaMDS(., trymax = 500, k = 2, trace=0) %>%
  vegan::scores() %>%
  as_tibble(., rownames = "sample") %>%
  dplyr::left_join(subset_meta, by = join_by(sample == sample)) %>%
  group_by(treatment) %>%
  mutate(x_cen = mean(NMDS1, na.rm = TRUE)) %>%
  mutate(y_cen = mean(NMDS2, na.rm = TRUE)) %>%
  ungroup() %>%
  ggplot(aes(x = NMDS1, y = NMDS2, color = treatment)) +
  geom_point(size = 4) +
  geom_segment(aes(x = x_cen, y = y_cen, xend = NMDS1, yend = NMDS2), alpha = 0.9, show.legend = FALSE) +
  theme(
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    panel.background = element_blank(),
    axis.line = element_line(size = 0.5, linetype = "solid", colour = "black"),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 12),
    legend.position = "right", legend.box = "vertical"
  )

ggarrange(p1, p2, p3, ncol=3, nrow=1, common.legend = TRUE, legend="right")
