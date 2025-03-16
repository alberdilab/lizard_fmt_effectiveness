#dbRDA

######CAP plots - wild-acclimation between Populations

wild_accli<- sample_metadata %>%
  filter(time_point %in% c("Acclimation", "Wild"))

samples_wild_accli <- sample_metadata %>%
  filter(time_point %in% c("Acclimation", "Wild")) %>% 
  select(Tube_code) %>% 
  pull()

neutral_accli <- as.matrix(beta_q1n$S)
neutral_accli <- as.dist(neutral_accli[rownames(neutral_accli) %in% samples_wild_accli,
                                       colnames(neutral_accli) %in% samples_wild_accli])

wild_accli$time_point <- relevel(wild_accli$time_point, ref = "Wild")
wild_accli$Population<-as.factor(wild_accli$Population)

cca_ord <- capscale(formula = neutral_accli ~ wild_accli$time_point* wild_accli$Population)


CAP_df <- as.data.frame(vegan::scores(cca_ord, display = "sites")) %>%
  rownames_to_column('Tube_code') %>%
  left_join(wild_accli, by = 'Tube_code') %>%
  column_to_rownames('Tube_code')%>%
  mutate(x_cen = mean(CAP1, na.rm = TRUE)) %>%
  mutate(y_cen = mean(CAP2, na.rm = TRUE))


biplot_scores <- as.data.frame(vegan::scores(cca_ord, display = "bp")) %>%
  rownames_to_column("Variable")
biplot_scores$Variable <- recode(biplot_scores$Variable, 
                                 "wild_accli$time_pointAcclimation" = "Acclimation",
                                 "wild_accli$PopulationHot_dry" = "Warm Population",
                                 "wild_accli$time_pointAcclimation:wild_accli$PopulationHot_dry" = "Interaction in Warm Population")

CAP_df %>%
group_by(Population, time_point) %>%
  mutate(x_cen = mean(CAP1, na.rm = TRUE)) %>%
  mutate(y_cen = mean(CAP2, na.rm = TRUE)) %>%
  ungroup() %>%
  ggplot(., aes(x=CAP1,y=CAP2, color=Population,shape = time_point)) +
  scale_color_manual(name="Population",
                     breaks=c("Cold_wet","Hot_dry"),
                     labels=c("Cold","Hot"),
                     values=c('#008080',"#d57d2c")) +
  geom_point(size=2) +
  geom_hline(yintercept = 0, linetype = "dotted") + 
  geom_vline(xintercept = 0, linetype = "dotted") +
  geom_segment(aes(x=x_cen, y=y_cen, xend=CAP1, yend=CAP2), alpha=0.2) +
  geom_segment(data = biplot_scores, aes(x = 0, y = 0, xend = CAP1, yend = CAP2),
               inherit.aes = FALSE, arrow = arrow(length = unit(0.2, "cm")), color = "black") +
  geom_text(data = biplot_scores, aes(x = CAP1, y = CAP2, label = Variable),
            inherit.aes = FALSE, color = "black", vjust = -0.5, hjust = 0.5)+
  theme_classic()


neutral_accli %>%
  metaMDS(.,trymax = 500, k=2, verbosity=FALSE, trace=FALSE) %>%
  vegan::scores() %>%
  as_tibble(., rownames = "sample") %>%
  left_join(wild_accli, by = join_by(sample == Tube_code))%>%
  group_by(Population, time_point) %>%
  mutate(x_cen = mean(NMDS1, na.rm = TRUE)) %>%
  mutate(y_cen = mean(NMDS2, na.rm = TRUE)) %>%
  ungroup() %>%
  ggplot(., aes(x=NMDS1,y=NMDS2, color=Population, shape=time_point)) +
  scale_color_manual(name="Population",
                     breaks=c("Cold_wet","Hot_dry"),
                     labels=c("Cold","Hot"),
                     values=c('#008080',"#d57d2c")) +
  geom_point(size=2) +
  geom_segment(aes(x=x_cen, y=y_cen, xend=NMDS1, yend=NMDS2), alpha=0.2) +
  labs(y = "NMDS2", x="NMDS1 \n Neutral beta diversity") +
  theme_classic() +
  theme(legend.position="none")



  