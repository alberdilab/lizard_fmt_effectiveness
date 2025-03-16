# Stacked area plot for taxonomic abundances

merged_data <- genome_counts_filt %>%
  mutate_at(vars(-genome),  ~ . / sum(.)) %>% #apply TSS normalisation
  pivot_longer(-genome, names_to = "sample", values_to = "count") %>% #reduce to minimum number of columns
  left_join(., genome_metadata, by = join_by(genome == genome)) %>% #append genome metadata
  left_join(., sample_metadata, by = join_by(sample == Tube_code)) %>% #append sample metadata
  filter(count > 0) #filter 0 counts



merged_tratment<-merged_data  %>%
  filter (type=="Treatment")



merged_tratment_long <- merged_tratment %>%
  pivot_longer(cols = starts_with("count"), names_to = "phylum", values_to = "count")


ggplot(merged_tratment, aes(
  x = time_point,
  y = count,
  fill = phylum,
  group = phylum
)) + #grouping enables keeping the same sorting of taxonomic units
  geom_bar(stat = "identity",
           colour = "white",
           linewidth = 0.1) + #plot stacked bars with white borders
  geom_area()+
  scale_fill_manual(values = phylum_colors) +
  facet_nested(. ~ time_point , scales = "free") + #facet per day and treatment
  guides(fill = guide_legend(ncol = 1)) +
  labs(fill = "Phylum", y = "Relative abundance", x = "Sample") +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      size = 0
    ),
    strip.text.x = element_text(
      size = 14,
      colour = "black",
      face = "bold"
    ),
    strip.background = element_rect(fill ="lightgrey"),
    axis.title = element_text(size = 18, face = "bold"),
    panel.background = element_blank(),
    legend.title = element_text(size = 20, face = "bold"),
    legend.text = element_text(size = 16)
  )


ggplot(merged_tratment, aes(
  x = time_point, # Ensure categorical x-axis
  y = count,
  fill = phylum,
  group = phylum
)) +
  geom_bar(stat = "identity",
           colour = "white",
           linewidth = 0.1) +
  geom_area(aes(group = phylum), alpha = 0.8, position = "stack") + 
  scale_fill_manual(values = phylum_colors) +
  scale_color_manual(values = phylum_colors) + # Match line color with fill
  #facet_nested(. ~ time_point, scales = "free") + 
  guides(fill = guide_legend(ncol = 1)) +
  labs(fill = "Phylum", y = "Relative abundance", x = "Sample") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 5),
    strip.text.x = element_text(size = 14, colour = "black", face = "bold"),
    strip.background = element_rect(fill = "lightgrey"),
    axis.title = element_text(size = 18, face = "bold"),
    panel.background = element_blank(),
    legend.title = element_text(size = 20, face = "bold"),
    legend.text = element_text(size = 16)
  )

ggplot(merged_tratment, aes(
  x = time_point, 
  #stratum = phylum, # Connecting across time points
  #alluvium= phylum,
  y=count,
  fill = phylum,
  label=phylum
)) +
  geom_bar(stat = "identity",
           colour = "white",
           linewidth = 0.1)+
  geom_alluvium(aes(group = phylum), alpha = 0.7, width = 0.4) +  # Smooth flows
  geom_stratum(width = 0.4, color = "white") +  # Stacked bars
  scale_fill_manual(values = phylum_colors) +
  labs(fill = "Phylum", y = "Relative abundance", x = "Time-point") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    strip.text.x = element_text(size = 14, colour = "black", face = "bold"),
    axis.title = element_text(size = 18, face = "bold"),
    legend.title = element_text(size = 20, face = "bold"),
    legend.text = element_text(size = 16)
  )


# install.packages("ggalluvial")
library(ggalluvial)

ggplot(data = merged_tratment,
       aes(axis1 = time_point, axis2 = time_point, y = count)) +
  geom_alluvium(aes(fill = phylum),
                curve_type = "quintic") +
  geom_stratum() +
  geom_text(stat = "stratum",
            aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Survey", "Response"),
                   expand = c(0.15, 0.05)) +
  theme_void() 

