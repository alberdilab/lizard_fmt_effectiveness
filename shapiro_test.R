#shapiro test for alpha div

#effect of captivity in cold

alpha_div %>%
  pivot_longer(-sample, names_to = "metric", values_to = "value") %>%
  left_join(., sample_metadata, by = join_by(sample == Tube_code)) %>%
  filter(Population == "Cold_wet" & time_point %in% c("1_Acclimation", "0_Wild") ) %>% 
  filter(metric=="richness") %>% 
  summarize(shapiro_p_value = shapiro.test(value)$p.value) %>%
  pull(shapiro_p_value) #0.007 -->wilcox.test

alpha_div %>%
  pivot_longer(-sample, names_to = "metric", values_to = "value") %>%
  left_join(., sample_metadata, by = join_by(sample == Tube_code)) %>%
  filter(Population == "Cold_wet" & time_point %in% c("1_Acclimation", "0_Wild") ) %>% 
  filter(metric=="neutral") %>% 
  summarize(shapiro_p_value = shapiro.test(value)$p.value) %>%
  pull(shapiro_p_value) #0.8

alpha_div %>%
  pivot_longer(-sample, names_to = "metric", values_to = "value") %>%
  left_join(., sample_metadata, by = join_by(sample == Tube_code)) %>%
  filter(Population == "Cold_wet" & time_point %in% c("1_Acclimation", "0_Wild") ) %>% 
  filter(metric=="phylogenetic") %>% 
  summarize(shapiro_p_value = shapiro.test(value)$p.value) %>%
  pull(shapiro_p_value) #0.8


#effect of antibiotics in cold


alpha_div %>%
  pivot_longer(-sample, names_to = "metric", values_to = "value") %>%
  left_join(., sample_metadata, by = join_by(sample == Tube_code)) %>%
  filter(Population == "Cold_wet" & time_point %in% c("1_Acclimation", "2_Antibiotics") ) %>%
  filter(metric=="richness") %>% 
  summarize(shapiro_p_value = shapiro.test(value)$p.value) %>%
  pull(shapiro_p_value) #0.001 -->wilcox test

alpha_div %>%
  pivot_longer(-sample, names_to = "metric", values_to = "value") %>%
  left_join(., sample_metadata, by = join_by(sample == Tube_code)) %>%
  filter(Population == "Cold_wet" & time_point %in% c("1_Acclimation", "2_Antibiotics") ) %>%
  filter(metric=="neutral") %>% 
  summarize(shapiro_p_value = shapiro.test(value)$p.value) %>%
  pull(shapiro_p_value) #0.006-->wilcox test

alpha_div %>%
  pivot_longer(-sample, names_to = "metric", values_to = "value") %>%
  left_join(., sample_metadata, by = join_by(sample == Tube_code)) %>%
  filter(Population == "Cold_wet" & time_point %in% c("1_Acclimation", "2_Antibiotics") ) %>%
  filter(metric=="phylogenetic") %>% 
  summarize(shapiro_p_value = shapiro.test(value)$p.value) %>%
  pull(shapiro_p_value) #0.2

#does antibiotics remove the differences in both populations

alpha_div %>%
  pivot_longer(-sample, names_to = "metric", values_to = "value") %>%
  left_join(., sample_metadata, by = join_by(sample == Tube_code)) %>%
  filter(time_point == "2_Antibiotics" ) %>%
  filter(metric=="richness") %>% 
  summarize(shapiro_p_value = shapiro.test(value)$p.value) %>%
  pull(shapiro_p_value) #0.001-->wilcox test

alpha_div %>%
  pivot_longer(-sample, names_to = "metric", values_to = "value") %>%
  left_join(., sample_metadata, by = join_by(sample == Tube_code)) %>%
  filter(time_point == "2_Antibiotics" ) %>%
  filter(metric=="neutral") %>% 
  summarize(shapiro_p_value = shapiro.test(value)$p.value) %>%
  pull(shapiro_p_value) #0.0003-->wilcox test

alpha_div %>%
  pivot_longer(-sample, names_to = "metric", values_to = "value") %>%
  left_join(., sample_metadata, by = join_by(sample == Tube_code)) %>%
  filter(time_point == "2_Antibiotics" ) %>%
  filter(metric=="phylogenetic") %>% 
  summarize(shapiro_p_value = shapiro.test(value)$p.value) %>%
  pull(shapiro_p_value) #0.06

#similaritis in donor samples

alpha_div %>%
  pivot_longer(-sample, names_to = "metric", values_to = "value") %>%
  left_join(., sample_metadata, by = join_by(sample == Tube_code)) %>%
  filter(type =="Hot_control" & time_point %in% c( "3_Transplant1", "4_Transplant2"))%>%
  filter(metric=="richness") %>% 
  summarize(shapiro_p_value = shapiro.test(value)$p.value) %>%
  pull(shapiro_p_value) #0.8

alpha_div %>%
  pivot_longer(-sample, names_to = "metric", values_to = "value") %>%
  left_join(., sample_metadata, by = join_by(sample == Tube_code)) %>%
  filter(type =="Hot_control" & time_point %in% c( "3_Transplant1", "4_Transplant2"))%>%
  filter(metric=="neutral") %>% 
  summarize(shapiro_p_value = shapiro.test(value)$p.value) %>%
  pull(shapiro_p_value) #0.8

alpha_div %>%
  pivot_longer(-sample, names_to = "metric", values_to = "value") %>%
  left_join(., sample_metadata, by = join_by(sample == Tube_code)) %>%
  filter(type =="Hot_control" & time_point %in% c( "3_Transplant1", "4_Transplant2"))%>%
  filter(metric=="phylogenetic") %>% 
  summarize(shapiro_p_value = shapiro.test(value)$p.value) %>%
  pull(shapiro_p_value)#0.14

#does donor and acclimation samples stay the same?

sample_metadata_transplant <- sample_metadata %>%
  mutate(time_point=case_when(
    time_point %in% c("3_Transplant1", "4_Transplant2") ~ "Transplant",
    TRUE ~ time_point
  ))

alpha_div %>%
  pivot_longer(-sample, names_to = "metric", values_to = "value") %>%
  left_join(., sample_metadata_transplant, by = join_by(sample == Tube_code)) %>%
  filter(type == "Hot_control" & time_point %in% c("1_Acclimation","Transplant"))%>%
  filter(metric=="richness") %>% 
  summarize(shapiro_p_value = shapiro.test(value)$p.value) %>%
  pull(shapiro_p_value) #0.8

alpha_div %>%
  pivot_longer(-sample, names_to = "metric", values_to = "value") %>%
  left_join(., sample_metadata_transplant, by = join_by(sample == Tube_code)) %>%
  filter(type == "Hot_control" & time_point %in% c("1_Acclimation","Transplant"))%>%
  filter(metric=="neutral") %>% 
  summarize(shapiro_p_value = shapiro.test(value)$p.value) %>%
  pull(shapiro_p_value) #0.7

alpha_div %>%
  pivot_longer(-sample, names_to = "metric", values_to = "value") %>%
  left_join(., sample_metadata_transplant, by = join_by(sample == Tube_code)) %>%
  filter(type == "Hot_control" & time_point %in% c("1_Acclimation","Transplant"))%>%
  filter(metric=="phylogenetic") %>% 
  summarize(shapiro_p_value = shapiro.test(value)$p.value) %>%
  pull(shapiro_p_value) #0.18

#difference between donor and recipient

alpha_div %>%
  pivot_longer(-sample, names_to = "metric", values_to = "value") %>%
  left_join(., sample_metadata_transplant, by = join_by(sample == Tube_code)) %>%
  filter(type == "Treatment" & time_point %in% c("1_Acclimation", "Transplant"))%>%
  filter(metric=="richness") %>% 
  summarize(shapiro_p_value = shapiro.test(value)$p.value) %>%
  pull(shapiro_p_value) #0.8

alpha_div %>%
  pivot_longer(-sample, names_to = "metric", values_to = "value") %>%
  left_join(., sample_metadata_transplant, by = join_by(sample == Tube_code)) %>%
  filter(type == "Treatment" & time_point %in% c("1_Acclimation", "Transplant"))%>%
  filter(metric=="neutral") %>% 
  summarize(shapiro_p_value = shapiro.test(value)$p.value) %>%
  pull(shapiro_p_value) #0.6

alpha_div %>%
  pivot_longer(-sample, names_to = "metric", values_to = "value") %>%
  left_join(., sample_metadata_transplant, by = join_by(sample == Tube_code)) %>%
  filter(type == "Treatment" & time_point %in% c("1_Acclimation", "Transplant"))%>%
  filter(metric=="phylogenetic") %>% 
  summarize(shapiro_p_value = shapiro.test(value)$p.value) %>%
  pull(shapiro_p_value) #0.9


#fmt change over time

alpha_div %>%
  pivot_longer(-sample, names_to = "metric", values_to = "value") %>%
  left_join(., sample_metadata, by = join_by(sample == Tube_code)) %>%
  filter(type=="Treatment" & time_point %in% c("5_Post-FMT1", "6_Post-FMT2" ))%>%
  filter(metric=="richness") %>% 
  summarize(shapiro_p_value = shapiro.test(value)$p.value) %>%
  pull(shapiro_p_value) #0.08
 
alpha_div %>%
  pivot_longer(-sample, names_to = "metric", values_to = "value") %>%
  left_join(., sample_metadata, by = join_by(sample == Tube_code)) %>%
  filter(type=="Treatment" & time_point %in% c("5_Post-FMT1", "6_Post-FMT2" ))%>%
  filter(metric=="neutral") %>% 
  summarize(shapiro_p_value = shapiro.test(value)$p.value) %>%
  pull(shapiro_p_value) #0.16

alpha_div %>%
  pivot_longer(-sample, names_to = "metric", values_to = "value") %>%
  left_join(., sample_metadata, by = join_by(sample == Tube_code)) %>%
  filter(type=="Treatment" & time_point %in% c("5_Post-FMT1", "6_Post-FMT2" ))%>%
  filter(metric=="phylogenetic") %>% 
  summarize(shapiro_p_value = shapiro.test(value)$p.value) %>%
  pull(shapiro_p_value) #0.03 -->wilcox test


#do community change after fmt compared to acclimation and antibiotics
alpha_div %>%
  pivot_longer(-sample, names_to = "metric", values_to = "value") %>%
  left_join(., sample_metadata, by = join_by(sample == Tube_code)) %>%
  filter(type=="Treatment" & time_point %in% c( "2_Antibiotics","1_Acclimation", "5_Post-FMT1"))%>%
  filter(metric=="richness") %>% 
  summarize(shapiro_p_value = shapiro.test(value)$p.value) %>%
  pull(shapiro_p_value) #0.07

alpha_div %>%
  pivot_longer(-sample, names_to = "metric", values_to = "value") %>%
  left_join(., sample_metadata, by = join_by(sample == Tube_code)) %>%
  filter(type=="Treatment" & time_point %in% c( "2_Antibiotics","1_Acclimation", "5_Post-FMT1"))%>%
  filter(metric=="neutral") %>% 
  summarize(shapiro_p_value = shapiro.test(value)$p.value) %>%
  pull(shapiro_p_value)#0.01 -->wilcox test

alpha_div %>%
  pivot_longer(-sample, names_to = "metric", values_to = "value") %>%
  left_join(., sample_metadata, by = join_by(sample == Tube_code)) %>%
  filter(type=="Treatment" & time_point %in% c( "2_Antibiotics","1_Acclimation", "5_Post-FMT1"))%>%
  filter(metric=="phylogenetic") %>% 
  summarize(shapiro_p_value = shapiro.test(value)$p.value) %>%
  pull(shapiro_p_value) #0.2


#is the gut microbiota similar to donor after fmt?

sample_metadata_fmt <- sample_metadata %>%
  mutate(time_point=case_when(
    time_point %in% c("5_Post-FMT1", "6_Post-FMT2") ~ "FMT",
    TRUE ~ time_point
  ))


alpha_div %>%
  pivot_longer(-sample, names_to = "metric", values_to = "value") %>%
  left_join(., sample_metadata_fmt, by = join_by(sample == Tube_code)) %>%
  filter(type=="Treatment" & time_point %in% c( "2_Antibiotics","1_Acclimation", "FMT"))%>%
  filter(metric=="richness") %>% 
  summarize(shapiro_p_value = shapiro.test(value)$p.value) %>%
  pull(shapiro_p_value) #0.03-->wilcox test

alpha_div %>%
  pivot_longer(-sample, names_to = "metric", values_to = "value") %>%
  left_join(., sample_metadata_fmt, by = join_by(sample == Tube_code)) %>%
  filter(type=="Treatment" & time_point %in% c( "2_Antibiotics","1_Acclimation", "FMT"))%>%
  filter(metric=="neutral") %>% 
  summarize(shapiro_p_value = shapiro.test(value)$p.value) %>%
  pull(shapiro_p_value) #0.02-->wilcox test

alpha_div %>%
  pivot_longer(-sample, names_to = "metric", values_to = "value") %>%
  left_join(., sample_metadata_fmt, by = join_by(sample == Tube_code)) %>%
  filter(type=="Treatment" & time_point %in% c( "2_Antibiotics","1_Acclimation", "FMT"))%>%
  filter(metric=="phylogenetic") %>% 
  summarize(shapiro_p_value = shapiro.test(value)$p.value) %>%
  pull(shapiro_p_value) #0.04-->wilcox test


