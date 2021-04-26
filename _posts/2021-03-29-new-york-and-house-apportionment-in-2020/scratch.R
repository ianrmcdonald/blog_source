  


merge_all %>%
  mutate(residual = quota - hunhill) %>% 
   filter(hunhill <= 2) %>% 
  ggplot(aes(x = residual))+ geom_histogram(stat = "density")

    #geom_abline(intercept = 0, slope = 1)



         