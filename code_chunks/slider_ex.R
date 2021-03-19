# slider example

slider::Slide_dbl(count, mean, before = 7, trim = .05)



dat_in %>%
  count(date) %>%
  mutate(rolling_avg = slider::slide_dbl(.x = n,.f = weighted.mean, .before = 14)) %>% 
  
  
  
  ~mean(.x), .before = 1, .after = 1

slide_dbl(1:5, ~mean(.x), .before = 1, .after = 1)

.x = AvgElix, w = NumPosit),