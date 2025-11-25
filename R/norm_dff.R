norm_dff <- function(df) {
  
  df %>%
    filter(stimulus != "no stimulus") %>%
    ungroup() %>%
    select(fly_id, dff) %>% 
    group_by(fly_id) %>% 
    summarise(normalizator = mean(dff),  
              .groups = "drop") %>%
    select_all() -> normalizator
  
  df %>%
    left_join(normalizator, by = 'fly_id') %>%
    mutate(dff = dff/normalizator) %>%
    select(-c(normalizator)) %>% 
    select_all() -> df_normalized
  
  
  return(df_normalized)
}
