product_disctionary <-
function(data,
                                .document = mode_description){
  document_expr <- enquo(.document)
  out_tbl <- data %>% 
      unnest_tokens(terms, !! document_expr, token = "words") %>% 
  mutate(terms = hunspell::hunspell_stem(terms)) %>% 
  unnest(terms) %>% 
  # Remove stop words 
  anti_join(stop_words, by = c("terms" = "word")) %>% 
  filter(!terms %in% colours()) %>% 
  filter(!terms %>% str_detect(pattern = "[0-9]")) %>% 
  
  mutate(n = 1)
  return(out_tbl)
}
