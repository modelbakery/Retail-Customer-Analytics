plot_kmeans_scree <-
function(kmeans_map_data, .metric = tot.withinss){
  
  metric_expr <- enquo(.metric)
  
  g <- kmeans_map_data %>% 
    unnest(glance) %>% 
    ggplot(aes(x = centers, y = !! metric_expr), colour = "#2c3e50") +
    geom_point(colour = "#2c3e50", size = 3) +
    geom_line(colour = "#2c3e50", size = 1) +
    ggrepel::geom_label_repel(aes(label = centers), colour = "#2c3e50", size = 4) +
    theme_tq()
  
  return(g)
  
}

kmeans_mapper <-
function(center = 3, data = X_train) {
    set.seed(42)
    data %>%
        kmeans(centers = center, nstart = 20)
}
