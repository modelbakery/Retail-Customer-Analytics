#==============================================================================|
# Visualisation Plot 
#==============================================================================|
# The following plots are stored in here:
# 1. Cummulative Plot: Useful for filtering out row elements by cumulative sum 
#    -> removing long tail. 
# 2. Lollipop Plot: Convey more information than the bar plot 
# 3. Heatmap Plot: Great for showing details in 3 dimensions 
#    -> correlation analysis 


# 1.1. Get Cumulative Frequency Table 
# -- Format data to cumulative Stats 
# -- Label Priority row by setting the maximum cumulative frequency 
# -- Add text label for visualisation 
get_rank_cumulative <-
function(data, .cumulative_max = 0.5){
    out_tbl <- data %>% 
        arrange(desc(n)) %>% 
        mutate(
            pct = n/ sum(n),
            cumulative_pct = cumsum(pct),
            priority = ifelse(cumulative_pct <= .cumulative_max, TRUE, FALSE)
        ) %>% 
        rowid_to_column(var = "rank") %>% 
        mutate(label_text = str_glue("Rank: {rank}
                                 Count: {n}
                                 Pct: {scales::percent(pct)}
                                 cumulative Pct: {scales::percent(cumulative_pct)}"))
    return(out_tbl)
}
# 1.2. Cumulative Curve Plot 
# -- Set maxmium slicing index 
# -- plotly embedded 
plot_lollipop <-
function(data, X_var = terms, Y_var = pct, point_size = n,
                          fct_reorder = FALSE, fct_rev = FALSE, facet_wrap = FALSE){
    
    X_expr <- enquo(X_var)
    Y_expr <- enquo(Y_var)
    
    # Factor Ordering 
    if(fct_reorder){
        data <- data %>% 
            mutate(
                terms = terms %>% as_factor() %>% fct_reorder(!! Y_expr)
                )
    }
    if(fct_rev){
        data <- data %>% 
            mutate(
                terms = terms %>% as_factor() %>% fct_reorder(!! Y_expr)
                )
    }
    
    point_size_expr <- enquo(point_size)
    
    # Geometrics
    g <- data %>% 
        ggplot(aes(!! X_expr, !! Y_expr)) +
        geom_segment(aes(xend = !! X_expr, yend = 0), size = 0.5) +
        geom_point(aes(size = !! point_size_expr)) +
        coord_flip() +
        theme_tq() +
        theme(
            legend.position = "none",
            plot.title = element_text(face = "bold"),
            plot.caption = element_text(face = "bold.italic")
        )
    
    return(g)
}
# 2. Lollipop Plot
plot_lollipop <-
function(data, X_var = terms, Y_var = pct, point_size = n,
                          fct_reorder = FALSE, fct_rev = FALSE, facet_wrap = FALSE){
    
    X_expr <- enquo(X_var)
    Y_expr <- enquo(Y_var)
    
    # Factor Ordering 
    if(fct_reorder){
        data <- data %>% 
            mutate(
                terms = terms %>% as_factor() %>% fct_reorder(!! Y_expr)
                )
    }
    if(fct_rev){
        data <- data %>% 
            mutate(
                terms = terms %>% as_factor() %>% fct_reorder(!! Y_expr)
                )
    }
    
    point_size_expr <- enquo(point_size)
    
    # Geometrics
    g <- data %>% 
        ggplot(aes(!! X_expr, !! Y_expr)) +
        geom_segment(aes(xend = !! X_expr, yend = 0), size = 0.5) +
        geom_point(aes(size = !! point_size_expr)) +
        coord_flip() +
        theme_tq() +
        theme(
            legend.position = "none",
            plot.title = element_text(face = "bold"),
            plot.caption = element_text(face = "bold.italic")
        )
    
    return(g)
}

# plot_hist_facet.R ----
# -- Plot Faceted Histogram Function 

# Operations:
## -- Generate Faceted Histogram to all type of data

plot_hist_facet <- function(data, bins = 10,  ncol = 5, 
                            fct_reorder = FALSE, fct_rev = FALSE, 
                            fill = palette_light()[[3]], 
                            color = "white", scale = "free") {
    
    data_factored <- data %>%
        # To operate factor as well as character features 
        mutate_if(is.character, as.factor) %>%
        # Convert to numeric data to operate binning 
        mutate_if(is.factor, as.numeric) %>%
        # Long format to make facets  
        # factor_key = TRUE to store key values as a factor 
        # -- Preserve original ordering of the columns 
        gather(key = key, value = value, factor_key = TRUE) 
    
    if (fct_reorder) {
        # Output Features in alphabetically order 
        # as.character() -> as.factor() arranges factors alphabetically  
        data_factored <- data_factored %>%
            mutate(key = as.character(key) %>% as.factor())
    }
    
    if (fct_rev) {
        # Output Features in a reverse alphabetically order 
        data_factored <- data_factored %>%
            mutate(key = fct_rev(key))
    }
    
    g <- data_factored %>%
        ggplot(aes(x = value, group = key)) +
        geom_histogram(bins = bins, fill = fill, color = color) +
        facet_wrap(~ key, ncol = ncol, scale = scale) + 
        theme_tq()
    
    return(g)
    
}

# Correlation_Function.R ----
# -- get_cor
# -- plot_cor

# quoo() and enquo() do the exact same thing. The only difference 
# is that quo() is used outside of functions and enquo() is used 
# inside.

# pairwise.complete.obs:
# -- Arugment of cor(). Not the default, but is almost always what 
# you want to do. If you use = "everything" (default), then you run the 
# risk of getting errors from missing values. 
# -- Any NA values will be ignored 

get_cor <- function(data, target, use = "pairwise.complete.obs",
                    fct_reorder = FALSE, fct_rev = FALSE) {
    
    feature_expr <- enquo(target)
    feature_name <- quo_name(feature_expr)
    
    data_cor <- data %>% 
        mutate_if(is.character, as.factor) %>% 
        mutate_if(is.factor, as.numeric) %>% 
        cor(use = use) %>% 
        as.tibble() %>% 
        mutate(feature = names(.)) %>% 
        select(feature, !! feature_expr) %>% 
        # remove target var row -> 1.00 correlation/not informative 
        filter(!(feature == feature_name)) %>% 
        mutate_if(is.character, as_factor)
    
    if(fct_reorder){
        data_cor <- data_cor %>% 
            mutate(feature = fct_reorder(feature, !! feature_expr)) %>% 
            arrange(feature)
    }
    
    if(fct_rev){
        data_cor <- data_cor %>% 
            mutate(feature = fct_rev(feature, !! feature_expr)) %>% 
            arrange(feature)
    }
    return(data_cor)
}


plot_cor <- function(data, target, fct_reorder = FALSE, fct_rev = FALSE, 
                     include_lbl = TRUE, lbl_precision = 2, lbl_position = "outward",
                     size = 2, line_size = 1, vert_size = 1, 
                     color_pos = palette_light()[[1]], 
                     color_neg = palette_light()[[2]]) {
    
    feature_expr <- enquo(target)
    feature_name <- quo_name(feature_expr)
    
    data_cor <- data %>%
        get_cor(!! feature_expr, fct_reorder = fct_reorder, fct_rev = fct_rev) %>%
        mutate(feature_name_text = round(!! feature_expr, lbl_precision)) %>%
        mutate(Correlation = case_when(
            (!! feature_expr) >= 0 ~ "Positive",
            TRUE                   ~ "Negative") %>% as.factor())
    
    g <- data_cor %>%
        ggplot(aes_string(x = feature_name, y = "feature", group = "feature")) +
        geom_point(aes(color = Correlation), size = size) +
        geom_segment(aes(xend = 0, yend = feature, color = Correlation), size = line_size) +
        geom_vline(xintercept = 0, color = palette_light()[[1]], size = vert_size) +
        expand_limits(x = c(-1, 1)) +
        theme_tq() +
        scale_color_manual(values = c(color_neg, color_pos)) 
    
    if (include_lbl) g <- g + geom_label(aes(label = feature_name_text), hjust = lbl_position)
    
    return(g)
    
}


## Plot Facet Radar Chart -> Visualising Clustered Components 
plot_facet_radar <- function(.data, colour = NULL,.facet_vars = NULL){
    
    colour_expr <- enquo(colour)
    facet_expr <- enquo(.facet_vars)
    
    g <- .data %>% 
        ggRadar(aes(colour = !! colour_expr), size = 1) + 
        facet_wrap(vars(!! facet_expr)) +
        theme_minimal() +
        scale_colour_viridis_d(begin = 0.25) +
        scale_fill_viridis_d(begin = 0.25) +
        labs(title = "Customer Purchase Morphology") +
        theme(
            plot.background  = element_rect(fill = "black"),
            panel.background = element_rect(fill = "black"),
            legend.position = "none",
            text = element_text(colour = "white", size = 10),
            axis.text = element_text(colour = "grey80", size = 10)
        )
    
    return(g)
}

