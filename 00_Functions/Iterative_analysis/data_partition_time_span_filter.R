filter_by_cohort_span <-
function(data, 
                                  .start_date = "2009-12",
                                  .end_date = "2010-12"){
    
    first_purhase_tbl <- trans_invoice_tbl %>% 
        group_by(customer_id) %>% 
        slice_min(invoice_date) %>% 
        ungroup()
    
    ids_cohort_selected <- first_purhase_tbl %>% 
    # Set Cohort Range
    filter_by_time(
        .start_date = .start_date,
        .end_date   = .end_date
    ) %>% 
    pull(customer_id) 
    
    out_tbl <- data %>% 
    filter(customer_id %in% ids_cohort_selected)
    
    return(out_tbl)
}
split_by_id <-
function(data, .size = 0.8){
    set.seed(42)
    ids_train <- data %>% 
    pull(customer_id) %>% 
    unique() %>% 
    sample(size = round(.size*length(.))) %>% 
    sort()
    
    # Train set 
    split_1_train_tbl <- cohort_selected_tbl %>% 
    filter(customer_id %in% ids_train)
    
    # Test set 
    split_1_test_tbl <- cohort_selected_tbl %>% 
    filter(!customer_id %in% ids_train) 
    
    out <- list(train = split_1_train_tbl, test = split_1_test_tbl)
    return(out)
}
split_by_time <-
function(data, .assess_days = "90"){
    split_2 <- time_series_split(
        data,
        assess = str_glue("{.assess_days} days"),
        cumulative = TRUE
    ) 
    
    # For 90-days forecast (testing) add spending label + amount 
    split_2_test_tbl<- testing(split_2) %>% 
        group_by(customer_id) %>% 
        summarise(
            "spend_{.assess_days}_total" := sum(stock_price),
            "spend_{.assess_days}_flag"  := 1
            ) 
    
    # Return training 
    split_2_train_tbl<- training(split_2) 
    
    out <- list(train = split_2_train_tbl, test = split_2_test_tbl)
    
    return(out)
}
