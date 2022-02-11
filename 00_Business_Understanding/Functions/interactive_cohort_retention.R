################################################################################
# Cohort Retention Functions for Interactive Dashboard - RShiny App
################################################################################
# Pull list of Cohort + "all" label for Reactive Button
generator_cohort_list_mapper <-
function(period){
    
    period_expr <- enquo(period)
    period_name <- quo_name(period_expr)
    # add label "all" -> Entire Cohort 
    tibble(cohort = "all") %>% 
        bind_rows(
          trans_invoice_tbl %>% 
                abstract_by_period(!!period_name) %>% 
                distinct(cohort)
        )
}

# 
abstract_by_period <-
function(data, period = month){
    
    period_expr <- enquo(period)
    period_name <- quo_name(period_expr)
    
    prep_tbl <- data %>% 
        
        mutate(invoice_period = floor_date(invoice_date, period_name)) %>% 
        group_by(customer_id) %>% 
        mutate(
            cohort_date   = min(invoice_period),
            cohort        = cohort_date %>% format("%Y%m") %>% as.factor(),
            lag_1_pur     = lag(invoice_period, k = 1) 
        ) %>% 
        ungroup() %>% 
        filter(invoice_period != max(invoice_period))
    
    return(prep_tbl)
    
}

abstract_by_cohort <-
function(data, cohort_label = all){
    
    cohort_group_expr <- enquo(cohort_label)
    cohort_group_name <- quo_name(cohort_group_expr)
    
    cohort_gr_list <- data %>% 
        distinct(cohort) %>% pull()
    
    # if statement: if given a specific cohort filter assigned cohort 
    if(cohort_group_name %in% cohort_gr_list){
        filter_tbl <- data %>% 
            filter(cohort == cohort_group_name) 
        
    } else if(cohort_group_name == "all"){
        filter_tbl <- data
        
    } else {
        
        return("Error: The argument 'cohort_group' outside the time frame")
        
    }
    
    return(filter_tbl)
}

abstract_cust_spend_by_period <-
function(data, period = month){
    
    period_expr <- enquo(period)
    period_name <- quo_name(period_expr)
    
    round_to_next_hundred <- function(x){
        x <- ifelse(x < 100, 100, reshape::round_any(x, 100, f = ceiling))
        return(x)
    }
    
    period_tbl <- data %>% 
        mutate(invoice_period = floor_date(invoice_date, period_name)) %>% 
        group_by(customer_id) %>% 
        summarise(
            .groups = "drop",
            cohort_date = min(invoice_period),
            cohort      = cohort_date %>% format("%Y%m"),
            no_months = n_distinct(invoice_period),
            total_spend = sum(invoice_spend),
            avg_monthly_spend = total_spend/no_months
        ) %>% 
        mutate(
            avg_monthly_spend_upper = avg_monthly_spend %>% round_to_next_hundred()
        )
    return(period_tbl)
}
get_cohort_heatmap <-
function(data){
  
  # Combine Active Users Data 
  plot_tbl <- data %>% 
    group_by(cohort_date, invoice_period) %>% 
    summarise(
      .groups = "drop",
      active_users = n_distinct(customer_id)
    ) %>% 
    
    # + Total Users Data
    inner_join(data %>% 
                 group_by(cohort_date) %>% 
                 summarise(
                   .groups = "drop",
                   total_users = n_distinct(customer_id)
                 ), by = "cohort_date") %>% 
    
    # + Average Spend Data
    inner_join(data %>% 
                 group_by(cohort_date, invoice_period) %>% 
                 summarise(
                   .groups = "drop",
                   avg_spend = mean(invoice_spend))) %>% 
    
    # + Retention Data
    mutate(retention = (active_users/total_users) %>% round(2)) %>% 
    group_by(cohort_date) %>% 
    
    # Number of K Months
    mutate(k_month = 1:n() -1) %>% ungroup() %>% 
    mutate(cohort_text_year  = cohort_date %>% year(),
           cohort_text_month = cohort_date %>% ymd() %>% months(abbreviate = TRUE),
           cohort_text       = str_c(cohort_text_month, cohort_text_year),
           label_text        = str_glue("Cohort Sales Performance in {cohort_text}:
                                            From the cohort of {total_users} customers, {active_users}, ({retention}%) made purchase in {(cohort_date + months(k_month))}.
                                            Those remaining customers spend ${avg_spend %>% round(0)} in average")) %>% 
    select(-cohort_text_year, -cohort_text_month)
  
  print(plot_tbl)
  
}
get_retention_by_status <-
function(data){
    
    # Customer Status labeling for each transcation 
    plot_tbl <- data %>% 
        mutate(cust_status = case_when(
            invoice_period == cohort_date ~ "new",
            invoice_period <= lag_1_pur + months(1) ~ "returning",
            invoice_period  > lag_1_pur + months(1) ~ "recovered",
            TRUE ~ "other")) %>% 
        ungroup() %>% 
        
        group_by(invoice_period) %>% 
        summarise(
            .groups = "drop",
            new_cust       = n_distinct(customer_id[cust_status == "new"]),
            return_cust    = n_distinct(customer_id[cust_status == "returning"]),
            recovered_cust = n_distinct(customer_id[cust_status == "recovered"])
        ) %>% 
        gather(key = cust_status, value = no_customer, -invoice_period) %>% 
        mutate(cust_status = factor(cust_status,
                                    levels = c("new_cust",
                                               "return_cust",
                                               "recovered_cust"),
                                    labels = c("New", "Returning", "Recovered")))  
    
    return(plot_tbl)
    
}
plot_heatmap <-
function(data, 
                         measure_var = active_users,
                         interactive = TRUE){
  
  var_expr <- enquo(measure_var)
  var_name <- quo_name(var_expr)
  
  g <- data %>% 
    ggplot(aes(k_month, 
               reorder(cohort_text, desc(cohort_date)),
               text = label_text,
               fill = !!var_expr)) +
    geom_raster() +
    scale_fill_continuous(
      type = "gradient",
      low = "#F7FBFF",
      high ="#08519C"
    ) +
    scale_x_continuous(
      breaks = seq(from = 1, to = 25, by = 1), 
      expand = c(0,0)
    ) +
    labs(
      x = str_glue("k-th period"),
      y = "Cohort Group",
      title = str_glue("Cohort Heatmap: {str_to_title(var_name)} by Cohort after first purchase")
    ) +
    theme_tq() +
    theme(
      legend.position = "none"
    )
  
  if(var_name == "retention"){
    g_1 <- g + geom_text(aes(
      label = scales::percent(formattable::formattable(!!var_expr),
                              suffix = "%")), 
      size = 3, 
      col = "black")
    
  } else if(var_name == "active_users"){
    g_1 <- g + geom_text(aes(label = !!var_expr), 
                         size = 3.5, 
                         col = "black")
    
  } else if(var_name == "avg_spend"){
    g_1 <- g + geom_text(aes(label = scales::dollar(!!var_expr, prefix = "$", round(1))), 
                         size = 2.5, 
                         col = "black")
  } else {
    return("Error: The argument 'period' must be either month or quarter")
  }
  
  if(interactive == TRUE){
    return(g_1 %>% 
             ggplotly(tooltip = "text") %>% layout(legend = list(orientation = 'h', y = -0.3)))
  } else {
    return(g_1)
  }
}
plot_retention_status <-
function(data, interactive = TRUE){
  
  plot_data <- data %>% 
    mutate(date_text_year  = invoice_period %>% year(),
           date_text_month = invoice_period %>% ymd() %>% months(abbreviate = TRUE),
           date_text       = str_c(date_text_month, date_text_year),
           label_text      = str_glue("{date_text}
                                       {cust_status}:{no_customer}")) %>%
    group_by(invoice_period) %>% 
    mutate(customers_cumsum = cumsum(no_customer)) %>% ungroup()
  
  g <- plot_data %>% 
    ggplot(aes(x = invoice_period, y = no_customer)) +
    geom_area(aes(fill = cust_status),
              colour = "white", size = 1,
              position = position_stack(reverse = T)) +
    geom_point(aes(y = customers_cumsum, text = label_text, colour = cust_status)) +
    scale_color_brewer("Blues", direction = -1) +
    scale_fill_brewer("Blues", direction = -1) +
    labs(
      x = str_glue("Invoice by Period"),
      y = "No. Customers",
      title = str_glue("Customer Retention Status Trend by Period")
    ) +
    theme_tq() +
    theme(legend.title = element_blank())
  
  # Plot B: Customer Retention Breakdown
  # Description: Monthly Customer Retention Proportions by status
  
  if(interactive == TRUE){
    return(ggplotly(g, tooltip = "text") %>% layout(hovermode = "x unified"))
  } else {
    return(g)
  }
  
}
plot_retention_breakdown <-
function(data, interactive = TRUE){
  
  plot_data <- data %>% 
    spread(key = cust_status, value = no_customer) %>% 
    mutate(total_customer  = New + Returning + Recovered,
           New        = New/total_customer,
           Returning  = Returning/total_customer,
           Recovered  = Recovered/total_customer) %>% 
    select(invoice_period, New:Recovered) %>% 
    gather(key = cust_status, value = prop_cust, -invoice_period) %>% 
    mutate(cust_status = factor(cust_status,
                                levels = c("New",
                                           "Returning",
                                           "Recovered"))) %>% 
    mutate(date_text_year  = invoice_period %>% year(),
           date_text_month = invoice_period %>% ymd() %>% months(abbreviate = TRUE),
           date_text       = str_c(date_text_month, date_text_year),
           label_text      = str_glue("{date_text}
                                       {cust_status}:{prop_cust}")) 

  g <- plot_data %>% 
    ggplot() +
    geom_col(aes(invoice_period, prop_cust, fill = cust_status, text = label_text), 
             position = position_stack(reverse = T)) +
    scale_fill_brewer("Blues", direction = -1) +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(
      x = str_glue("Invoice Period"),
      y = "No. Customers",
      title = str_glue("Customer Retention Breakdown Porportions by Period")
    ) +
    theme_tq() +
    theme(legend.title = element_blank())
  
  if(interactive == TRUE){
    return(ggplotly(g, tooltip = "text") %>% layout(hovermode = "x unified"))
  } else {
    return(g)
  }
  
}
plot_spending_trend <-
function(data, interactive = TRUE){
  
  spend_tbl <- data %>% 
    group_by(invoice_period) %>% 
    summarise(
      .groups     = "drop",
      total_spend = sum(invoice_spend),
      avg_spend   = mean(invoice_spend)) %>% 
    mutate(date_text_year  = invoice_period %>% year(),
           date_text_month = invoice_period %>% ymd() %>% months(abbreviate = TRUE),
           date_text       = str_c(date_text_month, date_text_year),
           label_text      = str_glue("{date_text}
                                     Total Spend: {scales::dollar(total_spend, scale = 1e-3, suffix = 'K')}
                                     Average Spend: {scales::dollar(avg_spend)}")) 
  
  ylim.prim <- c(min(spend_tbl$total_spend), max(spend_tbl$total_spend))   
  ylim.sec  <- c(min(spend_tbl$avg_spend), max(spend_tbl$avg_spend))
  
  b <- diff(ylim.prim)/diff(ylim.sec)
  a <- ylim.sec[1] - b*ylim.sec[1]
  
  trans <- ~ ((. - a)/b)
  
  g <- spend_tbl %>% 
    ggplot(aes(x = invoice_period)) +
    geom_col(aes(y = total_spend, text = label_text), fill = "#4292C6") +
    geom_line(aes(y = a + avg_spend*b), col = "#08306B", size = 2.5) +
    scale_y_continuous(labels = dollar_format(scale = 1e-3, suffix = "K"),
                       sec.axis = sec_axis(~ (. - a)/b, name = "Average Spend ($)")) +
    labs(
      x = str_glue("Invoice Period"),
      y = "Total Spend",
      title = str_glue("Spending Trends by Period")
    ) +
    theme_tq() +
    theme(
      axis.line.y.left   = element_line(color = "#4292C6"), 
      axis.ticks.y.left  = element_line(color = "#4292C6"),
      axis.text.y.left   = element_text(color = "#4292C6"), 
      axis.line.y.right  = element_line(color = "#08306B"), 
      axis.ticks.y.right = element_line(color = "#08306B"),
      axis.text.y.right  = element_text(color = "#08306B")
    ) 
  
  if(interactive == TRUE){
    return(ggplotly(g, tooltip = "text") %>% layout(hovermode = "x unified"))
  } else {
    return(g)
  }
  
}


plot_spending_distribution <-
function(data){

    g <- data %>% 
        filter(!avg_monthly_spend > 4000) %>% 
        ggplot(aes(x = avg_monthly_spend_upper)) +
        geom_bar(fill = "#4292C6") +
        scale_x_continuous(labels = scales::dollar_format(scale = 1e-3, suffix = "K")) +
        labs(
            x = str_glue("Average Period Spend"),
            y = "No. Customers",
            title = str_glue("Average Period Spend Distribution")
        ) +
        theme_tq()
    
    return(g)
    
}
