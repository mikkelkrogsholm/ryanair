parse_json_url <- function(url){
  
  data <- jsonlite::fromJSON(url)
  
  dates <- data$trips$dates[[1]]
  
  flight_info <- dates %>%
    tibble::as_tibble() %>%
    dplyr::mutate(n = purrr::map_int(flights, nrow)) %>%
    dplyr::filter(n > 0) %>%
    dplyr::select(-n) %>%
    dplyr::mutate(info = purrr::map(flights, function(x){
      faresLeft <- x$faresLeft  
      fareClass <- x$regularFare$fareClass
      amount <- x$regularFare$fares[[1]]$amount
      time_up <- x$time[[1]][1]
      time_down <- x$time[[1]][2]
      
      tibble::tibble(faresLeft, fareClass, amount, time_up, time_down)
    })) %>%
    tidyr:::unnest(info) %>%
    dplyr::select(time_up, time_down, amount, faresLeft)
  
  return(flight_info)
}
get_times <- function(org, dest, date_from = Sys.Date(), date_to = Sys.Date() + 28) {
  
  url_raw <- "https://desktopapps.ryanair.com/v4/da-dk/availability?ADT=1&CHD=0&DateOut=2019-08-27&Destination=VIE&FlexDaysOut=6&INF=0&IncludeConnectingFlights=true&Origin=CPH&RoundTrip=false&TEEN=0&ToUs=AGREED&exists=false&promoCode="
  url_glue <- "https://desktopapps.ryanair.com/v4/da-dk/availability?ADT=1&CHD=0&DateOut={dates}&Destination={destx}&FlexDaysOut=6&INF=0&IncludeConnectingFlights=true&Origin={orgx}&RoundTrip=false&TEEN=0&ToUs=AGREED&exists=false&promoCode="
  
  dates <- seq.Date(from = as.Date(date_from), to = as.Date(date_to), by = 6)
  
  ##
  m <- glue::glue("Making {length(dates) * 2} API calls to cover your date range")
  message(m)
  ##
  
  orgx <- org
  destx <- dest

  urls <- glue::glue(url_glue)
  df_out <- purrr::map_dfr(urls, parse_json_url) %>% dplyr::distinct()
  
  ##
  m <- glue::glue("Done with outbound fligths")
  message(m)
  ##
  
  orgx <- dest 
  destx <- org
  urls <- glue::glue(url_glue)
  df_back <- purrr::map_dfr(urls, parse_json_url) %>%  dplyr::distinct()
  
  ##
  m <- glue::glue("Done with homebound fligths")
  message(m)
  ##
  
  out <- df_out %>% dplyr::select(time_out = time_up, amount_out = amount)
  back <- df_back %>% dplyr::select(time_back = time_up, amount_back = amount)
  
  df <- tidyr::crossing(out, back) %>%
    dplyr::mutate(amount = amount_out + amount_back) %>%
    dplyr::select(time_out, time_back, amount) %>%
    dplyr::filter(time_out < time_back)
  
  ##
  m <- glue::glue("Returning possible flight combinations and price")
  message(m)
  ##
  
  return(df)
}
plot_df <- function(df, max_days, min_days, max_amount){
  
  plot_data <- df %>%
    dplyr::mutate(total_time = lubridate::ymd_hms(time_back) - lubridate::ymd_hms(time_out),
                  total_days = as.numeric(total_time) / 24) %>%
    dplyr::filter(total_days <= max_days,
                  total_days >= min_days,
                  amount <= max_amount)
  
  
  date_range <- lubridate::as_date(range(c(plot_data$time_out, plot_data$time_back)))
  dates <- seq.Date(date_range[1], date_range[2], by = "day")
  
  w_df <- tibble::tibble(dates) %>%
    dplyr::mutate(wd = lubridate::wday(dates, week_start = 1),
                  wend = wd > 5, 
                  w = lubridate::isoweek(dates)) %>% 
    dplyr::filter(wend) %>%
    tidyr::spread(wd, dates) %>%
    dplyr::select(sat = `6`, sun = `7`)
  
  
  ggplot2::ggplot(plot_data) + 
    ggplot2::geom_rect(data = w_df, ggplot2::aes(xmin = sat, xmax = sun, ymin = -Inf, ymax = Inf),
                       fill = "lightblue", alpha = .5) +
    ggplot2::geom_segment(ggplot2::aes(x = as.Date(time_out), 
                                       xend = as.Date(time_back), 
                                       y = amount,
                                       yend = amount)) +
    ggplot2::geom_point(ggplot2::aes(x = as.Date(time_out),
                                     y = amount)) +
    ggplot2::geom_point(ggplot2::aes(x = as.Date(time_back),
                                     y = amount)) +
    ggrepel::geom_text_repel(ggplot2::aes(x = as.Date(time_out),
                                          y = amount,
                                          label = as.character(as.Date(time_out)))) +
    ggplot2::scale_x_date(date_minor_breaks = "1 day") +
    ggplot2::labs(y = "Flight Price", x = "") +
    ggplot2::theme_minimal() 
  
}


