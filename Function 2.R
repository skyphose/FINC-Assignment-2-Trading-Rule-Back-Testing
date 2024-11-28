# Simulate Multiple Backtest Periods
simulate_multiple_backtests <- function(ticker, testing_period, date_range, dvi_threshold = 0.5) {
  # Extract start and end years from date_range
  start_year <- as.numeric(date_range[1])
  end_year <- as.numeric(date_range[2])
  
  # Create an empty list to store the results from each period
  all_results <- list()
  
  # Create a vector to store cumulative returns for plotting
  cumulative_returns <- data.frame(Period = character(), Cumulative_Return = numeric())
  
  # Loop through each period in the range
  for (start in seq(start_year, end_year - testing_period + 1)) {
    end <- start + testing_period - 1
    period_start_date <- paste0(start, "-01-01")
    period_end_date <- paste0(end, "-12-31")
    
    # Call Function 1 to run the backtest for this period
    period_result <- base_level_backtest(ticker, period_start_date, period_end_date, dvi_threshold)
    
    # Store the results in the list
    all_results[[paste0(start, "-", end)]] <- period_result
    
    # Store the cumulative return for this period for plotting
    cumulative_returns <- rbind(cumulative_returns, data.frame(
      Period = paste0(start, "-", end),
      Cumulative_Return = period_result$Cumulative_Return
    ))
  }
  
  # Create a data frame to store the summarized results
  summary_df <- data.frame(
    Period = character(),
    Total_Long_Trades = numeric(),
    Total_Short_Trades = numeric(),
    Percent_Long = numeric(),
    Percent_Short = numeric(),
    Cumulative_Return = numeric()
  )
  
  # Loop over all the results and calculate the mean values
  for (period in names(all_results)) {
    result <- all_results[[period]]
    
    # Add the period results to the summary data frame
    summary_df <- rbind(summary_df, data.frame(
      Period = period,
      Total_Long_Trades = result$Total_Long_Trades,
      Total_Short_Trades = result$Total_Short_Trades,
      Percent_Long = result$Percent_Long,
      Percent_Short = result$Percent_Short,
      Cumulative_Return = result$Cumulative_Return
    ))
  }
  
  # Calculate the average of each metric over all periods
  summary_means <- colMeans(summary_df[, -1], na.rm = TRUE)
  summary_df <- rbind(summary_df, c("Average", summary_means))
  
  # Explicitly print the cumulative returns plot
  plot_cumulative_return <- ggplot(cumulative_returns, aes(x = Period, y = Cumulative_Return)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    labs(title = "Cumulative Return for Each Backtest Period", x = "Period", y = "Cumulative Return") +
    theme_minimal()
  
  print(plot_cumulative_return)  # Explicitly print the plot
  
  # Return the summary table
  return(summary_df)
}

# Example: Run simulation for "JNJ" with 3-year periods, data from 2010 to 2016, and DVI threshold 0.5
simulation_results <- simulate_multiple_backtests("JNJ", 3, c("2010", "2016"), 0.5)
print(simulation_results)
