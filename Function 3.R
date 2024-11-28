# Simulate Multiple DVI Thresholds
simulate_multiple_dvi_thresholds <- function(ticker, start_date, end_date, low_threshold, high_threshold, increment = 0.01) {
  # Create a sequence of DVI thresholds from low to high with the given increment
  dvi_thresholds <- seq(low_threshold, high_threshold, by = increment)
  
  # Create an empty list to store results for each threshold
  all_results <- list()
  
  # Loop through each DVI threshold and perform backtest
  for (threshold in dvi_thresholds) {
    # Call Function 1 to run the backtest for the current DVI threshold
    result <- base_level_backtest(ticker, start_date, end_date, threshold)
    
    # Store the results in the list
    all_results[[as.character(threshold)]] <- result
  }
  
  # Create a data frame to store the summarized results
  summary_df <- data.frame(
    DVI_Threshold = numeric(),
    Total_Long_Trades = numeric(),
    Total_Short_Trades = numeric(),
    Cumulative_Return = numeric()
  )
  
  # Loop over all the results and extract relevant data
  for (threshold in names(all_results)) {
    result <- all_results[[threshold]]
    
    # Add the threshold results to the summary data frame
    summary_df <- rbind(summary_df, data.frame(
      DVI_Threshold = as.numeric(threshold),
      Total_Long_Trades = result$Total_Long_Trades,
      Total_Short_Trades = result$Total_Short_Trades,
      Cumulative_Return = result$Cumulative_Return
    ))
  }
  
  # Plot the results using ggplot2
  plot <- ggplot(summary_df, aes(x = DVI_Threshold, y = Cumulative_Return)) +
    geom_line(color = "steelblue", size = 1) +
    geom_point(color = "red") +
    # Add data labels to the points
    geom_text(aes(label = round(Cumulative_Return, 4)), vjust = -0.5, color = "black", size = 3) +
    labs(title = "Cumulative Return vs DVI Threshold", x = "DVI Threshold", y = "Cumulative Return") +
    theme_minimal()
  
  # Explicitly print the plot to ensure it is displayed
  print(plot)
  
  # Return the summary table
  return(summary_df)
}

# Example: Run simulation for "JNJ" from 2014-01-01 to 2017-12-31, testing DVI thresholds from 0.4 to 0.6 with increments of 0.01
dvi_results <- simulate_multiple_dvi_thresholds("JNJ", "2014-01-01", "2017-12-31", 0.4, 0.6, 0.01)
print(dvi_results)
