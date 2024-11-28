# Function 1: Base Level Backtest
base_level_backtest <- function(ticker, start_date, end_date, dvi_threshold = 0.5) {
  # Load stock data
  price_data <- get_stock_data(ticker, start_date, end_date)
  
  # Calculate DVI (using RSI as a proxy for this example)
  dvi <- RSI(Cl(price_data), n = 14) / 100  # Normalize RSI to [0,1]
  
  # Create trading signal (1 for long, -1 for short)
  signal <- Lag(ifelse(dvi < dvi_threshold, 1, -1))  # Lag to avoid look-ahead bias
  
  # Calculate daily returns and strategy returns
  daily_returns <- ROC(Cl(price_data))
  strategy_returns <- daily_returns * signal
  strategy_returns <- na.omit(strategy_returns)
  
  # Summary statistics
  long_trades <- sum(signal == 1, na.rm = TRUE)
  short_trades <- sum(signal == -1, na.rm = TRUE)
  percent_long <- mean(signal == 1, na.rm = TRUE) * 100
  percent_short <- mean(signal == -1, na.rm = TRUE) * 100
  cumulative_return <- prod(1 + strategy_returns) - 1
  
  # Results
  summary_results <- data.frame(
    Total_Long_Trades = long_trades,
    Total_Short_Trades = short_trades,
    Percent_Long = percent_long,
    Percent_Short = percent_short,
    Cumulative_Return = cumulative_return
  )
  
  # Plot the cumulative returns (explicitly print the plot)
  cumulative_equity_curve <- exp(cumsum(strategy_returns))
  
  # Print the plot explicitly
  plot(cumulative_equity_curve, type = "l", col = "blue", 
       main = "Cumulative Return from SMA Strategy", 
       xlab = "Time", ylab = "Cumulative Return")
  
  return(summary_results)
}

# Example: Run backtest for "JNJ" from 2014-01-01 to 2017-12-31 with default DVI threshold of 0.5
results <- base_level_backtest("JNJ", "2014-01-01", "2017-12-31", 0.5)
print(results)

