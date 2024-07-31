
## Algorithmic Trading Strategy

## Introduction

In this assignment, you will develop an algorithmic trading strategy by incorporating financial metrics to evaluate its profitability. This exercise simulates a real-world scenario where you, as part of a financial technology team, need to present an improved version of a trading algorithm that not only executes trades but also calculates and reports on the financial performance of those trades.

## Background

Following a successful presentation to the Board of Directors, you have been tasked by the Trading Strategies Team to modify your trading algorithm. This modification should include tracking the costs and proceeds of trades to facilitate a deeper evaluation of the algorithm’s profitability, including calculating the Return on Investment (ROI).

After meeting with the Trading Strategies Team, you were asked to include costs, proceeds, and return on investments metrics to assess the profitability of your trading algorithm.

## Objectives

1. **Load and Prepare Data:** Open and run the starter code to create a DataFrame with stock closing data.

2. **Implement Trading Algorithm:** Create a simple trading algorithm based on daily price changes.

3. **Customize Trading Period:** Choose your entry and exit dates.

4. **Report Financial Performance:** Analyze and report the total profit or loss (P/L) and the ROI of the trading strategy.

5. **Implement a Trading Strategy:** Implement a trading strategy and analyze the total updated P/L and ROI. 

6. **Discussion:** Summarise your finding.


## Instructions

### Step 1: Data Loading

Start by running the provided code cells in the "Data Loading" section to generate a DataFrame containing AMD stock closing data. This will serve as the basis for your trading decisions. First, create a data frame named `amd_df` with the given closing prices and corresponding dates. 

```r
# Load data from CSV file
amd_df <- read.csv("AMD.csv")
# Convert the date column to Date type and Adjusted Close as numeric
amd_df$date <- as.Date(amd_df$Date)
amd_df$close <- as.numeric(amd_df$Adj.Close)
amd_df <- amd_df[, c("date", "close")]
```

#### Plotting the Data
Plot the closing prices over time to visualize the price movement.
```r
plot(amd_df$date, amd_df$close,'l')
```


### Step 2: Trading Algorithm
Implement the trading algorithm as per the instructions. You should initialize necessary variables, and loop through the dataframe to execute trades based on the set conditions.

- Initialize Columns: Start by ensuring dataframe has columns 'trade_type', 'costs_proceeds' and 'accumulated_shares'.
- Change the algorithm by modifying the loop to include the cost and proceeds metrics for buys of 100 shares. Make sure that the algorithm checks the following conditions and executes the strategy for each one:
  - If the previous price = 0, set 'trade_type' to 'buy', and set the 'costs_proceeds' column to the current share price multiplied by a `share_size` value of 100. Make sure to take the negative value of the expression so that the cost reflects money leaving an account. Finally, make sure to add the bought shares to an `accumulated_shares` variable.
  - Otherwise, if the price of the current day is less than that of the previous day, set the 'trade_type' to 'buy'. Set the 'costs_proceeds' to the current share price multiplied by a `share_size` value of 100.
  - You will not modify the algorithm for instances where the current day’s price is greater than the previous day’s price or when it is equal to the previous day’s price.
  - If this is the last day of trading, set the 'trade_type' to 'sell'. In this case, also set the 'costs_proceeds' column to the total number in the `accumulated_shares` variable multiplied by the price of the last day.


```r
# Initialize columns for trade type, cost/proceeds, and accumulated shares in amd_df
amd_df$trade_type <- NA
amd_df$costs_proceeds <- NA  # Corrected column name
amd_df$accumulated_shares <- 0  # Initialize if needed for tracking

# Initialize variables for trading logic
previous_price <- 0
share_size <- 100
accumulated_shares <- 0

#Data frame loop which executes trades based off given conditions
for (i in 1:nrow(amd_df)) {
current_price <- amd_df$close[i]
if (previous_price == 0) {
#Buy - opening
amd_df$trade_type[i] <- "Buy"
#Costs proceeds reflects money leaving the account (negative value)
amd_df$cost_proceeds[i] <- -share_size * current_price
accumulated_shares <- accumulated_shares + share_size
} else if (current_price < previous_price) {
#Buy if the current price is less than the previous price
amd_df$trade_type[i] <- "Buy"
#Costs proceeds reflects money leaving the account (negative value)
amd_df$cost_proceeds[i] <- -share_size * current_price
accumulated_shares <- accumulated_shares + share_size
}
#Change previous price to current price at the end of a loop iteration
previous_price <- current_price
#Quantity of total accumulated shares
amd_df$accumulated_shares[i] <- accumulated_shares
}
#Last day of trading needs a different code
#Define the last row in amd_df as last_day to indicate final day of trading
last_day <- nrow(amd_df)
amd_df$trade_type[last_day] <- "Sell"
amd_df$cost_proceeds[last_day] <- amd_df$close[last_day] * accumulated_shares
```


### Step 3: Customize Trading Period
- Define a trading period you wanted in the past five years 
```r

# Convert date column to Date type
amd_df <- mutate(amd_df, date = as.Date(date))
# Ensure start_date and end_date are Date types
start_date <- as.Date("2023-09-25")
end_date <- as.Date("2024-03-06")
# Filter the dataframe based on the date range
trading_period <- filter(amd_df, date >= start_date & date <= end_date)
```


### Step 4: Run Your Algorithm and Analyze Results
After running your algorithm, check if the trades were executed as expected. Calculate the total profit or loss and ROI from the trades.

- Total Profit/Loss Calculation: Calculate the total profit or loss from your trades. This should be the sum of all entries in the 'costs_proceeds' column of your dataframe. This column records the financial impact of each trade, reflecting money spent on buys as negative values and money gained from sells as positive values.
- Invested Capital: Calculate the total capital invested. This is equal to the sum of the 'costs_proceeds' values for all 'buy' transactions. Since these entries are negative (representing money spent), you should take the negative sum of these values to reflect the total amount invested.
- ROI Formula: $$\text{ROI} = \left( \frac{\text{Total Profit or Loss}}{\text{Total Capital Invested}} \right) \times 100$$

```r

#Total profit/loss calculation (NA values are ignored, Buys are negative, Sells are positive)
total_profit_loss <- sum(trading_period$cost_proceeds, na.rm = TRUE)
#Total invested capital
total_invested_capital <- -sum(trading_period$cost_proceeds[trading_period$trade_type == "Buy"],
na.rm = TRUE)
#ROI calculation
roi <- (total_profit_loss / total_invested_capital) * 100
#Print the calculations
cat("Total Profit/Loss:", total_profit_loss, "\n")
cat("Total Invested Capital:", total_invested_capital, "\n")
cat("ROI:", roi, "%\n")

```

### Step 5: Profit-Taking Strategy or Stop-Loss Mechanism (Choose 1)
- Option 1: Implement a profit-taking strategy that you sell half of your holdings if the price has increased by a certain percentage (e.g., 20%) from the average purchase price.
- Option 2: Implement a stop-loss mechanism in the trading strategy that you sell half of your holdings if the stock falls by a certain percentage (e.g., 20%) from the average purchase price. You don't need to buy 100 stocks on the days that the stop-loss mechanism is triggered.


```r

#Profit taking strategy - sell half the holdings if stock price increases by 42%
profit_taking_percentage <- 0.42
#Calculate the average purchase price of the closing prices
average_purchase_price <- mean(trading_period$close[trading_period$trade_type == "Buy"], na.rm = TRUE)
#Target price is the price of the stock after the 42% increase
target_price <- (1 + profit_taking_percentage) * average_purchase_price
#Data frame 'for' loop
for (i in 1:nrow(trading_period)) {
# Check if the trade type is Buy and closing price is greater than or equal to the target price
if (!is.na(trading_period$trade_type[i]) && trading_period$trade_type[i] == "Buy"
&& trading_period$close[i] >= target_price) {
trading_period$trade_type[i] <- "sell_half_of_holdings"
trading_period$cost_proceeds[i] <- trading_period$close[i] * (trading_period$accumulated_shares[i] /
accumulated_shares <- accumulated_shares / 2
trading_period$accumulated_shares[i] <- accumulated_shares
}
}
#Recalculate the total profit/loss and ROI after applying the profit-taking strategy
total_profit_loss <- sum(trading_period$cost_proceeds, na.rm = TRUE)
total_invested_capital <- -sum(trading_period$cost_proceeds[trading_period$trade_type == "Buy"],
na.rm = TRUE)
roi <- (total_profit_loss / total_invested_capital) * 100
#Print P/L and ROI after profit-taking trading strategy
cat("Total Profit/Loss after profit-taking strategy: ", total_profit_loss, "\n")
cat("Total Invested Capital after profit-taking strategy: ", total_invested_capital, "\n")
cat("ROI after profit-taking strategy: ", roi, "%\n")

```


### Step 6: Summarize Your Findings
- Did your P/L and ROI improve over your chosen period?
- Relate your results to a relevant market event and explain why these outcomes may have occurred.


```r
# Fill your code here and Discuss

cat("**Discussion:** My P/L and ROI improved significantly over my chosen period of time,
2023-09-25 to 2024-03-06. My P/L increased from -709193 as of 2023-09-25 to 5383168 on
2024-03-06, and my ROI increased from -100% as of 2023-09-25 to 781.6647% on 2024-03-06.
This increase was seen after implementing the profit-taking strategy in which I sold
half of my stocks after seeing a 42% increase in the stock price.
This substantial increase was a result of the stock price booming during my trading period.
On Wednesday, December 6, 2023, AMD CEO Lisa Su discussed a new graphics processor
designed for AI servers, with Microsoft and Meta as committed users. The rise in AMD
shares on the following Thursday suggests that investors believe in the chipmaker's
upward potential and strong market sentiment.
Additionally, the artificial intelligence revolution has caused a surging demand
for AI chips, which AMD manufactures. Noticing such demand for AI chips, investors
gained confidence in the potential stock price increase, causing an increase in
demand for the share. This market event substantially impacted the stock price
to boom, allowing for my profit-taking trading strategy to be effective.")

```





