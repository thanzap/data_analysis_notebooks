# Convert the date index to datetime
df.index = pd.to_datetime(df.index)
# Plot the entire time series df and show gridlines
df.plot(grid=True)
plt.show()
# Slice the dataset to keep only 2012
df2012 = df["2012"]
# Merge stocks and bonds DataFrames using join()
dfstocks_and_bonds = stocksdf.join(bondsdf, how="inner")
# Compute percent change using pct_change()
dfreturns = dfstocks_and_bonds.pct_change()
# Compute correlation using corr()
correlation = dfreturns["SP500"].corr(dfreturns["US10Y"])
print("Correlation of stocks and interest rates: ", correlation)
# Make scatter plot
plt.scatter(dfreturns["SP500"],dfreturns["US10Y"])
plt.show()
#Python packages to perform regressions
#statsmodels
import statsmodels.api as sm
sm.OLS(y,x).fit()
#numpy
np.polyfit(x,y,deg=1)
#Pandas
pd.ols(y,x)
#scipy
from scipy import stats
stats.linregress(x,y)
#example of statsmodels regression R-squared
# Import the statsmodels module
import statsmodels.api as sm
# Compute correlation of x and y
correlation = dfreturns["SP500"].corr(dfreturns["US10Y"])
print("The correlation between x and y is %4.2f" %(correlation))
# Add a constant to the DataFrame dfx
df1returns = sm.add_constant(dfreturns)
# Regress y on dfx1
result = sm.OLS(y, df1returns).fit()
# Print out the results and look at the relationship between R-squared and the correlation above
print(result.summary())
#auto correlation is a correlation of a time series  with a lagged copy of itself called Lag-one autocorrelation- serial correlation
#negative autocorrelation called Mean Reversion
#positive autocorrelation called trend following or momentum
# Convert the daily data to weekly data
MSFT = MSFT.resample(rule="W", how="last")
MSFT.head()
# Compute the percentage change of prices
returns = MSFT.pct_change()
# Compute and print the autocorrelation of returns
autocorrelation = returns["Adj Close"].autocorr()
print("The autocorrelation of weekly returns is %4.2f" %(autocorrelation))
# Compute the daily change in interest rates 
daily_diff = daily_rates.diff()
daily_rates.head()
daily_diff.head()
# Compute and print the autocorrelation of daily changes
autocorrelation_daily = daily_diff['US10Y'].autocorr()
print("The autocorrelation of daily interest rate changes is %4.2f" %(autocorrelation_daily))
# Convert the daily data to annual data
yearly_rates = daily_rates.resample(rule="A").last()
# Repeat above for annual data
yearly_diff = yearly_rates.diff()
autocorrelation_yearly = yearly_diff['US10Y'].autocorr()
print("The autocorrelation of annual interest rate changes is %4.2f" %(autocorrelation_yearly))

# Import the plot_acf module from statsmodels and sqrt from math
from statsmodels.graphics.tsaplots import plot_acf
from math import sqrt
# Compute and print the autocorrelation of MSFT weekly returns
autocorrelation = returns['Adj Close'].autocorr()
print("The autocorrelation of weekly MSFT returns is %4.2f" %(autocorrelation))
# Find the number of observations by taking the length of the returns DataFrame
nobs = len(returns)
# Compute the approximate confidence interval
conf = 1.96/sqrt(nobs)
print("The approximate confidence interval is +/- %4.2f" %(conf))
# Plot the autocorrelation function with 95% confidence intervals and 20 lags using plot_acf
plot_acf(returns, alpha=0.05, lags=20)
plt.show()

#Generate a Random Walk
# Generate 500 random steps with mean=0 and standard deviation=1
steps = np.random.normal(loc=0, scale=1.0, size=500)
# Set first element to 0 so that the first price will be the starting stock price
steps[0]=0
# Simulate stock prices, P with a starting price of 100
P = 100 + np.cumsum(steps)
# Plot the simulated stock prices
plt.plot(P)
plt.title("Simulated Random Walk")
plt.show()

#are stock prices a random walk ??- Augmented Dickey-Fuller Test
# Import the adfuller module from statsmodels
from statsmodels.tsa.stattools import adfuller
# Run the ADF test on the price series and print out the results
results = adfuller(AMZN['Adj Close'])
print(results)
# Just print out the p-value
print('The p-value of the test on prices is: ' + str(results[1]))

#seasonal adjustment during tax season
# Import the plot_acf module from statsmodels
from statsmodels.graphics.tsaplots import plot_acf
# Seasonally adjust quarterly earnings
HRBsa = HRB.diff(4)
# Print the first 10 rows of the seasonally adjusted series
print(HRBsa.head(10))
# Drop the NaN data in the first four rows
HRBsa = HRBsa.dropna()
# Plot the autocorrelation function of the seasonally adjusted series
plot_acf(HRBsa)
plt.show()

#Estimating an AR model
# Import the ARMA module from statsmodels
from statsmodels.tsa.arima_model import ARMA
# Fit an AR(1) model to the first simulated data
mod = ARMA(simulated_data_1, order=(1,0))
res = mod.fit()
# Print out summary information on the fit
print(res.summary())
# Print out the estimate for the constant and for phi
print("When the true phi=0.9, the estimate of phi (and the constant) are:")
print(res.params)

#Forecast interest_rate data
# Import the ARMA module from statsmodels
from statsmodels.tsa.arima_model import ARMA
# Forecast interest rates using an AR(1) model
mod = ARMA(interest_rate_data, order=(1,0))
res = mod.fit()
# Plot the original series and the forecasted series
res.plot_predict(start=0,end="2022")
plt.legend(fontsize=8)
plt.show()

#Estimate Order of Model: PACF
# Import the modules for simulating data and for plotting the PACF
from statsmodels.tsa.arima_process import ArmaProcess
from statsmodels.graphics.tsaplots import plot_pacf
# Simulate AR(1) with phi=+0.6
ma = np.array([1])
ar = np.array([1, -0.6])
AR_object = ArmaProcess(ar, ma)
simulated_data_1 = AR_object.generate_sample(nsample=5000)
# Plot PACF for AR(1)
plot_pacf(simulated_data_1, lags=20)
plt.show()
# Simulate AR(2) with phi1=+0.6, phi2=+0.3
ma = np.array([1])
ar = np.array([1, -0.6, -0.3])
AR_object = ArmaProcess(ar, ma)
simulated_data_2 = AR_object.generate_sample(nsample=5000)
# Plot PACF for AR(2)
plot_pacf(simulated_data_2, lags=20)
plt.show()

#Estimate Order of Model: Information Criteria
# Import the module for estimating an ARMA model
from statsmodels.tsa.arima_model import ARM
# Fit the data to an AR(p) for p = 0,...,6 , and save the BIC
BIC = np.zeros(7)
for p in range(7):
    mod = ARMA(simulated_data_2, order=(p,0))
    res = mod.fit()
# Save BIC for AR(p)    
    BIC[p] = res.bic
# Plot the BIC as a function of p
plt.plot(range(1,7), BIC[1:7], marker='o')
plt.xlabel('Order of AR Model')
plt.ylabel('Bayesian Information Criterion')
plt.show()

#Simulate MA(1) time series 
# import the module for simulating data
from statsmodels.tsa.arima_process import ArmaProcess
# Plot 1: MA parameter = -0.9
plt.subplot(2,1,1)
ar1 = np.array([1])
ma1 = np.array([1, -0.9])
MA_object1 = ArmaProcess(ar1, ma1)
simulated_data_1 = MA_object1.generate_sample(nsample=1000)
plt.plot(simulated_data_1)
# Plot 2: MA parameter = +0.9
plt.subplot(2,1,2)
ar2 = np.array([1])
ma2 = np.array([1, 0.9])
MA_object2 = ArmaProcess(ar2, ma2)
simulated_data_2 = MA_object2.generate_sample(nsample=1000)
plt.plot(simulated_data_2)
plt.show()

##Estimation and Forecasting an MA Model
# Import the ARMA module from statsmodels
from statsmodels.tsa.arima_model import ARMA
# Fit an MA(1) model to the first simulated data
mod = ARMA(simulated_data_1, order=(0,1))
res = mod.fit()
# Print out summary information on the fit
print(res.summary())
# Print out the estimate for the constant and for theta
print("When the true theta=-0.9, the estimate of theta (and the constant) are:")
print(res.params)

# Import the ARMA module from statsmodels
from statsmodels.tsa.arima_model import ARMA
# Forecast the first MA(1) model
mod = ARMA(simulated_data_1, order=(0,1))
res = mod.fit()
res.plot_predict(start=990, end=1010)
plt.show()

#Data cleaning example 
# import datetime module
import datetime
# Change the first date to zero
intraday.iloc[0,0] = 0
# Change the column headers to 'DATE' and 'CLOSE'
intraday.columns = ["DATE","CLOSE"]
# Examine the data types for each column
print(intraday.dtypes)
# Convert DATE column to numeric
intraday['DATE'] = pd.to_numeric(intraday['DATE'])
# Make the `DATE` column the new index
intraday = intraday.set_index("DATE")
# Notice that some rows are missing
print("If there were no missing rows, there would be 391 rows of minute data")
print("The actual length of the DataFrame is:", len(intraday))
# Everything
set_everything = set(range(391))
# The intraday index as a set
set_intraday = set(intraday.index)
# Calculate the difference
set_missing = set_everything - set_intraday
# Print the difference
print("Missing rows: ", set_missing)
# Fill in the missing rows
intraday = intraday.reindex(range(391), method="ffill")
# From previous step
intraday = intraday.reindex(range(391), method='ffill')
# Change the index to the intraday times
intraday.index = pd.date_range(start='2017-09-01 9:30', end='2017-09-01 16:00', freq="1min")
# Plot the intraday time series
intraday.plot(grid=True)
plt.show()
#applying an ma model
# Import plot_acf and ARMA modules from statsmodels
from statsmodels.graphics.tsaplots import plot_acf
from statsmodels.tsa.arima_model import ARMA
# Compute returns from prices and drop the NaN
returns = intraday.pct_change()
returns = returns.dropna()
# Plot ACF of returns with lags up to 60 minutes
plot_acf(returns, lags=60)
plt.show()
# Fit the data to an MA(1) model
mod = ARMA(returns, order=(0,1))
res = mod.fit()
print(res.params)

#Exaxmple of natural gas and heating oil
# Plot the prices separately
plt.subplot(2,1,1)
plt.plot(7.25*HO, label='Heating Oil')
plt.plot(NG, label='Natural Gas')
plt.legend(loc='best', fontsize='small')
# Plot the spread
plt.subplot(2,1,2)
plt.plot(7.25*HO-NG, label='Spread')
plt.legend(loc='best', fontsize='small')
plt.axhline(y=0, linestyle='--', color='k')
plt.show()

#Verify that the products are cointegrated-First apply the Dickey-Fuller test separately to show they are random walks. 
#Then apply the test to the difference, which should strongly reject the random walk hypothesis. We assume the cointegration vector (1,-1)
# Import the adfuller module from statsmodels
from statsmodels.tsa.stattools import adfuller
# Compute the ADF for HO and NG
result_HO = adfuller(HO["Close"])
print("The p-value for the ADF test on HO is ", result_HO[1])
result_NG = adfuller(NG['Close'])
print("The p-value for the ADF test on NG is ", result_NG[1])
# Compute the ADF of the spread
result_spread = adfuller(7.25 * HO["Close"] - NG["Close"])
print("The p-value for the ADF test on the spread is ", result_spread[1])

#Are Bitcoin and Ethereum Cointegrated? Here we will also compute the cointegration vector (1,-b)
#Add a constant to the ETH DataFrame using sm.add_constant()
#Regress BTC on ETH using sm.OLS(y,x).fit(), where y is the dependent variable and x is the independent variable, and save the results in result.
#The intercept is in result.params[0] and the slope in result.params[1]
#Run ADF test on BTC  ETH
#if p-value<0,05 we can say that the two coins are cointegrated
# Import the statsmodels module for regression and the adfuller function
import statsmodels.api as sm
from statsmodels.tsa.stattools import adfuller
# Regress BTC on ETH
ETH = sm.add_constant(ETH)
result = sm.OLS(BTC,ETH).fit()
print(result.params)
# Compute ADF
b = result.params[1]
adf_stats = adfuller(BTC['Price'] - b*ETH['Price'])
print("The p-value for the ADF test is ", adf_stats[1])

#Case study- Climate change
# Import the adfuller function from the statsmodels module
from statsmodels.tsa.stattools import adfuller
# Convert the index to a datetime object
temp_NY.index = pd.to_datetime(temp_NY.index, format='%Y') #format means annual in this case
temp_NY.head()
# Plot average temperatures
temp_NY.plot()
plt.show()
# Compute and print ADF p-value
result = adfuller(temp_NY['TAVG'])
print("The p-value for the ADF test is ", result[1])
#The p-value for the ADF test is  0.5832938987871152 so we can assume that the data follow a random walk with drift
#Since the temperature series, temp_NY, is a random walk with drift, take first differences to make it stationary. 
#Then compute the sample ACF and PACF. This will provide some guidance on the order of the model.
# Import the modules for plotting the sample ACF and PACF
from statsmodels.graphics.tsaplots import plot_acf, plot_pacf
# Take first difference of the temperature Series
chg_temp = temp_NY.diff()
chg_temp = chg_temp.dropna()
chg_temp.head()
# Plot the ACF  and PACF on the same page
fig, axes = plt.subplots(2,1)
# Plot the ACF
plot_acf(chg_temp, lags=20, ax=axes[0])
# Plot the PACF
plot_pacf(chg_temp, lags=20, ax=axes[1])
plt.show()

#when in the ACF chart spikes are decreasing and goes to zero line quickly and 
# at the same time in the PACF we have only 1-2 significant spikes we should use AR model.
#if we have 1 significant spike in the PACF chart we use AR1 model if we have 2 we use AR2 model.

#Which ARMA model is the best? The model with the lowest AIC is the best choice
# Import the module for estimating an ARMA model
from statsmodels.tsa.arima_model import ARMA
# Fit the data to an AR(1) model and print AIC:
mod_ar1 = ARMA(chg_temp, order=(1, 0))
res_ar1 = mod_ar1.fit()
print("The AIC for an AR(1) is: ", res_ar1.aic)
# Fit the data to an AR(2) model and print AIC:
mod_ar2 = ARMA(chg_temp, order=(0, 1))
res_ar2 = mod_ar2.fit()
print("The AIC for an AR(2) is: ", res_ar2.aic)
# Fit the data to an ARMA(1,1) model and print AIC:
mod_arma11 = ARMA(chg_temp, order=(1,1))
res_arma11 = mod_arma11.fit()
print("The AIC for an ARMA(1,1) is: ", res_arma11.aic)

#Forecasting 
# Import the ARIMA module from statsmodels
from statsmodels.tsa.arima_model import ARIMA
# Forecast temperatures using an ARIMA(1,1,1) model
mod = ARIMA(temp_NY, order=(1,1,1))
res = mod.fit()
# Plot the original series and the forecasted series
res.plot_predict(start='1872-01-01', end='2046-01-01')
plt.show()

#####TRADING BASICS
# Load the data
bitcoin_data = pd.read_csv("bitcoin_data.csv", index_col="Date", parse_dates=True)
# Print the top 5 rows
print(bitcoin_data.head())
#lineplot
# Plot the daily high price
plt.plot(bitcoin_data['High'], color='green')
# Plot the daily low price
plt.plot(bitcoin_data['Low'], color='red')
plt.title('Daily high low prices')
plt.show()
#A candlestick chart is a style of chart that packs multiple pieces of price information into one chart. 
#It can provide you with a good sense of price action and visual aid for technical analysis.
# Define the candlestick data with an interactive chart
import plotly.graph_objects as go
candlestick = go.Candlestick(
    x=bitcoin_data.index,
    open=bitcoin_data['Open'],
    high=bitcoin_data['High'],
    low=bitcoin_data['Low'],
    close=bitcoin_data['Close'])
# Create a candlestick figure   
fig = go.Figure(data=[candlestick])
fig.update_layout(title='Bitcoin prices')                        
# Show the plot
fig.show()

#Resampling data
# Resample the data to daily by calculating the mean values
eurusd_daily = eurusd_4h.resample('D').mean() # "H" hourly - "W" weekly - "M" monthly
# Calculate daily returns
tsla_data['daily_return'] = tsla_data['Close'].pct_change() * 100
# Plot the histogram
tsla_data['daily_return'].hist(bins=100, color='red')
plt.ylabel('Frequency')
plt.xlabel('Daily return')
plt.title('Daily return histogram')
plt.show()

#Calculate and plot SMAs
##Daily price data is inherently messy and noisy.
# You want to analyze the Apple stock daily price data,
# and plan to add a simple moving average (SMA) indicator to smooth out the data and indicate if the trend is bullish or bearish 
# Calculate SMA
aapl_data['sma_50'] = aapl_data['Close'].rolling(50).mean()
# Plot the SMA
plt.plot(aapl_data['sma_50'], color='green', label='SMA_50')
# Plot the close price
plt.plot(aapl_data['Close'], color='red', label='Close')
# Customize and show the plot
plt.title('Simple moving averages')
plt.legend()
plt.show()

##Financial trading with bt package- a flexible framework for defining and backtesting trading strategies
#The bt process 
#step1 : Get the historical price data
#step2 : Define the strategy
#step3 : Backtest the strategy with the data
#step4 : Evaluate the result
# bt can fetch data online STEP1
bt_data=bt.get("goog, amzn, tsla", start="2020-6-1", end="2020-12-1")
# Define the strategy STEP2
bt_strategy=bt.Strategy("Trade Weekly",[ bt.algos.RunWeekly(),bt.algos.SelectAll(),bt.algos.WeighEqually(),bt.algos.Rebalance()])
# Backtest STEP3
bt_test= bt.Backtest(bt_strategy,bt_data)
bt_res=bt.run(bt_test)
#Evaluate the result STEP4
bt_res.plot(title="Backtest result")
bt_res.get_transactions()

##Trend indicator MAs
#types of indicators
#1) Trend indicators measure the direction or strength of a trend (MA, ADX)
#2) Momentum indicators measure the velocity of price movement (RSI)
#3) Volatility indicators measure the magnitude of price deviations (Bolling bands)

#we will use talib technical analysis library which includes 150+ technical indicator implementations
import talib
#Moving Average indicators 
#1) SMA simple moving average 
#2) EMA exponential moving average
#Characteristics
#1) Move with the price
#2) Smooth out data to better indicate the price direction
#The main difference betweend SMA and EMA is that EMAs gives higher weight to the more recent data while SMAs assign equal weight to all data points.

# Calculate and plot two EMAs
#A 12-period EMA and 26-period EMA are two moving averages used in calculating a more complex indicator called MACD (Moving Average Convergence Divergence). 
# The MACD turns two EMAs into a momentum indicator by subtracting the longer EMA from the shorter one.
#talib has been imported for you, and matplotlib.pyplot has been imported as plt
# Calculate 12-day EMA
stock_data['EMA_12'] = talib.EMA(stock_data['Close'], timeperiod=12)
# Calculate 26-day EMA
stock_data['EMA_26'] = talib.EMA(stock_data['Close'], timeperiod=26)
# Plot the EMAs with price
plt.plot(stock_data['EMA_12'], label='EMA_12')
plt.plot(stock_data['EMA_26'], label='EMA_26')
plt.plot(stock_data['Close'], label='Close')
# Customize and show the plot
plt.legend()
plt.title('EMAs')
plt.show()
#As you can see a shorter-term EMA moves faster with the price than the longer-term EMA,
#  hence comparing the gap between them can reveal how fast the price is changing, that is, the price momentum.

#SMA vs. EMA
# Calculate the SMA
stock_data['SMA'] = talib.SMA(stock_data['Close'], timeperiod=50)
# Calculate the EMA
stock_data['EMA'] = talib.EMA(stock_data['Close'], timeperiod=50)
# Plot the SMA, EMA with price
plt.plot(stock_data['SMA'], label='SMA')
plt.plot(stock_data['EMA'], label='EMA')
plt.plot(stock_data['Close'], label='Close')
# Customize and show the plot
plt.legend()
plt.title('SMA vs EMA')
plt.show()


#Strength indicator ADX
# Average Directional Movement Index (ADX)
# ADX measures the strength of a trend
# Oscilates between 0-100 - ADX < 25= No trend, ADX > 25 = Trending Market, ADX >50 = Strong trending market
# Derived from the smoothed averages of the difference between +DI and -DI (+DI quantify the presence of an uptren, -DI quantify the presence of a downtrend)
# Calculation input: HIGH, LOW, CLOSE prices of each period
# Calculate ADX
stock_data['ADX'] = talib.ADX(stock_data['High'], stock_data['Low'], stock_data['Close'],timeperiod=14) #Default timepreiod is 14
#The ADX can quantify the strength of a trend, but does not suggest the bullish or bearish trend direction.
#Calculate and Visualize the ADX
# Calculate ADX
stock_data['ADX'] = talib.ADX(stock_data['High'], stock_data['Low'], stock_data['Close'])
# Create subplots
fig, (ax1, ax2) = plt.subplots(2)
# Plot ADX with the price
ax1.set_ylabel('Price')
ax1.plot(stock_data['Close'])
ax2.set_ylabel('ADX')
ax2.plot(stock_data['ADX'], color='red')
ax1.set_title('Price and ADX')
plt.show()
 
#Momentum indicator RSI
#Relative strength index 
#Measures the momentum of a trend - Momentum is the speed or rising or falling in prices
# Oscilates between 0-100 - RSI > 70 = Overbought-Overvalued, RSI < 30 = Oversold-Undervalued
# Calculate  RSI 
stock_data['RSI'] = talib.RSI(stock_data['Close'],timeperiod=14) #Default timepreiod is 14
#Visualize the RSI
# Create subplots
fig, (ax1, ax2) = plt.subplots(2)
# Plot ADX with the price
ax1.set_ylabel('Price')
ax1.plot(stock_data['Close'])
ax2.set_ylabel('RSI')
ax2.plot(stock_data['RSI'], color='red')
ax1.set_title('Price and RSI')
plt.show()

#Volatility indicator Bollinger Bands
# Measures price volatility 
# Composed of 3 lines - Middle band=n-period simple moving average, Upper band= k-standard deviations above the middle band, Lower band= k-standard deviations below the middle band
# n-period default is 20 and k-standard deviations default is 2 
# The wider the band, the more volatile the asset prices
# Measure whether a price is too high or too low on a relative basis- Relatively High= Price close to upper band, Relatively Low= Price close to lower band 
# Calculate BBANDS
upper,mid,lower =talib.BBANDS(stock_data["Close"], nbdevup=2, nbdevdn=2, timeperiod=20) #nbdevup=2,nbdevdn=2 by default, timeperiod=20 by default also
#Visualize BBANDS
#Plot the Bollinger Bands
plt.plot(stock_data["Close"], label="Price")
plt.plot(upper,label="Upper Band")
plt.plot(lower,label="Lower Band")
plt.plot(mid, label="Mid Band")
#Customize and show the plot
plt.title("Bollinger Bands")
plt.legend()
plt.show()

##Trading signals 
#Triggers to long or short financial assets based on predetermined criteria
#Can be constructed using: A technical indicator, Multiple indicators, Combination of market data and indicators
# Used in algorithmic trading
#Signal example -> Price > SMA (long when the price rises above the SMA)

#Construct the signal
#Get price data by the stock ticker
price_data= bt.get('aapl', start='2019-11-1', end='2020-12-1')
#Calculate SMA
sma= price_data.rolling(20).mean() #OR 
import talib
sma=talib.SMA(price_data["Close"], timeperiod=20)
#Define a signal-based strategy
bt_strategy= bt.Strategy("AboveEMA", [bt.algos.SelectWhere(price_data > sma),
                                      bt.algos.WeighEqually(),
                                      bt.algos.Rebalance()])
#Create the backtest and run it
bt_backtest=bt.Backtest(bt_strategy, price_data)
bt_result=bt.run(bt_backtest)
#Plot the backtest result
bt_result.plot(title="Backtest result")

#Trend following strategies
#Bet the price trend will continue in the same direction
#Use trend indicators such as moving averages, ADX to construct trading signals
#Mean reversion strategies
#Bet the price tends to reverse back towards the mean
#Use indicators such as RSI, BBANDS to construct trading signals

#MA crossover strategy
#Two EMA crossover: 
# -Long signal: the short-term EMA crosses above the long-term EMA
# -Short signal: the short-term EMA crosses below the long-term EMA
#Calculate the indicators
import talib
EMA_short = talib.EMA(price_data['Close'],timeperiod=10).to_frame()
EMA_long = talib.EMA(price_data['Close'],timeperiod=40).to_frame()
# Create the signal DataFrame
signal = EMA_long.copy()
signal[EMA_long.isnull()] = 0
# Construct the signal
signal[EMA_short > EMA_long] = 1
signal[EMA_short < EMA_long] = -1
# Merge the data 
combined_df = bt.merge(signal,price_data,EMA_short,EMA_long)
combined_df.columns = ['signal', 'Price', 'EMA_short', 'EMA_long']
# Plot the signal, price and MAs
combined_df.plot(secondary_y=['signal'])
plt.show()
# Define the strategy
bt_strategy = bt.Strategy('EMA_crossover', 
                          [bt.algos.WeighTarget(signal),
                           bt.algos.Rebalance()])
#Create the backtest and run it
bt_backtest=bt.Backtest(bt_strategy, price_data)
bt_result=bt.run(bt_backtest)
#Plot the backtest result
bt_result.plot(title="Backtest result")

#Mean reversion strategy
#RSI-based mean reversion strategy
# -Long signal: RSI < 30 . It suggest the asset is likely oversold and the price may soon rise
# -Short signal: RSI > 70 . It suggests the asset is likely overbought and the price may soon reverse
#Calculate the indicator
import talib
#Calculate the RSI
stock_rsi= talib.RSI(price_data["Close"]).to_frame()
# Create the signal DataFrame
signal = EMA_long.copy()
signal[EMA_long.isnull()] = 0
#Construct the signal
signal[stock_rsi < 30] = 1
signal[stock_rsi > 70] = -1
signal[(stock_rsi <= 70) & (stock_rsi >= 30)] = 0
#Plot the RSI
stock_rsi.plot()
plt.title("RSI")
#Plot the signal
#Merge data into one dataframe
combined_df=bt.merge(signal,stock_data)
combined_df.columns=["Signal","Price"]
#Plot the signal with price
combined_df.plot(secondary_y=["Signal"])
# Define the strategy
bt_strategy = bt.Strategy('RSI_MeanReversion', 
                          [bt.algos.WeighTarget(signal),
                           bt.algos.Rebalance()])
#Create the backtest and run it
bt_backtest=bt.Backtest(bt_strategy, price_data)
bt_result=bt.run(bt_backtest)
#Plot the backtest result
bt_result.plot(title="Backtest result")

#Strategy optimization to find the best parameters
#Function for the process of implementing the strategy with SMA
def signal_strategy(ticker, period, name, start='2018-4-1', end='2020-11-1'): #We can give default values in the parameters of the function
    # Get the data and calculate SMA   
    price_data = bt.get(ticker, start=start, end=end)    
    sma = price_data.rolling(period).mean()
    # Define the signal-based strategy    
    bt_strategy = bt.Strategy(name,
                              [bt.algos.SelectWhere(price_data>sma),
                              bt.algos.WeighEqually(),
                              bt.algos.Rebalance()])
    # Return the backtestreturn 
    return bt.Backtest(bt_strategy, price_data)

#Then we call the function several times to pass different SMA parameters
ticker="aapl"
sma20= signal_strategy(ticker,period=20,name="SMA20")
sma50=signal_strategy(ticker,period=50,name="SMA50")
sma100=signal_strategy(ticker,period=100,name="SMA100")
#Run backtest and compare plots as results
bt_results=bt.run(sma20,sma50,sma100)
bt_results.plot(title="Strategy optimization")

#A benchmark is a standard or point of reference against which a strategy can be compared or assessed
# Example of benchmark: a strategy that uses signals to actively trade stocks can use a passive buy and hold strategy as a benchmark
# Example of benchmark: S&P 500 index is often used as a benchmark for equities
def buy_and_hold (ticker,name,start='2018-4-1', end='2020-11-1'): #We can give default values in the parameters of the function
    #Get the data
    price_data = bt.get(ticker, start=start, end=end)
    #Define the benchmark strategy
    bt_strategy = bt.Strategy(name,
                              [bt.algos.RunOnce(),
                              bt.algos.SelectAll(),
                              bt.algos.WeighEqually(),
                              bt.algos.Rebalance()])
    # Return the backtestreturn 
    return bt.Backtest(bt_strategy, price_data)
benchmark=buy_and_hold(ticker,name="benchmark")
#Run all backtests and plot the results
bt_results=bt.run(sma20,sma50,sma100,benchmark)
bt_results.plot(title="Strategy benchmarking")

#Example of strategy optimization for SMA based signals - The fast moving average is quick to capture the trend, 
# but also quick to react to the market noise when there is no trend.
def signal_strategy(price_data, period, name):
    # Calculate SMA
    sma = price_data.rolling(period).mean()
    # Define the signal-based Strategy
    bt_strategy = bt.Strategy(name, 
                              [bt.algos.SelectWhere(price_data > sma),
                               bt.algos.WeighEqually(),
                               bt.algos.Rebalance()])
    # Return the backtest
    return bt.Backtest(bt_strategy, price_data)
# Create signal strategy backtest
sma10 = signal_strategy(price_data, period=10, name='SMA10')
sma30 = signal_strategy(price_data, period=30, name='SMA30')
sma50 = signal_strategy(price_data, period=50, name='SMA50')
# Run all backtests and plot the resutls
bt_results = bt.run(sma10,sma30,sma50)
bt_results.plot(title='Strategy optimization')
plt.show()

#Strategy return analysis
#Get all backtest stats
resinfo=bt_results.stats
print(resinfo.index)
#Get daily,monthly and yearly returns
print("Daily return: %.4f"% resinfo.loc["daily_mean"])
print("Monthly return: %.4f"% resinfo.loc["monthly_mean"])
print("Yearly return: %.4f"% resinfo.loc["yearly_mean"])
#Compound annual growth rate
#Get the compound annual growth rate
print("Compound annual growth rate: %.4f"% resinfo.loc["cagr"])
#Plot the weekly return histogram
bt_results.plot_histograms(bins=50, freq="w") # freq=frequency, default value is daily
#Get the lookback returns
lookback_returns=bt_results.display_lookback_returns()
print(lookback_returns)

#Performance metric evaluation 
#Drawdown is a peak-to-trough decline during a specific period for an asset or a trading account.
# Obtain all backtest stats
resInfo = bt_results.stats
# Get the max drawdown
max_drawdown = resInfo.loc['max_drawdown']
print('Maximum drawdown: %.2f'% max_drawdown)
# Get the average drawdown
avg_drawdown = resInfo.loc['avg_drawdown']
print('Average drawdown: %.2f'% avg_drawdown)
# Get the average drawdown days
avg_drawdown_days = resInfo.loc['avg_drawdown_days']
print('Average drawdown days: %.0f'% avg_drawdown_days)

#Calmar ratio= CALifornia Managed Accounts report
#The higher the calmar ratio the better a strategy performed on a risk-adjusted basis
#Typically a calmar ratio larget than 3 is considered excellent.
# Get the CAGR
cagr = resInfo.loc["cagr"]
print('Compound annual growth rate: %.4f'% cagr)
# Get the max drawdown
max_drawdown = resInfo.loc['max_drawdown']
print('Maximum drawdown: %.2f'% max_drawdown)
# Calculate Calmar ratio manually
calmar_calc = cagr / max_drawdown * (-1)
print('Calmar Ratio calculated: %.2f'% calmar_calc)
# Get the Calmar ratio
calmar = resInfo.loc['calmar']
print('Calmar Ratio: %.2f'% calmar)

#Sharpe ratio and sortino ratio
#Risk-adjusted return Make performance comparable among different strategies
#A ratio that describes risk involved in obtaining the return
#Sharpe ratio
#The bigger the Sharpe ratio, the more attractive the return
#Get all backtest stats
resinfo=bt_results.stats
print(resinfo.index)
#Get daily,monthly and yearly returns
print("Sharpe ratio daily: %.4f"% resinfo.loc["daily_sharpe"])
print("Sharpe ratio monthly: %.4f"% resinfo.loc["monthly_sharpe"])
print("Sharpe ratio annually: %.4f"% resinfo.loc["yearly_sharpe"])
#Calulate sharpe ratio manually
# Obtain annual return
annual_return = resinfo.loc['yearly_mean']
# Obtain annual volatility
volatility = resinfo.loc['yearly_vol']
# Calculate Sharpe ratio manually
sharpe_ratio = annual_return / volatility
print('Sharpe ratio annually %.2f'% sharpe_ratio)

#Limitations of Sharpe ratio
#Penalize both the  good and bad volatility
#Upside volatility can skew the ratio downward

#Sortino ratio
#Get all backtest stats
resinfo=bt_results.stats
print(resinfo.index)
#Get daily,monthly and yearly returns
print("Sortino ratio daily: %.4f"% resinfo.loc["daily_sortino"])
print("Sortino ratio monthly: %.4f"% resinfo.loc["monthly_sortino"])
print("Sortino ratio annually: %.4f"% resinfo.loc["yearly_sortino"])
#The bigger the sortino ratio the better the performance



































