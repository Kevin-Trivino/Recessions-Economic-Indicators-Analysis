# Recessions-Exploratory-Analysis
This analysis examines historical recession data and economic data pulled from the FRED website. The objective is to identify what to expect from recessions along with trends and patterns between recessions and economic indicators that can give warning signs to a downturn in the economy. It is important to know that some of these trends using economic indicators have already been identified previously by other analyst so I am not pretending to have discovered them. However learning about past analysis has inspired me to to dive deeper into the relationships and explore new potential connections.

## recessions-and-economic-indicators-eda.ipynb
Jupyter Notebook file containg the analysis code performed in R. Personal comments and conclusions from analysis included throughout the notebook.

## fredmeta.csv
Data was collected from the FRED website.

Contains economic indicators often associated with recessions along with recession status data. Data collected on smallest time unit and earliest time date available for each indicator which results in many nulls but increased flexibility for the users of this dataset.

recession: "1" recessionary period, "2" non-recessionary period (Monthly)
cpi: CPI (1982-1984=INDEX 100) (Monthly)
gdp: Real GDP Billions of Chained 2017 Dollars (Quarterly)
unemployment: Unemployment Rate (Monthly)
m2: M2 Billions of Dollars (Monthly)
fed_funds: Federal Funds Rate (Monthly)
ten_two: 10-Year Treasury Constant Maturity Minus 2-Year Treasury Constant Maturity (Monthly)
residential: Real Residential Property Price Rate (Quarterly)
