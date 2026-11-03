# Can Machine Learning and Deep Learning Models Handle Non-Stationarity in Multivariate Macroeconomic Time Series? Evidence from Ghana's Cedi Exchange Rate (USD/GHS), 2002-2021

Code and data for the paper of the same title, submitted to *Computational Economics*.

The study compares one-month-ahead forecasts of the monthly USD/GHS exchange rate from five machine learning and deep learning models (SVR, XGBoost, LSTM, NARX, echo state network), each fitted on levels and on first differences, with ARIMAX and a random walk with drift as benchmarks. Predictors are CPI inflation, cocoa prices and crude oil prices. Models are estimated on the first 191 months (February 2002 to December 2017) and tested on the last 48 months (January 2018 to December 2021).





## Data

| Variable | Source |
|---|---|
| USD/GHS exchange rate (monthly average of the interbank rate) | Ghana Statistical Service (GSS) StatsBank, Exchange rates |
| CPI month-on-month inflation | GSS StatsBank, Prices and inflation |
| Cocoa price (US dollars per tonne) | Bank of Ghana, Commodity prices |
| Crude oil price (US dollars per barrel) | Bank of Ghana, Commodity prices |

The three sources were merged by month. The longest unbroken run with all four series starts in January 2002, and the sample ends in December 2021. Rows with any missing value were dropped. Data were accessed on [date accessed].

Check the terms of use of each source before reusing the data.

## How to run

1. Install the software listed below.
2. Run the R scripts in `r/` in numerical order.
3. Run the Python scripts in `python/` in numerical order.
4. Outputs (tables and figures) are written to [output folder].

The random seed is fixed at 23 in both languages. Results from neural networks (NARX, LSTM) can still differ slightly across machines, operating systems and library versions.

## Software

- R 4.5.0 with tidyverse 2.0.0, tseries 0.10.58, forecast 9.0.1, lmtest 0.9.40 (see `requirements_R.txt`)
- Python 3.12.7 with the packages in `requirements.txt`:
  `pip install -r requirements.txt`

## Citation

If you use this code or data, please cite  Zenodo. https://doi.org/10.5281/zenodo.23918938

## Licence


Data: the merged dataset is compiled from public sources (Ghana Statistical Service and Bank of Ghana). The original data remain subject to the terms of those sources. 

