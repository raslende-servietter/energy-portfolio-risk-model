# energy-portfolio-risk-model

# Energy Portfolio Risk Modelling

This project develops a compact market-risk framework for a synthetic energy portfolio using real historical market data. The analysis focuses on Value-at-Risk, Expected Shortfall, model backtesting and stress testing.

## Portfolio

The portfolio contains approximate initial exposures of:

* €4m German day-ahead electricity
* €3m Henry Hub natural gas
* €3m Brent crude oil

Daily portfolio P&L is constructed from changes in the underlying commodity prices. A fixed USD/EUR conversion rate is used to isolate commodity risk from FX risk.

## Data

The analysis uses daily market data from 2019–2025 for:

* German day-ahead electricity prices
* Henry Hub natural gas spot prices
* Brent crude oil spot prices

The series are merged on common observation dates without forward-filling missing trading days.

## Methods

The project implements:

* Parametric Value-at-Risk
* Historical Simulation VaR
* Expected Shortfall
* 500-day rolling 99% VaR backtesting
* Kupiec unconditional coverage tests
* Historical and hypothetical stress testing

## Main Results

The energy risk factors exhibit substantial non-normality and fat tails.

At the portfolio level:

| Model                 | 99% VaR | 99% Expected Shortfall |
| --------------------- | ------: | ---------------------: |
| Parametric            |  €5.26m |                 €6.02m |
| Historical Simulation |  €7.53m |                €10.85m |

Rolling backtesting produced:

| Model                 | 99% VaR breaches | Breach rate |
| --------------------- | ---------------: | ----------: |
| Parametric            |               35 |       2.86% |
| Historical Simulation |               26 |       2.12% |

The expected breach rate was 1%. Kupiec coverage tests reject correct calibration for both models, although Historical Simulation performs better.

The joint empirical 1% downside stress scenario produces a portfolio loss of approximately **€8.17m**, while the worst historical portfolio day produces a loss of approximately **€17.76m**.

German power is the dominant contributor to portfolio tail risk.

## Key Takeaway

The results illustrate that standard Gaussian risk assumptions can materially underestimate extreme energy-market losses. Historical Simulation captures more tail risk, but backtesting shows that it also understates realized extreme losses during volatile periods.

## Limitations

The project uses a simplified linear portfolio and does not model:

* forward curves
* options or nonlinear sensitivities
* FX risk
* storage contracts
* liquidity or basis risk
* time-varying volatility

These are natural extensions of the framework.

## Tools

* R
* dplyr
* ggplot2
* readxl

## Outputs

The repository includes:

* the full R analysis
* rolling VaR backtest figure
* model comparison results
* stress-test results
