# Step 1: Installazione e caricamento dei pacchetti R necessari

# Installazione dei pacchetti (esegui questa sezione solo una volta)

install.packages(c("quantmod", "xts", "zoo", "dplyr", "tidyquant", "ggplot2", "forecast", "PerformanceAnalytics", "rugarch", "TTR", "corrplot", "quantstrat", "timeSeries", "forexdata"))
install.packages('kableExtra')

# Caricamento dei pacchetti

library(quantmod)              # Importa i dati finanziari
library(tseries)               # Gestione di modelli autoregressivi
library(timeSeries)            # Statistiche su serie storiche
library(xts)                   # Gestisce le serie storiche temporali
library(zoo)                   # Gestisce i dati non regolari
library(dplyr)                 # Manipola i dati generici
library(tidyquant)             # Analisi quantitativa dei dati finanziari
library(ggplot2)               # Creazione di grafici
library(forecast)              # Previsione dei dati temporali
library(PerformanceAnalytics)  # Analisi delle prestazioni finanziarie
library(rugarch)               # Modellazione di serie storiche finanziarie con ARCH/GARCH
library(TTR)                   # Analisi tecnica per i dati storici
library(kableExtra)            # Tool per personalizzazione grafica
library(knitr)                 # Generazione di documenti in diversi formati

 # Step 2: Acquisizione dei dati storici dell'indice S&P 500

# Data di inizio e fine per l'acquisizione dei dati
start_date = "2013-01-01"
end_date = "2023-12-31"
risk_free_rate = 0.02

# Acquisizione dei dati
getSymbols("^GSPC", from = start_date, to = end_date)

# Visualizzazione della testa e coda dei dati
head(GSPC)
tail(GSPC)

# Creazione del grafico a barre
bar_chart <- barChart(Cl(GSPC), theme = chartTheme("white"))


# Step 3: Calcolo delle metriche e degli indicatori dell'indice S&P 500

# Calcolo del rendimento giornaliero
sp500_log_returns <- diff(log(Ad(GSPC)))
log_returns <- na.omit(sp500_log_returns) # Rimozione dei valori mancanti

# Calcolo del log-rendimento complessivo
log_return_total <- sum(log_returns)

# Calcolo della volatilità
volatility <- sd(log_returns) * sqrt(252)  # Assumendo 252 giorni di trading all'anno

# Calcolo del massimo drawdown
equity_curve <- cumprod(1 + log_returns)
peak <- cummax(equity_curve)
drawdown <- (equity_curve - peak) / peak
max_drawdown <- min(drawdown)

# Calcolo del Sharpe Ratio
risk_free_rate <- 0.02  # Tasso di interesse privo di rischio
sharpe_ratio <- ((mean(log_returns)*252) - risk_free_rate) / volatility

# Creazione della Matrice di metriche e indicatori
metrics <- c("Log-Rendimento Totale", "Log-Rendimento Annuale", "Volatilità", "Massimo Drawdown", "Sharpe Ratio")
values <- c(percent(log_return_total), percent(mean(log_returns)*252), percent(volatility), percent(max_drawdown), round(sharpe_ratio, 2))
df <- data.frame(Metriche = metrics, Valori = values)
print(df)

# Creazione della tabella con caselle
table <- kable(df, format = "html") %>%
  kable_styling(bootstrap_options = "basic", full_width = FALSE)
print(table)



# Step 4: Analisi Multivariata

# Caricamento dei dati degli assets
assets <- c("^FTSE", "^N225", "000001.SS", "^DJI", "AAPL", "GOOG", "MSFT", "MTA", "GLD", "USO")
asset_data <- lapply(assets, function(symbol) {
  getSymbols(symbol, from = start_date, to = end_date, auto.assign = FALSE)
})

# Estrazione prezzi di chiusura
closing_prices <- lapply(asset_data, function(data) {
  Cl(data)
})

# Calcolo dei log-rendimenti giornalieri
daily_log_returns <- lapply(closing_prices, function(prices) {
  log_returns <- diff(log(prices))
  colnames(log_returns) <- gsub("\\^|\\.", "", colnames(log_returns))
  log_returns
})

# Assegnazione nomi alle colonne
names(daily_log_returns) <- assets

# Creazione di una matrice di dati contenente tutti i rendimenti
all_log_returns <- cbind(sp500_log_returns, daily_log_returns$`^FTSE`, daily_log_returns$`^N225`,
                         daily_log_returns$`000001.SS`, daily_log_returns$`^DJI`,
                         daily_log_returns$AAPL, daily_log_returns$GOOG,
                         daily_log_returns$MSFT, daily_log_returns$MTA,
                         daily_log_returns$GLD, daily_log_returns$USO)

# Rimozione righe con valori mancanti
all_returns <- na.omit(all_log_returns)
sp500_returns <- na.omit(sp500_log_returns)

# Date e osservazioni in comune
common_dates <- index(all_returns) %in% index(sp500_returns) # date in comune
all_returns <- all_returns[common_dates, ] # coincidenza tra n osservazioni per entrambi i dataset
sp500_returns <- sp500_returns[1:nrow(all_returns), ] # Rimozione righe in eccesso per l'S&P

# Modello di regressione lineare multipla
multi_model <- lm(GSPC.Adjusted ~ . - GSPC.Adjusted, data = all_returns)
summary(multi_model)

# Calcolo della matrice di correlazione e risultati
cor_matrix <- cor(all_returns)
library(corrplot)
corrplot(cor_matrix, method = "color")

# Matrice di correlazione per le variabili esplicative
cor_matrix_esp <- cor(all_returns[, c("FTSEClose", "AAPLClose", "GOOGClose", "MTAClose", "DJIClose", "N225Close", "USOClose")])
print(cor_matrix_esp)

# Nuovo modello di regressione multipla, al netto della multicollinearità
lr_model <- lm(GSPC.Adjusted ~ . - GSPC.Adjusted - N225Close - X000001SSClose - MTAClose - USOClose, data = all_returns)
summary(lr_model)

# Deconposizione della serie temporale
library(forecast)
time_series <- ts(sp500_returns, frequency = 252)
decomposition <- decompose(time_series)
plot(decomposition)
dates <- index(sp500_returns)
seasonal_data <- data.frame(Date = dates, Seasonal = decomposition$seasonal) #dataframe con date e stagionalità
plot(seasonal_data)

# Grafico della Stagionalità mensile
# Conversione colonna date ed estrazione mesi ed anni
seasonal_data$Date <- as.Date(seasonal_data$Date)
seasonal_data$Month <- format(seasonal_data$Date, "%m")
seasonal_data$Year <- format(seasonal_data$Date, "%Y")

# Raggruppamento dati mensili e calcolo del valore medio
monthly_means <- seasonal_data %>%
  group_by(Year, Month) %>%
  summarise(Mean_Seasonal = mean(Seasonal))

# Tracciamento del grafico
library(ggplot2)
ggplot(monthly_means, aes(x = Month, y = Mean_Seasonal, group = Year, color = Year)) +
  geom_line() +
  labs(x = "Months", y = "Value Variation", title = "Seasonality") +
  theme_minimal()





# Step 5: Analisi dei residui

# Calcolo dei residui e visualizzazione
residuals <- decomposition$random
residuals <- na.omit(residuals)
plot(residuals, type = "l", main = "Residuals")

# Calcola i valori predetti dal tuo modello (dovresti averli già)
predicted_values <- decomposition$trend + decomposition$seasonal

# Crea un grafico a dispersione dei residui rispetto ai valori predetti
plot(predicted_values, residuals, main = "Residuals Scatterplot",
     xlab = "Predicted Values", ylab = "Residuals", pch = 19, col = "black")

# Aggiungi una linea a 0 per facilitare la visualizzazione dei residui
abline(h = 0, col = "black")


# Calcola la media e la deviazione standard dei residui
mean_residuals <- mean(residuals)
sd_residuals <- sd(residuals)
cat("Media dei residui:", mean_residuals, "\n")
cat("Deviazione standard dei residui:", sd_residuals, "\n")

# Carica le librerie necessarie
library(lmtest)
library(sandwich)

# Calcolo dei residui e visualizzazione
residuals <- decomposition$random
residuals <- na.omit(residuals)

# Calcola i valori predetti dal tuo modello (dovresti averli già)
predicted_values <- decomposition$trend + decomposition$seasonal

# Crea un grafico a dispersione dei residui rispetto ai valori predetti
plot(predicted_values, residuals, main = "Residuals Scatterplot",
     xlab = "Predicted Values", ylab = "Residuals", pch = 19, col = "black")

# Aggiungi una linea a 0 per facilitare la visualizzazione dei residui
abline(h = 0, col = "black")

# Calcola la media e la deviazione standard dei residui
mean_residuals <- mean(residuals)
sd_residuals <- sd(residuals)
cat("Media dei residui:", mean_residuals, "\n")
cat("Deviazione standard dei residui:", sd_residuals, "\n")

# Test di normalità dei residui (test di Shapiro-Wilk)
shapiro.test(residuals)
Box.test(residuals, lag = 20, type = "Ljung-Box")

# Grafico Q-Q dei residui
qqnorm(residuals)
qqline(residuals)

# Correzione di White e visualizzazione dati e grafico
residuals_model <- lm(residuals ~ 1) # Modello lineare
white_model <- coeftest(residuals_model, vcov. = sandwich)

# Estrai i residui corretti dal modello di White
white_model <- lm(residuals ~ 1, data = data.frame(residuals = residuals))
white_residuals <- residuals(white_model)

# Verifica e allineamento delle lunghezze
if (length(white_residuals) != nrow(sp500_log_returns)) {
  # Allinea i dati in base alla lunghezza minima
  min_length <- min(length(white_residuals), nrow(sp500_log_returns))
  
  sp500_log_returns <- sp500_log_returns[1:min_length, ]
  white_residuals <- white_residuals[1:min_length]
}

# Crea il dataframe ordinato
ordered_data <- data.frame(
  Time = index(sp500_log_returns),  # Usa la funzione index per ottenere le date
  Corrected_Residuals = white_residuals
)

# Ordina i dati in base al tempo
ordered_data <- ordered_data[order(ordered_data$Time, na.last = NA), ]

# Visualizza il grafico dei residui corretti rispetto alla serie temporale con una linea continua
plot(ordered_data$Time, ordered_data$Corrected_Residuals, 
     type = "n",  # Imposta il tipo di grafico a "nessuno" per inizializzare il grafico senza punti
     xlab = "Time", ylab = "Corrected Residuals",
     main = "Corrected Residuals vs S&P500 Time Series")

# Aggiungi la linea continua
lines(ordered_data$Time, ordered_data$Corrected_Residuals, col = "black")
abline(h = 0, col = "red")
grid()










# STEP 6: PREVISIONE CON MODELLI ARMA, SVM, RANDOM FOREST E PSI SIGMA NETWORK
# Caricamento delle librerie necessarie
install.packages(c("forecast", "e1071", "randomForest", "neuralnet"))
library(forecast)
library(e1071)
library(randomForest)
library(neuralnet)

# Lunghezza totale dei dati
total_length <- nrow(all_returns)

# Dimensioni di ciascun periodo
train_length <- round(0.7 * total_length)
test_length <- total_length - train_length

# Creazione dataset divisi per periodo
train_data <- all_returns[1:train_length, ]
test_data <- all_returns[(train_length + 1):(train_length + test_length), ]



# Modello 1: ARIMA
# Selezione del modello ARIMA
arma_model <- auto.arima(train_data$GSPC.Adjusted, stationary = TRUE, trace = TRUE)

# Riepilogo del modello ARIMA
summary(arma_model)

# Verifica dei residui del modello ARIMA
checkresiduals(arma_model)

# Generazione delle previsioni giornaliere e della varianza giornaliera
arma_forecast <- forecast(arma_model, h = test_length)
arma_variance <- fitted(arma_model)

# Calcolo delle performance annualizzate
arma_annualized_returns <- mean(arma_forecast$mean) * 252
arma_annualized_volatility <- sqrt(mean(arma_variance) * 252)

# Calcolo del tasso di Sharpe, assumendo un tasso risk-free del 2% annuo
risk_free_rate_annual <- 0.02
arma_sharpe_ratio <- (arma_annualized_returns - risk_free_rate_annual) / arma_annualized_volatility

# Calcolo dell'RMSE
arma_rmse <- sqrt(mean((as.numeric(arma_forecast$mean) - as.numeric(test_data$GSPC.Adjusted))^2))

# Stampa delle metriche
arma_metrics <- data.frame(
  Periodo = "ARMA",
  'Rendimento Annuale' = arma_annualized_returns,
  'Volatilità Annuale' = arma_annualized_volatility,
  Sharpe = arma_sharpe_ratio,
  RMSE = arma_rmse
)
print("Metriche Arma")
print(arma_metrics)

# Calcolo del massimo drawdown
calculate_max_drawdown <- function(returns) {
  cumulative_returns <- cumsum(returns)
  peak <- max(cumulative_returns)
  trough <- min(cumulative_returns[which(cumulative_returns == peak):length(cumulative_returns)])
  max_drawdown <- (peak - trough) / peak
  return(max_drawdown)
}

# calcolo Max Drawdown
arma_forecast_mean <- as.numeric(arma_forecast$mean)
arma_forecast_mean <- arma_forecast_mean[!is.na(arma_forecast_mean)]
arma_forecast_xts <- xts(arma_forecast_mean, order.by = index(test_data))
arma_max_drawdown <- - maxDrawdown(arma_forecast_xts)*100






# Modello 2: SVM (Support Vector Machine)
# Addestramento del modello SVM
svm_model <- svm(GSPC.Adjusted ~ ., data = train_data)

# Effettua previsioni con il modello SVM
svm_forecast <- predict(svm_model, newdata = test_data)

# Calcolo delle performance annualizzate
svm_annualized_returns <- mean(svm_forecast) * 252
svm_annualized_volatility <- sqrt(var(svm_forecast) * 252)

# Calcolo del tasso di Sharpe, assumendo un tasso risk-free del 2% annuo
risk_free_rate_annual <- 0.02
svm_sharpe_ratio <- (svm_annualized_returns - risk_free_rate_annual) / svm_annualized_volatility

# Calcolo dell'RMSE
svm_rmse <- sqrt(mean((svm_forecast - test_data$GSPC.Adjusted)^2))

# Stampa delle metriche per il modello SVM
svm_metrics <- data.frame(
  Periodo = "SVM",
  'Rendimento Annuale' = svm_annualized_returns,
  'Volatilità Annuale' = svm_annualized_volatility,
  Sharpe = svm_sharpe_ratio,
  RMSE = svm_rmse
)
print("Metriche SVM")
print(svm_metrics)


# Calcolo Max Drawdown SVM
svm_forecast <- as.numeric(svm_forecast)
svm_forecast <- svm_forecast[!is.na(svm_forecast)]
svm_forecast_xts <- xts(svm_forecast, order.by = index(test_data))
svm_max_drawdown <- - maxDrawdown(svm_forecast_xts)



# Modello 3: Random Forest
# Imposta il seed per la riproducibilità
set.seed(123)

# Addestramento del modello Random Forest
rf_model <- randomForest(GSPC.Adjusted ~ ., data = train_data)

# Effettua previsioni con il modello Random Forest
rf_forecast <- predict(rf_model, newdata = test_data)

# Calcolo delle performance annualizzate
rf_annualized_returns <- mean(rf_forecast) * 252
rf_annualized_volatility <- sqrt(var(rf_forecast) * 252)

# Calcolo del tasso di Sharpe, assumendo un tasso risk-free del 2% annuo
risk_free_rate_annual <- 0.02
rf_sharpe_ratio <- (rf_annualized_returns - risk_free_rate_annual) / rf_annualized_volatility

# Calcolo dell'RMSE
rf_rmse <- sqrt(mean((rf_forecast - test_data$GSPC.Adjusted)^2))

# Stampa delle metriche per il modello Random Forest
rf_metrics <- data.frame(
  Periodo = "Random Forest",
  'Rendimento Annuale' = rf_annualized_returns,
  'Volatilità Annuale' = rf_annualized_volatility,
  Sharpe = rf_sharpe_ratio,
  RMSE = rf_rmse
)
print("Metriche Random Forest")
print(rf_metrics)

# Calcolo Max Drawdown Random Forest
rf_forecast <- as.numeric(rf_forecast)
rf_forecast <- rf_forecast[!is.na(rf_forecast)]
rf_forecast_xts <- xts(rf_forecast, order.by = index(test_data))
rf_max_drawdown <- - maxDrawdown(rf_forecast_xts)



# Modello 4: Rete Neurale con Psi Sigma Network
# Carica la libreria
library(neuralnet)

# Addestramento del modello di rete neurale
psi_model <- neuralnet(GSPC.Adjusted ~ ., data = train_data, hidden = c(10, 5))

# Effettua previsioni con il modello di rete neurale
psi_forecast <- predict(psi_model, newdata = test_data)

# Calcolo delle performance annualizzate
psi_annualized_returns <- mean(psi_forecast) * 252
psi_annualized_volatility <- sqrt(var(psi_forecast) * 252)

# Calcolo del tasso di Sharpe, assumendo un tasso risk-free del 2% annuo
risk_free_rate_annual <- 0.02
psi_sharpe_ratio <- (psi_annualized_returns - risk_free_rate_annual) / psi_annualized_volatility

# Calcolo dell'RMSE
psi_rmse <- sqrt(mean((psi_forecast - test_data$GSPC.Adjusted)^2))

# Stampa delle metriche per il modello di rete neurale
psi_metrics <- data.frame(
  Periodo = "Rete Neurale",
  'Rendimento Annuale' = psi_annualized_returns,
  'Volatilità Annuale' = psi_annualized_volatility,
  Sharpe = psi_sharpe_ratio,
  RMSE = psi_rmse
)
print("Metriche Rete Neurale")
print(psi_metrics)

# Calcolo Max Drawdown Psi Sigma Network
psi_forecast <- as.numeric(psi_forecast)
psi_forecast <- psi_forecast[!is.na(psi_forecast)]
psi_forecast_xts <- xts(psi_forecast, order.by = index(test_data))
psi_max_drawdown <- - maxDrawdown(psi_forecast_xts)


# CONFRONTO TRA METRICHE DEI MODELLI
# Caricamento della libreria per la formattazione in percentuale
library(scales)

# Creazione tabella per il confronto tra i risultati di ciascun modello
comparison_models <- data.frame(
  Modello = c("ARMA", "SVM", "Random Forest", "Psi Sigma Network"),
  'Rendimento Annuale' = percent(c(arma_annualized_returns, svm_annualized_returns, rf_annualized_returns, psi_annualized_returns)),
  'Volatilità Annuale' = percent(c(arma_annualized_volatility, svm_annualized_volatility, rf_annualized_volatility, psi_annualized_volatility)),
  'Drawdown' = percent(c(arma_max_drawdown, svm_max_drawdown, rf_max_drawdown, psi_max_drawdown)),
  'Sharpe' = round(c(arma_sharpe_ratio, svm_sharpe_ratio, rf_sharpe_ratio, psi_sharpe_ratio), 2)
)
print("Comparazioni metriche tra modelli")
print(comparison_models)

tabella_comparativa <- kable(comparison_models, format = "html") %>%
  kable_styling(bootstrap_options = "basic", full_width = FALSE)
print(tabella_comparativa)

# GRAFICO RMSE
# Crea un dataframe con i RMSE dei modelli
model_names <- c("ARIMA", "SVM", "Random Forest", "Psi Sigma Network")
rmse_values <- percent(c(arma_rmse, svm_rmse, rf_rmse, psi_rmse))
rmse_df <- data.frame(Modello = model_names, RMSE = rmse_values)

# Crea il grafico a barre
library(ggplot2)

ggplot(rmse_df, aes(x = Modello, y = RMSE, fill = Modello)) +
  geom_bar(stat = "identity") +
  labs(title = "Confronto degli RMSE dei Modelli",
       y = "RMSE") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))





# STEP 7: Analisi e confronto tra ETF e Fondo
# Acquisizione dei dati storici per IVV e FCNTX
getSymbols("IVV", from = start_date, to = end_date)
getSymbols("FCNTX", from = start_date, to = end_date)
getSymbols("^GSPC", from = start_date, to = end_date)

# CALCOLI
# Calcolo dei log-rendimenti giornalieri di IVV e FCNTX
ivv_log_returns <- dailyReturn(IVV$IVV.Adjusted, type = "log")
fcntx_log_returns <- dailyReturn(FCNTX$FCNTX.Adjusted, type = "log")
sp500_log_returns <- dailyReturn(GSPC$GSPC.Adjusted, type = "log")

# Calcolo dei logrendimenti complessivi
ivv_overall_log <- sum(ivv_log_returns)
fcntx_overall_log <- sum(fcntx_log_returns)
sp_overall_log <- sum(sp500_log_returns)


# Calcolo della volatilità per fondo, etf ed SP
volatility_ivv <- sd(ivv_log_returns) * sqrt(252)  # Assumendo 252 giorni di trading all'anno
volatility_fcntx <- sd(fcntx_log_returns) * sqrt(252)
volatility_sp500 <- sd(sp500_log_returns) * sqrt(252)


# Calcolo del massimo drawdown per IVV
equity_curve_ivv <- cumprod(1 + ivv_log_returns)
peak_ivv <- cummax(equity_curve_ivv)
drawdown_ivv <- (equity_curve_ivv - peak_ivv) / peak_ivv
max_drawdown_ivv <- min(drawdown_ivv)

# Calcolo del massimo drawdown per FCNTX
equity_curve_fcntx <- cumprod(1 + fcntx_log_returns)
peak_fcntx <- cummax(equity_curve_fcntx)
drawdown_fcntx <- (equity_curve_fcntx - peak_fcntx) / peak_fcntx
max_drawdown_fcntx <- min(drawdown_fcntx)

# Calcolo del massimo drawdown per S&P500
equity_curve_sp500 <- cumprod(1 + sp500_log_returns)
peak_sp500 <- cummax(equity_curve_fcntx)
drawdown_sp500 <- (equity_curve_fcntx - peak_fcntx) / peak_fcntx
max_drawdown_sp500 <- min(drawdown_fcntx)


# Calcolo del rapporto di Sharpe per IVV e FCNTX
sharpe_ratio_ivv <- ((mean(ivv_log_returns)*252) - risk_free_rate) / volatility_ivv
sharpe_ratio_fcntx <- ((mean(fcntx_log_returns)*252) - risk_free_rate) / volatility_fcntx
sharpe_ratio_sp500 <- ((mean(sp500_log_returns)*252) - risk_free_rate) / volatility_sp500


# CONFRONTO
# Creazione di una tabella comparativa
comparison_table <- data.frame(
  Asset = c("GSPC", "IVV", "FCNTX"),
  Rendimento.Totale = percent(c(sp_overall_log, ivv_overall_log, fcntx_overall_log)),
  Rendimento.Annuale = percent(c(mean(sp500_log_returns)*252, mean(ivv_log_returns)*252, mean(fcntx_log_returns)*252)),
  Volatilità = percent(c(volatility_sp500, volatility_ivv, volatility_fcntx)),
  Drawdown = percent(c(max_drawdown_sp500, max_drawdown_ivv, max_drawdown_fcntx)),
  Sharpe = round(c(sharpe_ratio_sp500, sharpe_ratio_ivv, sharpe_ratio_fcntx),2)
)

# Visualizzazione della tabella comparativa
print(comparison_table)
table_a <- kable(comparison_table, format = "html") %>%
  kable_styling(bootstrap_options = "basic", full_width = FALSE)
print(table_a)

# Creazione dataframe contenente i dati di IVV, FCNTX ed GSPC
price_data <- data.frame(
  Date = index(IVV$IVV.Adjusted),  # Data
  IVV = as.numeric(IVV$IVV.Adjusted),  # Prezzi di IVV
  FCNTX = as.numeric(FCNTX$FCNTX.Adjusted),  # Prezzi di FCNTX
  GSPC = as.numeric(GSPC$GSPC.Adjusted)  # Prezzi di GSPC (S&P 500)
)


# Crea il grafico a linea
library(ggplot2)
ggplot(data = price_data, aes(x = Date)) +
  geom_line(aes(y = IVV, color = "IVV"), size = 1) +
  geom_line(aes(y = FCNTX, color = "FCNTX"), size = 1) +
  geom_line(aes(y = GSPC, color = "GSPC"), size = 1) +
  labs(title = "Prezzi di IVV, FCNTX ed S&P 500",
       x = "Data", y = "Prezzo di Chiusura") +
  scale_color_manual(values = c("IVV" = "blue", "FCNTX" = "red", "GSPC" = "green")) +
  theme_minimal()

# Crea il grafico a linea
library(ggplot2)
ggplot(data = price_data, aes(x = Date)) +
  geom_line(aes(y = IVV, color = "IVV"), size = 1) +
  geom_line(aes(y = FCNTX, color = "FCNTX"), size = 1) +
  labs(title = "IVV e FCNTX a confronto",
       x = "Data", y = "Prezzo di Chiusura") +
  scale_color_manual(values = c("IVV" = "blue", "FCNTX" = "red")) +
  theme_minimal()




# STEP 8: REGRESSIONE
# Creazione di un dataframe per i log-rendimenti
log_returns_df <- data.frame(  
  SP500_Log_Returns = sp500_log_returns,
  IVV_Log_Returns = ivv_log_returns,
  FCNTX_Log_Returns = fcntx_log_returns
)
colnames(log_returns_df) <- c("SP500_Log_Returns", "IVV_Log_Returns", "FCNTX_Log_Returns")

# Modello di regressione lineare
model_ivv_fcntx <- lm(sp500_log_returns ~ . - SP500_Log_Returns, data = log_returns_df)
summary(model_ivv_fcntx)


# ANALISI DEI RESIDUI
# Calcola i residui
lm_residuals <- residuals(model_ivv_fcntx)

# Rimuovi valori mancanti se presenti
lm_residuals <- na.omit(lm_residuals)

# Crea un vettore di date dalla data iniziale alla data finale dei residui
dates <- index(lm_residuals)

# Grafico dei residui con le date sull'asse x
plot(dates, lm_residuals, type = "l", ylab = "Residui", main = "Grafico dei Residui")


# Grafico Q-Q dei residui
qqnorm(lm_residuals)
qqline(lm_residuals)

# Calcola i valori predetti dal modello
predicted <- predict(model_ivv_fcntx)

# Calcola i residui
residuals <- residuals(model_ivv_fcntx)

# Grafico dei residui in funzione dei valori predetti
plot(predicted, lm_residuals, xlab = "Valori Predetti", ylab = "Residui",
     main = "Grafico dei Residui in Funzione dei Valori Predetti")

# Installazione pacchetti per il test di White
# Installa e carica il pacchetto 'lmtest'
# install.packages("lmtest")
library(lmtest)
# Installa e carica il pacchetto 'sandwich'
#install.packages("sandwich")
library(sandwich)
#install.packages('coeftest')

# Applica la correzione di White al modello
white_model <- coeftest(model_ivv_fcntx, vcov. = sandwich)
summary(white_model)








# STEP 9: Creazione portafogli
# Expense Ratio e Turnover per IVV e FCNTX
expense_ratio_ivv <- 0.03 / 100  # 0,03%
expense_ratio_fcntx <- 0.55 / 100  # 0,55%
turnover_ivv <- 3 / 100  # 3%
turnover_fcntx <- 27 / 100  # 27%

# Rendimento attesi annuo corretto per IVV e FCNTX
rendimento_atteso_ivv <- mean(ivv_log_returns) * 2768 - (expense_ratio_ivv + turnover_ivv)
rendimento_atteso_fcntx <- mean(fcntx_log_returns) * 2768 - (expense_ratio_fcntx + turnover_fcntx)

# PORTAFOGLIO A: EQUIPONDERATO
peso_ivv <- 0.5  # Peso di IVV
peso_fcntx <- 0.5  # Peso di FCNTX

# Rendimento atteso del portafoglio equiponderato
rendimento_atteso_portafoglio_equi <- (peso_ivv * rendimento_atteso_ivv) + (peso_fcntx * rendimento_atteso_fcntx)

# Volatilità del portafoglio equiponderato
volatilità_portafoglio_equi <- sqrt((peso_ivv^2 * volatility_ivv^2) + (peso_fcntx^2 * volatility_fcntx^2) + (2 * peso_ivv * peso_fcntx * cov(ivv_log_returns, fcntx_log_returns) * sqrt(252)))
rischio_portafoglio_equi <- volatilità_portafoglio_equi

# Massimo Drawdown del portafoglio equiponderato (usando l'equity curve del portafoglio)
equity_curve_portafoglio_equi <- cumprod(1 + (peso_ivv * ivv_log_returns) + (peso_fcntx * fcntx_log_returns))
peak_portafoglio_equi <- cummax(equity_curve_portafoglio_equi)
drawdown_portafoglio_equi <- (equity_curve_portafoglio_equi - peak_portafoglio_equi) / peak_portafoglio_equi
max_drawdown_portafoglio_equi <- min(drawdown_portafoglio_equi)

# Rapporto di Sharpe del portafoglio equiponderato
sharpe_ratio_portafoglio_equi <- ((rendimento_atteso_portafoglio_equi/10) - risk_free_rate) / volatilità_portafoglio_equi


# PORTAFOGLIO B: MEDIA-VARIANZA
# Creazione del vettore dei rendimenti attesi dei singoli asset
rendimenti_attesi <- c(rendimento_atteso_ivv, rendimento_atteso_fcntx)

# Matrice di covarianza dei rendimenti dei singoli asset
matrice_covarianza <- cov(cbind(ivv_log_returns, fcntx_log_returns)) * 252

# Creazione di un vettore di pesi iniziali (inizialmente equiponderati)
pesi_iniziali <- rep(1, 2) / 2

# Funzione obiettivo (minimizzazione della varianza)
funzione_obiettivo <- function(pesi, rendimenti_attesi, matrice_covarianza) {
  varianza_portafoglio <- t(pesi) %*% matrice_covarianza %*% pesi
  return(varianza_portafoglio)
}

# Ottimizzazione del portafoglio
library(quadprog)

portafoglio_media_varianza <- solve.QP(Dmat = 2 * matrice_covarianza, dvec = rep(0, 2),
                                       Amat = cbind(rendimenti_attesi, rep(1, 2)),
                                       bvec = c(rendimento_atteso_portafoglio_equi, 1),
                                       meq = 1)

# Pesi ottimali
pesi_media_varianza <- portafoglio_media_varianza$solution

# Calcolo del rendimento atteso del portafoglio
rendimento_atteso_media_varianza <- t(pesi_media_varianza) %*% rendimenti_attesi - (pesi_media_varianza[1] * expense_ratio_ivv + pesi_media_varianza[2] * expense_ratio_fcntx)

# Calcolo della volatilità del portafoglio
volatilità_media_varianza <- sqrt(t(pesi_media_varianza) %*% matrice_covarianza %*% pesi_media_varianza)

# Massimo drawdown del portafoglio (usando l'equity curve)
equity_curve_media_varianza <- cumprod(1 + (pesi_media_varianza[1] * ivv_log_returns) + (pesi_media_varianza[2] * fcntx_log_returns))
peak_media_varianza <- cummax(equity_curve_media_varianza)
drawdown_media_varianza <- (equity_curve_media_varianza - peak_media_varianza) / peak_media_varianza
max_drawdown_media_varianza <- min(drawdown_media_varianza)

# Rapporto di Sharpe del portafoglio
sharpe_ratio_media_varianza <- ((rendimento_atteso_media_varianza/10) - risk_free_rate) / volatilità_media_varianza


# PORTAFOGLIO 3: MEDIA-CVaR
# Livello di confidenza alpha
alpha <- 0.95

# Calcolo del quantile al livello di confidenza alpha per il rendimento del portafoglio
quantile_portafoglio <- quantile(pesi_iniziali[1] * ivv_log_returns + pesi_iniziali[2] * fcntx_log_returns, 1 - alpha)

# Creazione di una funzione obiettivo
funzione_obiettivo_cvar <- function(pesi, rendimenti_attesi, matrice_covarianza, alpha) {
  portafoglio_rendimenti <- pesi[1] * ivv_log_returns + pesi[2] * fcntx_log_returns
  cvar <- mean(portafoglio_rendimenti[portafoglio_rendimenti < quantile_portafoglio])
  return(-cvar)  # Minimizza il CVaR invece del suo opposto
}

# Risoluzione del problema di ottimizzazione
portafoglio_cvar <- optim(par = pesi_iniziali, fn = funzione_obiettivo_cvar, 
                          rendimenti_attesi = rendimenti_attesi,
                          matrice_covarianza = matrice_covarianza, alpha = alpha)

# Estrazione dei pesi ottimali
pesi_cvar <- portafoglio_cvar$par

# Calcolo del rendimento atteso del portafoglio
rendimento_atteso_cvar <- t(pesi_cvar) %*% rendimenti_attesi - (pesi_cvar[1] * expense_ratio_ivv + pesi_cvar[2] * expense_ratio_fcntx)

# Volatilità del portafoglio
volatilità_cvar <- sqrt(t(pesi_cvar) %*% matrice_covarianza %*% pesi_cvar)

# Massimo drawdown del portafoglio (usando l'equity curve)
equity_curve_cvar <- cumprod(1 + (pesi_cvar[1] * ivv_log_returns) + (pesi_cvar[2] * fcntx_log_returns))
peak_cvar <- cummax(equity_curve_cvar)
drawdown_cvar <- (equity_curve_cvar - peak_cvar) / peak_cvar
max_drawdown_cvar <- min(drawdown_cvar)

# Rapporto di Sharpe del portafoglio
sharpe_ratio_cvar <- ((rendimento_atteso_cvar/10) - risk_free_rate) / volatilità_cvar


# CONFRONTO TRA PORTAFOGLI
# Creazione della tabella comparativa
comparative_table <- data.frame(Portafoglio = c("GSPC", "Equiponderato", "Media-Varianza", "Media-CVaR"),
                                Rendimento.Totale = percent(c(sp_overall_log, rendimento_atteso_portafoglio_equi, rendimento_atteso_media_varianza, rendimento_atteso_cvar)),
                                Rendimento.Annuale = percent(c(mean(sp500_log_returns)*252, (rendimento_atteso_portafoglio_equi)/(2768/252), 
                                                       (rendimento_atteso_media_varianza)/(2768/252), 
                                                       (rendimento_atteso_cvar)/(2768/252))),
                                Volatilità = percent(c(volatility_sp500, volatilità_portafoglio_equi, volatilità_media_varianza, volatilità_cvar)),
                                Drawdown = percent(c(max_drawdown_sp500, max_drawdown_portafoglio_equi, max_drawdown_media_varianza, max_drawdown_cvar)),
                                Sharpe = round(c(sharpe_ratio_sp500, sharpe_ratio_portafoglio_equi, sharpe_ratio_media_varianza, sharpe_ratio_cvar),2))


# Visualizzazione della tabella comparativa
print(comparative_table)
final_table <- kable(comparative_table, format = "html") %>%
  kable_styling(bootstrap_options = "basic", full_width = FALSE)
print(final_table)

#install.packages("ggplot2")
library(ggplot2)

# Dati per il confronto
portafogli <- c("S&P 500", "Equiponderato", "Media-Varianza", "Media-CVaR")
rendimenti_complessivi <- c(sp_overall_log, rendimento_atteso_portafoglio_equi, rendimento_atteso_media_varianza, rendimento_atteso_cvar)
sharpe_ratio <- c(sharpe_ratio_sp500, sharpe_ratio_portafoglio_equi, sharpe_ratio_media_varianza, sharpe_ratio_cvar)

# Creazione del dataframe per il grafico
grafico_rendimenti <- data.frame(Portafoglio = portafogli, Rendimento_Complessivo = rendimenti_complessivi)
grafico_sharpe <- data.frame(Portafoglio = portafogli, Sharpe_Ratio = sharpe_ratio)

# Grafico a barre per il rendimento complessivo
ggplot(grafico_rendimenti, aes(x = Portafoglio, y = Rendimento_Complessivo, fill = Portafoglio)) +
  geom_bar(stat = "identity") +
  labs(title = "Modelli portafogli", y = "Rendimento Totale") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_text(aes(label = percent(Rendimento_Complessivo)), vjust = -0.3) +
  scale_fill_brewer(palette = "Set3")

# Grafico a barre per lo Sharpe Ratio
ggplot(grafico_sharpe, aes(x = Portafoglio, y = Sharpe_Ratio, fill = Portafoglio)) +
  geom_bar(stat = "identity") +
  labs(title = "Confronto Sharpe Ratio", y = "Sharpe Ratio") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_text(aes(label = round(Sharpe_Ratio, 2)), vjust = -0.3) +
  scale_fill_brewer(palette = "Set3")




# Visualizzazione delle ponderazioni degli asset nei tre portafogli
cat("Ponderazione degli asset nei tre portafogli:\n")
cat("Portafoglio Equiponderato:\n")
cat("Peso IVV:", peso_ivv, "\n")
cat("Peso FCNTX:", peso_fcntx, "\n\n")

cat("Portafoglio Media-Varianza:\n")
cat("Peso IVV:", pesi_media_varianza[1], "\n")
cat("Peso FCNTX:", pesi_media_varianza[2], "\n\n")

cat("Portafoglio Media-CVaR:\n")
cat("Peso IVV:", pesi_cvar[1], "\n")
cat("Peso FCNTX:", pesi_cvar[2], "\n\n")
