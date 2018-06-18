  
  #====================================================================================================
  #
  # Time series cross validation
  # 
  #====================================================================================================
  
    library(here)
    library(caret)
    library(forecast)
    library(tidyverse)

    # Sample data
    data(economics)
    unemploy <- economics$psavert[1:100]
    
    # Timeslices
    timeslices <- createTimeSlices(unemploy, initialWindow = 60, fixedWindow = TRUE, horizon = 24)

    # Matrix to store errors
    arima.error <- ets.error <- ave.error <- matrix(NA, length(timeslices$train), 24)
    
    # Loop
    for (i in 1:length(timeslices$train)) {
    
      # Subset data
      train <- unemploy[timeslices$train[i] %>% unlist() %>% unname()]
      test <- unemploy[timeslices$test[i] %>% unlist() %>% unname()]
  
      # Forecasts
      arima <- forecast(auto.arima(train), h = 24)["mean"] %>% unlist() %>% unname()
      ets <- forecast(ets(train), h = 24)["mean"] %>% unlist() %>% unname()
      ave <- rowMeans(cbind(arima, ets))
      
      # Calculatet error and store
      arima.error[i, 1:length(arima)] <- abs(arima - test)
      ets.error[i, 1:length(arima)] <- abs(ets - test)
      ave.error[i, 1:length(arima)] <- abs(ave - test)   
    
    }
    
    # Plot
    plot(colMeans(arima.error), type = "l", col = 2, xlab = "Forecast horizon", ylab = "Mean absolute error")
    lines(colMeans(ets.error), col = 3)
    lines(colMeans(ave.error), col = 4)
    legend("topleft", legend = c("ARIMA", "ETS", "Average"), col = 2:4, lty = 1)
    
    