
  #====================================
  #
  # Reorder factors
  #
  #====================================
  
  # vector of factors
  x <- as.factor(c("0-1", "1-2", "2-3", "10-11"))

  # look at default ordering
  order(x)
  
  # reorder to custom order
  x <- factor(x, levels = c("0-1", "1-2", "2-3", "10-11"))

  # check new order
  order(x)
  