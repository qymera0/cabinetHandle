res_check <- function(real, pred){

        predictions <- data.frame(real = real,
                                  pred = pred,
                                  res = real - pred)
        
        # Color to surrounding graphic elements
        
        generalColor <- "antiquewhite4"
        
        # Color internal graphics elements
        
        Color <- "midnightblue"
        
        # real vs predicted
        
        g1 <- ggplot(predictions, aes(x = pred, y = real)) +
                geom_point(col = Color) + 
                geom_abline(slope = 1, intercept = 0, col = Color) + 
                labs(x = "Prediction", 
                     y = "Real",
                     title = "Real vs predicted values") +
                theme_minimal() +
                theme(panel.border = element_blank(), 
                      panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(), 
                      axis.line = element_line(colour = generalColor),
                      axis.title = element_text(colour = generalColor),
                      axis.text = element_text(colour = generalColor),
                      title = element_text(color = generalColor))
        
        # Normality check
        
        g2 <- ggqqplot(data = predictions,
                       x = "res",
                       title = "Normality check",
                       col = Color) +
                theme_minimal() +
                theme(panel.border = element_blank(), 
                      panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(), 
                      axis.line = element_line(colour = generalColor),
                      axis.title = element_text(colour = generalColor),
                      axis.text = element_text(colour = generalColor),
                      title = element_text(color = generalColor))
        
        # Residuals vs Fitted
        
        g3 <- ggplot(predictions, aes(x = pred, y = res)) +
                geom_point(col = Color) + 
                labs(x = "Predictions", 
                     y = "Residuals",
                     title = "Residuals vs predictions") +
                theme_minimal() +
                theme(panel.border = element_blank(), 
                      panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(), 
                      axis.line = element_line(colour = generalColor),
                      axis.title = element_text(colour = generalColor),
                      axis.text = element_text(colour = generalColor),
                      title = element_text(color = generalColor))
        
        # Histogram
        
        g4 <- ggplot(predictions, aes(res)) +
                geom_histogram(binwidth = 1, col = Color, fill = Color) +
                labs(x = "Residuals", 
                     y = "Count",
                     title = "Residuals Distribution") +
                theme_minimal() +
                theme(panel.border = element_blank(), 
                      panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(), 
                      axis.line = element_line(colour = generalColor),
                      axis.title = element_text(colour = generalColor),
                      axis.text = element_text(colour = generalColor),
                      title = element_text(color = generalColor))
        
        # Plot a panel of four graphics
        
        grid.arrange(g1, g2, g3, g4,
                     nrow = 2)       
                
}


