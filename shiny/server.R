# server. R
require("shiny")
source("Preprocessing.R")
shinyServer(function(input,output){
  data <- reactive({
    sig.names <- as.character((anova.p %>% subset(p.value < input$ANOVA_p2))$var)
    var.names <- c(sig.names, names(n.factor_all(lc)))
    lc2 <- lc[,c(var.names)]
    lc2 <- lc2 %>% select(-purpose)
    lc.categorical <- lc2[,names(n.factor_all(lc2))]
    lc.categorical <- lc.categorical %>% select(-loan_status)
    lc.numeric <- lc2[,!names(lc2) %in% names(n.factor_all(lc2))]
    # Transform the categorical column to multiple numeric columns
    categorical.list <- apply(lc.categorical, 2, function(x) model.matrix(~ x + 0))
    lc3 <- as.data.frame(cbind(categorical.list, lc.numeric))
    lc3$loan_status <- lc$loan_status
    # Group the loan_status into 3 groups
    if(input$log_out == "Fully_Paid and Potential Delinquency Events"){
      lc3 <- lc3 %>% mutate(y = !(loan_status == "Current"))
      lc3$y[lc3$loan_status == "Fully_Paid"] <- 2
      lc3 <- lc3 %>% select(-loan_status)
      ## Logistic Regression
      lc4 <- lc3 %>% subset(y != 0)
      # Fully Paid set to be zero, while potential delinquency events set to be 1
      lc4$y[lc4$y == 2] <- 0 
    }else{
      if(input$log_out == "Current and Potential Delinquency Events"){
        lc3 <- lc3 %>% mutate(y = !(loan_status == "Fully_Paid"))
        lc3$y[lc3$loan_status == "Current"] <- 2
        lc3 <- lc3 %>% select(-loan_status)
        ## Logistic Regression
        lc4 <- lc3 %>% subset(y != 0)
        # Current set to be zero, while potential delinquency events set to be 1
        lc4$y[lc4$y == 2] <- 0 
      }else{
        lc3 <- lc3 %>% mutate(y = (loan_status == "Fully_Paid"))
        lc3$y[lc3$loan_status == "Current"] <- 2
        lc3 <- lc3 %>% select(-loan_status)
        lc4 <- lc3 %>% subset(y != 0)
        # Current set to be zero, while Fully Paid set to be 1
        lc4$y[lc4$y == 2] <- 0 
      }
    }
    log.f <- logistic(lc4)
    pred <- predict(log.f, lc4, type = "response")
    # Compare the pred and true value
    compare.data <- data.frame(pred = pred, true = lc4$y)
    
    return(compare.data)
  })
  roc <- reactive({
    # roc, cost of FP = 1, cost of FN = 5
    # Calculate the roc(FP: pred=1,true=0; FN:pred=0, true=1)
    roc <- calculate_roc(data(), input$cost_of_FP, input$cost_of_FN, n = 100)
    return(roc)
    
  })
  pred1 <- reactive({
    pred1 = as.numeric(data()$pred >= input$threshold)
  })
  output$ANOVA_plot <- renderPlot({
    anova.p_plot <- anova.p %>% subset(p.value < input$ANOVA_p) %>%
      mutate(var = reorder(x = var, X = p.value, min)) %>%
      ggplot(aes(x = var, y = p.value, color = var)) +
      geom_point() +
      geom_hline(yintercept = input$ANOVA_pbar, colour = "red", linetype = 3) +
      labs(x = "Variable", y = "p-value", title = "ANOVA p-value")
    print(anova.p_plot)
    output$downloadPlot1 <- downloadHandler(
      filename = function() { paste("ANOVA", '.png', sep='') },
      content = function(file) {
        ggsave(file, plot = anova.p_plot, device = "png")
      }
    )
  })
  output$logistic_plot <- renderPlot({
    
    # Prediction and true status when threshold = input$threshold plot
    pred_type_plot <- plot_pred_type_distribution(data(), input$threshold)
    print(pred_type_plot)
    output$downloadPlot2 <- downloadHandler(
      filename = function() { paste("Prediction Accuracy", '.png', sep='') },
      content = function(file) {
        ggsave(file, plot = pred_type_plot, device = "png")
      }
    )
  })
  output$pred_error <- renderPrint({
    
    k <- paste0("The In-sample Prediction Error under this threshold is ", 
                (1 - sum(pred1() == data()$true, na.rm = T)/length(pred1())))
    print(k)
  })
  output$roc_plot <- renderPlot({
    
    # Plot the roc, cost of FP = 1, cost of FN = 5
    plot.roc <- plot_roc(roc(), input$threshold, input$cost_of_FP, input$cost_of_FN)
    print(plot.roc)
    output$downloadPlot3 <- downloadHandler(
      filename = function() { paste("ROC Curve", '.png', sep='') },
      content = function(file) {
        ggsave(file, plot = plot.roc, device = "png")
      }
    )
  })
  output$cost <- renderPrint({

    FP <- sum(pred1() == 1 & data()$true == 0,na.rm = T)*input$cost_of_FP
    FN <- sum(pred1() == 0 & data()$true == 1, na.rm = T)*input$cost_of_FN

    k2 <- paste0("The Cost under this threshold is ", FP + FN)
    print(k2)
  })
})