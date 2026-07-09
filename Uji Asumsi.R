  # 4.1 Normalitas
  output$out_norm <- renderPrint({
    req(model_lm()); mod <- model_lm(); res <- resid(mod); n_data <- length(res)
    cat("--- UJI NORMALITAS ---\n")
    if(n_data < 5000) { uji <- shapiro.test(res); print(uji); cat("\nKeputusan:", ifelse(uji$p.value > 0.05, "Aman (Normal)", "Dilanggar"))
    } else { cat("Skewness:", skewness(res), "Kurtosis:", kurtosis(res), "\nAsimtotik check.") }
  })
  
  # 4.2 Heteroskedastisitas
  output$out_het <- renderPrint({
    req(model_lm(), processed_data()); mod <- model_lm(); data_reg <- processed_data()$data; form <- as.formula(paste(input$var_y, "~", paste(input$var_x, collapse = " + ")))
    cat("--- UJI HETEROSKEDASTISITAS ---\n")
    if(length(resid(mod)) < 1000) { uji <- bptest(form, data = data_reg) } else { data_reg$fit_val <- fitted(mod); uji <- bptest(form, ~ fit_val + I(fit_val^2), data = data_reg) }
    print(uji); cat("\nKeputusan:", ifelse(uji$p.value > 0.05, "Aman (Homoskedastisitas)", "Dilanggar"))
  })
  
  # 4.3 Multikolinearitas
  output$out_multi <- renderPrint({
    req(model_lm()); cat("--- UJI MULTIKOLINEARITAS ---\n")
    if(length(input$var_x) > 1) { print(vif(model_lm())) } else { cat("Dilewati (Hanya 1 X).\n") }
  })
  
  # 4.4 Autokorelasi
  output$out_auto <- renderPrint({
    req(model_lm()); cat("--- UJI AUTOKORELASI ---\n"); uji <- dwtest(model_lm()); print(uji)
    cat("\nKeputusan:", ifelse(uji$p.value > 0.05, "Aman", "Dilanggar"))
  })