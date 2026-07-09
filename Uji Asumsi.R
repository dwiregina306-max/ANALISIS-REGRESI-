  # 4.1 Uji Normalitas + Solusi Jika Melanggar
  output$out_norm <- renderPrint({
    req(model_lm())
    mod <- model_lm()
    res <- resid(mod)
    n_data <- length(res)
    
    cat("--- UJI NORMALITAS RESIDUAL ---\n\n")
    if(n_data < 5000) {
      uji_norm <- shapiro.test(res)
      cat("Metode: Shapiro-Wilk (Karena N < 5000)\n")
      cat("Aturan: Data normal jika p-value > 0.05\n")
      cat("H0: Residual berdistribusi normal\n")
      cat("H1: Residual tidak berdistribusi normal\n\n")
      print(uji_norm)
      
      keputusan <- ifelse(uji_norm$p.value > 0.05, "Gagal tolak H0", "Tolak H0")
      interp <- ifelse(uji_norm$p.value > 0.05, 
                       "Interpretasi: Pola residual menyebar secara acak dan seimbang (normal). Asumsi TERPENUHI.", 
                       "Interpretasi: Pola residual tidak normal. Asumsi DILANGGAR.")
      cat("Keputusan:", keputusan, "\n")
      cat(interp, "\n")
      
      # FITUR BARU: Solusi bersyarat jika dilanggar (p-value <= 0.05)
      if(uji_norm$p.value <= 0.05) {
        cat("\n💡 REKOMENDASI SOLUSI (ASUMSI DILANGGAR):\n")
        cat("1. Lakukan transformasi data pada variabel Y (misal: bentuk Logaritma Natural [Ln], Akar Kuadrat, atau Box-Cox).\n")
        cat("2. Cek kembali bagian 'Preprocessing', kemungkinan masih ada data pencilan (outlier) ekstrem yang mengganggu distribusi.\n")
        cat("3. Jika ukuran sampel Anda sebenarnya cukup besar (>30 atau >100), Anda dapat mengabaikan pelanggaran ini berdasarkan Asimtotik / Teorema Limit Pusat.")
      }
      
    } else {
      skew <- skewness(res)
      kurt <- kurtosis(res)
      cat("Metode: Skewness & Kurtosis (Karena N >= 5000)\n")
      cat("Aturan: Normal jika Skewness mendekati 0 (ideal: -2 sd 2) dan Kurtosis mendekati 3.\n\n")
      cat("Nilai Skewness:", skew, "\n")
      cat("Nilai Kurtosis:", kurt, "\n\n")
      
      is_normal <- abs(skew) < 2 && (kurt > 1 && kurt < 5)
      interp <- ifelse(is_normal,
                       "Interpretasi: Metrik kelencengan data wajar. Asumsi normalitas TERPENUHI untuk data besar.",
                       "Interpretasi: Kelencengan data terlalu ekstrem. Asumsi DILANGGAR.")
      cat("Keputusan: Berdasarkan evaluasi kelencengan.\n")
      cat(interp, "\n")
      
      # FITUR BARU: Solusi bersyarat data besar jika dilanggar
      if(!is_normal) {
        cat("\n💡 REKOMENDASI SOLUSI (ASUMSI DILANGGAR):\n")
        cat("1. Distribusi data terlalu miring. Pertimbangkan melakukan transformasi non-linear pada variabel Y.\n")
        cat("2. Jika sebaran data asli sangat asimetris, pertimbangkan beralih menggunakan Generalized Linear Model (GLM) dengan link function yang sesuai.")
      }
    }
  })
  
  # 4.2 Uji Heteroskedastisitas + Solusi Jika Melanggar
  output$out_het <- renderPrint({
    req(model_lm(), processed_data())
    mod <- model_lm()
    res <- resid(mod)
    n_data <- length(res)
    data_reg <- processed_data()$data
    formula_reg <- as.formula(paste(input$var_y, "~", paste(input$var_x, collapse = " + ")))
    
    cat("--- UJI HETEROSKEDASTISITAS ---\n\n")
    cat("H0: Ragam residual konstan (Homoskedastisitas / Aman)\n")
    cat("H1: Ragam residual tidak konstan (Heteroskedastisitas / Bermasalah)\n\n")
    
    if(n_data < 1000) {
      cat("Metode: Breusch-Pagan Test (Karena N < 1000)\n")
      uji_het <- bptest(formula_reg, data = data_reg)
    } else {
      cat("Metode: White Test / Pendekatan Fitted Values (Karena N >= 1000)\n")
      data_reg$fit_val <- fitted(mod)
      uji_het <- bptest(formula_reg, ~ fit_val + I(fit_val^2), data = data_reg)
    }
    print(uji_het)
    
    keputusan_het <- ifelse(uji_het$p.value > 0.05, "Gagal tolak H0", "Tolak H0")
    interp_het <- ifelse(uji_het$p.value > 0.05, 
                         "Interpretasi: Ragam residual konstan. Model stabil (Asumsi TERPENUHI).", 
                         "Interpretasi: Ragam residual tidak konstan. Model tidak stabil (Asumsi DILANGGAR).")
    cat("Keputusan:", keputusan_het, "\n")
    cat(interp_het, "\n")
    
    # FITUR BARU: Solusi bersyarat jika heteroskedastisitas dilanggar (p-value <= 0.05)
    if(uji_het$p.value <= 0.05) {
      cat("\n💡 REKOMENDASI SOLUSI (ASUMSI DILANGGAR):\n")
      cat("1. Lakukan transformasi Logaritma Natural pada variabel Y (mengubah Y menjadi Ln_Y) untuk menstabilkan varians (ragam) residual.\n")
      cat("2. Gunakan metode estimasi WLS (Weighted Least Squares / Kuadrat Terkecil Terbobot) alih-alih OLS standar.\n")
      cat("3. Anda bisa menggunakan penyesuaian 'Robust Standard Errors' (White's Correction) saat melakukan uji signifikansi koefisien secara manual.")
    }
  })