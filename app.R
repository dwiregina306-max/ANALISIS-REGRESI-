library(shiny)
library(readxl)
library(dplyr)
library(lmtest)
library(car)
library(moments)
library(bslib)

# ==========================================
# 1. USER INTERFACE (UI)
# ==========================================
ui <- page_navbar(
  title = "Analisis Regresi",
  theme = bs_theme(version = 5, bootswatch = "flatly", primary = "#2C3E50", secondary = "#18BC9C"),
  
  # --- TAB 1: BERANDA (LANDING PAGE) ---
  nav_panel(
    title = "Beranda", icon = icon("home"),
    div(
      class = "container", style = "margin-top: 40px; text-align: center;",
      tags$h1(tags$b("Platform Analisis Regresi Linear"), style = "color: #2C3E50; font-weight: 800;"),
      tags$p(class = "lead", "Solusi analisis statistik yang cepat, akurat, dan komprehensif."),
      tags$hr(style = "width: 100px; margin: 25px auto; border-top: 3px solid #18BC9C; opacity: 1;"),
      tags$br(),
      layout_column_wrap(
        width = 1/2, 
        card(
          class = "shadow-sm", card_header(icon("info-circle"), tags$b(" Tentang Aplikasi"), class = "bg-primary text-white"),
          card_body(style = "text-align: justify; font-size: 1.1em;", "Aplikasi evaluasi model regresi linear berganda secara otomatis. Mencakup prapemrosesan, Uji Asumsi Klasik, dan deteksi anomali data.")
        ),
        card(
          class = "shadow-sm", card_header(icon("list-ol"), tags$b(" Panduan Penggunaan"), style = "background-color: #18BC9C; color: white;"),
          card_body(style = "text-align: left; font-size: 1.1em;", tags$ol(tags$li("Siapkan data (.csv/.xlsx)."), tags$li("Buka Ruang Analisis."), tags$li("Unggah data."), tags$li("Pilih X dan Y."), tags$li("Jalankan.")))
        )
      )
    )
  ),
  nav_panel(
    title = "Ruang Analisis",
    icon = icon("chart-line"),
    
    sidebarLayout(
      sidebarPanel(
        width = 3, 
        tags$h4(tags$b("Panel Input"), style = "color: #2C3E50;"),
        tags$hr(),
        fileInput("file_data", "1. Unggah Data (CSV/XLSX)", accept = c(".csv", ".xlsx")),
        uiOutput("ui_y"),
        uiOutput("ui_x"),
        tags$br(),
        actionButton("run_analysis", "Jalankan Analisis", 
                     icon = icon("play"), 
                     class = "btn-primary", 
                     style = "width: 100%; font-weight: bold; border-radius: 8px;")
      ),
      
      mainPanel(
        width = 9,
        navset_card_underline(
          title = tags$b("Output Analisis"),
          
          # 1. Preprocessing
          nav_panel("Preprocessing", verbatimTextOutput("out_preprocessing")),
          
          # 2. Uji Asumsi (Rekomendasi Solusi akan muncul di sini jika melanggar)
          nav_panel("Uji Asumsi", 
                    br(),
                    navset_pill( 
                      nav_panel("1. Normalitas", verbatimTextOutput("out_norm")),
                      nav_panel("2. Heteroskedastisitas", verbatimTextOutput("out_het")),
                      nav_panel("3. Multikolinearitas", verbatimTextOutput("out_multi")),
                      nav_panel("4. Autokorelasi", verbatimTextOutput("out_auto"))
                    )
          ),
          
          # 3. Diagnostik Plot
          nav_panel("Diagnostic Plots", 
                    br(),
                    fluidRow(
                      column(6, card(full_screen = TRUE, plotOutput("plot_resid_fit"))),
                      column(6, card(full_screen = TRUE, plotOutput("plot_qq"))),
                      column(6, card(full_screen = TRUE, plotOutput("plot_scale_loc"))),
                      column(6, card(full_screen = TRUE, plotOutput("plot_resid_lev")))
                    ),
                    br(),
                    verbatimTextOutput("out_plot_interp")
          ),
          
          # 4. Output Regresi
          nav_panel("Output Regresi", verbatimTextOutput("out_regresi")),
          
          # 5. Interpretasi Model
          nav_panel("Interpretasi Model", verbatimTextOutput("out_model_interpretasi")),
          
          # 6. R-Square PRESS
          nav_panel("R-Square PRESS", verbatimTextOutput("out_rsquare_press"))
        )
      )
    )
  )
)
# ==========================================
# 2. SERVER LOGIC
# ==========================================
server <- function(input, output, session) {
  
  # 1. Membaca Data (Kompatibel untuk CSV & XLSX)
  raw_data <- reactive({
    req(input$file_data)
    ext <- tolower(tools::file_ext(input$file_data$name)) 
    
    if (ext == "csv") {
      df <- read.csv(input$file_data$datapath)
    } else if (ext == "xlsx") {
      df <- as.data.frame(read_excel(input$file_data$datapath)) 
    } else {
      stop("Format file tidak didukung. Gunakan CSV atau XLSX.")
    }
    return(df)
  })
  
  # 2. UI Dinamis (DENGAN FILTER NUMERIK)
  output$ui_y <- renderUI({
    req(raw_data())
    data_num <- raw_data() %>% select(where(is.numeric))
    selectInput("var_y", "Pilih Variabel Dependen (Y) [Hanya Numerik]:", choices = names(data_num))
  })
  
  output$ui_x <- renderUI({
    req(raw_data())
    selectInput("var_x", "Pilih Variabel Independen (X):", choices = names(raw_data()), multiple = TRUE)
  })
  
  # 3. Preprocessing Data
  processed_data <- eventReactive(input$run_analysis, {
    req(raw_data(), input$var_y, input$var_x)
    
    df <- raw_data()[, c(input$var_y, input$var_x), drop = FALSE]
    n_awal <- nrow(df)
    
    na_rows <- which(!complete.cases(df))
    df_nona <- na.omit(df)
    
    Q1 <- quantile(df_nona[[input$var_y]], 0.25)
    Q3 <- quantile(df_nona[[input$var_y]], 0.75)
    IQR_val <- Q3 - Q1
    lower_bound <- Q1 - 1.5 * IQR_val
    upper_bound <- Q3 + 1.5 * IQR_val
    
    outlier_rows <- which(df_nona[[input$var_y]] < lower_bound | df_nona[[input$var_y]] > upper_bound)
    df_clean <- df_nona
    if(length(outlier_rows) > 0) {
      df_clean <- df_nona[-outlier_rows, ]
    }
    
    log_text <- paste(
      "--- LAPORAN PREPROCESSING DATA ---\n",
      "Jumlah data awal:", n_awal, "baris\n\n",
      "1. Penanganan Missing Value:\n",
      "- Tindakan: Menghapus baris yang mengandung nilai kosong (NA).\n",
      "- Baris yang dihapus:", ifelse(length(na_rows) > 0, paste(na_rows, collapse = ", "), "Tidak ada"), "\n\n",
      "2. Penanganan Outlier Ekstrem (Metode IQR pada Variabel Dependen):\n",
      "- Tindakan: Menghapus nilai di luar batas [", round(lower_bound, 2), ",", round(upper_bound, 2), "]\n",
      "- Baris yang terindikasi dan dihapus:", ifelse(length(outlier_rows) > 0, paste(outlier_rows, collapse = ", "), "Tidak ada"), "\n\n",
      "Jumlah data akhir siap dianalisis:", nrow(df_clean), "baris"
    )
    
    list(data = df_clean, log = log_text)
  })
  
  output$out_preprocessing <- renderPrint({
    cat(processed_data()$log)
  })
  
  # Membuat Model Regresi
  model_lm <- reactive({
    data_reg <- processed_data()$data
    formula_reg <- as.formula(paste(input$var_y, "~", paste(input$var_x, collapse = " + ")))
    lm(formula_reg, data = data_reg)
  })

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
  # 4.3 Uji Multikolinearitas + Solusi Jika Melanggar
  output$out_multi <- renderPrint({
    req(model_lm())
    mod <- model_lm()
    
    cat("--- UJI MULTIKOLINEARITAS ---\n\n")
    if(length(input$var_x) > 1) {
      cat("Metode: Variance Inflation Factor (VIF)\n")
      cat("Aturan Rentang:\n")
      cat(" - 0 hingga 5  : Aman dari multikolinearitas\n")
      cat(" - 5 hingga 10 : Ada indikasi multikolinearitas\n")
      cat(" - Di atas 10  : Pasti ada multikolinearitas parah\n\n")
      
      vif_res <- vif(mod)
      print(vif_res)
      
      # FITUR BARU: Deteksi otomatis apakah ada nilai VIF atau GVIF yang melanggar batas aman (>10)
      any_violation <- FALSE
      if (is.matrix(vif_res)) {
        if ("GVIF" %in% colnames(vif_res)) {
          if (any(vif_res[, "GVIF"] > 10)) any_violation <- TRUE
        } else {
          if (any(vif_res > 10)) any_violation <- TRUE
        }
      } else {
        if (any(vif_res > 10)) any_violation <- TRUE
      }
      
      if (any_violation) {
        cat("\n💡 REKOMENDASI SOLUSI (ASUMSI DILANGGAR):\n")
        cat("1. Keluarkan salah satu dari dua variabel independen (X) yang saling berkolerasi sangat kuat (yang memicu nilai VIF membengkak).\n")
        cat("2. Gabungkan variabel-variabel X yang mirip menjadi satu skor komposit tunggal (misal lewat rata-rata atau analisis PCA).\n")
        cat("3. Perbanyak ukuran sampel data Anda jika memungkinkan, karena penambahan data dapat membantu memisahkan efek antar variabel X.")
      }
      
    } else {
      cat("Uji VIF dilewati karena variabel independen (X) yang dipilih hanya ada satu.\n")
    }
  })
  
  # 4.4 Uji Autokorelasi + Solusi Jika Melanggar
  output$out_auto <- renderPrint({
    req(model_lm())
    mod <- model_lm()
    
    cat("--- UJI AUTOKORELASI ---\n\n")
    cat("Metode: Durbin-Watson Test\n")
    cat("H0: Tidak ada autokorelasi pada residual (Aman)\n")
    cat("H1: Terdapat autokorelasi (Bermasalah)\n\n")
    
    uji_dw <- dwtest(mod)
    print(uji_dw)
    
    keputusan_dw <- ifelse(uji_dw$p.value > 0.05, "Gagal tolak H0", "Tolak H0")
    interp_dw <- ifelse(uji_dw$p.value > 0.05, 
                        "Interpretasi: Observasi independen satu sama lain. Asumsi TERPENUHI.", 
                        "Interpretasi: Terdapat pola autokorelasi. Asumsi DILANGGAR.")
    cat("Keputusan:", keputusan_dw, "\n")
    cat(interp_dw, "\n")
    
    # FITUR BARU: Solusi bersyarat jika autokorelasi dilanggar (p-value <= 0.05)
    if(uji_dw$p.value <= 0.05) {
      cat("\n💡 REKOMENDASI SOLUSI (ASUMSI DILANGGAR):\n")
      cat("1. Jika data Anda adalah deret waktu (Time Series), tambahkan variabel lag (misal Y pada t-1 atau X pada t-1) ke dalam model regresi.\n")
      cat("2. Gunakan metode estimasi khusus deret waktu seperti prosedur Cochrane-Orcutt atau Prais-Winsten untuk membersihkan korelasi serial.\n")
      cat("3. Ubah variabel data Anda ke dalam bentuk nilai selisih pertama (First Difference: data sekarang dikurangi data sebelumnya).")
    }
  })
  
  # 5. Diagnostic Plots
  output$plot_resid_fit <- renderPlot({ plot(model_lm(), 1, main = "Residuals vs Fitted") })
  output$plot_qq <- renderPlot({ plot(model_lm(), 2, main = "Normal Q-Q") })
  output$plot_scale_loc <- renderPlot({ plot(model_lm(), 3, main = "Scale-Location") })
  output$plot_resid_lev <- renderPlot({ plot(model_lm(), 5, main = "Residuals vs Leverage") })
  
  output$out_plot_interp <- renderPrint({
    cat("--- PENJELASAN 4 PLOT DIAGNOSTIK ---\n\n")
    cat("1. Residuals vs Fitted\n")
    cat("Tujuan: Mengecek asumsi linearitas dan homoskedastisitas.\n")
    cat("Kondisi Ideal: Titik titik tersebar acak tanpa membentuk pola (seperti huruf U atau corong) di sekitar garis horizontal nol.\n\n")
    
    cat("2. Normal Q-Q\n")
    cat("Tujuan: Mengecek apakah residual berdistribusi normal.\n")
    cat("Kondisi Ideal: Titik titik harus mengikuti atau menempel erat pada garis diagonal putus putus.\n\n")
    
    cat("3. Scale-Location\n")
    cat("Tujuan: Pemeriksaan lanjutan untuk asumsi varians konstan (Homoskedastisitas).\n")
    cat("Kondisi Ideal: Garis merah mendatar (horizontal) dan titik tersebar merata secara acak tanpa melebar atau menyempit di satu sisi.\n\n")
    
    cat("4. Residuals vs Leverage\n")
    cat("Tujuan: Mendeteksi keberadaan outlier yang memiliki pengaruh kuat (influential points) yang dapat menarik garis regresi.\n")
    cat("Kondisi Ideal: Tidak ada titik yang melewati garis putus putus merah (Cook's distance). Jika ada, data tersebut mengubah model secara drastis.\n")
  })