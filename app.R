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