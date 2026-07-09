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
  # Menggunakan tema "flatly" untuk tampilan yang modern dan bersih
  theme = bs_theme(version = 5, bootswatch = "flatly", primary = "#2C3E50", secondary = "#18BC9C"),
  
  # --- TAB 1: BERANDA (LANDING PAGE) ---
  nav_panel(
    title = "Beranda",
    icon = icon("home"),
    
    div(
      class = "container", style = "margin-top: 40px; text-align: center;",
      
      # Hero Section
      tags$h1(tags$b("Platform Analisis Regresi Linear"), style = "color: #2C3E50; font-weight: 800;"),
      tags$p(class = "lead", "Solusi analisis statistik yang cepat, akurat, dan komprehensif."),
      tags$hr(style = "width: 100px; margin: 25px auto; border-top: 3px solid #18BC9C; opacity: 1;"),
      tags$br(),
      
      # Layout Grid untuk Kartu Informasi
      layout_column_wrap(
        width = 1/2, 
        
        # Kartu Kiri: Tentang Aplikasi
        card(
          class = "shadow-sm",
          card_header(icon("info-circle"), tags$b(" Tentang Aplikasi"), class = "bg-primary text-white"),
          card_body(
            style = "text-align: justify; font-size: 1.1em;",
            "Aplikasi ini dirancang untuk melakukan evaluasi model regresi linear berganda secara otomatis. 
             Fitur yang tersedia mencakup prapemrosesan data (penanganan missing value & outlier), 
             Uji Asumsi Klasik (Normalitas, Heteroskedastisitas, Multikolinearitas, Autokorelasi), 
             serta deteksi anomali data (Leverage, Cook's Distance, DFFITS)."
          )
        ),
        
        # Kartu Kanan: Panduan Penggunaan
        card(
          class = "shadow-sm",
          card_header(icon("list-ol"), tags$b(" Panduan Penggunaan"), style = "background-color: #18BC9C; color: white;"),
          card_body(
            style = "text-align: left; font-size: 1.1em;",
            tags$ol(
              tags$li("Siapkan data dalam format ", tags$b(".csv"), " atau ", tags$b(".xlsx"), "."),
              tags$li("Buka menu ", tags$b("Ruang Analisis"), " di bilah navigasi atas."),
              tags$li("Unggah data Anda pada panel sebelah kiri."),
              tags$li("Pilih Variabel Y (otomatis mendeteksi numerik) dan X."),
              tags$li("Klik ", tags$b("Jalankan Analisis"), " dan evaluasi hasilnya.")
            )
          )
        )
      )
    )
  )