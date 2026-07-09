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
  )

