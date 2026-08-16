library(httr)
library(xml2)
library(tidyverse)
library(glue)
library(forcats)
library(openxlsx)
library(gt)
library(DT)
library(plotly)
library(ggplot2)
library(sf)
library(hrbrthemes)
library(bs4Dash)
library(gtExtras)
library(googlesheets4)
library(geodata)
library(bslib)
library(shiny)
library(leaflet)
library(rmapshaper)
library(geojsonsf)
library(rsconnect)
library(leaflet.extras)

sa_path <- "service_account.json"

if (!file.exists(sa_path) && nzchar(Sys.getenv("GSHEETS_SERVICE_ACCOUNT_JSON"))) {
  sa_path <- tempfile(fileext = ".json")
  writeLines(Sys.getenv("GSHEETS_SERVICE_ACCOUNT_JSON"), sa_path)
}

gs4_auth(path = sa_path)

df <- read_sheet("https://docs.google.com/spreadsheets/d/1gAf-ZHo--M9baRjjQbKeW9B5jWXiHtkj8IXwU4gNxN4", 1)


colsname_to_change <- c(
  "patient_id", "proposed_patient_id", "patient_name", "patient_gender",
  "birth_date", "education", "district", "religion", "profession",
  "patient_number", "nid_no_birth_certificate", "stage", "treatment",
  "hospital", "doctors_name", "date_of_diagnosis", "cancer_type",
  "cancer_affected_body_parts", "registration_date", "admission_number",
  "total_stay", "status", "attd_name", "attd_age", "attd_gender",
  "attd_patient_relation", "attd_district", "attd_profession",
  "attd_education_lvl", "attd_phone", "family_income",
  "cancer_treatment_cost", "marital_status", "num_children",
  "household_size", "missing_info_checklist"
)

colnames(df) <- colsname_to_change
df <- df %>% filter(proposed_patient_id != "Registration Date is missing")

df <- df |>
  mutate(
    across(
      c(patient_gender, education, district, profession, stage,
        treatment, hospital, cancer_type, cancer_affected_body_parts,
        status, attd_gender, attd_patient_relation, attd_district,
        attd_profession, attd_education_lvl, marital_status, religion),
      ~ factor(trimws(as.character(.x)))
    ),
    across(
      c(birth_date, registration_date, date_of_diagnosis),
      ~ as.Date(.x, origin = "1899-12-30")
    ),
    age=as.integer(difftime(registration_date, birth_date, units = "days") / 365.25),
    age_group=cut(age, breaks = c(-Inf,14, 20,45,Inf), labels = c("Child (≤14)", "Youth (15–20)", "Adult (21–45)", "Elder (45+)")),
    major_cancer_sites=fct_collapse(cancer_affected_body_parts,
                                    Oral=c("Lip, Oral Cavity", "Salivary Glands", "Oropharynx", "Nasopharynx","Hypopharynx"),
                                    Digestive=c("Oesophagus", "Anus", "Stomach","Colon","Rectum","Liver and Intrahepatic Bile Ducts","Pancreas",
                                                "Gallbladder"),
                                    Respiratory=c("Larynx","Trachea, Bronchus, and Lung","Mesothelioma"),
                                    Hematologic=c("Hodgkin Lymphoma","Non-Hodgkin Lymphoma","Multiple Myeloma","Leukaemia"),
                                    `Reproductive (Male)`=c("Penis","Prostate","Testis"),
                                    `Reproductive (Female)`=c("Breast", "Vulva","Vagina","Cervix Uteri","Corpus Uteri","Ovary"),
                                    `Urinary Tract`= c("Kidney","Bladder"),
                                    Skin=c("Melanoma of Skin", "Non-melanoma Skin Cancer" ),
                                    Musculoskeletal=c("Bone sarcoma","Rhabdomyosarcoma","Soft tissue sarcoma"),
                                    `Other cancers (brain ocular and thyroid)`=c("Brain and Central Nervous System","Ocular cancer","Thyroid"
                                    )))
replace_values <- function(x, patterns, replacements) {
  for (i in seq_along(patterns)) {
    x <- ifelse(grepl(patterns[i], trimws(x), ignore.case = TRUE), replacements[i], x)
  }
  x
}

patterns_sf     <- c("Chittagong","Comilla","Barisal","Netrakona","Jessore",
                     "Nawabganj","Brahamanbaria","Brahmanbaria","Maulvibazar",
                     "Bogra","Cox.s Bazar","Cox's Bazar")
replacements_sf <- c("Chattogram","Cumilla","Barishal","Netrokona","Jashore",
                     "Chapai Nawabganj","Brahmanbaria","Brahmanbaria","Moulvibazar",
                     "Bogura","Coxs Bazar","Coxs Bazar")

df <- df |>
  mutate(across(
    c(district, attd_district),
    ~ factor(str_to_title(replace_values(
      trimws(gsub("\\s*\\(.*\\)", "", as.character(.x))),
      patterns_sf, replacements_sf
    )))
  ))

geom_cache <- "data/bd_sf_geom.rds"

if (file.exists(geom_cache)) {
  bd_sf_geom <- readRDS(geom_cache)
} else {
  bd_sf_geom <- gadm("BGD", level = 2, path = "data/") |>
    st_as_sf() |>
    rmapshaper::ms_simplify(keep = 0.05, keep_shapes = TRUE) |>
    st_cast("MULTIPOLYGON") |>
    select(geometry, NAME_2) |>
    mutate(NAME_2 = str_to_title(replace_values(NAME_2, patterns_sf, replacements_sf)))
  saveRDS(bd_sf_geom, geom_cache)
}

df2<-df%>%filter(age<=14)
df2<-df2%>%mutate(age_group=cut(age,c(-Inf,5,Inf), labels = c("0-5", "5-14")))
age_adhoc_2<- df2 %>%select(age_group, patient_gender) %>%group_by(age_group, patient_gender) %>%summarise(n = n(),prop = n / nrow(df2),.groups = "drop")
age_adhoc <- df2 %>%select(age_group, major_cancer_sites) %>%group_by(age_group, major_cancer_sites) %>%summarise(n = n(),prop = n / nrow(df2), .groups = "drop" ) %>%arrange(prop)

df_r<-df%>%select(age_group, patient_gender, district,cancer_treatment_cost, family_income, major_cancer_sites, hospital, treatment, stage, marital_status, num_children, household_size, registration_date, date_of_diagnosis)%>%
  drop_na()

ui <- fluidPage(
  tags$style(HTML("
  /* Sticky sidebar */
  .bslib-sidebar-layout > .sidebar {
    position: sticky;
    top: 0;
    align-self: start;
    min-height: 90vh;
    overflow-y: auto;
  }

  /* Hide resize handle — correct class name in 0.9.0 */
  .bslib-sidebar-resize-handle {
    display: none !important;
  }

  /* Sticky toggle button */
  .bslib-sidebar-layout > .collapse-toggle {
    position: sticky;
    top: 10px;
    align-self: start;
    z-index: 999;
  }
  .leaflet-container {
    background: #ffffff !important;
  }
  .bslib-sidebar-layout > .sidebar .accordion-item {
  background-color: transparent !important;
  border-left: none !important;
  border-right: none !important;
}

.bslib-sidebar-layout > .sidebar .accordion-button {
  background-color: transparent !important;
  box-shadow: none !important;
}

.bslib-sidebar-layout > .sidebar .accordion-button:not(.collapsed) {
  background-color: transparent !important;
  box-shadow: none !important;
}

.bslib-sidebar-layout > .sidebar .accordion-body {
  background-color: transparent !important;
}

.bslib-sidebar-layout > .sidebar .accordion {
  --bs-accordion-bg: transparent;
}
")),
  
  page_navbar(
    title = "BANCAT- AlokNibash Cancer Registry",
    id    = "navbar",
    theme = bs_theme(
      preset    = "lumen",
      base_font = font_collection(
        font_google("Poppins", local = FALSE),
        "Roboto", "sans-serif"
      )
    ),
    
    nav_panel(
      "Overview",
      layout_sidebar(                      
        sidebar = sidebar(
          resizable = FALSE,
          width    = 350,
          position = "left",
          bg="#ffffff",
          border       = TRUE,
          border_color = "#dee2e6",
          bslib::accordion(
            id   = "ui_download",
            open = FALSE,
            bslib::accordion_panel(
              "Filter",
              icon = icon("filter"),
              dateRangeInput(
                inputId   = "date", label = "Date Range",
                min       = min(df$registration_date, na.rm = TRUE),
                max       = max(df$registration_date, na.rm = TRUE),
                start     = min(df$registration_date, na.rm = TRUE),
                end       = max(df$registration_date, na.rm = TRUE),
                startview = "year", format = "yyyy-mm-dd"
              ),
              selectInput("gender", "Gender:", c("All", levels(df$patient_gender))),
              selectInput("age_group", "Age Group: ",c("All", levels(df$age_group)), selected = "All", multiple = TRUE),
              selectInput(
                "cancer_type", "Type of Cancer",
                choices  = c("All", levels(df$major_cancer_sites)),
                selected = "All", multiple = TRUE
              ),
              
              actionButton("reset_overview",
                           label = tagList(icon("rotate-left"), " Reset"),
                           class="btn btn-outline-secondary btn-sm float-end")
            ),
            
            bslib::accordion_panel(
              "Report",
              icon  =icon("file-code"),
              checkboxInput(
                "report_all",
                tags$span("Select All Sections", style = "font-weight:600;"),
                value = TRUE
              ),
              checkboxGroupInput(
                inputId  = "report_sections",
                label    = NULL,
                choices  = c(
                  "Admission Trends"   = "trends",
                  "Age & Gender"       = "age_gender",
                  "Economic Situation" = "economic",
                  "Family Dynamics"    = "family",
                  "District Map"       = "map",
                  "Cancer Summary"     = "cancer"
                ),
                selected = c("trends", "age_gender", "economic", "family", "map", "cancer")
              ),
              downloadButton(
                outputId = "download_pdf",
                label    = tagList(icon("file"), " Download Report"),
                class    = "btn-danger btn-sm w-100 mt-1"
              )
            )
          )
          
        ),
        
        navset_card_tab(
          id = "overview_tabs",
          
          nav_panel(
            "Admission Trends",
            icon = icon("chart-line"),
            card(plotlyOutput("plot")),
            card(
              layout_column_wrap(
                width = 1/2,
                value_box(
                  title            = "Total Registered Patients:",
                  value            = uiOutput("total_cancer_type"),
                  showcase         = icon("hospital-user"),
                  showcase_layout  = "left center",
                  theme            = value_box_theme(bg = "#E8EDF2", fg = "#2C3947"),
                  p("(In Alok Nibash 1 and 2)",
                    style = "font-size: 0.85rem; color: gray;"),
                  div(style = "margin-top: 0.75rem;",
                      bslib::input_switch("show_1_pct", "Show (%)", value = FALSE)
                  )
                ),
                value_box(
                  title           = "Monthly Average Growth Rate of New Admissions:",
                  "Growth rate over time:",
                  value           = uiOutput("avg_growth"),
                  showcase        = plotlyOutput("growth_rate", height = "100%"),
                  showcase_layout = showcase_bottom(height = 0.40),
                  height          = "260px",
                  theme           = value_box_theme(fg = "#093C5D", bg = "#F5F5F5")
                )
              )
            )
          ),
          
          nav_panel(
            "Patient Demographics",
            icon = icon("users"),
            bslib::accordion(
              id="demo_accordion",
              open = FALSE,
              bslib::accordion_panel(
                "Age and Gender",
                icon = icon("people-group"),
                layout_column_wrap(
                  width = 1/3,
                  value_box(
                    "Patients' Average Age",
                    value           = uiOutput("age"),
                    showcase        = gt_output("age_freq"),
                    showcase_layout = showcase_bottom(height = 0.45),
                    theme           = "text-purple"
                  ),
                  value_box(
                    title           = "Male Patients", "(Count)",
                    value           = uiOutput("male_count"),
                    showcase        = icon("person"),
                    showcase_layout = "left center",
                    theme           = value_box_theme(bg = "#DFF1F1", fg = "#1a6eb5")
                  ),
                  value_box(
                    title           = "Female Patients", "(Count)",
                    value           = uiOutput("female_count"),
                    showcase        = icon("person-dress"),
                    showcase_layout = "left center",
                    theme           = value_box_theme(bg = "#FFF6F6", fg = "#ad5c7f")
                  )
                ),
                bslib::input_switch("show_pct", "Show (%)", value = FALSE)
              ),
              bslib::accordion_panel(
                "Economic Situation",
                icon = icon("money-bill-trend-up"),
                card(
                  card_header("Treatment Cost:"),
                  height = 450,
                  card_body(min_height = 350, plotlyOutput("expenditure"))
                ),
                card(
                  card_header("Average Monthly Income Distribution:"),
                  height = 500,
                  card_body(min_height = 450, plotlyOutput("income_dist"))
                )
              ),
              bslib::accordion_panel(
                "Family Dynamic",
                icon = icon("people-roof"),
                layout_column_wrap(
                  width = 1/2,
                  value_box(
                    title = "Average Household size:",
                    value=textOutput("household_size_output"),
                    showcase        = icon("house-user"),
                    showcase_layout = "left center",
                    theme           = value_box_theme(bg = "#DFF1F1", fg = "#093C5D")
                  ),
                  value_box(
                    title = "Average Number of Children per Household",
                    value=textOutput("children_number"),
                    showcase        = icon("children"),
                    showcase_layout = "left center",
                    theme           = value_box_theme(bg = "#F5F5F5", fg = "#3E8E7E")
                  )
                )
              ),
              bslib::accordion_panel(
                title = "Additional Information",
                icon=icon("plus-circle"),
                card(
                  card_header("Patient Distribution by District"),
                  height = 850,
                  card_body(min_height = 700, leafletOutput("district_map"))
                )
              )
            )
            
          ),
          
          nav_panel(
            "Patient Medical Summary",
            icon = icon("file-medical"),
            card(
              card_header("Type of Cancer"),
              height = 650,
              card_body(min_height = 550, plotlyOutput("cancer_barplot"))
            )
          )
        )
      )
    )
  )
)

server<-function(input, output, session) {
  filtered_data <- reactive({
    df %>%
      filter(registration_date >= input$date[1], registration_date <= input$date[2]) %>%
      filter(if (input$gender == "All") TRUE else patient_gender == input$gender) %>%
      filter(if ("All" %in% input$cancer_type) TRUE else major_cancer_sites %in% input$cancer_type)%>%
      filter(if ("All" %in% input$age_group) TRUE else age_group %in% input$age_group)%>%
      group_by(registration_date) %>%
      summarise(count = n(), .groups = "drop") %>%
      arrange(registration_date) %>%
      mutate(cumulative_count = cumsum(count))
  })
  
  
  
  filtered_data_1<-reactive({
    df%>%filter(registration_date >= input$date[1], registration_date <= input$date[2]) %>%
      filter(if (input$gender == "All") TRUE else patient_gender == input$gender) %>%
      filter(if ("All" %in% input$cancer_type) TRUE else major_cancer_sites %in% input$cancer_type)%>%
      filter(if ("All" %in% input$age_group) TRUE else age_group %in% input$age_group)
  })
  
  monetary_data<-reactive({
    filtered_data_1()%>%select(registration_date, cancer_treatment_cost, family_income)%>%mutate(Year = year(registration_date))%>%
      group_by(Year) %>% summarise(
        average_treatment_cost = round(mean(cancer_treatment_cost, na.rm = TRUE), -3),
        average_monthly_income = mean(family_income, na.rm = TRUE), .groups = "drop") %>%
      mutate(average_yearly_income = round(12 * average_monthly_income, -3))
  })
  
  gender_count <- reactive({
    filtered_data_1() %>%
      filter(!is.na(patient_gender)) %>%
      group_by(patient_gender) %>%
      summarise(count = n(), .groups = "drop") %>%
      mutate(pct = count / sum(count))
  })
  
  growth_rate_data<-reactive({
    filtered_data() %>%
      mutate(Year = year(registration_date), Month = month(registration_date)) %>%
      group_by(Year, Month) %>%
      summarise(cumulative_count = max(cumulative_count, na.rm = TRUE), .groups = "drop") %>%
      mutate(
        date        = make_date(Year, Month, 1),
        date_label  = format(date, "%b %Y"),
        monthly_new = cumulative_count - lag(cumulative_count),
        growth_rate = round((monthly_new / lag(cumulative_count)) * 100, 1),
        growth_rate = ifelse(row_number() <= 3, NA, growth_rate)
      )
  })
  
  
  output$plot <- renderPlotly({
    
    plot_ly(filtered_data() , x = ~registration_date, y = ~cumulative_count,
            type = "scatter", mode = "lines",
            fill = "tozeroy",
            fillcolor = "rgba(70,44,125,0.5)",
            line = list(color = "#462C7D")) %>%
      config(displayModeBar = FALSE) %>%
      layout(
        dragmode = FALSE,
        xaxis = list(
          title = "Registration Date",
          range = c(input$date_start - 15, input$date_end + 15),
          fixedrange = TRUE
        ),
        yaxis = list(
          title = "Total Patients",
          rangemode = "nonnegative",
          fixedrange = TRUE
        )
      )
  })
  output$age <- renderText({
    round(mean(filtered_data_1()$age, na.rm = TRUE), 1)
  })
  
  output$male_count <- renderText({
    row <- gender_count() %>% filter(patient_gender == "Male")
    if (nrow(row) == 0) return("0")
    fmt_val(row$count, row$pct, if (input$show_pct) "pct" else "count") 
    
  })
  output$total_cancer_type<-renderText({
    val<-filtered_data()%>%summarise(total = max(cumulative_count, na.rm=TRUE))%>% pull(total)
    if (length(val) == 0) return("0")
    fmt_val(val, val/nrow(df), if(input$show_1_pct) "pct" else "count")
    
    
  })
  output$female_count <- renderText({
    row <- gender_count() %>% filter(patient_gender == "Female")
    if (nrow(row) == 0) return("0")
    fmt_val(row$count, row$pct, if (input$show_pct) "pct" else "count")
  })
  
  
  ##Growth rate
  
  output$avg_growth <- renderText({
    val <- growth_rate_data()%>%pull(growth_rate) %>% mean(na.rm = TRUE) %>%round(1) %>% paste0("%") 
    if (length(val) == 0) 0 else val
  })
  
  output$growth_rate <- renderPlotly({
    r1 <- growth_rate_data() %>% filter(!is.na(growth_rate))
    
    plot_ly(r1, x = ~date, y = ~growth_rate,
            type = "scatter", mode = "lines",
            fill = "tozeroy",
            fillcolor = "rgba(187, 213, 218, 0.6)",
            line = list(color = "#3B7597"),
            hovertemplate = "%{x|%b %Y}: %{y:.1f}%<extra></extra>") %>%  
      config(displayModeBar = FALSE, responsive = TRUE) %>%
      layout(
        dragmode = FALSE,
        margin = list(l = 5, r = 5, t = 5, b = 5),
        xaxis = list(
          title = "",
          tickformat = "%b %Y",
          fixedrange = TRUE,
          showgrid = FALSE
        ),
        yaxis = list(
          title = "",
          ticksuffix = "%",                # % after every y axis number
          rangemode = "nonnegative",
          fixedrange = TRUE,
          showgrid = FALSE
        )
      )
  })
  
  fmt_val <- function(count, pct, fmt) {
    if (fmt == "pct") paste0(round(pct * 100, 1), "%") else as.character(count)
  }
  
  
  output$expenditure <- renderPlotly({
    df     <- monetary_data()
    avg_exp    <- mean(df$average_treatment_cost, na.rm = TRUE)
    latest <- df %>% slice_max(Year)
    
    plot_ly(df, x = ~Year, y = ~average_treatment_cost) %>%
      add_lines(
        line = list(color = "#2C3E50", width = 2),
        hovertemplate = "%{x}: BDT %{y:,.0f}<extra></extra>"
      ) %>%
      add_markers(
        data = latest,
        marker = list(color = "#35858E", size = 15, symbol = "circle"),
        hovertemplate = "Latest avg. — %{x}: BDT %{y:,.0f}<extra></extra>"
      ) %>%
      layout(
        paper_bgcolor = "transparent",
        plot_bgcolor  = "transparent",
        font   = list(family = "Poppins, sans-serif", color = "#2C3E50"),
        margin = list(l = 0, r = 80, t = 10, b = 0),
        showlegend = FALSE,
        shapes = list(list(
          type = "line", xref = "paper", x0 = 0, x1 = 1,
          yref = "y",    y0 = avg_exp, y1 = avg_exp,
          line = list(color = "#BDC3C7", width = 1, dash = "dot")
        )),
        annotations = list(list(
          xref = "paper", x = 1.01, yref = "y", y = avg_exp,
          text = paste0("Overall Avg.<br><b>", round(avg_exp, -3), "</b>"),
          showarrow = FALSE, xanchor = "left",
          font = list(size = 10, color = "#2C3E50")
        )),
        xaxis = list(
          title = list( text = "Year",  standoff = 15,font = list(size = 12, color = "#2C3E50", family = "Poppins, sans-serif")
          ),type = "category",showgrid = FALSE, zeroline = FALSE, fixedrange = TRUE
        ),
        yaxis = list(title = list(text = "Avg. Treatment Cost (BDT)", standoff = 15,font = list(size = 12, color = "#2C3E50", family = "Poppins, sans-serif")
        ),showgrid = TRUE,zeroline = FALSE,gridcolor = "rgba(0,0,0,0.06)", ticklen   = 6,
        tickpad   = 8, fixedrange = TRUE)
      ) %>%
      config(displayModeBar = FALSE, responsive = TRUE)
  })
  
  
  
  output$age_freq<-render_gt({
    fmt <- if (input$show_pct) "pct" else "count"
    
    base <- filtered_data_1() %>%
      filter(!is.na(age_group)) %>%
      mutate(age_group = droplevels(age_group)) %>%
      group_by(age_group) %>%
      summarise(Count = n()) %>%
      mutate(prop=Count/sum(Count))
    
    if(fmt=="count"){
      table_data<-base%>%select(age_group, Count)%>%
        pivot_wider(names_from = age_group, values_from = Count)%>%
        select( any_of(c("Child (≤14)", "Youth (15–20)", "Adult (21–45)", "Elder (45+)")))
      rng <- range(unlist(table_data), na.rm = TRUE)
      k<-table_data %>%
        gt() %>%
        tab_header(
          title = md("Age Group Distribution")
        ) %>%
        data_color(
          columns = everything(),
          method = "numeric",
          domain = rng,
          palette = c("#FFFDF0", "#EDE9E6", "#DDDDDD")
        )
    } else{
      table_data<-base%>%select(age_group, prop)%>%
        pivot_wider(names_from = age_group, values_from = prop)%>%
        select( any_of(c("Child (≤14)", "Youth (15–20)", "Adult (21–45)", "Elder (45+)")))
      rng <- range(unlist(table_data), na.rm = TRUE)
      k<-table_data %>%
        gt() %>%
        tab_header(
          title = md("Age Group Distribution")
        ) %>%
        data_color(
          columns = everything(),
          method = "numeric",
          domain = rng,
          palette = c("#FFFDF0", "#EDE9E6", "#DDDDDD")
        )%>%
        fmt_percent(columns = everything(), decimals = 1)
      
    }
    
    
    k%>% gt_theme_nytimes()%>% 
      tab_options(
        table.font.names = "Helvetica",
        table.font.size = px(17),
        
        heading.title.font.size = px(16),
        heading.title.font.weight = "bold",
        
        column_labels.font.size = px(15),
        column_labels.font.weight = "bold",
        column_labels.background.color = "white",
        
        data_row.padding = px(12)
      ) %>%
      
      cols_align(
        align = "left",
        columns = everything()
      )
    
  })
  
  output$income_dist <- renderPlotly({
    df      <- filtered_data_1()
    
    req(nrow(df) > 0, !all(is.na(df$family_income)))
    
    income  <- df$family_income[!is.na(df$family_income) & df$family_income > 0]  # ← exclude 0 and NA
    
    max_val <- ceiling(max(income) / 5000) * 5000
    breaks  <- seq(0, max(max_val, 40000), by = 5000)
    counts  <- hist(income, breaks = breaks, plot = FALSE, include.lowest = FALSE)
    total   <- sum(counts$counts)
    
    bin_df <- data.frame(
      x        = counts$mids,
      y        = counts$counts,
      hov_text = paste0(
        "Income: BDT", format(counts$breaks[-length(counts$breaks)], big.mark = ","),
        " – BDT",      format(counts$breaks[-1], big.mark = ","), "<br>",
        "Patients: ",   counts$counts, "<br>",
        "Share: ",      round(counts$counts / total * 100, 1), "%"
      )
    ) |> dplyr::filter(x <= 40000)
    
    # Define thresholds
    poverty_line    <- 10000   # adjust to your context
    low_income_line <- 25000
    
    plot_ly(bin_df, x = ~x, y = ~y, customdata = ~hov_text) %>%
      add_bars(
        width  = 5000,
        marker = list(
          color = "rgba(231, 76, 60, 0.6)",
          line  = list(color = "rgba(231, 76, 60, 1)", width = 1)
        ),
        hovertemplate = "%{customdata}<extra></extra>"
      ) %>%
      layout(
        paper_bgcolor = "transparent",
        plot_bgcolor  = "transparent",
        font          = list(family = "Poppins, sans-serif", color = "#2C3E50"),
        margin        = list(l = 0, r = 0, t = 30, b = 0),
        showlegend    = FALSE,
        
        shapes = list(
          # Poverty line
          list(
            type = "line", xref = "x", yref = "paper",
            x0 = poverty_line, x1 = poverty_line, y0 = 0, y1 = 1,
            line = list(color = "#E74C3C", width = 1.5, dash = "dot")
          ),
          # Low income line
          list(
            type = "line", xref = "x", yref = "paper",
            x0 = low_income_line, x1 = low_income_line, y0 = 0, y1 = 1,
            line = list(color = "#E67E22", width = 1.5, dash = "dot")
          ),
          # Shade below poverty line
          list(
            type    = "rect",
            xref    = "x",    x0 = 0,             x1 = poverty_line,
            yref    = "paper", y0 = 0,             y1 = 1,
            fillcolor = "rgba(231, 76, 60, 0.1)",
            line    = list(width = 0)
          ),
          list(
            type="rect",
            xref="x", x0=poverty_line, x1=low_income_line,
            yref="paper", y0=0, y1=1,
            fillcolor="rgba(230, 126, 34, 0.1)",
            line=list(width=0)
          )
        ),
        
        annotations = list(
          list(
            xref = "x", x = poverty_line,
            yref = "paper", y = -0.15,
            text = paste0("<b>Poverty Line</b><br>BDT ", 
                          format(poverty_line, big.mark = ",")),
            showarrow = FALSE, yanchor = "bottom",
            font = list(size = 10, color = "#E74C3C")
          ),
          list(
            xref = "x", x = low_income_line,
            yref = "paper", y = 1,
            text = paste0("<b>Low Income</b><br>BDT ",
                          format(low_income_line, big.mark = ",")),
            showarrow = FALSE, yanchor = "bottom",
            font = list(size = 10, color = "#E67E22")
          ),
          list(
            xref = "paper", x = 0.15,
            yref = "paper", y = .95,
            text = paste0(
              "<b>", sum(df$family_income <= 10000, na.rm = TRUE), " (",
              round(mean(df$family_income <= 10000, na.rm = TRUE) * 100, 1),
              "%)</b> ", "<br>patients <br> below <br><b>BDT 10,000</b>"
            ),
            showarrow = FALSE,
            xanchor   = "left",
            font      = list(size = 12, color = "#0B1849")
          ),
          list(
            xref = "paper", x = 0.53,
            yref = "paper", y = .95,
            text = paste0(
              "<b>", sum(df$family_income <= 25000, na.rm = TRUE), " (",
              round(mean(df$family_income <= 25000, na.rm = TRUE) * 100, 1),
              "%)</b>", "<br>patients <br> below <br><b>BDT 25,000</b>"
            ),
            showarrow = FALSE,
            xanchor   = "left",
            font      = list(size = 12, color = "#0B1849")
          )
        ),
        
        xaxis = list(
          title    = list(text = "Monthly Household Income (BDT)", standoff = 15,
                          font = list(size = 12, color = "#2C3E50")),
          range     = c(0, 40000),
          showgrid = FALSE, zeroline = FALSE,
          ticklen  = 6,     tickpad  = 8,
          tickformat = ",0f", fixedrange = TRUE
        ),
        yaxis = list(
          title    = list(text = "Number of Patients", standoff = 15,
                          font = list(size = 12, color = "#2C3E50")),
          showgrid  = TRUE, zeroline = FALSE,
          gridcolor = "rgba(0,0,0,0.06)",
          ticklen   = 6,    tickpad  = 8,fixedrange = TRUE
        )
      ) %>%
      config(displayModeBar = FALSE, responsive = TRUE)
  })
  
  output$household_size_output<-renderText({
    val<-filtered_data_1()%>%summarise(hs=round(mean (household_size, na.rm=TRUE),2))%>%pull()
    if (length(val) == 0) 0 else val
  })
  output$children_number<-renderText({
    val<-filtered_data_1()%>%summarise(hs=round(mean (num_children, na.rm=TRUE),2))%>%pull()
    if (length(val) == 0) 0 else val
  })
  
  
  
  
  
  
  
  output$district_map <- renderLeaflet({
    df <- filtered_data_1()
    
    district_counts <- df |>
      filter(!is.na(district)) |>
      count(district, name = "n") |>
      mutate(district = as.character(district))
    
    # Major cancer site breakdown per district
    district_cancer <- df |>
      filter(!is.na(district), !is.na(major_cancer_sites),
             major_cancer_sites != "Unknown",
             major_cancer_sites != "Not Applicable") |>
      mutate(district = as.character(district)) |>
      group_by(district, major_cancer_sites) |>
      summarise(site_count = n(), .groups = "drop") |>
      group_by(district) |>
      mutate(
        site_pct  = round(site_count / sum(site_count) * 100, 1),
        site_line = paste0(major_cancer_sites, ": ", site_count, " (", site_pct, "%)")
      ) |>
      summarise(
        cancer_breakdown = paste(site_line, collapse = "<br>"),
        .groups = "drop"
      )
    
    # Join everything together
    map_data <- bd_sf_geom |>
      left_join(district_counts,  by = c("NAME_2" = "district")) |>
      left_join(district_cancer,  by = c("NAME_2" = "district")) |>
      mutate(
        n  = replace_na(n, 0),
        cancer_breakdown = replace_na(cancer_breakdown, "No data")
      )
    
    pal <- colorNumeric("magma", domain = map_data$n, reverse = TRUE)
    
    leaflet(map_data) |>
      addControl(
        html     = "<div style='background:white; padding:6px 12px; border-radius:4px;
                  font-size:14px; font-weight:bold; box-shadow:0 1px 4px rgba(0,0,0,0.3);'>
                  Choropleth Heat Map
                  </div>",
        position = "topright"
      ) |>
      leaflet.extras::addFullscreenControl(position = "topleft", pseudoFullscreen = TRUE) |>
      addPolygons(
        group       = "districts",
        fillColor   = ~pal(n),
        fillOpacity = 1,
        color       = "white",
        weight      = 0.5,
        label = ~paste0(NAME_2, " — ", n, " patients") |>
          lapply(htmltools::HTML),
        popup = ~paste0(
          "<div style='font-family:Poppins,sans-serif; min-width:180px;'>",
          "<b style='font-size:14px;'>", NAME_2, "</b><br>",
          "<span style='color:gray; font-size:12px;'>Total Patients: </span>",
          "<b>", n, "</b><br><br>",
          "<span style='font-size:12px; color:#2C3E50;'><b>Cancer Sites</b></span><br>",
          "<span style='font-size:11px; line-height:1.8;'>", cancer_breakdown, "</span>",
          "</div>"
        ) |> lapply(htmltools::HTML),
        highlightOptions = highlightOptions(
          weight      = 2,
          fillOpacity = 0.8,
          bringToFront = TRUE
        )
      ) |>
      leaflet.extras::addSearchFeatures(
        targetGroups = "districts",
        options      = leaflet.extras::searchFeaturesOptions(
          zoom      = 10,
          openPopup = TRUE
        )
      ) |>
      addEasyButton(
        easyButton(
          icon    = "fa-2x fa-home",
          title   = "Reset View",
          onClick = JS("function(btn, map){ map.setView([23.685, 90.356], 7); }")
        )
      ) |>
      addLegend(pal = pal, values = ~n, title = "Patients", position = "bottomright")
  })
  
  
  
  
  output$cancer_barplot <- renderPlotly({
    df <- filtered_data_1()
    
    df_1 <- df %>%
      select(major_cancer_sites, cancer_affected_body_parts) %>%
      filter(
        !is.na(major_cancer_sites),
        major_cancer_sites != "Unknown",
        major_cancer_sites != "Not Applicable"
      ) %>%
      group_by(major_cancer_sites, cancer_affected_body_parts) %>%
      summarise(count = n(), .groups = "drop") %>%
      group_by(major_cancer_sites) %>%
      mutate(
        Prop               = count / sum(count),
        major_cancer_count = sum(count),
        major_prop         = paste0(round(major_cancer_count / nrow(df) * 100, 1), "%"),
        # ← build the sub-row detail here while all columns are available
        detail_line        = paste0(cancer_affected_body_parts, ": ", count,
                                    " (", round(Prop * 100, 1), "%)")
      ) %>%
      summarise(
        major_cancer_count = first(major_cancer_count),
        major_prop         = first(major_prop),
        hov_text           = paste0(
          "<b>", first(major_cancer_sites), ":  ",
          first(major_cancer_count), " (", first(major_prop), ")</b><br>",
          paste(detail_line, collapse = "<br>")   # ← collapse the pre-built lines
        ),
        .groups = "drop"
      )
    
    plot_ly(df_1,
            x          = ~reorder(major_cancer_sites, -major_cancer_count),
            y          = ~major_cancer_count,
            customdata = ~hov_text) %>%
      add_bars(
        text          = ~major_prop,
        textposition  = "outside",
        cliponaxis    = FALSE,
        textfont      = list(color = "#2C3E50", size = 11, family = "Poppins, sans-serif"),
        marker        = list(
          color = "rgba(223, 241, 241, 0.6)",
          line  = list(color = "rgba(44, 62, 80, 1)", width = 2)
        ),
        hovertemplate = "%{customdata}<extra></extra>"
      ) %>%
      layout(
        paper_bgcolor = "transparent",
        plot_bgcolor  = "transparent",
        font          = list(family = "Poppins, sans-serif", color = "#2C3E50"),
        margin        = list(l = 0, r = 0, t = 50, b = 0),
        showlegend    = FALSE,
        xaxis = list(
          title    = list(text = "Major Cancer Sites", font = list(size = 12, color = "#2C3E50"),standoff = 10),
          showgrid = FALSE, zeroline = FALSE,
          ticklen  = 6, tickpad = 8, fixedrange = TRUE
        ),
        yaxis = list(
          title    = list(text = "Number of Patients", font = list(size = 12, color = "#2C3E50"), standoff = 15),
          showgrid  = TRUE, zeroline = FALSE,
          gridcolor = "rgba(0,0,0,0.06)",
          ticklen   = 6, tickpad = 8, fixedrange = TRUE
        )
      ) %>%
      config(displayModeBar = FALSE, responsive = TRUE)
  })
  
  all_secs <- c("trends", "age_gender", "economic", "family", "map", "cancer")
  
  observeEvent(input$report_all, ignoreInit = TRUE, {
    updateCheckboxGroupInput(
      session, "report_sections",
      selected = if (input$report_all) all_secs else character(0)
    )
  })
  
  
  observeEvent(input$report_sections, ignoreNULL = FALSE, ignoreInit = TRUE, {
    updateCheckboxInput(
      session, "report_all",
      value = setequal(input$report_sections, all_secs)
    )
  })
  
  
  output$download_pdf <- downloadHandler(
    filename = function() {
      paste0("BANCAT_Report_", format(Sys.Date(), "%Y%m%d"), ".html")  
    },
    content = function(file) {
      showNotification(
        "Generating report… this may take a few seconds.",             
        type = "message", duration = NULL, id = "pdf_note"
      )
      on.exit(removeNotification("pdf_note"))
      
      tmp_rmd <- tempfile(fileext = ".Rmd")
      file.copy("report.Rmd", tmp_rmd, overwrite = TRUE)
      
      rmarkdown::render(
        input       = tmp_rmd,
        output_file = file,
        params = list(
          filtered_data_1  = filtered_data_1(),
          monetary_data    = monetary_data(),
          growth_rate_data = growth_rate_data(),
          gender_count     = gender_count(),
          sections         = input$report_sections,
          date_from        = input$date[1],
          date_to          = input$date[2],
          gender_filter    = paste(input$gender, collapse = ", "),
          cancer_filter    = paste(input$cancer_type, collapse = ", "),
          age_filter       = paste(input$age_group, collapse = ", ")
        ),
        envir = new.env(parent = globalenv())
      )
    },
    contentType = "text/html"                                          
  )
  
  
  
}
shinyApp(ui, server)