library(blackmarbler)
library(dplyr)
library(geodata)
library(ggplot2)
library(lubridate)
library(raster)
library(readxl)
library(ggrepel)
library(sf)
library(stargazer)
library(stringr)
library(tidyverse)
library(viridis)

### Phase 1: load gdp data, clean the data, process the data from short to long
# some data cleaning is done in this step through observing the excel file and
# choosing the correct sheet number, skip value, and column names (if necessary)
excel_file_path <- "/Users/kentongoldthorpe/OneDrive/Documents/MBA/FA24_1/ECON_CAP/zb_vrp_2021.xlsx"
sheet_number <- 8
skip_value <- 7
col_names <- c("NL_NAME_1",
               "GDP_2017", "GDP_2018", "GDP_2019", "GDP_2020", "GDP_2021",
               "PER_2017", "PER_2018", "PER_2019", "PER_2020", "PER_2021")

ukraine_gdp <- read_excel(
  excel_file_path,
  sheet = sheet_number,
  skip = skip_value,
  col_names = FALSE) %>%
  setNames(col_names)

rm(col_names, excel_file_path, sheet_number, skip_value)

# clean the data
ukraine_gdp <- ukraine_gdp %>% filter(GDP_2017 > 0)

# Process the data from short to long and correctly handle names,
# while also converting "GDP" and "PER" columns to numeric and applying ex rate
ukraine_gdp <- ukraine_gdp %>%
  pivot_longer(
    cols = starts_with("GDP_") | starts_with("PER_"),
    names_to = c(".value", "date"), # date not year to align with future data
    names_pattern = "(GDP|PER)_(\\d{4})"
  ) %>%
  mutate(
    date = as.integer(date),
    GDP = as.numeric(GDP) * 1e6 * 0.026,
    PER = as.numeric(PER) * 0.026,
    POP = GDP/PER,
  )

### Phase 2: load the vector data from gadm, clean the data, process the data
# through blackmarbler bm_extract (this turns the data long)
ukraine <- gadm(country = "UKR", level=1, path = tempdir()) |> st_as_sf()

# clean the data
Kiev <- ukraine %>% filter(str_detect(NAME_1, "\\?") | str_detect(NAME_1, "Kiev City"))
ukraine[ukraine$NAME_1 == "Kiev City", "geometry"] <- st_union(Kiev$geometry)
rm(Kiev)
ukraine <- ukraine %>% filter(!(str_detect(NAME_1, "\\?")) & !(str_detect(TYPE_1, "Auto")))
# using the excel data as authoritative, below are names that are NA or different
ukraine[is.na(ukraine$NL_NAME_1), "NL_NAME_1"] <- "Львівська"
ukraine[ukraine$NL_NAME_1 == "Дніпропетро́вська", "NL_NAME_1"] <- "Дніпропетровська"
ukraine[ukraine$NL_NAME_1 == "Доне́цька", "NL_NAME_1"] <- "Донецька"
ukraine[ukraine$NL_NAME_1 == "Київ", "NL_NAME_1"] <- "м.Київ"

# Bearer token from NASA
bearer <- "eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJlbWFpbF9hZGRyZXNzIjoia2VudG9uZ29sZHRob3JwZUB1LmJvaXNlc3RhdGUuZWR1IiwiaXNzIjoiQVBTIE9BdXRoMiBBdXRoZW50aWNhdG9yIiwiaWF0IjoxNzEyOTMyOTMxLCJuYmYiOjE3MTI5MzI5MzEsImV4cCI6MTg3MDYxMjkzMSwidWlkIjoia2VudG9uZ29sZHRob3JwZSIsInRva2VuQ3JlYXRvciI6ImtlbnRvbmdvbGR0aG9ycGUifQ.K2tyVGHaMJXm18azCS4LMTLDB4CaaeH3bR1WNyejTAg"

# bm_extract Processing
ntl_df <- bm_extract(roi_sf = ukraine,
                     product_id = "VNP46A4",
                     date = 2017:2021,
                     bearer = bearer,)

### Phase 3: Combine the data, collect regression, display results
ntl_df_combined <- left_join(ntl_df, ukraine_gdp, by = c("NL_NAME_1", "date"))
ntl_df_combined <- ntl_df_combined %>% mutate(SOL = n_non_na_pixels * ntl_mean)

# Trends over time
ntl_df_combined |>
  ggplot() +
  geom_col(aes(x = date,
               y = SOL),
           fill = "darkorange") +
  facet_wrap(~NAME_1) +
  labs(x = NULL,
       y = "NTL Sum of Annual Lights",
       title = "Ukraine Admin Level 1") +
  scale_x_continuous(labels = seq(2017, 2021, 2),
                     breaks = seq(2017, 2021, 2)) +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold"))

# Collect Regressions
fixed_GDP_SOL <- lm(GDP ~ SOL, data=ntl_df_combined)
fixed_GDP_SOL_LOG <- lm(log(GDP) ~ log(SOL), data=ntl_df_combined)
# Region
fixed_GDP_SOL_DUM <- lm(GDP ~ SOL + factor(NAME_1), data=ntl_df_combined)
fixed_GDP_SOL_DUM_NC <- lm(GDP ~ SOL + factor(NAME_1) - 1, data=ntl_df_combined)
# Region and SOL
fixed_GDP_SOLxDUM <- lm(GDP ~ SOL * factor(NAME_1), data=ntl_df_combined)
fixed_GDP_SOLxDUM_NC <- lm(GDP ~ SOL * factor(NAME_1) - 1, data=ntl_df_combined)
# SOL
fixed_GDP_SOLxDUM_X <- lm(GDP ~ SOL:factor(NAME_1), data=ntl_df_combined)
fixed_GDP_SOLxDUM_NC_X <- lm(GDP ~ SOL:factor(NAME_1) - 1, data=ntl_df_combined)
# List of Regressions
fitted_models = list(fixed_GDP_SOL_DUM, fixed_GDP_SOL_DUM_NC, fixed_GDP_SOLxDUM, fixed_GDP_SOLxDUM_NC, fixed_GDP_SOLxDUM_X, fixed_GDP_SOLxDUM_NC_X)
# Star gazer function
call_star <- function(fitted_models) {
  stargazer(fitted_models,
            type = "text", 
            title = "Regression Results Summary",
            intercept.bottom = FALSE,
            omit.stat = c("ser"),
            digits = 4)
}
# Display results
call_star(fitted_models)
# Calculate fitted values for each model
ntl_df_combined <- ntl_df_combined %>%
  mutate(
    fitted_GDP_SOL = (predict(fixed_GDP_SOL)),
    fitted_GDP_SOLaDUM_C = (predict(fixed_GDP_SOL_DUM)),
    fitted_GDP_SOLbDUM_C = (predict(fixed_GDP_SOLxDUM)),
    fitted_GDP_SOLcDUM_C_L = (predict(fixed_GDP_SOLxDUM_X)),
    fitted_GDP_SOLaDUM_NC = (predict(fixed_GDP_SOL_DUM_NC)),
    fitted_GDP_SOLbDUM_NC = (predict(fixed_GDP_SOLxDUM_NC)),
    fitted_GDP_SOLcDUM_NC_L = (predict(fixed_GDP_SOLxDUM_NC_X)),
  )
# Reshape to long format
reshape_data_for_plotting <- function(data, fitted_col_names) {
  data %>%
    dplyr::select(GDP, SOL, POP, all_of(fitted_col_names)) %>%
    pivot_longer(
      cols = starts_with("fitted"),
      names_to = "model",
      values_to = "fitted_values"
    )
}
# Create plot
plot_fitted_vs_actual <- function(data, title = "Comparison of Linear Models") {
  ggplot(data, aes(x = GDP, y = fitted_values, color = model)) +
    geom_point(alpha = 0.6) +  # Using points
    geom_line(aes(group = model), linetype = "dashed") +
    facet_wrap(~model) +  # Use facets to handle different scales
    labs(
      title = title,
      x = "Actual GDP",
      y = "Fitted Values"
    ) +
    theme_minimal() +
    scale_color_viridis_d()  # Using viridis for color scale
}
# Define column names for each scenario
fitted_cols1 = c("fitted_GDP_SOLaDUM_C", "fitted_GDP_SOLaDUM_NC")
fitted_cols2 = c("fitted_GDP_SOLbDUM_C", "fitted_GDP_SOLbDUM_NC")
fitted_cols3 = c("fitted_GDP_SOLcDUM_C_L", "fitted_GDP_SOLcDUM_NC_L")
# Reshape data
ols_data1 <- reshape_data_for_plotting(ntl_df_combined, fitted_cols1)
ols_data2 <- reshape_data_for_plotting(ntl_df_combined, fitted_cols2)
ols_data3 <- reshape_data_for_plotting(ntl_df_combined, fitted_cols3)
# Plot data
plot_fitted_vs_actual(ols_data1, "Model Comparison for Fitted Models: Regional Factor")
plot_fitted_vs_actual(ols_data2, "Model Comparison for Fitted Models: Regional & SOL Factor")
plot_fitted_vs_actual(ols_data3, "Model Comparison for Fitted Models: SOL Factor")

#### Phase 4
# Define the function
process_SOL_summaries <- function(sol_battle, sol_previous_year, model, battle_dates) {
  # Calculate SOL values
  sol_battle <- sol_battle %>% mutate(SOL = ntl_mean * n_non_na_pixels)
  
  sol_previous_year <- sol_previous_year %>% mutate(SOL = ntl_mean * n_non_na_pixels)
  
  # Sum SOL values grouped by NAME_1
  sol_battle_summed <- sol_battle %>%
    group_by(NAME_1) %>%
    summarise(sol_battle = sum(SOL, na.rm = TRUE))
  
  sol_previous_year_summed <- sol_previous_year %>%
    group_by(NAME_1) %>%
    summarise(sol_previous = sum(SOL, na.rm = TRUE))
  
  # Join and calculate differences
  sol_battle_summed <- left_join(sol_battle_summed, sol_previous_year_summed, by = "NAME_1")
  sol_battle_summed <- sol_battle_summed %>%
    mutate(SOL = sol_battle - sol_previous)
  
  # Prepare coefficients dataframe
  coeffs <- coef(model)
  coefficients_df <- data.frame(NAME_1 = names(coeffs), Coefficient = coeffs)
  coefficients_df$NAME_1 <- gsub("SOL:factor\\(NAME_1\\)", "", coefficients_df$NAME_1)
  coefficients_df <- left_join(coefficients_df, sol_battle_summed, by = "NAME_1")
  
  # Calculate estimated GDP change
  coefficients_df <- coefficients_df %>%
    mutate(EstGDPdelta = Coefficient * SOL / 1e9 * length(battle_dates) / 365)
  
  return(coefficients_df)
}

# Fetching data for the current & previous year of the battle
battle_dates <- seq.Date(from = as.Date("2022-02-25"), to = as.Date("2022-04-05"), by = "day")
previous_dates <- seq.Date(from = as.Date("2021-02-25"), to = as.Date("2021-04-05"), by = "day")
sol_battle <- bm_extract(roi_sf = ukraine, product_id = "VNP46A2", date = battle_dates, bearer = bearer)
sol_previous_year <- bm_extract(roi_sf = ukraine, product_id = "VNP46A2", date = previous_dates, bearer = bearer)

kyiv <- process_SOL_summaries(sol_battle, sol_previous_year, fixed_GDP_SOLxDUM_NC_X, battle_dates)

plot_ukraine_data <- function(ukraine, region_data) {
  # Join the data
  ukraine_region <- left_join(ukraine, region_data, by = "NAME_1")
  
  # Create the plot
  plot <- ggplot(data = ukraine_region) +
    geom_sf(aes(fill = EstGDPdelta), size = 0.7) +  # Use fill for region colors
    geom_sf_text(aes(label = NAME_1), check_overlap = FALSE) +  # Add labels
    scale_fill_gradient2(low = "blue", mid = "white", high = "red", 
                         midpoint = 0, 
                         limit = c(min(ukraine_region$EstGDPdelta, na.rm = TRUE), max(ukraine_region$EstGDPdelta, na.rm = TRUE)),
                         space = "Lab") +  # Set midpoint color and define color scale around it
    labs(title = "Change in Nighttime Lights (NTL) in Ukraine",
         fill = "EstGDPdelta") +
    theme_minimal() +
    theme(legend.position = "right")
  
  return(plot)
}

ukraine_kyiv <- plot_ukraine_data(ukraine, kyiv)
ukraine_kyiv
