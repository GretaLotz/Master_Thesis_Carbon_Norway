

iea <- read_excel("iea.xlsx", sheet=2)

iea_norway <- iea %>%
  filter(country == "Norway")

# load data
world_bank_norway <- read_excel("world_bank_norway.xls")

# Filter the data for the relevant indicators
filtered_data <- world_bank_norway %>%
  filter(indicator_code %in% c("EN.ATM.CO2E.LF.KT", "EN.ATM.CO2E.GF.KT", "EN.ATM.CO2E.SF.KT"))

# Aggregate the data by country and indicator, summing across years
aggregated_data <- filtered_data %>%
  group_by(country, country_code, indicator, indicator_code) %>%
  summarize(across(where(is.numeric), sum, na.rm = TRUE), .groups = "drop")

# Calculate the sum across year columns for the total fuel combustion row
total_combustion_row <- aggregated_data %>%
  filter(indicator_code %in% c("EN.ATM.CO2E.LF.KT", "EN.ATM.CO2E.GF.KT", "EN.ATM.CO2E.SF.KT")) %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE), .groups = "drop")


# Add a new row with the total fuel combustion
total_combustion_row <- total_combustion_row %>%
  mutate(indicator = "Total Fuel Combustion (kt)",
         indicator_code = "TFC",
         country = "Norway",
         country_code = "NOR")

# Combine the aggregated data with the new total combustion row
co2_emissions_df <- bind_rows(aggregated_data, total_combustion_row)


total_fuel_combustion_row <- co2_emissions_df %>%
  filter(indicator_code == "TFC")
#long format
co2_emissions_df_long <- co2_emissions_df %>%
  gather(key = "year", value = "value", -c(country, country_code, indicator, indicator_code))


norway_long <- total_fuel_combustion_row %>%
  gather(key = "year", value = "value", -c(country, country_code, indicator, indicator_code))


norway_long <- norway_long %>%
  filter(year >= 1960 & year <=2005)
###

iea_norway_long <- iea_norway %>%
  gather(key = "year", value = "value", -c(country))

iea_norway_long <- iea_norway_long %>%
  filter(year >= 1960 & year <=2005)


# Add value of data frame iea_norway_long to norway_long
norway_long <- norway_long %>%
  mutate(co2_trans_percent = iea_norway_long$value)
# rename variable name "value" to be more self-explanatory
norway_long <- norway_long %>%
  rename(co2_fuel_absolute = value)

# calculate the absolute value of kt CO2 from the percentage and the total kt of fuel combustion
norway_long <- norway_long %>%
  mutate(co2_fuel_absolute = as.numeric(co2_fuel_absolute),
         co2_trans_percent = as.numeric(co2_trans_percent),
         year = as.numeric(year),
         co2_transport_kt = co2_fuel_absolute * co2_trans_percent)

### population

# Provide the path to your Excel file
file_path <- "population_norway.xlsx"

# Read the Excel file into a data frame
population_norway <- read_excel(file_path)

population_norway <- population_norway %>%
  mutate(year = as.numeric(year))

per_capita_co2 <- left_join(norway_long, population_norway, by = "year")

per_capita_co2 <- per_capita_co2 %>%
  mutate(value_metric_tons = co2_transport_kt * 1000)  # Convert kilotons to metric tons

# Calculate metric tons per capita
per_capita_co2 <- per_capita_co2 %>%
  mutate(CO2_transport_capita = value_metric_tons / (population))  # Convert population to thousands for per capita calculation

# per_capita_co2 <- per_capita_co2 %>%
 # filter(year <= 2015)

per_capita_co2 <- per_capita_co2 %>%
  mutate(year = as.numeric(year))




ggplot(per_capita_co2, aes(x = year, y = CO2_transport_capita)) +
  geom_line() +
  labs(title = "Road Sector CO2 Emissions in Norway per capita",
       x = "",
       y = "CO2 emissions from the transport sector per capita (mt)") +
  scale_x_continuous(breaks = seq(min(per_capita_co2$year, na.rm = TRUE), max(per_capita_co2$year, na.rm = TRUE), by = 20)) +
  theme_minimal()

ggplot(norway_long, aes(x = year, y = co2_trans_percent)) +
  geom_line() +
  labs(x = "Year", y = "CO2 Transport Percent", title = "CO2 Transport Percent Over Year") +
  theme_minimal()

ggplot(norway_long, aes(x = year, y = co2_fuel_absolute)) +
  geom_line() +
  labs(x = "Year", y = "CO2 Fuel", title = "CO2 from Fuel Emissions over Year") +
  theme_minimal()

ggplot(per_capita_co2, aes(x = as.numeric(year), y = population)) +
  geom_line() +
  labs(x = "Year", y = "Population", title = "Population Norway") +
  theme_minimal()

library(dplyr)
library(ggplot2)
library(cowplot)

# Filter data for the first plot to include data only up to 2005
co2_emissions_df_long_filtered <- co2_emissions_df_long %>%
  filter(year <= 2005)

# Plot 1: CO2 Emissions Over Time (up to 2005)
plot1 <- ggplot(co2_emissions_df_long_filtered, aes(x = as.numeric(year), y = value)) +
  geom_line(aes(color = indicator_code), show.legend = T) + # Remove legend
  labs(title = "CO2 Emissions from Fuel Types",
       x = "Year",
       y = "CO2 Emissions (kilotons)") +
  theme_minimal()

# Plot 2: CO2 Transport Percent Over Year
plot2 <- ggplot(norway_long, aes(x = year, y = co2_trans_percent)) +
  geom_line() +
  labs(x = "Year", y = "CO2 Transport Percent", title = "CO2 Transport Percent Over Year") +
  theme_minimal()

# Combine plots
combined_plot <- plot_grid(plot1, plot2, ncol = 1)

# Display combined plot
print(combined_plot)
print(plot1)
