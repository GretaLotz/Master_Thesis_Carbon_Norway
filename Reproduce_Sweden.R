# load data
world_bank_sweden <- read_excel("world_bank_sweden.xls")

# Filter the data for the relevant indicators
filtered_data <- world_bank_sweden %>%
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
         country = "Sweden",
         country_code = "SWE")

# Combine the aggregated data with the new total combustion row
co2_emissions_df <- bind_rows(aggregated_data, total_combustion_row)


total_fuel_combustion_row <- co2_emissions_df %>%
  filter(indicator_code == "TFC")
#long format

sweden_long <- total_fuel_combustion_row %>%
  gather(key = "year", value = "value", -c(country, country_code, indicator, indicator_code))


# Plot the data
ggplot(sweden_long, aes(x = as.numeric(year), y = as.numeric(value))) +
  geom_line() +
  geom_vline(xintercept = 1991, linetype = "dashed", color = "black") +
  annotate("text", x = 1991, y = 2000, 
           label = "Carbon Tax", color = "black", vjust = -0.5, hjust = -0.5) +
  labs(title = "Road Sector CO2 Emissions in Sweden",
       x = "",
       y = "CO2 emissions from liquid fuel consumption (kt)") +
  theme_minimal()

# Filter the data for transport emissions (% of total fuel combustion) indicator
subset_co2_transport <- world_bank_sweden %>%
  filter(indicator_code %in% c("EN.CO2.TRAN.ZS"))

subset_co2_transport_long <- subset_co2_transport %>%
  gather(key = "year", value = "value", -c(country, country_code, indicator, indicator_code))

# Get percentages into a different format
subset_co2_transport_long$value <- subset_co2_transport_long$value / 100


# Add value of data frame subset_co2_transport_long to sweden_long
sweden_long <- sweden_long %>%
  mutate(co2_trans_percent = subset_co2_transport_long$value)
# rename variable name "value" to be more self-explanatory
sweden_long <- sweden_long %>%
  rename(co2_fuel_absolute = value)

# calculate the absolute value of kt CO2 from the percentage and the total kt of fuel combustion
sweden_long <- sweden_long %>%
  mutate(co2_transport_kt = co2_fuel_absolute * co2_trans_percent)


population_data <- world_bank_sweden %>%
  filter(indicator_code == "SP.POP.TOTL")

# Pivot from wide to long format
population_data_long <- pivot_longer(
  population_data,
  cols = starts_with("19") | starts_with("20"), # Select columns for years 1960-2022
  names_to = "year",
  values_to = "population"
)

# Convert year to numeric
population_data_long$year <- as.numeric(population_data_long$year)
sweden_long$year <- as.numeric(sweden_long$year)



per_capita_co2 <- left_join(sweden_long, population_data_long, by = "year")


per_capita_co2 <- per_capita_co2 %>%
  mutate(value_metric_tons = co2_transport_kt * 1000)  # Convert kilotons to metric tons

per_capita_co2 <- per_capita_co2 %>%
  filter(year <= 2005)
# Calculate metric tons per capita
per_capita_co2 <- per_capita_co2 %>%
  mutate(CO2_transport_capita = value_metric_tons / (population))  # Convert population to thousands for per capita calculation


ggplot(per_capita_co2, aes(x = year, y = as.numeric(CO2_transport_capita))) +
  geom_line()+
  geom_vline(xintercept = 1991, linetype = "dashed", color = "black") +
  annotate("text", x = 1991, y = 4, 
           label = "Carbon Tax", color = "black", vjust = -0.5, hjust = -0.5) +
  labs(title = "Road Sector CO2 Emissions in Sweden per capita",
       x = "",
       y = "CO2 emissions from fuel consumption per capita (mt)") +
  scale_y_continuous(limits = c(0, 5)) +  # Set y-axis limits from 0 to 5
  theme_minimal()




