
#### compare historic data with dargay calculated

# import data
vehicles_norway <- read_excel("vehicles_norway.xlsx", sheet = 2)

# add up columns of 4-wheel vehicles
vehicles_norway <- vehicles_norway %>%
  mutate(selection_vehicles = passenger_cars + buses + vans + lorries + combined_vehicles)

# join population data
vehicles_norway <- left_join(vehicles_norway, population_norway, by = "year")

# calculate vehicles per 1000 people
vehicles_norway <- vehicles_norway %>%
  mutate(vehicles_per_1000 = (selection_vehicles / population) * 1000)

# year column as numeric

vehicles_norway <- vehicles_norway %>%
  mutate(year = as.numeric(year))

# join gdp per capita column
vehicles_norway <- left_join(vehicles_norway, norway_gdp %>% select(year, gdp_per_capita), by = "year")



ggplot(data = vehicles_norway, aes(x = gdp_per_capita, y = vehicles_per_1000)) +
  # Add a line plot
  geom_line(color = "blue") +
  # Add labels for x-axis and y-axis
  labs(x = "GDP per Capita", y = "Vehicles per Capita",
       title = "Vehicles per Capita vs GDP per Capita") +
  # Set y-axis limits
  ylim(0, 1000) +
  # Customize the theme
  theme_minimal()



ggplot(data = dargay_et_al_calc, aes(x = gdp_per_capita, y = vehicles_capita)) +
  # Add a line plot
  geom_line(color = "blue") +
  # Add labels for x-axis and y-axis
  labs(x = "GDP per Capita", y = "Vehicles per Capita",
       title = "Vehicles per Capita vs GDP per Capita") +
  # Set y-axis limits
  ylim(0, 1000) +
  # Customize the theme
  theme_minimal()



library(patchwork)

# Plot for vehicles_norway data
plot1 <- ggplot(data = vehicles_norway, aes(x = gdp_per_capita, y = vehicles_per_1000)) +
  geom_line(color = "blue") +
  labs(x = "GDP per Capita", y = "Vehicles per 1000 inhabitants",
       title = "Historical Data Norway") +
  ylim(0, 1000) +
  theme_minimal()

# Plot for dargay_et_al_calc data
plot2 <- ggplot(data = dargay_et_al_calc, aes(x = gdp_per_capita, y = vehicles_capita)) +
  geom_line(color = "blue") +
  labs(x = "GDP per Capita", y = "Vehicles per 1000 inhabitants",
       title = "Dargay calculation") +
  ylim(0, 1000) +
  theme_minimal()

# Combine both plots
combined_plot <- plot1 + plot2

# Display the combined plot
combined_plot



#### Norway
## not really different whether I use 1995 gdp or 2005
pwt_data <- read_excel("pwt61.xlsx", sheet = 2)
pwt_data$gdp_per_capita_95 <- pwt_data$cgdp

norway_gdp <- pwt_data %>%
  filter(country == "Norway", year >= 1960, year <= 2005)%>%
  select(country, year, gdp_per_capita_95)

# Provide the path to your Excel file
file_path <- "world_bank_usa.xls"

# Read the Excel file into a data frame
world_bank_usa <- read_excel(file_path)


# Filter the data for the indicator code EN.POP.DNST for the US
pop_density_usa <- world_bank_usa %>%
  filter(indicator_code == "EN.POP.DNST")

# Convert the data frame from wide to long format
pop_density_usa_long <- pop_density_usa %>%
  pivot_longer(cols = starts_with("19") | starts_with("20"),
               names_to = "year",
               values_to = "density_usa")



# Filter the data for the indicator code SP.URB.TOTL.IN.ZS for the US
urban_pop_usa <- world_bank_usa %>%
  filter(indicator_code == "SP.URB.TOTL.IN.ZS")

# Convert the data frame from wide to long format
urban_pop_usa_long <- urban_pop_usa %>%
  pivot_longer(cols = starts_with("19") | starts_with("20"),
               names_to = "year",
               values_to = "urbanization_usa")


# Convert "year" column to character in all data frames
urban_pop_usa_long$year <- as.character(urban_pop_usa_long$year)
pop_density_usa_long$year <- as.character(pop_density_usa_long$year)
pop_density_norway_long$year <- as.character(pop_density_norway_long$year)
urbanization_norway_long$year <- as.character(urbanization_norway_long$year)


# Filter and select relevant columns from each data frame
urban_pop_usa_filtered <- urban_pop_usa_long %>%
  filter(year >= 1960 & year <= 2005) %>%
  select(year, urbanization_usa)

pop_density_usa_filtered <- pop_density_usa_long %>%
  filter(year >= 1960 & year <= 2005) %>%
  select(year, density_usa)

pop_density_norway_filtered <- pop_density_norway_long %>%
  filter(year >= 1960 & year <= 2005) %>%
  select(year, density)

urbanization_norway_filtered <- urbanization_norway_long %>%
  filter(year >= 1960 & year <= 2005) %>%
  select(year, urbanization)

# Merge the data frames
dargay_et_al_calc <- urban_pop_usa_filtered %>%
  full_join(pop_density_usa_filtered, by = "year") %>%
  full_join(pop_density_norway_filtered, by = "year") %>%
  full_join(urbanization_norway_filtered, by = "year")



# dargay_et_al_calc contains the merged data frame with columns:
# year, urbanization_usa, density_usa, density, urbanization

# Calculate Dit and Uit for each year
dargay_et_al_calc <- dargay_et_al_calc %>%
  mutate(D_it = ifelse(density > density_usa, density - density_usa, 0),
         U_it = ifelse(urbanization > urbanization_usa, urbanization - urbanization_usa, 0))

# add GDP columns
dargay_et_al_calc <- merge(norway_gdp, dargay_et_al_calc, by = "year")

dargay_et_al_calc$gdp_per_capita_95 <- as.numeric(dargay_et_al_calc$gdp_per_capita_95)

dargay_et_al_calc <- dargay_et_al_calc %>%
  arrange(year) %>%
  mutate(R_it = ifelse(gdp_per_capita_95 - lag(gdp_per_capita_95, default = first(gdp_per_capita_95)) > 0, 1, 0),
         F_it = ifelse(gdp_per_capita_95 - lag(gdp_per_capita_95, default = first(gdp_per_capita_95)) < 0, 1, 0))

# Check the updated data frame
head(dargay_et_al_calc)


calculate_V <- function(alpha, beta, gamma, lambda, theta_R, theta_F, phi, data_frame, initial_V) {
  # Access values from the data frame
  R_it <- data_frame$R_it
  F_it <- data_frame$F_it
  # D_it <- data_frame$D_it
  # U_it <- data_frame$U_it
  GDP <- data_frame$gdp_per_capita_95
  
  # Initialize V with the initial value
  V <- rep(NA, nrow(data_frame))
  V[1] <- initial_V
  
  # Calculate V iteratively for each year
  for (i in 2:length(V)) {
    # Calculate the expression for V
    V[[i]] <- (gamma) * (theta_R * R_it[[i]] + theta_F * F_it[[i]]) * exp(alpha * exp(beta * GDP[i])) +
      (1 - theta_R * R_it[[i]] - theta_F * F_it[[i]]) * V[i-1]
  }
  
  return(V)
}

#  V[[i]] <- (gamma + lambda * D_it[[i]] + phi * U_it[[i]]) * (theta_R * R_it[[i]] + theta_F * F_it[[i]]) * exp(alpha * exp(beta * GDP[[i]])) + (1 - theta_R * R_it[[i]] - theta_F * F_it[[i]]) * V[[i-1]]

# Define the initial value of V for the first year
initial_V <- 95  # value for 1960, Dargay et al 2007

# External parameters
alpha <- -5.897
beta <- -0.13
gamma <- 852
lambda <- -0.000388
theta_R <- 0.095
theta_F <- 0.084
phi <- -0.007765

# Data frame
data_frame <- dargay_et_al_calc

# Call the function with external parameters and your data frame
result <- calculate_V(alpha, beta, gamma, lambda, theta_R, theta_F, phi, data_frame, initial_V)

# Add the result vector as a new column to your data frame
dargay_et_al_calc <- cbind(dargay_et_al_calc, vehicles_capita = result)




# Create a ggplot object and specify the data and aesthetics
ggplot(data = dargay_et_al_calc, aes(x = gdp_per_capita_95, y = vehicles_capita)) +
  # Add a line plot
  geom_line(color = "blue") +
  # Add labels for x-axis and y-axis
  labs(x = "GDP per Capita", y = "Vehicles per Capita",
       title = "Vehicles per Capita vs GDP per Capita") +
  # Set y-axis limits
  ylim(0, 1000) +
  # Customize the theme
  theme_minimal()


# Rename the column in dargay_et_al_calc
dargay_et_al_calc <- dargay_et_al_calc %>%
  rename(vehicles_capita_dargay = vehicles_capita)

# Merge the renamed column to vehicles_norway based on year
combined_data <- left_join(vehicles_norway, dargay_et_al_calc %>% select(year, vehicles_capita_dargay), by = "year")


# Reshape the data to long format
long_data <- combined_data %>%
  pivot_longer(
    cols = c("vehicles_capita", "vehicles_capita_dargay"),
    names_to = "series",
    values_to = "value"
  )


# Filter the data for years between 1960 and 2005
long_data <- long_data %>%
  filter(year >= 1960 & year <= 2005)


# Create the ggplot object and specify the data and aesthetics
ggplot(data = long_data, aes(x = year, y = value, color = series, linetype = series)) +
  geom_line() +  # Add a line plot
  labs(x = NULL, y = "Vehicles per Capita",
       title = NULL) +  # Set labels
  scale_color_manual(values = c("vehicles_capita" = "black", "vehicles_capita_dargay" = "black"),
                     labels = c("Vehicles Statistics Norway", "Vehicles Dargay et al. 2007")) +  # Labels for color
  scale_linetype_manual(values = c("vehicles_capita" = "solid", "vehicles_capita_dargay" = "dashed"),
                        labels = c("Vehicles Statistics Norway", "Vehicles Dargay et al. 2007")) +  # Labels for linetype
  theme_classic() +  # Use a classic theme
  theme(legend.title = element_blank(),  # Remove the legend title
        legend.position = "bottom")  # Move legend to the bottom






#### USA
## end up with values too large as well

usa_subset <- carbontax_fullsample %>% 
  filter(country == "United States")

calculate_V <- function(alpha, beta, gamma, lambda, theta_R, theta_F, phi, data_frame, initial_V) {
  # Access values from the data frame
  R_it <- data_frame$R_it
  F_it <- data_frame$F_it
  # D_it <- data_frame$D_it
  # U_it <- data_frame$U_it
  GDP <- data_frame$GDP_per_capita
  
  # Initialize V with the initial value
  V <- rep(NA, nrow(data_frame))
  V[1] <- initial_V
  
  # Calculate V iteratively for each year
  for (i in 2:length(V)) {
    # Calculate the expression for V
    V[[i]] <- (gamma) * (theta_R * R_it[[i]] + theta_F * F_it[[i]]) * exp(alpha * exp(beta * GDP[i])) +
      (1 - theta_R * R_it[[i]] - theta_F * F_it[[i]]) * V[i-1]
  }
  
  return(V)
}

#  V[[i]] <- (gamma + lambda * D_it[[i]] + phi * U_it[[i]]) * (theta_R * R_it[[i]] + theta_F * F_it[[i]]) * exp(alpha * exp(beta * GDP[[i]])) + (1 - theta_R * R_it[[i]] - theta_F * F_it[[i]]) * V[[i-1]]

# Define the initial value of V for the first year
initial_V <- 411  # value for 1960, Dargay et al 2007

# External parameters
alpha <- -5.897
beta <- -0.2
gamma <- 852
lambda <- -0.000388
theta_R <- 0.095
theta_F <- 0.084
phi <- -0.007765

# Data frame
data_frame <- usa_subset

# Call the function with external parameters and your data frame
result <- calculate_V(alpha, beta, gamma, lambda, theta_R, theta_F, phi, data_frame, initial_V)

# Add the result vector as a new column to your data frame
usa_subset <- cbind(usa_subset, vehic_capita = result)
