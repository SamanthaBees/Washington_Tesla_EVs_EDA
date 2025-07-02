# Libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)


# Read in the data

df <- read.csv("C:/Users/saman/OneDrive - Douglas College/Analyst Builder/Github/R_EVs_EDA/Electric_Vehicle_Population_Data.csv")

# Data Cleaning

clean_df <- df %>%
  rename_with(~ gsub("\\.", "_", .)) %>%
  mutate(
    Base_MSRP = as.numeric(Base_MSRP),
    Tesla = ifelse(Make == "TESLA","TESLA", "OTHER")
  )



# EDA Questions


# 1.	What percentage of EVs in Washington are Tesla?

total_vehicles <- nrow(clean_df)
tesla_vehicles <- clean_df %>% 
  filter(Tesla == "TESLA") %>% 
  nrow()

tesla_market_share <- round(tesla_vehicles/total_vehicles * 100, 1)

market_df <- data.frame(
  Group = c("TESLA", "OTHER"),
  Market_Share = c(tesla_market_share, 100 - tesla_market_share)
)

market_df <- market_df %>%
  mutate(
    Label = paste0(Market_Share, "%"),
    ypos = cumsum(Market_Share) - 0.5 * Market_Share
  )

ggplot(market_df, aes(x = "", y = Market_Share, fill = Group)) +
  geom_col(width = 1) +
  coord_polar("y") +
  geom_text(aes(y = ypos, label= Label)) +
  labs(title = "Tesla Market Share in Washington (%)") +
  theme_void() +
  scale_fill_manual(values = c(
    "TESLA" = "#9ACD32",
    "OTHER" = "#E0EEE0"
  ))





# 2.	Top Tesla Models selling in the area. 
# -   How much are we making from these models?

make_model_sold <- clean_df %>%
  filter(Tesla == "TESLA") %>%
  group_by(Make,Model) %>%
  summarise(Count_sold = n(), .groups = "drop") %>%
  arrange(desc(Count_sold))

ggplot(make_model_sold, aes(x = reorder(Model, Count_sold), y= Count_sold)) +
  geom_col(fill = "olivedrab3") +
  coord_flip() +
  labs(title = "Tesla Models Sold - All Time") +
  theme_minimal()


msrp_by_year <- clean_df %>%
  filter(Base_MSRP > 0) %>%
  filter(Tesla == "TESLA") %>%
  group_by(Model_Year, Make, Model) %>%
  summarise(min(Base_MSRP)) %>%
  arrange(Model_Year, Make, Model)

# There were not good base msrp data in this dataset - supplementing with online data.
df_tesla_prices <- read.csv("C:/Users/saman/OneDrive - Douglas College/Analyst Builder/Github/R_EVs_EDA/Tesla_Current_Base_Prices.csv")

# transforming model data to upper case
df_tesla_prices <- df_tesla_prices %>%
  mutate(Model = toupper(Model))

# Getting our numbers
Top_Tesla_Models_Priced <- make_model_sold %>%
  left_join(df_tesla_prices, by = "Model") %>%
  mutate(Estimate_revenue = as.numeric(Count_sold) * as.numeric(Base_Price_USD)) 

# Final Estimates for lifetime sales
Top_Tesla_Models_Priced <- Top_Tesla_Models_Priced %>%
  filter(!is.na(Estimate_revenue)) %>%
  group_by(Model) %>%
  slice_min(Base_Price_USD) %>%
    ungroup()


# Viz
ggplot(Top_Tesla_Models_Priced, aes(x = reorder(Model, Estimate_revenue), y= Estimate_revenue)) +
  geom_col(fill = "forestgreen") +
  coord_flip() +
  labs(title = "Estimated Revenue Per Model") +
  scale_y_continuous(labels = label_comma()) +
  theme_minimal()


# ADD IN

tesla_sold_by_year <- clean_df %>%
  filter(Tesla == "TESLA")%>%
  group_by(Model_Year) %>%
  summarise(Count = n()) %>%
  arrange(Model_Year)


ggplot(tesla_sold_by_year, aes(x = Model_Year, y = Count)) +
  geom_line(color = "#9ACD32", size = 1.1) +
  geom_point(color = "red", size = 2) +
  labs(title = "Tesla Vehicles Sold by Year") +
  theme_minimal()






# 3.	Tesla vs. Competitors - Range and MSRP. 
#  -  AVG and Median

# Average Range and MSRP
Average_Range_for_EVs <- clean_df %>%
  filter(Electric_Vehicle_Type == "Battery Electric Vehicle (BEV)",
         Electric_Range != 0,
         Base_MSRP != 0) %>%
  group_by(Tesla) %>%
  summarise(AVG_Range = mean(Electric_Range),
            AVG_MSRP = mean(Base_MSRP))
   

# Median Range and MSRP 
Median_Range_for_EVs <- clean_df %>%
  filter(Electric_Vehicle_Type == "Battery Electric Vehicle (BEV)",
         Electric_Range != 0,
         Base_MSRP != 0) %>%
  group_by(Tesla) %>%
  summarise(Median_Range = median(Electric_Range),
            Median_MSRP = median(Base_MSRP))
# MSRP data is not fully populated and may skew results.



#Visualizations for MSRP and Range AVG

#Range
ggplot(Average_Range_for_EVs, aes(x = Tesla, y = AVG_Range, fill = Tesla)) +
  geom_col() +
  labs(title = "AVG Electric Range (BEV)") +
  theme_minimal() +
  scale_fill_manual(values = c(
    "TESLA" = "#9ACD32",
    "OTHER" = "#E0EEE0"
  ))


#MSRP
ggplot(Average_Range_for_EVs, aes(x = Tesla, y = AVG_MSRP, fill = Tesla)) +
  geom_col() +
  labs(title = "AVG MSRP (BEV)") +
  theme_minimal() +
  scale_fill_manual(values = c(
    "TESLA" = "#9ACD32",
    "OTHER" = "#E0EEE0"
  ))






# 4.	PHEV vs BEV Trends

bhev_vs_bev <- clean_df %>%
  group_by(Model_Year, Electric_Vehicle_Type) %>%
  summarise(Count = n(), .groups = "drop")

ggplot(bhev_vs_bev, aes(x = Model_Year, y = Count, color = Electric_Vehicle_Type)) +
  geom_line(size = 1.1) +
  geom_point(size = 2) +
  labs(title = "PHEV vs BEV Trends Over Time") +
  theme_minimal() 





# 5. Top Electric Utilities in Washington for Tesla

Utility_Count_for_Teslas <- clean_df %>%
  filter(Tesla == "TESLA" ) %>%
  group_by(Electric_Utility) %>%
  summarise(Count = n(), .groups = "drop") %>%
  arrange(desc(Count))%>%
  head(5)

ggplot(Utility_Count_for_Teslas, aes(x = reorder(Electric_Utility, Count), y = Count)) +
  geom_col(fill = "forestgreen") +
  coord_flip() +
  labs(title = "Top Electric Utilities for Tesla in Washington", x = "Utility", y = "Tesla Count") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 4))

  
