# Install and load necessary libraries
#if (!requireNamespace("readxl", quietly = TRUE)) install.packages("readxl")
#if (!requireNamespace("tidyverse", quietly = TRUE)) install.packages("tidyverse")
#if (!requireNamespace("data.table", quietly = TRUE)) install.packages("data.table")
#if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
#if (!requireNamespace("skimr", quietly = TRUE)) install.packages("skimr")
#if (!requireNamespace("corrplot", quietly = TRUE)) install.packages("corrplot")
#if (!requireNamespace("naniar", quietly = TRUE)) install.packages("naniar")

# Load libraries
library(readxl)
library(tidyverse)
library(data.table)
library(ggplot2)
library(skimr)
library(corrplot)
library(naniar)

# Step 1: Load Data
file_path <- "/Users/srishtigupta/Downloads/merged_tequila_data.xlsx"  # Update with actual file path
data <- read_excel(file_path)


# Step 2: Basic Data Overview
cat("Basic structure of the data:\n")
str(data)
cat("\nDimensions of the data:\n")
print(dim(data))
cat("\nColumn names in the data:\n")
print(names(data))

# Step 3: Check for Missing Values
cat("\nSummary of missing values:\n")
missing_values <- colSums(is.na(data))
print(missing_values)

# Impute missing values instead of dropping rows
# For numeric columns, fill missing values with the median of the column
numeric_columns <- data %>% select(where(is.numeric))
data <- data %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), median(., na.rm = TRUE), .)))

# For categorical columns, fill missing values with the mode
mode_impute <- function(x) {
  uniq <- unique(na.omit(x))
  uniq[which.max(tabulate(match(x, uniq)))]
}
categorical_columns <- data %>% select(where(is.character))
data <- data %>%
  mutate(across(where(is.character), ~ ifelse(is.na(.), mode_impute(.), .)))

# Confirming all missing values are handled
cat("\nRemaining missing values after imputation:\n")
print(colSums(is.na(data)))


# Visualize missing values with a heatmap
gg_miss_var(data) + labs(title = "Missing Data by Variable")

# Handle missing values (example: imputing 'Retail' with median if few missing values)
data <- data %>%
  mutate(Retail = ifelse(is.na(Retail), median(Retail, na.rm = TRUE), Retail))

# Step 4: Outlier Detection and Handling, Removing outliers in 'Retail' based on the IQR method
q1 <- quantile(data$Retail, 0.25, na.rm = TRUE)
q3 <- quantile(data$Retail, 0.75, na.rm = TRUE)
iqr <- q3 - q1
outlier_limit <- q3 + 1.5 * iqr  # Upper limit for outliers

# Filter out rows with Retail values above the outlier limit
data <- data %>% filter(Retail <= outlier_limit | is.na(Retail))

# Step 5: Summary Statistics
cat("\nSummary statistics of the data:\n")
print(summary(data))
skim(data)


head(data)
colnames(data)





# Load necessary libraries
library(tidyverse)
library(ggplot2)



# Load necessary libraries
library(tidyverse)
library(ggplot2)

# Summarize Hispanic Population and Sales by PriceZone
price_zone_summary <- data %>%
  group_by(PriceZone) %>%
  summarise(
    Total_Sales = sum(sales_dollars_L52wk, na.rm = TRUE),
    Avg_Hispanic_Pop = mean(Hispanic, na.rm = TRUE)
  ) %>%
  arrange(desc(Total_Sales))  # Sort by Total Sales

# Convert Total Sales to Millions for better readability
price_zone_summary <- price_zone_summary %>%
  mutate(Total_Sales_Millions = Total_Sales / 1e6)

# Highlight Price Zones with Hispanic Population > 30%
price_zone_summary <- price_zone_summary %>%
  mutate(Highlight = ifelse(Avg_Hispanic_Pop > 0.3, "Above 30% Hispanic Population", "Below 30%"))

# Improved Visualization for Sales and Hispanic Population
ggplot(price_zone_summary, aes(x = reorder(PriceZone, Total_Sales_Millions))) +
  # Bar chart for Total Sales with highlight
  geom_bar(
    aes(y = Total_Sales_Millions, fill = Highlight), 
    stat = "identity", alpha = 0.8, show.legend = TRUE
  ) +
  # Line chart for Hispanic Population
  geom_line(
    aes(y = Avg_Hispanic_Pop * max(Total_Sales_Millions), group = 1),
    color = "darkblue", size = 1.2
  ) +
  # Points for Hispanic Population
  geom_point(
    aes(y = Avg_Hispanic_Pop * max(Total_Sales_Millions)),
    color = "darkred", size = 3
  ) +
  # Add percentage labels for points above 30%
  geom_text(
    aes(
      y = Avg_Hispanic_Pop * max(Total_Sales_Millions),
      label = ifelse(Avg_Hispanic_Pop > 0.3, paste0(round(Avg_Hispanic_Pop * 100), "%"), "")
    ),
    vjust = -0.5, size = 3, color = "black"
  ) +
  # Adjust Y-axis for Total Sales in Millions
  scale_y_continuous(
    name = "Total Sales ($ in Millions)",
    sec.axis = sec_axis(~ . / max(price_zone_summary$Total_Sales_Millions) * 100,
                        name = "Hispanic Population (%)")
  ) +
  # Adjust color gradient and legend
  scale_fill_manual(
    values = c("Above 30% Hispanic Population" = "darkorange", "Below 30%" = "lightblue"),
    name = "Hispanic Population"
  ) +
  # Labels and title
  labs(
    title = "Sales and Hispanic Population by Price Zone",
    x = "Price Zone",
    y = "Total Sales ($ in Millions)"
  ) +
  # Theme adjustments for better visuals
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    axis.title.y.right = element_text(color = "darkblue")
  )




# Standardize item names for consistency
store_1012_data <- store_1012_data %>%
  mutate(item_name = str_trim(item_name),  # Remove extra spaces
         item_name = tolower(item_name))   # Convert to lowercase for uniformity


# Summarize sales for "Gold" and "Silver" tequilas
gold_silver_summary <- store_1012_data %>%
  filter(str_detect(item_name, "gold|silver")) %>%
  group_by(item_name) %>%
  summarise(
    total_sales = sum(sales_dollars_L52wk, na.rm = TRUE),
    normalized_sales = sum(Normalized_Sales_L52W, na.rm = TRUE)
  ) %>%
  arrange(desc(total_sales))

print(gold_silver_summary)


# Validate recommendations against top-selling items
recommended_check <- recommended_tequilas %>%
  inner_join(gold_silver_summary, by = "item_name") %>%
  select(item_name, total_sales, normalized_sales)

print(recommended_check)


# Bar plot for comparison
ggplot(gold_silver_summary, aes(x = reorder(item_name, -total_sales), y = total_sales, fill = item_name)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  geom_text(aes(label = round(total_sales, 0)), vjust = -0.5, size = 3) +
  labs(
    title = "Comparison of 'Gold' and 'Silver' Tequilas",
    x = "Tequila Name",
    y = "Total Sales (L52W)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10)
  )
















# Filter rows for all "Jose Cuervo" tequilas in Store 1012
jose_cuervo_tequilas <- store_1012_data %>% 
  filter(str_detect(item_name, "Jose Cuervo")) %>% 
  select(item_name, sales_dollars_L52wk, Normalized_Sales_L52W) %>% 
  arrange(desc(sales_dollars_L52wk))

print("Jose Cuervo Tequilas:")
print(jose_cuervo_tequilas)

# Compare "Gold" vs. "Silver"
gold_vs_silver <- store_1012_data %>% 
  filter(str_detect(item_name, "Gold|Silver")) %>% 
  select(item_name, sales_dollars_L52wk, Normalized_Sales_L52W) %>% 
  arrange(desc(sales_dollars_L52wk))

print("Comparison of 'Gold' and 'Silver' Tequilas:")
print(gold_vs_silver)



# Check top recommended tequilas by Normalized Sales
recommended_tequilas <- store_1012_data %>% 
  arrange(desc(Normalized_Sales_L52W)) %>% 
  select(item_name, sales_dollars_L52wk, Normalized_Sales_L52W) %>% 
  head(10)

print("Top Recommended Tequilas Based on Normalized Sales:")
print(recommended_tequilas)

# Visualize Gold vs. Silver sales metrics
gold_silver_comparison <- store_1012_data %>% 
  filter(str_detect(item_name, "Gold|Silver")) %>% 
  select(item_name, sales_dollars_L52wk, Normalized_Sales_L52W) %>% 
  pivot_longer(cols = c(sales_dollars_L52wk, Normalized_Sales_L52W), 
               names_to = "Metric", 
               values_to = "Value")

ggplot(gold_silver_comparison, aes(x = item_name, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Comparison of 'Gold' and 'Silver' Tequilas",
    x = "Tequila Name",
    y = "Sales Metrics",
    fill = "Metric"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))









# Inspect the top-selling tequila
print(top_tequila)


# Inspect recommended tequilas
print(recommended_tequilas)
# Filter rows for "Gold" and "Silver"
gold_tequilas <- store_1012_data %>% filter(str_detect(item_name, "Gold"))
silver_tequilas <- store_1012_data %>% filter(str_detect(item_name, "Silver"))

print(gold_tequilas)
print(silver_tequilas)


# Update the subtitle dynamically
ggplot(top_10_recommended, aes(x = reorder(item_name, -Normalized_Sales_L52W), y = Normalized_Sales_L52W)) +
  geom_bar(stat = "identity", fill = "darkorange", alpha = 0.8) +
  geom_text(aes(label = round(Normalized_Sales_L52W, 1)), vjust = -0.5, size = 3) +
  labs(
    title = "Top Recommended Tequilas for Store 1012",
    subtitle = paste("Top-Selling Tequila: ", top_tequila$item_name),
    x = "Tequila Name",
    y = "Normalized Sales (L52W)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
  )







# Load necessary libraries
library(readxl)
library(tidyverse)

# Load the datasets
merged_data_path <- "~/Downloads/merged_tequila_data.xlsx"  # Update with correct file path
residuals_data_path <- "~/Downloads/full_residuals_data.xlsx"  # Update with correct file path

merged_data <- read_excel(merged_data_path)

# Step 1: Filter data for Store 1012
store_1012_data <- merged_data %>% filter(store_number == 1012)

# Step 2: Get top-selling tequila in Store 1012
top_tequila <- store_1012_data %>%
  arrange(desc(sales_dollars_L52wk)) %>%
  select(item_code, category, item_name, sales_dollars_L52wk) %>%
  slice(1)

# Extract the category of the top-selling tequila
top_tequila_category <- top_tequila$category[1]

# Step 3: Match the characteristics to the recommended list
recommended_tequilas <- store_1012_data %>%
  filter(category == top_tequila_category) %>%
  arrange(desc(Normalized_Sales_L52W)) %>%
  select(item_code, item_name, category, Normalized_Sales_L52W)

# Step 4: Print the Results
cat("\nTop-Selling Tequila in Store 1012:\n")
print(top_tequila)

cat("\nRecommended Tequilas Matching Top-Selling Category:\n")
print(recommended_tequilas)

# Step 5: Justification for choosing Store 1012
justification <- "Store 1012 is middle of the pack in terms of sales for both premium and non-premium tequilas, making it an ideal candidate for a more customer-driven assortment strategy."
cat("\nJustification:\n", justification, "\n")

# Step 6: Visualization to Show Findings
# Prepare data for visualization
top_10_recommended <- recommended_tequilas %>%
  top_n(10, wt = Normalized_Sales_L52W)

# Create visualization
library(ggplot2)
ggplot(top_10_recommended, aes(x = reorder(item_name, -Normalized_Sales_L52W), y = Normalized_Sales_L52W)) +
  geom_bar(stat = "identity", fill = "darkorange", alpha = 0.8) +
  geom_text(aes(label = round(Normalized_Sales_L52W, 1)), vjust = -0.5, size = 3) +
  labs(
    title = "Top Recommended Tequilas for Store 1012",
    subtitle = paste("Top-Selling Tequila: ", top_tequila$item_name),
    x = "Tequila Name",
    y = "Normalized Sales (L52W)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
  )









# Load necessary libraries
library(readxl)
library(tidyverse)

# Load the datasets
merged_data_path <- "~/Downloads/merged_tequila_data.xlsx"  # Update with correct file path
residuals_data_path <- "~/Downloads/full_residuals_data.xlsx"  # Update with correct file path

# Load datasets
merged_data <- read_excel(merged_data_path)
residuals_data <- read_excel(residuals_data_path)

# Step 1: Filter data for Store 1012
store_1012_data <- merged_data %>% filter(store_number == 1012)

# Step 2: Get top-selling tequila in Store 1012
top_tequila <- store_1012_data %>%
  arrange(desc(sales_dollars_L52wk)) %>%
  select(item_code, category, item_name, sales_dollars_L52wk) %>%
  slice(1)

# Extract the category of the top-selling tequila
top_tequila_category <- top_tequila$category[1]

# Step 3: Match the characteristics to the recommended list
recommended_tequilas <- store_1012_data %>%
  filter(category == top_tequila_category) %>%
  arrange(desc(Normalized_Sales_L52W)) %>%
  select(item_code, item_name, category, Normalized_Sales_L52W)

# Print the results
cat("\nTop-Selling Tequila in Store 1012:\n")
print(top_tequila)

cat("\nRecommended Tequilas Matching Top-Selling Category:\n")
print(recommended_tequilas)

# Justification for choosing Store 1012
justification <- "Store 1012 is middle of the pack in terms of sales for both premium and non-premium tequilas, making it an ideal candidate for a more customer-driven assortment strategy."
cat("\nJustification:\n", justification, "\n")

# Step 4: Visualization to Show Findings
# Prepare data for visualization
top_10_recommended <- recommended_tequilas %>%
  top_n(10, wt = Normalized_Sales_L52W)

# Visualization
ggplot(top_10_recommended, aes(x = reorder(item_name, -Normalized_Sales_L52W), y = Normalized_Sales_L52W)) +
  geom_bar(stat = "identity", fill = "darkorange", alpha = 0.8) +
  geom_text(aes(label = round(Normalized_Sales_L52W, 1)), vjust = -0.5, size = 3) +
  labs(
    title = "Top Recommended Tequilas for Store 1012",
    subtitle = paste("Top-Selling Tequila: ", top_tequila$item_name),
    x = "Tequila Name",
    y = "Normalized Sales (L52W)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
  )






filter(category == top_tequila_category)


colnames(merged_data)












# Step 3: Clustering PriceZones Based on Sales and Demographics
clustering_data <- price_zone_summary %>%
  select(Total_Sales, Avg_Hispanic_Pop) %>%
  scale()  # Normalize the data

set.seed(123)  # For reproducibility
clusters <- kmeans(clustering_data, centers = 3)  # Choose 3 clusters

# Add cluster information to the dataset
price_zone_summary$Cluster <- as.factor(clusters$cluster)

# Rename clusters for better comprehension
price_zone_summary$Cluster <- recode(price_zone_summary$Cluster,
                                     "1" = "Low Sales & Moderate Diversity",
                                     "2" = "High Sales & Balanced Diversity",
                                     "3" = "High Sales & High Hispanic Population"
)

# Step 4: Improve Cluster Visualization
ggplot(price_zone_summary, aes(x = Total_Sales, y = Avg_Hispanic_Pop, color = Cluster)) +
  geom_point(size = 4, alpha = 0.6) +  # Adjusted opacity for overlapping points
  geom_text(aes(label = paste0(round(Avg_Hispanic_Pop * 100), "%")), vjust = -0.5, size = 3) +
  scale_color_manual(
    values = c(
      "Low Sales & Moderate Diversity" = "red",
      "High Sales & Balanced Diversity" = "green",
      "High Sales & High Hispanic Population" = "blue"
    ),
    name = "Cluster Categories"
  ) +
  labs(
    title = "Clustering of Price Zones",
    x = "Total Sales (L52 Weeks)",
    y = "Hispanic Population (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9)
  )

















# Load necessary libraries
library(tidyverse)
library(ggplot2)

# Step 1: Summarize Hispanic Population and Sales by PriceZone
price_zone_summary <- data %>%
  group_by(PriceZone) %>%
  summarise(
    Total_Sales = sum(sales_dollars_L52wk, na.rm = TRUE),
    Avg_Hispanic_Pop = mean(Hispanic, na.rm = TRUE)
  ) %>%
  arrange(desc(Total_Sales))  # Sort by Total Sales

# Step 2: Create Visualization
ggplot(price_zone_summary, aes(x = reorder(PriceZone, Total_Sales))) +
  # Bar chart for Total Sales
  geom_bar(aes(y = Total_Sales, fill = Total_Sales), stat = "identity", alpha = 0.8) +
  # Line chart for Hispanic Population
  geom_line(
    aes(y = Avg_Hispanic_Pop * max(Total_Sales), group = 1),
    color = "blue", size = 1.2
  ) +
  # Add points for Hispanic Population
  geom_point(
    aes(y = Avg_Hispanic_Pop * max(Total_Sales)),
    color = "red", size = 2
  ) +
  # Add percentage labels for Hispanic Population
  geom_text(
    aes(y = Avg_Hispanic_Pop * max(Total_Sales),
        label = paste0(round(Avg_Hispanic_Pop * 100, 1), "%")),
    vjust = -0.5, size = 3, color = "black"
  ) +
  # Gradient fill for bars
  scale_fill_gradient(low = "yellow", high = "red") +
  # Adjust y-axis for dual metrics
  scale_y_continuous(
    name = "Total Sales (L52 Weeks)",
    sec.axis = sec_axis(~ . / max(price_zone_summary$Total_Sales) * 100,
                        name = "Hispanic Population (%)")
  ) +
  # Labels and title
  labs(
    title = "Sales and Hispanic Population by Price Zone",
    x = "Price Zone",
    fill = "Total Sales"
  ) +
  # Theme for better visualization
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title.y.right = element_text(color = "blue")
  )

# Step 3: Clustering PriceZones Based on Sales and Demographics
clustering_data <- price_zone_summary %>%
  select(Total_Sales, Avg_Hispanic_Pop) %>%
  scale()  # Normalize the data

set.seed(123)  # For reproducibility
clusters <- kmeans(clustering_data, centers = 3)  # Choose 3 clusters

# Add cluster information to the dataset
price_zone_summary$Cluster <- as.factor(clusters$cluster)

# Step 4: Visualize Clusters
ggplot(price_zone_summary, aes(x = Total_Sales, y = Avg_Hispanic_Pop, color = Cluster)) +
  geom_point(size = 4, alpha = 0.8) +
  labs(
    title = "Clustering of Price Zones",
    x = "Total Sales (L52 Weeks)",
    y = "Average Hispanic Population (%)",
    color = "Cluster"
  ) +
  theme_minimal()


#step 6A
# Create an Age_Group column
data <- data %>%
  mutate(Age_Group = case_when(
    TequilaUnder65 > TequilaOver65 ~ "Under 65",
    TRUE ~ "Over 65"
  ))

# Summarize sales by age group
age_group_sales <- data %>%
  group_by(Age_Group) %>%
  summarise(Total_Sales = sum(sales_dollars_L52wk, na.rm = TRUE))

# Bar chart for age group sales
ggplot(age_group_sales, aes(x = Age_Group, y = Total_Sales, fill = Age_Group)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  labs(
    title = "Sales Distribution Across Age Groups",
    x = "Age Group",
    y = "Total Sales (L52 Weeks)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none",
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
  )


# Create price range buckets
data <- data %>%
  mutate(Price_Range = cut(Retail, breaks = c(0, 20, 64, Inf), labels = c("<$20", "$20-$64", ">$64")))

# Summarize sales by price range
price_range_sales <- data %>%
  group_by(Price_Range) %>%
  summarise(Total_Sales = sum(sales_dollars_L52wk, na.rm = TRUE))



#step 6B



# Boxplot for price range sales
ggplot(price_range_sales, aes(x = Price_Range, y = Total_Sales, fill = Price_Range)) +
  geom_bar(stat = "identity", alpha = 0.6) +
  labs(
    title = "Sales Distribution by Price Range",
    x = "Price Range",
    y = "Total Sales (L52 Weeks)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
  )

# Summarize sales by income level
income_sales <- data %>%
  group_by(`HH_Income>100K`) %>%
  summarise(Total_Sales = sum(sales_dollars_L52wk, na.rm = TRUE))

# Boxplot for income level vs. sales
ggplot(income_sales, aes(x = factor(`HH_Income>100K`), y = Total_Sales, fill = factor(`HH_Income>100K`))) +
  geom_bar(stat = "identity", alpha = 0.6) +
  labs(
    title = "Sales Distribution by Income Level",
    x = "Income Level (>100K)",
    y = "Total Sales (L52 Weeks)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    legend.position = "none"
  )





# Load Required Libraries
library(dplyr)
library(ggplot2)

# Group Data into Income Brackets for Simplified Visualization
income_sales <- data %>%
  mutate(Income_Bracket = case_when(
    `HH_Income>100K` <= 0.3 ~ "Low Income",
    `HH_Income>100K` <= 0.6 ~ "Middle Income",
    TRUE ~ "High Income"
  )) %>%
  group_by(Income_Bracket) %>%
  summarise(Total_Sales = sum(sales_dollars_L52wk, na.rm = TRUE))

# Bar Chart for Total Sales by Income Bracket
ggplot(income_sales, aes(x = Income_Bracket, y = Total_Sales, fill = Income_Bracket)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  labs(
    title = "Sales Distribution by Income Bracket",
    x = "Income Bracket",
    y = "Total Sales (L52 Weeks)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 12),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    legend.position = "none"
  )

# Group Data by Price Range for Simplified Visualization
price_range_sales <- data %>%
  mutate(Price_Range = case_when(
    Retail <= 20 ~ "<$20",
    Retail <= 64 ~ "$20-$64",
    TRUE ~ ">$64"
  )) %>%
  group_by(Price_Range) %>%
  summarise(Total_Sales = sum(sales_dollars_L52wk, na.rm = TRUE))

# Bar Chart for Total Sales by Price Range
ggplot(price_range_sales, aes(x = Price_Range, y = Total_Sales, fill = Price_Range)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  labs(
    title = "Sales Distribution by Price Range",
    x = "Price Range",
    y = "Total Sales (L52 Weeks)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 12),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    legend.position = "none"
  )







#step 6C
# Group data into income ranges











# Summarize Hispanic population and sales by state
state_hispanic <- data %>%
  group_by(state) %>%
  summarise(
    Total_Sales = sum(sales_dollars_L52wk, na.rm = TRUE),
    Avg_Hispanic_Pop = mean(Hispanic, na.rm = TRUE)
  ) %>%
  arrange(desc(Total_Sales))  # Sort by Total Sales

# Horizontal scatter plot with annotations
ggplot(state_hispanic, aes(x = Total_Sales, y = reorder(state, Total_Sales), size = Avg_Hispanic_Pop, color = Total_Sales)) +
  geom_point(alpha = 0.8) +
  scale_color_gradient(low = "yellow", high = "red") +
  labs(
    title = "Sales vs. Hispanic Population by State",
    x = "Total Sales (L52 Weeks)",
    y = "State",
    size = "Hispanic Population (%)",
    color = "Total Sales"
  ) +
  geom_text(aes(label = ifelse(state %in% c("CA", "TX", "FL"), state, "")), hjust = -0.5, size = 4) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 12),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
  )








# Bar chart for Total Sales with overlayed line for Hispanic Population
ggplot(state_hispanic, aes(x = reorder(state, Total_Sales))) +
  geom_bar(aes(y = Total_Sales, fill = Total_Sales), stat = "identity", alpha = 0.8) +
  geom_line(aes(y = Avg_Hispanic_Pop * max(Total_Sales), group = 1), color = "blue", size = 1.2) +
  scale_fill_gradient(low = "yellow", high = "red") +
  scale_y_continuous(
    name = "Total Sales (L52 Weeks)",
    sec.axis = sec_axis(~ . / max(state_hispanic$Total_Sales), name = "Hispanic Population (%)")
  ) +
  labs(
    title = "Sales and Hispanic Population by State",
    x = "State",
    fill = "Total Sales"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
  )















# Summarize Total Sales and Average African American Population by State
state_african_american <- data %>%
  group_by(state) %>%
  summarise(
    Total_Sales = sum(sales_dollars_L52wk, na.rm = TRUE),
    Avg_African_American_Pop = mean(AfricanAmerican, na.rm = TRUE)
  ) %>%
  arrange(desc(Total_Sales))  # Sort by Total Sales



ggplot(state_african_american, aes(x = reorder(state, Total_Sales))) +
  geom_bar(aes(y = Total_Sales, fill = Total_Sales), stat = "identity", alpha = 0.8) +
  geom_line(aes(y = Avg_African_American_Pop * max(Total_Sales), group = 1), 
            color = "blue", size = 1.2) +
  scale_fill_gradient(low = "yellow", high = "red") +
  scale_y_continuous(
    name = "Total Sales (L52 Weeks)",
    sec.axis = sec_axis(~ . / max(state_african_american$Total_Sales), 
                        name = "African American Population (%)")
  ) +
  labs(
    title = "Sales and African American Population by State",
    x = "State",
    fill = "Total Sales"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
  )




# Calculate Correlation
correlation <- cor(state_african_american$Avg_African_American_Pop, 
                   state_african_american$Total_Sales, 
                   use = "complete.obs")

print(paste("Correlation between African American population and Total Sales:", 
            round(correlation, 2)))



state_african_american <- state_african_american %>%
  mutate(Pop_Bracket = case_when(
    Avg_African_American_Pop <= 0.05 ~ "Low African American Population",
    Avg_African_American_Pop <= 0.15 ~ "Medium African American Population",
    TRUE ~ "High African American Population"
  ))

# Faceted Bar Chart
ggplot(state_african_american, aes(x = reorder(state, Total_Sales))) +
  geom_bar(aes(y = Total_Sales, fill = Total_Sales), stat = "identity", alpha = 0.8) +
  facet_wrap(~ Pop_Bracket, scales = "free_x") +
  scale_fill_gradient(low = "yellow", high = "red") +
  labs(
    title = "Sales by African American Population Bracket",
    x = "State",
    y = "Total Sales (L52 Weeks)",
    fill = "Total Sales"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
  )









# Summarize Total Sales and Average African American Population by Price Zone
price_zone_african_american <- data %>%
  group_by(PriceZone) %>%
  summarise(
    Total_Sales = sum(sales_dollars_L52wk, na.rm = TRUE),
    Avg_African_American_Pop = mean(AfricanAmerican, na.rm = TRUE)
  ) %>%
  arrange(desc(Total_Sales))  # Sort by Total Sales

ggplot(price_zone_african_american, aes(x = reorder(PriceZone, Total_Sales))) +
  geom_bar(aes(y = Total_Sales, fill = Total_Sales), stat = "identity", alpha = 0.8) +
  geom_line(aes(y = Avg_African_American_Pop * max(Total_Sales), group = 1), 
            color = "blue", size = 1.2) +
  scale_fill_gradient(low = "yellow", high = "red") +
  scale_y_continuous(
    name = "Total Sales (L52 Weeks)",
    sec.axis = sec_axis(~ . / max(price_zone_african_american$Total_Sales), 
                        name = "African American Population (%)")
  ) +
  labs(
    title = "Sales and African American Population by Price Zone",
    x = "Price Zone",
    fill = "Total Sales"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
  )



# Create Population Brackets
price_zone_african_american <- price_zone_african_american %>%
  mutate(Pop_Bracket = case_when(
    Avg_African_American_Pop <= 0.05 ~ "Low African American Population",
    Avg_African_American_Pop <= 0.15 ~ "Medium African American Population",
    TRUE ~ "High African American Population"
  ))

# Faceted Bar Chart by Population Bracket
ggplot(price_zone_african_american, aes(x = reorder(PriceZone, Total_Sales))) +
  geom_bar(aes(y = Total_Sales, fill = Total_Sales), stat = "identity", alpha = 0.8) +
  facet_wrap(~ Pop_Bracket, scales = "free_x") +
  scale_fill_gradient(low = "yellow", high = "red") +
  labs(
    title = "Sales by African American Population Bracket in Price Zones",
    x = "Price Zone",
    y = "Total Sales (L52 Weeks)",
    fill = "Total Sales"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
  )



# Correlation Analysis
correlation_price_zone <- cor(price_zone_african_american$Avg_African_American_Pop, 
                              price_zone_african_american$Total_Sales, 
                              use = "complete.obs")

print(paste("Correlation between African American population and Total Sales in Price Zones:", 
            round(correlation_price_zone, 2)))







#step 6 D
# Summarize Total Sales and Median Income by Price Zone
price_zone_summary <- data %>%
  group_by(PriceZone) %>%
  summarise(
    Total_Sales = sum(sales_dollars_L52wk, na.rm = TRUE),
    Avg_Median_Income = mean(Median_HH_Income, na.rm = TRUE)
  )

# Create a bar chart with a line overlay
ggplot(price_zone_summary, aes(x = reorder(PriceZone, Total_Sales))) +
  geom_bar(aes(y = Total_Sales, fill = Total_Sales), stat = "identity", alpha = 0.8) +
  geom_line(
    aes(y = Avg_Median_Income * max(Total_Sales) / max(Avg_Median_Income), group = 1),
    color = "blue", size = 1.2
  ) +
  scale_fill_gradient(low = "lightgreen", high = "darkgreen") +
  scale_y_continuous(
    name = "Total Sales (L52 Weeks)",
    sec.axis = sec_axis(~ . / max(price_zone_summary$Total_Sales) * max(price_zone_summary$Avg_Median_Income), 
                        name = "Average Median Income")
  ) +
  labs(
    title = "Total Sales and Median Income by Price Zone",
    x = "Price Zone",
    fill = "Total Sales"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
  )













# Select relevant columns for clustering
clustering_data <- price_zone_african_american %>%
  select(Total_Sales, Avg_African_American_Pop)

# Scale the data
clustering_data_scaled <- scale(clustering_data)

# Perform K-means clustering
set.seed(123)
clusters <- kmeans(clustering_data_scaled, centers = 3)  # Adjust centers based on analysis

# Add cluster information to the original data
price_zone_african_american$Cluster <- as.factor(clusters$cluster)

# Visualize Clusters
ggplot(price_zone_african_american, aes(x = Total_Sales, y = Avg_African_American_Pop, color = Cluster)) +
  geom_point(size = 4, alpha = 0.8) +
  labs(
    title = "Clustering of Price Zones",
    x = "Total Sales (L52 Weeks)",
    y = "Average African American Population (%)",
    color = "Cluster"
  ) +
  theme_minimal()











# Summarize by Price Zone and Include Multiple Demographics
demographic_summary <- data %>%
  group_by(PriceZone) %>%
  summarise(
    Total_Sales = sum(sales_dollars_L52wk, na.rm = TRUE),
    Avg_African_American_Pop = mean(AfricanAmerican, na.rm = TRUE),
    Avg_Hispanic_Pop = mean(Hispanic, na.rm = TRUE),
    Avg_Asian_Pop = mean(Asian, na.rm = TRUE)
  )

# Visualization
ggplot(demographic_summary, aes(x = Avg_African_American_Pop, y = Total_Sales)) +
  geom_point(aes(color = Avg_Hispanic_Pop, size = Avg_Asian_Pop), alpha = 0.8) +
  scale_color_gradient(low = "yellow", high = "red") +
  labs(
    title = "Demographics and Sales in Price Zones",
    x = "African American Population (%)",
    y = "Total Sales (L52 Weeks)",
    color = "Hispanic Population (%)",
    size = "Asian Population (%)"
  ) +
  theme_minimal()


# Summarize by Price Zone with Average Retail Price
price_sensitivity <- data %>%
  group_by(PriceZone) %>%
  summarise(
    Avg_Retail_Price = mean(Retail, na.rm = TRUE),
    Total_Sales = sum(sales_dollars_L52wk, na.rm = TRUE)
  )

# Scatter Plot of Price vs. Sales
ggplot(price_sensitivity, aes(x = Avg_Retail_Price, y = Total_Sales)) +
  geom_point(color = "blue", size = 4, alpha = 0.8) +
  geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +
  labs(
    title = "Price Sensitivity Analysis",
    x = "Average Retail Price ($)",
    y = "Total Sales (L52 Weeks)"
  ) +
  theme_minimal()






# Group by Price Zone and Time
time_trend <- data %>%
  group_by(PriceZone, Open_Date) %>%
  summarise(Total_Sales = sum(sales_dollars_L52wk, na.rm = TRUE))

# Line Plot of Sales Over Time
ggplot(time_trend, aes(x = Open_Date, y = Total_Sales, color = PriceZone)) +
  geom_line(size = 1, alpha = 0.8) +
  labs(
    title = "Sales Trends Over Time by Price Zone",
    x = "Date",
    y = "Total Sales (L52 Weeks)",
    color = "Price Zone"
  ) +
  theme_minimal()






# Rank Price Zones
ranked_zones <- price_zone_african_american %>%
  mutate(Sales_Rank = rank(-Total_Sales),
         Population_Rank = rank(-Avg_African_American_Pop))

# Table of Top Zones
top_zones <- ranked_zones %>%
  arrange(Sales_Rank) %>%
  head(10)

print(top_zones)





# Compare clusters based on sales and demographics
ggplot(price_zone_african_american, aes(x = Cluster, y = Total_Sales, fill = Cluster)) +
  geom_boxplot() +
  labs(
    title = "Sales Distribution Across Clusters",
    x = "Cluster",
    y = "Total Sales (L52 Weeks)",
    fill = "Cluster"
  ) +
  theme_minimal()








# Load necessary library
library(ggplot2)

# Example dataset (replace with your actual dataset)
data <- data.frame(
  Cluster = c(1, 2, 2, 3, 3, 1, 2, 3),
  Total_Sales = c(1000000, 5000000, 7000000, 15000000, 20000000, 1200000, 8000000, 18000000)
)

# Rename Cluster labels to meaningful names
data$Cluster <- factor(data$Cluster, 
                       levels = c(1, 2, 3), 
                       labels = c("Low Sales Zones", "Moderate Sales Zones", "High Sales Zones"))

# Create the boxplot
ggplot(data, aes(x = Cluster, y = Total_Sales, fill = Cluster)) +
  geom_boxplot(outlier.colour = "black", outlier.shape = 16, outlier.size = 2, alpha = 0.7) +
  labs(
    title = "Sales Distribution Across Clusters",
    x = "Sales Zones (Cluster)",
    y = "Total Sales (L52 Weeks)",
    fill = "Cluster"
  ) +
  scale_fill_manual(
    values = c("Low Sales Zones" = "red", 
               "Moderate Sales Zones" = "green", 
               "High Sales Zones" = "blue")
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 12)
  )






# Load necessary library
library(ggplot2)

# Example dataset (replace with your actual dataset)
data <- data.frame(
  Cluster = c(1, 2, 2, 3, 3, 1, 2, 3),
  Total_Sales = c(1000000, 5000000, 7000000, 15000000, 20000000, 1200000, 8000000, 18000000)
)

# Rename Cluster labels to reflect specific characteristics
data$Cluster <- factor(data$Cluster, 
                       levels = c(1, 2, 3), 
                       labels = c("Low Sales - Low Demographics", 
                                  "Moderate Sales - Mixed Demographics", 
                                  "High Sales - High Demographics"))

# Create the boxplot with enhanced labels and meaningful insights
ggplot(data, aes(x = Cluster, y = Total_Sales, fill = Cluster)) +
  geom_boxplot(outlier.colour = "black", outlier.shape = 16, outlier.size = 2, alpha = 0.7) +
  labs(
    title = "Sales Distribution Across Clusters",
    x = "Cluster Characteristics",
    y = "Total Sales (L52 Weeks)",
    fill = "Cluster"
  ) +
  scale_fill_manual(
    values = c("Low Sales - Low Demographics" = "lightblue", 
               "Moderate Sales - Mixed Demographics" = "orange", 
               "High Sales - High Demographics" = "darkgreen")
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 12)
  )










# Bar chart for top zones with population overlay
ggplot(top_zones, aes(x = reorder(PriceZone, Total_Sales), y = Total_Sales, fill = Avg_African_American_Pop)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  geom_text(aes(label = round(Total_Sales / 1e9, 1)), vjust = -0.5) +
  scale_fill_gradient(low = "yellow", high = "red") +
  labs(
    title = "Top 10 Price Zones by Total Sales",
    x = "Price Zone",
    y = "Total Sales (Billion $)",
    fill = "African American Population (%)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




# Identify underperforming zones
underperforming_zones <- price_zone_african_american %>%
  filter(Total_Sales < median(Total_Sales), Avg_African_American_Pop > median(Avg_African_American_Pop)) %>%
  arrange(desc(Avg_African_American_Pop))

print(underperforming_zones)



# Heatmap for demographic impact on sales
heatmap_data <- demographic_summary %>%
  select(-PriceZone) %>%
  cor()

corrplot(heatmap_data, method = "color", type = "upper", tl.col = "black", tl.srt = 45)














#step 6E

# Focus on top 15 Price Zones by Total Sales
top_price_zones <- price_zone_summary %>%
  arrange(desc(Total_Sales)) %>%
  head(15)

# Simplified bar and line chart
ggplot(top_price_zones, aes(x = reorder(PriceZone, Total_Sales))) +
  geom_bar(aes(y = Total_Sales, fill = Total_Sales), stat = "identity", alpha = 0.8) +
  geom_line(
    aes(y = Avg_Median_Income * max(Total_Sales) / max(Avg_Median_Income), group = 1),
    color = "blue", size = 1.2
  ) +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  scale_y_continuous(
    name = "Total Sales (L52 Weeks)",
    sec.axis = sec_axis(~ . / max(top_price_zones$Total_Sales) * max(top_price_zones$Avg_Median_Income), 
                        name = "Average Median Income")
  ) +
  labs(
    title = "Top 15 Price Zones by Total Sales and Median Income",
    x = "Price Zone",
    fill = "Total Sales"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
  )





# step 6F
install.packages("patchwork")
# Separate charts for Sales and Median Income
library(patchwork)

sales_chart <- ggplot(price_zone_summary, aes(x = reorder(PriceZone, Total_Sales), y = Total_Sales)) +
  geom_bar(stat = "identity", fill = "steelblue", alpha = 0.8) +
  labs(
    title = "Total Sales by Price Zone",
    x = "Price Zone",
    y = "Total Sales (L52 Weeks)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10))

income_chart <- ggplot(price_zone_summary, aes(x = reorder(PriceZone, Avg_Median_Income), y = Avg_Median_Income)) +
  geom_bar(stat = "identity", fill = "darkorange", alpha = 0.8) +
  labs(
    title = "Median Income by Price Zone",
    x = "Price Zone",
    y = "Median Income"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10))

# Combine both charts
sales_chart / income_chart










#step 6g 
# Summarize Hispanic population and sales by Price Zone
price_zone_hispanic <- data %>%
  group_by(PriceZone) %>%
  summarise(
    Total_Sales = sum(sales_dollars_L52wk, na.rm = TRUE),
    Avg_Hispanic_Pop = mean(Hispanic, na.rm = TRUE)
  ) %>%
  arrange(desc(Total_Sales))  # Sort by Total Sales

# Create a bar chart for Total Sales with a line overlay for Hispanic Population
ggplot(price_zone_hispanic, aes(x = reorder(PriceZone, Total_Sales))) +
  geom_bar(aes(y = Total_Sales, fill = Total_Sales), stat = "identity", alpha = 0.8) +
  geom_line(
    aes(y = Avg_Hispanic_Pop * max(Total_Sales), group = 1), 
    color = "blue", size = 1.2
  ) +
  scale_fill_gradient(low = "yellow", high = "red") +
  scale_y_continuous(
    name = "Total Sales (L52 Weeks)",
    sec.axis = sec_axis(~ . / max(price_zone_hispanic$Total_Sales) * 100, name = "Hispanic Population (%)")
  ) +
  labs(
    title = "Sales and Hispanic Population by Price Zone",
    x = "Price Zone",
    fill = "Total Sales"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
  )












# step 6h
# Summarize Hispanic population and sales by Price Zone
price_zone_hispanic <- data %>%
  group_by(PriceZone) %>%
  summarise(
    Total_Sales = sum(sales_dollars_L52wk, na.rm = TRUE),
    Avg_Hispanic_Pop = mean(Hispanic, na.rm = TRUE)
  ) %>%
  arrange(desc(Total_Sales))  # Sort by Total Sales

# Create a bar chart for Total Sales with a line overlay for Hispanic Population
ggplot(price_zone_hispanic, aes(x = reorder(PriceZone, Total_Sales))) +
  geom_bar(aes(y = Total_Sales, fill = Total_Sales), stat = "identity", alpha = 0.8) +
  geom_line(
    aes(y = Avg_Hispanic_Pop * max(Total_Sales), group = 1), 
    color = "blue", size = 1.2
  ) +
  geom_point(
    aes(
      y = Avg_Hispanic_Pop * max(Total_Sales),
      color = ifelse(Avg_Hispanic_Pop > 0.3, "Highlight", "Normal")
    ),
    size = 3, alpha = 0.9
  ) +
  geom_text(
    aes(
      y = Avg_Hispanic_Pop * max(Total_Sales),
      label = ifelse(Avg_Hispanic_Pop > 0.3, paste0(round(Avg_Hispanic_Pop * 100, 1), "%"), ""),
      fontface = ifelse(Avg_Hispanic_Pop > 0.3, "bold", "plain")
    ),
    vjust = -0.5, size = 4
  ) +
  scale_fill_gradient(low = "yellow", high = "red") +
  scale_color_manual(
    values = c("Highlight" = "red", "Normal" = "black"),
    guide = "none"
  ) +
  scale_y_continuous(
    name = "Total Sales (L52 Weeks)",
    sec.axis = sec_axis(~ . / max(price_zone_hispanic$Total_Sales) * 100, name = "Hispanic Population (%)")
  ) +
  labs(
    title = "Sales and Hispanic Population by Price Zone",
    x = "Price Zone",
    fill = "Total Sales"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
  )


#step 6 I 

# Filter to focus on top 15 Price Zones by Total Sales
top_price_zones <- price_zone_hispanic %>%
  arrange(desc(Total_Sales)) %>%
  head(15)

# Create a cleaner bar chart with a line overlay
ggplot(top_price_zones, aes(x = reorder(PriceZone, Total_Sales))) +
  geom_bar(aes(y = Total_Sales, fill = Total_Sales), stat = "identity", alpha = 0.8) +
  geom_line(
    aes(y = Avg_Hispanic_Pop * max(Total_Sales), group = 1), 
    color = "blue", size = 1.2
  ) +
  geom_point(
    aes(
      y = Avg_Hispanic_Pop * max(Total_Sales),
      color = ifelse(Avg_Hispanic_Pop > 0.3, "Highlight", "Normal")
    ),
    size = 3, alpha = 0.9
  ) +
  geom_text(
    aes(
      y = Avg_Hispanic_Pop * max(Total_Sales),
      label = ifelse(Avg_Hispanic_Pop > 0.3, paste0(round(Avg_Hispanic_Pop * 100, 1), "%"), "")
    ),
    vjust = -0.5, size = 4
  ) +
  scale_fill_gradient(low = "yellow", high = "red") +
  scale_color_manual(
    values = c("Highlight" = "red", "Normal" = "black"),
    guide = "none"
  ) +
  scale_y_continuous(
    name = "Total Sales (L52 Weeks)",
    sec.axis = sec_axis(~ . / max(top_price_zones$Total_Sales) * 100, name = "Hispanic Population (%)")
  ) +
  labs(
    title = "Top 15 Price Zones: Sales and Hispanic Population",
    x = "Price Zone",
    fill = "Total Sales"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
  )



# step 6j
# Summarize Hispanic population, sales, and households (population proxy) by Price Zone
price_zone_summary <- data %>%
  group_by(PriceZone) %>%
  summarise(
    Total_Sales = sum(sales_dollars_L52wk, na.rm = TRUE),
    Avg_Hispanic_Pop = mean(Hispanic, na.rm = TRUE),
    Avg_Households = mean(Households, na.rm = TRUE),  # Use Households as a proxy for population
    Avg_Stores = mean(store_number, na.rm = TRUE)     # Use store_number for average stores
  ) %>%
  arrange(desc(Total_Sales))  # Sort by Total Sales

# Focus on the top 15 Price Zones by Total Sales
top_price_zones <- price_zone_summary %>%
  head(15)

# Create a combined bar and line chart
ggplot(top_price_zones, aes(x = reorder(PriceZone, Total_Sales))) +
  geom_bar(aes(y = Total_Sales, fill = Total_Sales), stat = "identity", alpha = 0.8) +
  geom_line(
    aes(y = Avg_Hispanic_Pop * max(Total_Sales), group = 1), 
    color = "blue", size = 1.2
  ) +
  geom_point(
    aes(
      y = Avg_Hispanic_Pop * max(Total_Sales),
      color = ifelse(Avg_Hispanic_Pop > 0.3, "Highlight", "Normal")
    ),
    size = 3, alpha = 0.9
  ) +
  geom_text(
    aes(
      y = Avg_Hispanic_Pop * max(Total_Sales),
      label = ifelse(Avg_Hispanic_Pop > 0.3, paste0(round(Avg_Hispanic_Pop * 100, 1), "%"), "")
    ),
    vjust = -0.5, size = 4
  ) +
  scale_fill_gradient(low = "yellow", high = "red") +
  scale_color_manual(
    values = c("Highlight" = "red", "Normal" = "black"),
    guide = "none"
  ) +
  scale_y_continuous(
    name = "Total Sales (L52 Weeks)",
    sec.axis = sec_axis(~ . / max(top_price_zones$Total_Sales) * 100, name = "Hispanic Population (%)")
  ) +
  labs(
    title = "Top 15 Price Zones: Sales, Hispanic Population, and Averages",
    subtitle = "Includes Total Sales, Avg. Hispanic Population (%), Avg. Households, and Avg. Stores",
    x = "Price Zone",
    fill = "Total Sales"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5)
  )

# Optionally, print average values for households and stores
summary_averages <- price_zone_summary %>%
  summarise(
    Avg_Total_Households = mean(Avg_Households, na.rm = TRUE),
    Avg_Total_Stores = mean(Avg_Stores, na.rm = TRUE)
  )
print(summary_averages)



#6k 
# Segment Price Zones into High, Medium, and Low Sales
price_zone_summary <- price_zone_summary %>%
  mutate(Sales_Segment = case_when(
    Total_Sales > quantile(Total_Sales, 0.75) ~ "High Sales",
    Total_Sales > quantile(Total_Sales, 0.25) ~ "Medium Sales",
    TRUE ~ "Low Sales"
  ))

# Summarize metrics by Sales Segment
sales_segment_summary <- price_zone_summary %>%
  group_by(Sales_Segment) %>%
  summarise(
    Avg_Total_Sales = mean(Total_Sales, na.rm = TRUE),
    Avg_Hispanic_Pop = mean(Avg_Hispanic_Pop, na.rm = TRUE),
    Avg_Households = mean(Avg_Households, na.rm = TRUE),
    Avg_Stores = mean(Avg_Stores, na.rm = TRUE)
  )

# Visualize the summary
ggplot(sales_segment_summary, aes(x = Sales_Segment)) +
  geom_bar(aes(y = Avg_Total_Sales, fill = Sales_Segment), stat = "identity", alpha = 0.8) +
  geom_text(aes(y = Avg_Total_Sales, label = round(Avg_Total_Sales, 0)), vjust = -0.5, size = 5) +
  labs(
    title = "Metrics by Sales Segment",
    y = "Average Total Sales (L52 Weeks)",
    x = "Sales Segment",
    fill = "Sales Segment"
  ) +
  theme_minimal()


#6K
# Create a subset of numeric columns
correlation_data <- price_zone_summary %>%
  select(Total_Sales, Avg_Hispanic_Pop, Avg_Households, Avg_Stores)

# Calculate correlations
cor_matrix <- cor(correlation_data, use = "complete.obs")

# Visualize correlations
corrplot(cor_matrix, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 45, 
         title = "Correlation Heatmap", mar = c(0, 0, 1, 0))




# Install and Load Required Libraries
install.packages(c("dplyr", "ggplot2", "ggrepel"))
library(dplyr)
library(ggplot2)
library(ggrepel)

# Prepare Data (Group by PriceZone and Summarize)
price_zone_summary <- data %>%
  group_by(PriceZone) %>%
  summarise(
    Total_Sales = sum(sales_dollars_L52wk, na.rm = TRUE),
    Avg_Hispanic_Pop = mean(Hispanic, na.rm = TRUE),
    Avg_Stores = mean(store_number, na.rm = TRUE)
  )

# Calculate Correlation
correlation <- cor(price_zone_summary$Avg_Hispanic_Pop, price_zone_summary$Total_Sales, use = "complete.obs")

# Create the Visualization
ggplot(price_zone_summary, aes(x = Avg_Hispanic_Pop, y = Total_Sales)) +
  geom_point(aes(size = Avg_Stores, color = Total_Sales), alpha = 0.8) +
  geom_smooth(method = "lm", se = FALSE, color = "blue", linetype = "dashed") +  # Trend Line
  geom_text_repel(aes(label = ifelse(Total_Sales > quantile(Total_Sales, 0.95), PriceZone, "")),
                  size = 3) +  # Annotate Outliers
  annotate("text", x = 0.5, y = max(price_zone_summary$Total_Sales) * 0.8,
           label = paste("Correlation:", round(correlation, 2)), size = 5, hjust = 0) +  # Correlation Label
  scale_color_gradient(low = "yellow", high = "red") +
  labs(
    title = "Hispanic Population vs. Total Sales with Insights",
    x = "Average Hispanic Population (%)",
    y = "Total Sales (L52 Weeks)",
    size = "Average Stores",
    color = "Total Sales"
  ) +
  theme_minimal()










# Summarize Hispanic population and sales by state
state_hispanic <- data %>%
  group_by(state) %>%
  summarise(
    Total_Sales = sum(sales_dollars_L52wk, na.rm = TRUE),
    Avg_Hispanic_Pop = mean(Hispanic, na.rm = TRUE)
  )

# Filter states with significant data (optional, based on data spread)
state_hispanic <- state_hispanic %>% filter(Total_Sales > 0, Avg_Hispanic_Pop > 0)

# Create a scatter plot with clearer visibility
ggplot(state_hispanic, aes(x = reorder(state, -Total_Sales), y = Avg_Hispanic_Pop, size = Total_Sales, color = Total_Sales)) +
  geom_point(alpha = 0.8) +
  scale_color_gradient(low = "lightyellow", high = "red") +
  labs(
    title = "Sales vs. Hispanic Population by State",
    x = "State",
    y = "Hispanic Population (%)",
    size = "Total Sales",
    color = "Total Sales"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
  )

























# Step 6: Exploratory Visualizations
# (1) Distribution of Numeric Variables
data %>%
  select(where(is.numeric)) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value") %>%
  ggplot(aes(x = value)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "black", alpha = 0.7) +
  facet_wrap(~ variable, scales = "free_x", ncol = 3) +
  labs(
    title = "Distribution of Numeric Variables with Outliers",
    x = "Value",
    y = "Frequency"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 10, face = "bold"),
    axis.title = element_text(size = 12),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
  )




# Step 7: Enhanced Correlation Analysis for Numeric Variables, Correlation Plot with Enhanced Aesthetics
# Load the corrplot library
library(corrplot)

# Define `numeric_data` by selecting only numeric columns from `data`
numeric_data <- data %>% select(where(is.numeric))
library(corrplot)

# Calculate correlation matrix with complete observations
cor_matrix <- cor(numeric_data, use = "complete.obs")

# Set non-significant correlations (absolute value < 0.2) to NA for a cleaner plot
cor_matrix[abs(cor_matrix) < 0.2] <- NA

# Generate enhanced correlation plot
corrplot(
  cor_matrix,
  method = "color",
  type = "upper",
  col = colorRampPalette(c("#4575b4", "#f7f7f7", "#d73027"))(200),  # Blue-White-Red palette
  na.label = " ",                # Remove labels for non-significant correlations
  title = "Significant Correlations of Numeric Variables",
  tl.col = "black",              # Black text for labels
  tl.cex = 0.9,                  # Adjust label size for readability
  tl.srt = 45,                   # Rotate labels for better alignment
  addCoef.col = "black",         # Show correlation values in black for clarity
  number.cex = 0.8,              # Increase correlation coefficient font size
  mar = c(0, 0, 1, 0)            # Minimal margin for maximum screen usage
)

library(corrplot)
library(dplyr)

# Define `numeric_data` by selecting only numeric columns from `data`, excluding 'vintage'
numeric_data <- data %>% select(where(is.numeric), -vintage)

# Calculate correlation matrix with complete observations
cor_matrix <- cor(numeric_data, use = "complete.obs")

# Generate a correlation plot with purple and fluorescent green color scheme
corrplot(
  cor_matrix,
  method = "color",             # Color-filled cells
  type = "upper",
  col = colorRampPalette(c("#7A00FF", "#FFFFFF", "#39FF14"))(200),  # Purple to White to Fluorescent Green gradient
  addCoef.col = "black",        # Show correlation values in black for clarity
  number.cex = 0.7,             # Font size for correlation coefficients
  tl.col = "black",             # Black text for labels
  tl.cex = 0.8,                 # Adjust label size for readability
  tl.srt = 45,                  # Rotate labels for better alignment
  title = "Enhanced Correlation Map with Purple and Green Scheme",
  mar = c(0, 0, 2, 0)           # Increase top margin for title visibility
)









library(corrplot)
library(RColorBrewer)

# Define `numeric_data` by selecting only numeric columns from `data`
numeric_data <- data %>% select(where(is.numeric))

# Calculate correlation matrix with complete observations
cor_matrix <- cor(numeric_data, use = "complete.obs")

# Set non-significant correlations (absolute value < 0.2) to NA for a cleaner plot
cor_matrix[abs(cor_matrix) < 0.2] <- NA

# Generate enhanced correlation plot with a fluorescent color palette
corrplot(
  cor_matrix,
  method = "color",
  type = "upper",
  col = colorRampPalette(c("#39FF14", "#0FF", "#7A00FF", "#8A2BE2"))(200),  # Neon Green, Aqua, Purple, and Blue Violet
  na.label = " ",                # Remove labels for non-significant correlations
  title = "Significant Correlations of Numeric Variables",
  tl.col = "black",              # Black text for labels
  tl.cex = 0.9,                  # Adjust label size for readability
  tl.srt = 45,                   # Rotate labels for better alignment
  addCoef.col = "black",         # Show correlation values in black for clarity
  number.cex = 0.8,              # Font size for correlation coefficients
  mar = c(0, 0, 2, 0)            # Slightly increase top margin for title visibility
)



# Step 8: Categorical Data Analysis - Focused and Insightful Visualization
# Filter out high-cardinality categorical columns
filtered_data <- data %>%
  select(category, package_type, PriceZone, state, Sales_Bucket, Store_Size, TequilaOver65, TequilaUnder65)

# Set a limit for the number of top categories to display per variable
top_n <- 5

# Plot the frequency of top categories for each selected variable
filtered_data %>%
  gather(key = "variable", value = "value") %>%
  group_by(variable, value) %>%
  summarise(count = n(), .groups = 'drop') %>%
  arrange(variable, desc(count)) %>%
  group_by(variable) %>%
  slice_max(count, n = top_n) %>%  # Keep only the top `n` categories per variable
  ggplot(aes(x = reorder(value, -count), y = count)) +
  geom_bar(stat = "identity", fill = "steelblue", color = "black", alpha = 0.7, width = 0.7) +  # Adjust bar width
  facet_wrap(~ variable, scales = "free", ncol = 2) +  # Arrange in two columns for readability
  labs(
    title = "Frequency of Top Categories in Selected Categorical Variables",
    x = "Category",
    y = "Frequency"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),  # Adjust font size and angle
    strip.text = element_text(face = "bold", size = 10),  # Bold facet titles for clarity
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)  # Center and bold the title
  )

# Step 9: Enhanced Time Series Analysis of Sales with Monthly Aggregation and Outlier Handling
# Ensure 'Open_Date' is in Date format
if (!inherits(data$Open_Date, "Date")) {
  data$Open_Date <- as.Date(data$Open_Date)
}

# Select a subset of top items by total sales to reduce clutter
top_items <- data %>%
  group_by(item_name) %>%
  summarise(total_sales = sum(Normalized_Sales_L52W, na.rm = TRUE)) %>%
  arrange(desc(total_sales)) %>%
  slice_max(order_by = total_sales, n = 5) %>%
  pull(item_name)

# Convert dates to yearly
data %>%
  filter(item_name %in% top_items, !is.na(Open_Date)) %>%
  mutate(year = year(Open_Date)) %>%  # Extract year
  group_by(year, item_name) %>%
  summarise(yearly_sales = sum(Normalized_Sales_L52W, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = year, y = yearly_sales, color = item_name)) +
  geom_line(size = 1.2) +  # Thicker lines for visibility
  geom_point(size = 1.5) +  # Add points for yearly sales values
  geom_text(
    aes(label = ifelse(yearly_sales == max(yearly_sales), round(yearly_sales, 0), "")),
    vjust = -0.5, size = 3  # Label peak sales points
  ) +
  labs(
    title = "Yearly Sales Trends for Top Items",
    x = "Year",
    y = "Total Sales",
    color = "Item Name"
  ) +
  scale_color_brewer(palette = "Set1") +  # Use distinct colors
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12),
    legend.position = "bottom"
  )



####step 10 :
# Check unique values to understand the structure
unique(data$`HH_Income>100K`)
# Ensure HH_Income>100K is a binary variable (0 and 1) and convert it to a factor
data <- data %>%
  mutate(`HH_Income>100K` = ifelse(`HH_Income>100K` > 0.5, "> 100K", "<= 100K"))

# Plot after converting HH_Income>100K to a factor with two levels
data %>%
  filter(item_name %in% top_items) %>%
  group_by(`HH_Income>100K`, item_name) %>%
  summarise(avg_sales = mean(Normalized_Sales_L52W, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = reorder(item_name, -avg_sales), y = avg_sales, fill = `HH_Income>100K`)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), color = "black") +
  geom_text(aes(label = round(avg_sales, 2)), 
            position = position_dodge(width = 0.8), 
            vjust = -0.5, 
            size = 3.5, 
            color = "black") +  # Adding labels on top of bars
  labs(
    title = "Average Sales by Household Income Level for Top Tequila Items",
    subtitle = "Comparison of top 3 items by average sales within each income level",
    x = "Item Name",
    y = "Average Sales",
    fill = "Household Income Level"
  ) +
  scale_fill_manual(values = c("<= 100K" = "skyblue", "> 100K" = "orange")) +  # Distinct colors for each group
  facet_wrap(~ `HH_Income>100K`, scales = "free_y") +  # Facet by income level
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray40"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),  # Improve readability of item names
    axis.text.y = element_text(size = 10),
    legend.position = "none"  # Remove legend since facets make it clear
  )


# Step 11: Investigate Sales Variability by Store Number
# Visualization of Sales Variability by Store Number with Focus on Extremes
# Select the top 10 and bottom 10 stores by sales variability
extreme_stores <- data %>%
  group_by(store_number) %>%
  summarise(sales_sd = sd(Normalized_Sales_L52W, na.rm = TRUE)) %>%
  arrange(desc(sales_sd)) %>%
  slice(c(1:10, (n() - 9):n()))  # Select top 10 and bottom 10

# Plot 
extreme_stores %>%
  ggplot(aes(x = reorder(store_number, -sales_sd), y = sales_sd)) +
  geom_col(aes(fill = ifelse(sales_sd > quantile(sales_sd, 0.9), "High", "Normal")),
           color = "black", width = 0.8) +
  geom_text(aes(label = round(sales_sd, 2)), vjust = -0.3, size = 3.5) +  # Display values for context
  scale_fill_manual(values = c("High" = "orange", "Normal" = "lightyellow"), name = "Anomaly") +
  labs(
    title = "Top and Bottom 10 Stores by Sales Variability",
    subtitle = "Highlighting stores with exceptionally high and low sales variability",
    x = "Store Number",
    y = "Sales Variability (Standard Deviation)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top"
  )

 #------------------------------------------------------------------

#Step 12 # Boxplot to show distribution of sales by household income level
data %>%
  mutate(Income_Level = ifelse(as.numeric(as.character(`HH_Income>100K`)) > 0.5, "> 100K", "<= 100K")) %>%
  ggplot(aes(x = Income_Level, y = Normalized_Sales_L52W, fill = Income_Level)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 1) +
  labs(
    title = "Distribution of Sales by Household Income Level",
    x = "Household Income Level",
    y = "Sales (Normalized Last 52 Weeks)"
  ) +
  scale_fill_manual(values = c("<= 100K" = "skyblue", "> 100K" = "orange")) +
  theme_minimal() +
  theme(legend.position = "none")


#Step 13 # Scatter plot of Median Household Income vs. Average Sales
data %>%
  mutate(Median_HH_Income = as.numeric(as.character(Median_HH_Income))) %>%
  group_by(Median_HH_Income) %>%
  summarise(avg_sales = mean(Normalized_Sales_L52W, na.rm = TRUE)) %>%
  ggplot(aes(x = Median_HH_Income, y = avg_sales)) +
  geom_point(color = "blue", size = 2) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(
    title = "Median Household Income vs. Average Sales",
    x = "Median Household Income",
    y = "Average Sales"
  ) +
  theme_minimal()

data %>%
  filter(!is.na(HaveBA) & !is.na(Normalized_Sales_L52W)) %>%
  mutate(Log_Sales = log10(Normalized_Sales_L52W + 1)) %>%
  ggplot(aes(x = HaveBA, y = Log_Sales)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_smooth(method = "gam", formula = y ~ s(x), color = "red", se = FALSE) +  # Using GAM
  labs(
    title = "Relationship Between Education Levels (HaveBA) and Sales",
    x = "Proportion of Population with a Bachelor's Degree",
    y = "Log-Transformed Sales"
  ) +
  theme_minimal()


# Aggregating sales data separately for each age group
data %>%
  summarise(
    Under65_Sales = sum(Normalized_Sales_L52W[TequilaUnder65 == 1], na.rm = TRUE),
    Over65_Sales = sum(Normalized_Sales_L52W[TequilaOver65 == 1], na.rm = TRUE)
  ) %>%
  # Transforming data for ggplot
  pivot_longer(
    cols = c(Under65_Sales, Over65_Sales), 
    names_to = "Age_Group", 
    values_to = "Total_Sales"
  ) %>%
  # Creating the bar chart
  ggplot(aes(x = Age_Group, y = Total_Sales, fill = Age_Group)) +
  geom_bar(stat = "identity", color = "black", width = 0.7) +
  labs(
    title = "Sales Distribution Across Age Groups",
    x = "Age Group",
    y = "Total Sales"
  ) +
  scale_fill_manual(values = c("Under65_Sales" = "skyblue", "Over65_Sales" = "orange")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    legend.position = "none"
  )


summary(data$Open_Date)
data <- data %>% filter(Open_Date >= as.Date("2000-01-01"))



