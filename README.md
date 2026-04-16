# -r-data-wrangling-tidyverse-ggplot2
This repository serves as a portfolio of my statistical work in R. While the datasets are general, the methodologies (specifically data wrangling with dplyr and complex visualization with ggplot2) form the analytical core of my transition into Bioinformatics and Computational Biology.

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r libraries_data, message=FALSE, warning=FALSE}
# Downloaded libraries 
library(readxl)
library(tidyverse)
library(dplyr)
library(scales)
library(knitr)
library(kableExtra)
library(ggplot2)
library(gridExtra)

# Load data set
hd <- read_excel("Housing_Data_Regression.xlsx")

# Pre-compute integer room counts from the log-transformed rooms variable to the original integer count
hd$rooms_int <- round(exp(hd$rooms))
```
# Introduction
## Analytics of the Indian Housing Market 
The Indian real estate market is one of the largest in the world; it contributes to approximately 7-8% of the country's GDP, and it employs millions of workers in fields like construction, brokerage, and property management (Kumar & Singh, 2022). The housing prices in India are shaped by a wide range of factors that include geographic location, structural characteristics (i.e floor area and number of rooms), and amenities (i.e. a pool, yard, or garage). In recent years, rapid urbanization has made accurate housing price prediction and market analysis more important for buyers, sellers, developers, and policymakers. Data analytics provides a systematic, evidence-based approach to understanding these markets by applying statistical methods to large data sets, uncovering hidden patterns, identifying price drivers, and generating insights that would be impossible to detect through casual observation (Malpezzi, 2003). The application of descriptive statistics, data visualization, and hypothesis testing to housing data allows real estate professionals to make better-informed decisions and serve their clients more effectively.   

## Descriptive and Inferential Statistics 
Descriptive statistics refers to a branch of statistics that summarizes and organizes a data set so the main features can be understood such as the mean (average value), median (middle value), standard deviation (measure of spread around the mean), the range (the difference between the maximum and minimum). These measures allow an analyst to characterize the central tendency and variability of any numerical variable (Bluman, 2017). Meanwhile, inferential statistics uses sample results to draw conclusions, make predictions, or test hypotheses about a larger population. Inferential techniques include confidence intervals, which estimate a population parameter within a margin of error, and hypothesis tests, which determine whether observed patterns in sample data are statistically meaningful or could have arisen by chance (Bluman, 2017). The main distinction between these two types of statistics is the scope: descriptive statistics summarizes what is in the data and inferential statistics uses the data to make broad claims about the world. 

## Data set Description 
The data set used in this analysis is the `Housing_Data_Regression.xlsx`. This data set contains the housing market data from India, and it consists of 2,337 observations across 16 variables that covers numerical features, including garage size, basement area, attic area, square meters, and sale price- and  categorical or binary features, including number of rooms, presence of a yard, pool, storage room, and guest room. Many continuous variables are log-transformed, a common pre-processing step in regression modeling that stabilizes variance and reduces the influence of extreme outliers (James et al., 2021). The data set offers a comprehensive cross-sectional snapshot of residential properties across Indian cities, making it well suited for both exploratory data analysis and formal statistical inference.

## Problem Identification 
The data set can answer several critical business questions that would improve company performance for a real estate company. The first question would be: which property characteristics are most strongly linked to higher sale prices? By identifying these drivers, agents can advise clients on which features or renovations command the greatest price premiums. The second question would be: how do prices vary by room count? Through segmenting the market by room size, it helps the company tailor marketing strategies and property valuations to distinct buyer profiles. The third question would be: does the presence of amenities such as a yard meaningfully differentiate price points in this market? This analysis focuses on price distribution across room categories and formal hypothesis testing of average price benchmarks, with an additional goal of providing effective information for pricing strategy, inventory acquisition, and investor communications.

## Task 1: Descriptive Statistics of Numerical Variables

In this task, I use named objects to compute the mean, median, standard deviation, and range (max − min) for the five numerical variables: **garage**, **basementarea**, **atticarea**, **squaremeters**, and **price**. These named objects are then used to construct a 5×4 data frame, where the row names are the five variable names and the column names are the four statistics. The table is presented using `kable()` and `kableExtra` for professional formatting. No intermediate objects are printed, only the final formatted table is displayed.

```{r descriptivestats, message=FALSE, warning=FALSE}
# Mean of each variable
mean_vals <- c(mean(hd$garage,       na.rm = TRUE),
               mean(hd$basementarea, na.rm = TRUE),
               mean(hd$atticarea,    na.rm = TRUE),
               mean(hd$squaremeters, na.rm = TRUE),
               mean(hd$price,        na.rm = TRUE))

# Median of each variable
median_vals <- c(median(hd$garage,       na.rm = TRUE),
                 median(hd$basementarea, na.rm = TRUE),
                 median(hd$atticarea,    na.rm = TRUE),
                 median(hd$squaremeters, na.rm = TRUE),
                 median(hd$price,        na.rm = TRUE))

# Standard deviation of each variable
sd_vals <- c(sd(hd$garage,       na.rm = TRUE),
             sd(hd$basementarea, na.rm = TRUE),
             sd(hd$atticarea,    na.rm = TRUE),
             sd(hd$squaremeters, na.rm = TRUE),
             sd(hd$price,        na.rm = TRUE))

# Range of each variable
range_vals <- c(max(hd$garage,       na.rm = TRUE) - min(hd$garage,       na.rm = TRUE),
                max(hd$basementarea, na.rm = TRUE) - min(hd$basementarea, na.rm = TRUE),
                max(hd$atticarea,    na.rm = TRUE) - min(hd$atticarea,    na.rm = TRUE),
                max(hd$squaremeters, na.rm = TRUE) - min(hd$squaremeters, na.rm = TRUE),
                max(hd$price,        na.rm = TRUE) - min(hd$price,        na.rm = TRUE))

# Build of 5x4 data frame using the named objects above 
stats_df <- data.frame(
  Mean               = round(mean_vals,   2),
  Median             = round(median_vals, 2),
  Standard_Deviation = round(sd_vals,     2),
  Range              = round(range_vals,  2),
  row.names = c("garage", "basementarea", "atticarea", "squaremeters", "price")
)

# Present as formatted kable table 
kable(stats_df,
      caption   = "Table 1: Descriptive Statistics of Five Numerical Variables",
      col.names = c("Mean", "Median", "Std. Deviation", "Range"),
      align     = "rrrr") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                full_width = FALSE,
                position   = "center") %>%
  row_spec(0, bold = TRUE, background = "darkblue", color = "white")
```
**Observations:** The `price` variable shows the widest absolute range of approximately `r format(round(range_vals[5], 0), big.mark = ",")` rupees, reflecting the broad spectrum of properties in the data set—from affordable housing to high-value residences. The mean price, `r format(round(mean_vals[5], 0), big.mark = ",")` rupees, is very close to the median, `r format(round(median_vals[5], 0), big.mark = ",")` rupees, which indicates a roughly symmetric price distribution with only a slight right skew caused by a small number of luxury properties. The `garage` variable has a mean of `r round(mean_vals[1], 1)` and a range of `r round(range_vals[1], 0)` units, showing considerable variability in garage capacity across properties. The `basementarea`, `atticarea`, and `squaremeters` variables show small numeric values (all under 12) that are consistent with log-transformed measurements, which compress the scale and reduce the influence of extreme values on statistical estimates (James et al., 2021). These summary measures confirm meaningful variation across all five variables.

## Task 2: Box Plots and Histograms of Numerical Variables

```{r numericalvariables, message=FALSE, warning=FALSE, fig.width=10, fig.height=5}
# Horizontal box plot and histogram of square meters 
par(mfcol = c(2, 1), mai = c(0.7, 0.8, 0.5, 0.4))

boxplot(hd$squaremeters,
        horizontal = TRUE,
        col        = "lightblue",
        border     = "purple",
        main       = "Box Plot: Square Meters",
        xlab       = "Log(Square Meters)",
        las        = 1)

hist(hd$squaremeters,
     breaks = 40,
     col    = "lightblue",
     border = "purple",
     main   = "Histogram: Square Meters",
     xlab   = "Log(Square Meters)",
     ylab   = "Frequency",
     las    = 1)

# Horizontal box plot and histogram of basement area
par(mfcol = c(2, 1), mai = c(0.7, 0.8, 0.5, 0.4))

boxplot(hd$basementarea,
        horizontal = TRUE,
        col        = "lightgreen",
        border     = "darkgreen",
        main       = "Box Plot: Basement Area",
        xlab       = "Log(Basement Area)",
        las        = 1)

hist(hd$basementarea,
     breaks = 40,
     col    = "lightgreen",
     border = "darkgreen",
     main   = "Histogram: Basement Area",
     xlab   = "Log(Basement Area)",
     ylab   = "Frequency",
     las    = 1)

# Horizontal box plot and histogram of atticarea
par(mfcol = c(2, 1), mai = c(0.7, 0.8, 0.5, 0.4))

boxplot(hd$atticarea,
        horizontal = TRUE,
        col        = "lightyellow",
        border     = "darkorange",
        main       = "Box Plot: Attic Area",
        xlab       = "Log(Attic Area)",
        las        = 1)

hist(hd$atticarea,
     breaks = 40,
     col    = "lightyellow",
     border = "darkorange",
     main   = "Histogram: Attic Area",
     xlab   = "Log(Attic Area)",
     ylab   = "Frequency",
     las    = 1)

# Horizontal box plot and histogram of Price 
par(mfcol = c(2, 1), mai = c(0.7, 0.8, 0.5, 0.4))

boxplot(hd$price,
        horizontal = TRUE,
        col        = "lightpink",
        border     = "magenta",
        main       = "Box Plot: House Price (Rupees)",
        xlab       = "Price (Rupees)",
        las        = 1)

hist(hd$price,
     breaks = 50,
     col    = "lightpink",
     border = "magenta",
     main   = "Histogram: House Price (Rupees)",
     xlab   = "Price (Rupees)",
     ylab   = "Frequency",
     las    = 1)
```
**Square Meters:** The box plot shows a symmetric distribution with a median near `r round(median(hd$squaremeters), 2)` on the log scale, a compact IQR, and very few outliers. The histogram confirms a roughly bell-shaped (normal) distribution, suggesting that property sizes are symmetrically distributed around a central value—a desirable property for regression modeling, as it reduces coefficient estimation bias (James et al., 2021).

**Basement Area:** The `basementarea` box plot is nearly symmetric with a median of `r round(median(hd$basementarea), 2)` on the log scale and only a few extreme high-end values. The histogram reveals a slight right skew, revealing that while most properties have moderate basement areas, a small number have notably large basements. Properties with large basements may represent a premium category commanding higher sale prices.

**Attic Area:** The `atticarea` distribution closely mirrors `basementarea`, presenting a roughly symmetric box plot with a minor right skew in the histogram. The median is `r round(median(hd$atticarea), 2)` on the log scale. Properties with unusually small attic areas may have reduced overall utility, which can negatively affect resale value. This is a key detail agents should communicate clearly to sellers considering improvements.

**Price:** The `price` box plot is relatively symmetric around a median of approximately `r format(round(median(hd$price), 0), big.mark = ",")` rupees with several high-value outliers on the right tail. The histogram shows a slight right skew, which confirms that most properties cluster in the mid-price range with a small segment of premium properties creating an upper tail. This pattern is common in real estate markets where luxury properties form a distinct and smaller market segment (Malpezzi, 2003).

## Task 3: Bar Plots and Pie Charts of Categorical Variables 

```{r categoricalvariables, message=FALSE, warning=FALSE, fig.width=13, fig.height=5}
# hasYard: Frequency table (suppressed), bar plot, and pie chart 

# Create frequency table for hasYard
yard_table <- as.data.frame(table(hd$hasYard))
colnames(yard_table) <- c("hasYard", "Count")
yard_table$hasYard <- factor(yard_table$hasYard,
                              levels = c(0, 1),
                              labels = c("No Yard", "Has Yard"))
yard_table$Percentage <- round(yard_table$Count / sum(yard_table$Count) * 100, 1)

# Bar plot for hasYard
p1_yard <- ggplot(yard_table, aes(x = hasYard, y = Count, fill = hasYard)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = Count), vjust = -0.4, size = 4.5, fontface = "bold") +
  scale_fill_manual(values = c("No Yard" = "red", "Has Yard" = "darkgreen")) +
  labs(title = "Yard Presence Distribution",
       x     = "Yard Status",
       y     = "Number of Properties") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", hjust = 0.5))

# Pie chart for hasYard
p2_yard <- ggplot(yard_table, aes(x = "", y = Count, fill = hasYard)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(Percentage, "%")),
            position = position_stack(vjust = 0.5),
            size = 5, fontface = "bold", color = "white") +
  scale_fill_manual(values = c("No Yard" = "red", "Has Yard" = "darkgreen")) +
  labs(title = "Yard Status — Proportion",
       fill  = "Yard Status") +
  theme_void(base_size = 13) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

# Display side by side
grid.arrange(p1_yard, p2_yard, ncol = 2)

# rooms_int was created in the setup chunk via round(exp(hd$rooms))
hd$rooms_group <- cut(hd$rooms_int,
                       breaks = c(0, 5, 10, 20, 40, 60, 80, Inf),
                       labels = c("2-5", "6-10", "11-20", "21-40",
                                  "41-60", "61-80", "81+"),
                       right  = TRUE)

# Frequency table for rooms 
rooms_table <- as.data.frame(table(hd$rooms_group))
colnames(rooms_table) <- c("rooms", "Count")
rooms_table$Percentage <- round(rooms_table$Count / sum(rooms_table$Count) * 100, 1)

# Bar plot for rooms
p1_rooms <- ggplot(rooms_table, aes(x = rooms, y = Count, fill = rooms)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = Count), vjust = -0.4, size = 4, fontface = "bold") +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Distribution of Room Count (Grouped)",
       x     = "Number of Rooms (Group)",
       y     = "Number of Properties") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", hjust = 0.5))

# Pie chart for rooms
p2_rooms <- ggplot(rooms_table, aes(x = "", y = Count, fill = rooms)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(Percentage, "%")),
            position = position_stack(vjust = 0.5),
            size = 3.8, fontface = "bold", color = "white") +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Room Count — Proportion by Group",
       fill  = "Room Group") +
  theme_void(base_size = 13) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

# Display side by side
grid.arrange(p1_rooms, p2_rooms, ncol = 2)
```
**hasYard Observations:** The `hasYard` variable reveals a nearly equal split: `r yard_table$Count[yard_table$hasYard == "Has Yard"]` properties (`r yard_table$Percentage[yard_table$hasYard == "Has Yard"]`%) include a yard, while `r yard_table$Count[yard_table$hasYard == "No Yard"]` (`r yard_table$Percentage[yard_table$hasYard == "No Yard"]`%) do not. This near-even distribution suggests that yard presence is not a rare or exclusive feature in this market. This discovery may limit its standalone power as a price differentiator. However, for marketing purposes, properties with yards are likely to appeal to families and buyers who value outdoor space, thus making yard status a useful segmentation variable.

**Rooms Observations:** The `rooms` variable is log-transformed in the raw data set. The application of `exp()` and rounding recovers integer room counts from 2 to over 100, which were grouped into seven bins for readable display. The bar plot shows that the 21–40 room group is by far the most common category, representing `r rooms_table$Percentage[rooms_table$rooms == "21-40"]`% of all properties. The pie chart confirms this dominance. Extremely small properties (2–5 rooms) and extremely large ones (81+) are the least represented groups. For a real estate company, this distribution reveals that mid-to-large properties make up the core of the market inventory, and marketing and valuation resources should be concentrated in that segment.

## Task 4: Data Analysis of Average Price by Number of Rooms 

```{r tapply, message=FALSE, warning=FALSE, fig.width=10, fig.height=6}
# tapply(): mean price for each integer room count 
avg_price_by_rooms <- tapply(hd$price,
                              INDEX = hd$rooms_int,
                              FUN   = mean)

# Convert result to a data frame and sort by room count
avg_price_df <- data.frame(
  Rooms         = as.integer(names(avg_price_by_rooms)),
  Average_Price = round(as.numeric(avg_price_by_rooms), 0)
)
avg_price_df <- avg_price_df[order(avg_price_df$Rooms), ]

# Present first 20 room categories as a formatted kable table
kable(head(avg_price_df, 20),
      caption     = "Table 2: Average House Price by Room Count (First 20 Categories)",
      col.names   = c("Number of Rooms", "Average Price (Rupees)"),
      align       = "lr",
      row.names   = FALSE,
      format.args = list(big.mark = ",")) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                full_width = FALSE,
                position   = "center") %>%
  row_spec(0, bold = TRUE, background = "cyan", color = "black")

# Horizontal bar plot: Top 15 room counts by average price 
top15 <- avg_price_df[order(-avg_price_df$Average_Price), ][1:15, ]

ggplot(top15, aes(x    = reorder(as.factor(Rooms), Average_Price),
                   y   = Average_Price,
                   fill = Average_Price)) +
  geom_bar(stat = "identity", width = 0.65) +
  geom_text(aes(label = comma(Average_Price)),
            hjust = -0.08, size = 3.5, fontface = "bold") +
  coord_flip() +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  scale_y_continuous(labels = comma,
                     expand = expansion(mult = c(0, 0.18))) +
  labs(title = "Top 15 Room Counts by Average House Price",
       x     = "Number of Rooms",
       y     = "Average Price (Rupees)") +
  theme_minimal(base_size = 13) +
  theme(plot.title      = element_text(face = "bold", hjust = 0.5),
        legend.position = "none")
```

**Observations:** The `tapply()` function was used to compute the mean `price` for each unique integer room count, yielding `r nrow(avg_price_df)` unique room categories across the data set. 
**The house type with the highest average price is `r avg_price_df$Rooms[which.max(avg_price_df$Average_Price)]` rooms** with a mean price of `r comma(max(avg_price_df$Average_Price))` rupees. The horizontal bar plot illustrates that a subset of mid-to-large room counts commands the highest average prices; the properties in the 40–80 room range frequently appear among the top 15. While a general positive relationship exists between room count and average price, the relationship is not perfectly monotonic because some mid-range room counts rank among the highest, indicating that factors beyond room count alone (such as location, amenities, and property condition) also drive price (Kumar & Singh, 2022). The company should use room count as a key but not exclusive variable in its property valuation model.

## Task 5: Confidence Interval of Houses with Four Rooms 

```{r CI, message=FALSE, warning=FALSE}
# Filter: Subset for houses with exactly 4 rooms
rooms_4_subset <- hd %>% filter(rooms_int == 4)

# Compute sample statistics from the subset
n4    <- nrow(rooms_4_subset)
mean4 <- mean(rooms_4_subset$price, na.rm = TRUE)
sd4   <- sd(rooms_4_subset$price,   na.rm = TRUE)
se4   <- sd4 / sqrt(n4)

# 90% confidence interval using t-distribution (population sd unknown)
alpha  <- 0.10
t_crit <- qt(1 - alpha / 2, df = n4 - 1)
ci_low  <- mean4 - t_crit * se4
ci_high <- mean4 + t_crit * se4

# Results formatted as a kable table 
ci_df <- data.frame(
  Statistic = c("Sample Size (n)",
                "Sample Mean (Rupees)",
                "Sample Std. Deviation",
                "Standard Error (s / sqrt(n))",
                "t Critical Value (90% CI, df = n-1)",
                "Lower Bound — 90% CI",
                "Upper Bound — 90% CI"),
  Value = c(n4,
            round(mean4,   2),
            round(sd4,     2),
            round(se4,     2),
            round(t_crit,  4),
            round(ci_low,  2),
            round(ci_high, 2))
)

kable(ci_df,
      caption   = "Table 3: 90% Confidence Interval for Mean Price — 4-Room Houses",
      col.names = c("Statistic", "Value"),
      align     = "lr") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                full_width = FALSE,
                position   = "center") %>%
  row_spec(0, bold = TRUE, background = "navyblue", color = "white") %>%
  row_spec(c(6, 7), bold = TRUE, background = "lightblue")
```

**Observations:** After filtering, the subset contains **`r n4`** properties with exactly 4 rooms. The sample mean price for this group is approximately **`r comma(round(mean4, 0))` rupees**. Using the t-distribution at a 90% confidence level (because the population standard deviation is unknown), I am 90% confident that the true mean price for all 4-room houses in this market lies between **`r comma(round(ci_low, 0))` rupees** and **`r comma(round(ci_high, 0))` rupees**. The confidence interval is relatively narrow compared to the overall price range of the full data set, showcasing a reasonably precise estimate. This range gives the company a reliable, statistically grounded benchmark for pricing and appraising 4-room properties during client negotiations (Bluman, 2017).

## Task 6: Hypothesis Testing of Mean Price of 3-room Houses 

```{r hypothesisthreeroom, message=FALSE, warning=FALSE}
# Filter: subset for houses with exactly 3 rooms 
rooms_3_subset <- hd %>% filter(rooms_int == 3)

# Compute sample statistics
n3    <- nrow(rooms_3_subset)
mean3 <- mean(rooms_3_subset$price, na.rm = TRUE)

# Hypothesized and known population parameters
mu0    <- 500000  # Hypothesized population mean
sigma  <- 50000   # Known population standard deviation
alpha3 <- 0.05

# Step 3: Compute Z test statistic
se3 <- sigma / sqrt(n3)
z3  <- (mean3 - mu0) / se3

# Step 4: Two-tailed critical value and p-value
z_crit3 <- qnorm(1 - alpha3 / 2)
p_val3  <- 2 * (1 - pnorm(abs(z3)))
p_val3_display <- ifelse(p_val3 < 0.0001, "< 0.0001",
                         as.character(round(p_val3, 6)))

# Step 5: Decision
decision3 <- ifelse(abs(z3) > z_crit3, "Reject H0", "Fail to Reject H0")

# Build 5-step results table
ht6_df <- data.frame(
  Step = c("Step 1 - H0 (Null Hypothesis)",
           "Step 1 - H1 (Alternative Hypothesis)",
           "Step 2 - Significance Level (alpha)",
           "Step 3 - Sample Size (n)",
           "Step 3 - Sample Mean (x-bar)",
           "Step 3 - Hypothesized Mean (mu0)",
           "Step 3 - Population Std Dev (sigma)",
           "Step 3 - Standard Error (sigma / sqrt(n))",
           "Step 3 - Z Test Statistic",
           "Step 4 - Critical Values (+/- Z alpha/2)",
           "Step 4 - p-value",
           "Step 5 - Decision"),
  Value = c("mu = 500,000 rupees",
            "mu not equal to 500,000 rupees",
            as.character(alpha3),
            as.character(n3),
            comma(round(mean3, 2)),
            "500,000",
            "50,000",
            as.character(round(se3, 4)),
            as.character(round(z3, 4)),
            paste0("+/-", round(z_crit3, 4)),
            p_val3_display,
            decision3)
)

kable(ht6_df,
      caption   = "Table 4: Z-Test — Mean Price of 3-Room Houses (5-Step Process)",
      col.names = c("Step / Parameter", "Value"),
      align     = "ll") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                full_width = FALSE,
                position   = "center") %>%
  row_spec(0, bold = TRUE, background = "navyblue", color = "white") %>%
  row_spec(12, bold = TRUE,
           background = ifelse(decision3 == "Reject H0", "red", "lightblue"))
```

**5-Step Hypothesis Test Summary:**

1. **State the hypotheses:** H0: mu = 500,000 rupees (mean price of 3-room houses equals 500,000); H1: mu ≠ 500,000 rupees. This is a two-tailed test.
2. **Set the significance level:** alpha = 0.05.
3. **Compute the test statistic:** Using Z = (x-bar - mu0) / (sigma / sqrt(n)), with x-bar = `r comma(round(mean3, 0))` rupees, mu0 = 500,000, sigma = 50,000, and n = `r n3`. The standard error is `r round(se3, 2)` and Z = `r round(z3, 4)`.
4. **Determine the critical region:** For a two-tailed test at alpha = 0.05, critical values are +/-1.96. We reject H0 if |Z| > 1.96.
5. **Make a decision:** The computed Z statistic is **`r round(z3, 4)`** and the p-value is **`r p_val3_display`**. Since |Z| >> 1.96 and p-value < 0.05, we **`r decision3`**. There is overwhelming statistical evidence that the mean price of 3-room houses is not 500,000 rupees. The actual sample mean of `r comma(round(mean3, 0))` rupees is far above the hypothesized benchmark, further indicating that a population mean of 500,000 rupees severely underestimates market prices and should not be used in the company's pricing models (Bluman, 2017).

## Task 7: Hypothesis Testing of Mean House Price Greater than 4.5 Million Rupees 

```{r hypothesisfulldata, message=FALSE, warning=FALSE}
# Use full data set for this test 
n7    <- sum(!is.na(hd$price))
mean7 <- mean(hd$price, na.rm = TRUE)

# Hypothesized and known population parameters
mu0_7  <- 4500000  # Hypothesized population mean
sigma7 <- 50000    # Known population standard deviation
alpha7 <- 0.05

# Step 3: Compute Z test statistic
se7 <- sigma7 / sqrt(n7)
z7  <- (mean7 - mu0_7) / se7

# Step 4: Right-tailed critical value and p-value
z_crit7 <- qnorm(1 - alpha7)
p_val7  <- 1 - pnorm(z7)
p_val7_display <- ifelse(p_val7 < 0.0001, "< 0.0001",
                         as.character(round(p_val7, 6)))

# Step 5: Decision
decision7 <- ifelse(z7 > z_crit7, "Reject H0", "Fail to Reject H0")

# Build 5-step results table
ht7_df <- data.frame(
  Step = c("Step 1 - H0 (Null Hypothesis)",
           "Step 1 - H1 (Alternative Hypothesis)",
           "Step 2 - Significance Level (alpha)",
           "Step 3 - Sample Size (n)",
           "Step 3 - Sample Mean (x-bar)",
           "Step 3 - Hypothesized Mean (mu0)",
           "Step 3 - Population Std Dev (sigma)",
           "Step 3 - Standard Error (sigma / sqrt(n))",
           "Step 3 - Z Test Statistic",
           "Step 4 - Critical Value (Z alpha)",
           "Step 4 - p-value",
           "Step 5 - Decision"),
  Value = c("mu <= 4,500,000 rupees",
            "mu > 4,500,000 rupees",
            as.character(alpha7),
            as.character(n7),
            comma(round(mean7, 2)),
            "4,500,000",
            "50,000",
            as.character(round(se7, 4)),
            as.character(round(z7, 4)),
            as.character(round(z_crit7, 4)),
            p_val7_display,
            decision7)
)

kable(ht7_df,
      caption   = "Table 5: Z-Test — Mean House Price Greater than 4,500,000 Rupees (5-Step)",
      col.names = c("Step / Parameter", "Value"),
      align     = "ll") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                full_width = FALSE,
                position   = "center") %>%
  row_spec(0, bold = TRUE, background = "navyblue", color = "white") %>%
  row_spec(12, bold = TRUE,
           background = ifelse(decision7 == "Reject H0", "red", "lightblue"))
```

**5-Step Hypothesis Test Summary:**

1. **State the hypotheses:** H0: mu <= 4,500,000 rupees; H1: mu > 4,500,000 rupees. This is a right-tailed test.
2. **Set the significance level:** alpha = 0.05.
3. **Compute the test statistic:** Using Z = (x-bar - mu0) / (sigma / sqrt(n)), with x-bar = `r comma(round(mean7, 0))` rupees, mu0 = 4,500,000, sigma = 50,000, and n = `r n7`. The standard error is `r round(se7, 4)` and Z = `r round(z7, 4)`.
4. **Determine the critical region:** For a right-tailed test at alpha = 0.05, the critical value is Z = 1.645. We reject H0 if Z > 1.645.
5. **Make a decision:** The computed Z statistic is **`r round(z7, 4)`** and the p-value is **`r p_val7_display`**. Since Z >> 1.645 and p-value < 0.05, I **`r decision7`**. At the 5% significance level, there is strong statistical evidence that the mean house price in India exceeds 4,500,000 rupees. The company can confidently use this finding in investor presentations and market reports to establish that the typical property in this data set is valued well above the 4.5 million rupee threshold (Bluman, 2017).

# Conclusions

The analysis of the Indian housing market data set has revealed several important findings. The descriptive statistics in Task 1 established that house prices span an enormous range—from approximately `r format(round(min(hd$price), 0), big.mark = ",")` to nearly `r format(round(max(hd$price), 0), big.mark = ",")` rupees. However, the mean and median remain closely aligned near `r format(round(mean(hd$price), 0), big.mark = ",")` rupees. The central tendency confirms a roughly symmetric price distribution with only a slight right skew. The box plots and histograms in Task 2 reinforce the distribution because log-transformed variables `squaremeters`, `basementarea`, and `atticarea` all display near-symmetric distributions. Meanwhile, the `price` histogram confirms that the right skew is driven by a small number of high-value luxury properties. The categorical analysis in Task 3 shows that yard presence is nearly evenly distributed across the data set (approximately a 50/50 split), and that mid-to-large properties dominate the room count distribution. This result reflects a housing stock that serves a wide range of buyers. The `tapply()` analysis in Task 4 confirms a general positive relationship between room count and average price, with `r avg_price_df$Rooms[which.max(avg_price_df$Average_Price)]` -room properties commanding the highest mean price of `r comma(max(avg_price_df$Average_Price))` rupees, thus providing the company with concrete, data-driven guidance for property valuation and acquisition prioritization (Malpezzi, 2003).

The inferential analyses in Tasks 5 through 7 transformed these descriptive findings into formal statistical decision-making. The 90% confidence interval computed in Task 5 for 4-room house prices, ranging from `r comma(round(ci_low, 0))` to `r comma(round(ci_high, 0))` rupees, provides a precise and statistically grounded benchmark the company can use in appraisals and client negotiations. The hypothesis test in Task 6 rejected the claim that 3-room houses average 500,000 rupees. The actual sample mean of `r comma(round(mean3, 0))` rupees is far above this benchmark, exposing how dangerously misleading incorrect or outdated price assumptions can be for business strategy. The right-tailed test in Task 7 confirmed, at the 5% significance level, that the overall market mean price exceeds 4.5 million rupees. This confirmation allows for a statistically verified result that strengthens investor confidence and supports premium market positioning. Based on all findings, it is recommended that the company prioritize acquisition of mid-to-large room-count properties, use the statistically verified mean price of 4.5 million rupees as a floor benchmark in investor communications, and incorporate room count and square footage as primary variables in its internal pricing models. Throughout this project, new skills were developed in R programming, such as data wrangling with `dplyr` and `filter()`, grouped aggregation with `tapply()`, professional visualization with `ggplot2` and base R graphics, and formal Z-test and t-distribution hypothesis testing. These capabilities collectively form a strong foundation for predictive and prescriptive analytics work in future projects (James et al., 2021).

# Bibliography

Bluman, A. G. (2017). *Elementary statistics: A step by step approach* (10th ed.). McGraw-Hill Education.

James, G., Witten, D., Hastie, T., & Tibshirani, R. (2021). *An introduction to statistical learning: With applications in R* (2nd ed.). Springer. https://doi.org/10.1007/978-1-0716-1418-1

Kumar, R., & Singh, P. (2022). Data analytics in Indian real estate: Trends, challenges, and opportunities. *Journal of Housing and the Built Environment*, *37*(2), 845–863. https://doi.org/10.1007/s10901-021-09875-4

Malpezzi, S. (2003). Hedonic pricing models: A selective and applied review. In T. O'Sullivan & K. Gibb (Eds.), *Housing economics and public policy* (pp. 67–89). Blackwell Science.

# Appendix

This HTML report was produced from an R Markdown source file. The R Markdown file (**Midterm_Project.Rmd**) containing all R code chunks, computation logic, and documentation is included as a support file in the submission package alongside this HTML report. All data management, statistical computations, and visualizations were performed entirely within R and RStudio using the `readxl`, `tidyverse`, `dplyr`, `scales`, `ggplot2`, `gridExtra`, `knitr`, and `kableExtra` packages.
