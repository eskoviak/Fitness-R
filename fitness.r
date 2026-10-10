# Data ingestion and analysis program
library(ggplot2)
library(ggpubr)

# Build the data frame from the CSV data
df <- read.csv("Weight.csv",
  stringsAsFactors = FALSE
)

# Convert the excel date to YYYY-MM-DD
df$Date <- as.Date(df$Date,
  format = "%m/%d/%y"
)

# Add Lean Body Mass (lbs) LBM and Fat Body Mass (lbs) FBM
df$LBM <- round(df$Weight * (1 - df$BodyFat / 100), 1)
df$FBM <- round(df$Weight - df$LBM, 1)

# Calculate BMR
df$BMR <- round(370 + (df$LBM / 2.204 * 21.6), 0)

# -----------------------------------------------------------------------------
# Plot 1: LBM vs FBM dual-axis plot over time
# -----------------------------------------------------------------------------
ylim_prim <- range(df$LBM)
ylim_sec <- range(df$FBM)

ggplot(df, aes(x = Date, group = 1)) +
  geom_line(aes(y = LBM, color = "LBM")) +
  geom_line(aes(y = FBM * ylim_prim[2] / ylim_sec[2], color = "FBM")) +
  scale_y_continuous(
    name = "LBM",
    sec.axis = sec_axis(~ . / (ylim_prim[2] / ylim_sec[2]), name = "FBM")
  ) +
  labs(color = "Metric") +
  scale_x_date(date_labels = "%m %d") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

ggsave("fitness_plot.png", width = 5, height = 3, dpi = 300)

# -----------------------------------------------------------------------------
# Plot 2: Body Fat trend over time with linear regression
# -----------------------------------------------------------------------------
model_trend <- lm(FBM ~ Date, df)

ggplot(df, aes(x = Date, y = FBM)) +
  geom_line() +
  geom_abline(intercept = coef(model_trend)[1], slope = coef(model_trend)[2], color = "Blue") +
  stat_cor(aes(label = after_stat(rr.label)), label.x.npc = "left", label.y.npc = "bottom") +
  labs(title = "Body Fat Trend")

ggsave("BodyFat Trend.png", width = 4, height = 3, dpi = 300)

# -----------------------------------------------------------------------------
# Plot 3: BodyWater vs BodyFat correlation
# BodyWater is only recorded from row 61 and above -- filter the df
# -----------------------------------------------------------------------------
model_water <- lm(FBM ~ BodyWater, df[61:nrow(df), ])

ggplot(df[61:nrow(df), ], aes(x = BodyWater, y = FBM)) +
  geom_point() +
  geom_abline(intercept = coef(model_water)[1], slope = coef(model_water)[2], color = "Blue") +
  stat_cor(aes(label = after_stat(rr.label)), label.x.npc = "left", label.y.npc = "bottom")

ggsave("BodyWater corelation.png", width = 4, height = 3, dpi = 300)

# -----------------------------------------------------------------------------
# Plot 4:  Histogram of body water
# # BodyWater is only recorded from row 61 and above -- filter the df
# -----------------------------------------------------------------------------

ggplot(df[61:nrow(df), ], aes(x = BodyWater)) +
  geom_histogram(binwidth = 0.1)

ggsave("BodyWater Histogram.png", width = 4, height = 3, dpi = 300)
