# Data ingestion and analysis program
library(ggplot2)
# Build the data frame from the excel data
# note need to update row seq when week is added
#df <- read.xlsx("~/Documents/Excel/Nutrition Info.xlsm", sheet = "Calculations",
#                cols=c(1,4,5,6,7,8),)

df <- read.csv("/Users/edmundlskoviak/iCloud/Data Sets/fitness/Weight.csv",
               stringsAsFactors = FALSE)

# Convert the excel date to YYYY-MM-DD
df$Date <- as.Date(df$Date,
                   format="%m/%d/%y"
#                   , origin = "1899-12-30"
  )

# Add Lean Body Mass (lbs) LBM and Fat Body Mass (lbs) FBM
df$LBM <- round(df$Weight * (1 - df$BodyFat/100),1)
df$FBM <- round(df$Weight - df$LBM,1)

# Calculate BMR
df$BMR <- round( 370+(df$LBM/2.204*21.6),0)

# Calculate a scaling factor (e.g., if LBM is ~70 and FBM is ~15, factor is ~4.6)
# Or just use a multiplier that makes them visually comparable
ylim_prim <- range(df$LBM)
ylim_sec <- range(df$FBM)


ggplot(df, aes(x = Date, group=1)) +
  geom_line(aes(y = LBM, color = "LBM")) +
  # Scale FBM to match LBM's visual range
  geom_line(aes(y = FBM * 3, color = "FBM")) + 
  scale_y_continuous(
    name = "LBM",
    # Revert the scale (divide by 4) for the second axis labels
    sec.axis = sec_axis(~./3, name = "FBM")
  ) +
  labs(color = "Metric") +
  scale_x_date(date_labels = "%m %d") +
  theme(axis.text.x=element_text(angle=60, hjust=1))

ggsave("fitness_plot.png", width = 8, height = 6, dpi = 300)
