# Case Study Exercise 1: Volleyball Match Statistics
# Team: Indudhara Swamy Vivekananda (4106839); Sanjana Basoor Hemanth Kumar (4108160); Rohit Mamgin (4086896)
# Exercise lead: Indudhara Swamy Vivekananda (4106839)

seed <- 366
set.seed(seed)

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(readr)
})

dir.create("outputs/exercise1", recursive = TRUE, showWarnings = FALSE)

# The file uses semicolons and contains an unnamed row-index column.
matches_raw <- read.csv2("data/matchStats.csv", check.names = FALSE,
                         na.strings = c("", "NA"))
matches_raw <- matches_raw[, -1]
matches_raw$Date <- as.Date(matches_raw$Date)

# Diagnose the only non-structural missing field against observed context.
reception_missing <- is.na(matches_raw$Receptions.Home)
set.seed(seed)
reception_missingness_tests <- tibble(
  comparison = c("Home team", "Away team", "Home total points", "Home digs", "Home kills"),
  test = c("Monte Carlo chi-squared", "Monte Carlo chi-squared", rep("Wilcoxon rank-sum", 3)),
  p_value = c(
    chisq.test(table(reception_missing, matches_raw$Home.Team), simulate.p.value=TRUE, B=9999)$p.value,
    chisq.test(table(reception_missing, matches_raw$Away.Team), simulate.p.value=TRUE, B=9999)$p.value,
    wilcox.test(matches_raw$Total.Points.Home ~ reception_missing)$p.value,
    wilcox.test(matches_raw$Digs.Home ~ reception_missing)$p.value,
    wilcox.test(matches_raw$Kills.Home ~ reception_missing)$p.value)
)
write.csv(reception_missingness_tests, "outputs/exercise1/reception_missingness_tests.csv", row.names=FALSE)

# Audit missingness before any imputation.
missing_audit <- tibble(
  variable = names(matches_raw),
  n_missing = vapply(matches_raw, function(x) sum(is.na(x)), integer(1)),
  pct_missing = 100 * n_missing / nrow(matches_raw)
) %>%
  mutate(
    mechanism = case_when(
      variable %in% c("Set4.Home", "Set4.Away", "Set5.Home", "Set5.Away") ~ "MNAR (structural)",
      variable == "Receptions.Home" ~ "MCAR (plausible)",
      TRUE ~ "No missing values"
    ),
    rationale = case_when(
      variable %in% c("Set4.Home", "Set4.Away") ~
        "Missing exactly when the match ended after three sets; absence depends on match length.",
      variable %in% c("Set5.Home", "Set5.Away") ~
        "Missing exactly when no deciding fifth set was played; absence depends on match length.",
      variable == "Receptions.Home" ~
        "No association was detected with either team or observed points, digs, or kills; recording loss is plausible.",
      TRUE ~ "Complete column; no missing-data mechanism needs to be assigned."
    ),
    imputation = case_when(
      variable %in% c("Set4.Home", "Set4.Away", "Set5.Home", "Set5.Away") ~
        "Zero: the set was not played, so zero preserves the sporting meaning.",
      variable == "Receptions.Home" ~
        "Overall median; robust to skew and suitable for a plausibly MCAR numeric field.",
      TRUE ~ "None required."
    )
  )
write.csv(missing_audit, "outputs/exercise1/missing_audit.csv", row.names = FALSE)

# Impute plausibly MCAR home receptions with the robust overall median.
global_receptions_median <- median(matches_raw$Receptions.Home, na.rm = TRUE)
matches <- matches_raw %>%
  mutate(
    Receptions.Home = coalesce(Receptions.Home, global_receptions_median),
    across(c(Set4.Home, Set4.Away, Set5.Home, Set5.Away), ~replace_na(.x, 0)),
    Home.Win = as.integer(Winner == Home.Team)
  )
stopifnot(sum(is.na(matches)) == 0)
write.csv(matches, "outputs/exercise1/matchStats_complete.csv", row.names = FALSE)

# Normality assessment: Shapiro-Wilk and QQ plot for total home points.
normality <- shapiro.test(matches$Total.Points.Home)
capture.output(normality, file = "outputs/exercise1/shapiro_total_points_home.txt")

p_qq <- ggplot(matches, aes(sample = Total.Points.Home)) +
  stat_qq(colour = "#2B3B8C", alpha = 0.75) +
  stat_qq_line(colour = "#E04488", linewidth = 0.8) +
  labs(title = "QQ plot: home-team total points", x = "Theoretical quantiles",
       y = "Observed quantiles") +
  theme_minimal(base_size = 11)
ggsave("outputs/exercise1/qq_total_points_home.pdf", p_qq, width = 5.8, height = 4.0)

# EDA 1: paired home-away differences with inferential summaries.
paired_metrics <- matches %>%
  transmute(Kills = Kills.Home - Kills.Away,
            Blocks = Blocks.Home - Blocks.Away,
            Aces = Aces.Home - Aces.Away,
            Digs = Digs.Home - Digs.Away,
            `Total points` = Total.Points.Home - Total.Points.Away) %>%
  pivot_longer(everything(), names_to = "Metric", values_to = "Home_minus_away")

p_diff <- ggplot(paired_metrics, aes(Metric, Home_minus_away, fill = Metric)) +
  geom_violin(alpha = 0.45, colour = NA) +
  geom_boxplot(width = 0.18, outlier.alpha = 0.35) +
  geom_hline(yintercept = 0, linetype = 2) +
  coord_flip() + guides(fill = "none") +
  labs(title = "Home-minus-away performance differences", x = NULL, y = "Difference") +
  theme_minimal(base_size = 11)
ggsave("outputs/exercise1/home_away_differences.pdf", p_diff, width = 6.2, height = 4.2)

# EDA 2: scoring components and total points.
long_components <- bind_rows(
  matches %>% transmute(Total = Total.Points.Home, Kills = Kills.Home,
                        Blocks = Blocks.Home, Aces = Aces.Home,
                        OpponentErrors = Opponents.Errors.Home),
  matches %>% transmute(Total = Total.Points.Away, Kills = Kills.Away,
                        Blocks = Blocks.Away, Aces = Aces.Away,
                        OpponentErrors = Opponents.Errors.Away)
) %>% pivot_longer(-Total, names_to = "Component", values_to = "Value")

p_components <- ggplot(long_components, aes(Value, Total)) +
  geom_point(alpha = 0.45, colour = "#2B3B8C") +
  geom_smooth(method = "lm", se = TRUE, colour = "#E04488") +
  facet_wrap(~Component, scales = "free_x") +
  labs(title = "Scoring components versus total points", x = "Component count",
       y = "Total points") + theme_minimal(base_size = 10)
ggsave("outputs/exercise1/scoring_components.pdf", p_components, width = 7.2, height = 5.2)

# Compact numerical results used by the report.
home_binom <- binom.test(sum(matches$Home.Win), nrow(matches), p = 0.5)
paired_tests <- tibble(
  metric = c("Kills", "Blocks", "Aces", "Digs", "Total points"),
  mean_home = c(mean(matches$Kills.Home), mean(matches$Blocks.Home), mean(matches$Aces.Home),
                mean(matches$Digs.Home), mean(matches$Total.Points.Home)),
  mean_away = c(mean(matches$Kills.Away), mean(matches$Blocks.Away), mean(matches$Aces.Away),
                mean(matches$Digs.Away), mean(matches$Total.Points.Away)),
  paired_t_p = c(t.test(matches$Kills.Home, matches$Kills.Away, paired=TRUE)$p.value,
                 t.test(matches$Blocks.Home, matches$Blocks.Away, paired=TRUE)$p.value,
                 t.test(matches$Aces.Home, matches$Aces.Away, paired=TRUE)$p.value,
                 t.test(matches$Digs.Home, matches$Digs.Away, paired=TRUE)$p.value,
                 t.test(matches$Total.Points.Home, matches$Total.Points.Away, paired=TRUE)$p.value),
  paired_wilcoxon_p = c(
    wilcox.test(matches$Kills.Home, matches$Kills.Away, paired=TRUE, exact=FALSE)$p.value,
    wilcox.test(matches$Blocks.Home, matches$Blocks.Away, paired=TRUE, exact=FALSE)$p.value,
    wilcox.test(matches$Aces.Home, matches$Aces.Away, paired=TRUE, exact=FALSE)$p.value,
    wilcox.test(matches$Digs.Home, matches$Digs.Away, paired=TRUE, exact=FALSE)$p.value,
    wilcox.test(matches$Total.Points.Home, matches$Total.Points.Away, paired=TRUE, exact=FALSE)$p.value)
)
write.csv(paired_tests, "outputs/exercise1/paired_tests.csv", row.names = FALSE)

summary_lines <- c(
  sprintf("n_matches=%d", nrow(matches)),
  sprintf("home_wins=%d", sum(matches$Home.Win)),
  sprintf("home_win_rate=%.4f", mean(matches$Home.Win)),
  sprintf("home_win_binom_p=%.6g", home_binom$p.value),
  sprintf("shapiro_W=%.5f", unname(normality$statistic)),
  sprintf("shapiro_p=%.6g", normality$p.value),
  sprintf("corr_kills_total=%.4f", cor(long_components$Value[long_components$Component=="Kills"], long_components$Total[long_components$Component=="Kills"]))
)
writeLines(summary_lines, "outputs/exercise1/key_results.txt")

