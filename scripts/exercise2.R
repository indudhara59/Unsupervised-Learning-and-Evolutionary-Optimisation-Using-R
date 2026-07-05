# Case Study Exercise 2: Volleyball Player Statistics
# Team: Indudhara Swamy Vivekananda (4106839); Sanjana Basoor Hemanth Kumar (4108160); Rohit Mamgin (4086896)
# Exercise lead: Sanjana Basoor Hemanth Kumar (4108160)

seed <- 366
set.seed(seed)

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(readr)
})
dir.create("outputs/exercise2", recursive = TRUE, showWarnings = FALSE)

players <- read.csv("data/playerStats.csv", check.names = FALSE, fileEncoding = "UTF-8") %>%
  mutate(Height = parse_number(Height))
error_columns <- grep("Errors$", names(players), value = TRUE)
players <- players %>% mutate(Total.Errors = rowSums(across(all_of(error_columns))))
write.csv(players, "outputs/exercise2/playerStats_enriched.csv", row.names = FALSE)

# Variable overview and descriptive statistics for every numeric variable.
overview <- tibble(variable = names(players), class = vapply(players, function(x) class(x)[1], character(1)),
                   n_unique = vapply(players, dplyr::n_distinct, integer(1)),
                   n_missing = vapply(players, function(x) sum(is.na(x)), integer(1)))
write.csv(overview, "outputs/exercise2/variable_overview.csv", row.names = FALSE)

numeric_summary <- players %>% summarise(across(where(is.numeric), list(
  n = ~sum(!is.na(.x)), mean = ~mean(.x, na.rm=TRUE), sd = ~sd(.x, na.rm=TRUE),
  median = ~median(.x, na.rm=TRUE), q1 = ~quantile(.x,.25,na.rm=TRUE),
  q3 = ~quantile(.x,.75,na.rm=TRUE), min = ~min(.x,na.rm=TRUE), max = ~max(.x,na.rm=TRUE)
))) %>% pivot_longer(everything(), names_to=c("variable","statistic"), names_pattern="(.*)_(n|mean|sd|median|q1|q3|min|max)$") %>%
  pivot_wider(names_from=statistic, values_from=value)
write.csv(numeric_summary, "outputs/exercise2/descriptive_statistics.csv", row.names=FALSE)

# Position-specific distributions reveal role specialisation.
position_long <- players %>% select(Position, `Aces`, `Blocks Per Match`, `Digs Per Match`,
                                    `Attacks Per Match`, Total.Errors) %>%
  pivot_longer(-Position, names_to="Metric", values_to="Value")
p_position <- ggplot(position_long, aes(Position, Value, fill=Position)) +
  geom_boxplot(outlier.alpha=.25) + facet_wrap(~Metric, scales="free_y", ncol=2) +
  coord_flip() + guides(fill="none") + labs(title="Player metrics differ strongly by position", x=NULL) +
  theme_minimal(base_size=9)
ggsave("outputs/exercise2/position_profiles.pdf", p_position, width=7.2, height=7.0)

# Correlation heatmap for a non-redundant set of interpretable numeric variables.
corr_vars <- c("Age","Height","Sets Per Match","Receives Per Match","Serves Per Match",
               "Blocks Per Match","Digs Per Match","Attacks Per Match","Total.Errors")
cormat <- cor(players[, corr_vars], use="pairwise.complete.obs")
corr_df <- as.data.frame(as.table(cormat))
p_corr <- ggplot(corr_df, aes(Var1, Var2, fill=Freq)) +
  geom_tile() + geom_text(aes(label=sprintf("%.2f",Freq)), size=2.4) +
  scale_fill_gradient2(low="#3F79B7", mid="white", high="#E04488", midpoint=0, limits=c(-1,1)) +
  coord_equal() + labs(title="Correlation matrix", x=NULL, y=NULL, fill="r") +
  theme_minimal(base_size=8) + theme(axis.text.x=element_text(angle=45,hjust=1))
ggsave("outputs/exercise2/correlation_matrix.pdf", p_corr, width=7.2, height=6.2)

# Univariate outliers: Tukey's 1.5-IQR fences for each numeric column.
num_names <- names(players)[vapply(players, is.numeric, logical(1))]
outlier_rows <- lapply(num_names, function(v) {
  x <- players[[v]]; q <- quantile(x,c(.25,.75),na.rm=TRUE); i <- q[2]-q[1]
  flag <- x < q[1]-1.5*i | x > q[2]+1.5*i
  tibble(variable=v, n_outliers=sum(flag,na.rm=TRUE), pct_outliers=100*mean(flag,na.rm=TRUE),
         lower=q[1]-1.5*i, upper=q[2]+1.5*i)
})
univariate_outliers <- bind_rows(outlier_rows)
write.csv(univariate_outliers,"outputs/exercise2/univariate_outliers.csv",row.names=FALSE)

# Multivariate outliers: position-conditional distances avoid treating legitimate role specialisation as anomalous.
mv_vars <- c("Age","Height","Sets Per Match","Receives Per Match","Serves Per Match",
             "Blocks Per Match","Digs Per Match","Attacks Per Match")
cutoff <- qchisq(.99, df=length(mv_vars))
mv_outliers <- players %>% group_by(Position) %>% group_modify(function(dat,key) {
  z <- scale(dat[,mv_vars])
  d2 <- mahalanobis(z, center=colMeans(z), cov=cov(z))
  dat %>% transmute(`Player Name`, Team, mahalanobis=d2,
                    cutoff=cutoff, outlier=d2>cutoff)
}) %>% ungroup() %>% arrange(desc(mahalanobis))
write.csv(mv_outliers,"outputs/exercise2/multivariate_outliers.csv",row.names=FALSE)

# Country means and comparison with official team statistics.
team_code <- c(ARG="Argentina",BRA="Brazil",BUL="Bulgaria",CAN="Canada",CHN="China",CUB="Cuba",
 FRA="France",GER="Germany",IRI="Iran",ITA="Italy",JPN="Japan",NED="Netherlands",POL="Poland",
 SLO="Slovenia",SRB="Serbia",TUR="Turkiye",UKR="Ukraine",USA="USA")
country_means <- players %>% group_by(Team) %>% summarise(across(where(is.numeric), mean), .groups="drop") %>%
  mutate(Team.Name=unname(team_code[Team]), Team.Key=if_else(Team=="TUR","TURKEY",iconv(Team.Name,to="ASCII//TRANSLIT")))
team_stats <- read.csv("data/teamStats.csv",check.names=FALSE,fileEncoding="UTF-8")
comparison <- country_means %>% left_join(team_stats %>% mutate(Team.Key=if_else(Rank==16,"TURKEY",iconv(Team,to="ASCII//TRANSLIT"))), by="Team.Key", suffix=c("",".Official"))
write.csv(comparison,"outputs/exercise2/country_means_team_comparison.csv",row.names=FALSE)

p_team <- ggplot(comparison,aes(Aces,Won,label=Team)) +
  geom_smooth(aes(x=Aces,y=Won,group=1),inherit.aes=FALSE,method="lm",se=TRUE,colour="#E04488") + geom_point(aes(size=`Serves Per Match`),colour="#2B3B8C") +
  geom_text(nudge_y=.25,size=2.6,check_overlap=TRUE) +
  labs(title="Country-level player means versus team wins",x="Mean aces",y="Matches won",size="Mean serves\nper match") +
  theme_minimal(base_size=10)
ggsave("outputs/exercise2/team_comparison.pdf",p_team,width=6.3,height=4.6)

# Exactly one best and worst record per position; ties are resolved by player name.
ranked <- players %>% arrange(Position,Total.Errors,`Player Name`)
best <- ranked %>% group_by(Position) %>% slice_min(Total.Errors,n=1,with_ties=FALSE) %>% mutate(Category="Best")
worst <- ranked %>% group_by(Position) %>% slice_max(Total.Errors,n=1,with_ties=FALSE) %>% mutate(Category="Worst")
best_worst <- bind_rows(best,worst) %>% select(Position,Category,`Player Name`,Team,Total.Errors) %>% arrange(Position,Category)
write.csv(best_worst,"outputs/exercise2/best_worst_by_position.csv",row.names=FALSE)

team_cor <- cor(comparison$Aces,comparison$Won)
serve_cor <- cor(comparison$`Serves Per Match`,comparison$Won)
writeLines(c(sprintf("n_players=%d",nrow(players)),sprintf("n_numeric=%d",length(num_names)),
 sprintf("mv_outliers_99pct=%d",sum(mv_outliers$outlier)),sprintf("corr_country_aces_wins=%.4f",team_cor),sprintf("corr_country_serves_wins=%.4f",serve_cor)),
 "outputs/exercise2/key_results.txt")


