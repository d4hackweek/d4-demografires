
alldat <- read_rds("./DATA/ALLDATA.RDS")

makettest <- function(var, title){
  df <- as.data.frame(t(unlist(t.test(get(var) ~ WFIRRISK, data = alldat))))
  df$Var <- paste0(title)
  return(df)
}

makettest("pct_native", "% Native American")

a<-makettest("SOVI_SCORE", "SOVI")

makettest("pct_immobile_1yr", "% Immobile 1 Year")
makettest("pov_percent", "Poverty %")
makettest("B19013_001E", "Median HH Income")

tnat <- as.data.frame(t(unlist(t.test(pct_native ~ WFIRRISK, data = alldat))))
tnat$Var <- "% Native American"

tfem <- as.data.frame(t(unlist(t.test(female ~ WFIRRISK, data = alldat))))
tfem$Var <- "% Female"

tnocit <- as.data.frame(t(unlist(t.test(noncit ~ WFIRRISK, data = alldat))))
tfem$Var <- "% Non Citizen"

tlths <- as.data.frame(t(unlist(t.test(percent_lessthanHS ~ WFIRRISK, data = alldat))))
tfem$Var <- "% Less Than HS"

textract <- as.data.frame(t(unlist(t.test(pct_extractive ~ WFIRRISK, data = alldat))))
textract$Var <- "% Extractive"

tblk <- as.data.frame(t(unlist(t.test(pct_black ~ WFIRRISK, data = alldat))))
tblk$Var <- "% Black"

thsp <- as.data.frame(t(unlist(t.test(pct_hispanic ~ WFIRRISK, data = alldat))))
thsp$Var <- "% Hispanic"

told <- as.data.frame(t(unlist(t.test(pct_65andolder ~ WFIRRISK, data = alldat))))
told$Var <- "% 65+"

tyoung <- as.data.frame(t(unlist(t.test(pct_under18years ~ WFIRRISK, data = alldat))))
tyoung$Var <- "% <18 years"

tpov <- as.data.frame(t(unlist(t.test(pov_percent ~ WFIRRISK, data = alldat))))
tpov$Var <- "Poverty Rate"

stats <- rbind(tblk, thsp, told, tyoung, tpov)
stats2 <- stats %>%
  dplyr::select("Variable" = Var, "df" = parameter.df, "t" = statistic.t, "p value" = p.value) %>%
  mutate(`italic(df)` = round(as.numeric(df), digits = 1),
         `italic(t)` = round(as.numeric(t), digits = 3),
         `italic(p)` = round(as.numeric(`p value` ), digits = 4)) %>%
  dplyr::select(Variable, `italic(df)` , `italic(t)`, `italic(p)`)

stats2$`italic(p)` = sub("^(-?)0.", "\\1.", sprintf("%.4f", stats2$`italic(p)`))


tt <- ttheme(colnames.style = colnames_style(face = "italic", parse=TRUE))
d<- tableGrob(stats2,
              rows=NULL,
              theme = tt
              # theme = ttheme_minimal()
)

title <- textGrob(expression(paste("Welch's two-sample ", italic("t"), " test")),gp=gpar(fontsize=20))
e<-gtable_add_grob(
  gtable_add_rows(d,
                  heights = grobHeight(title) + unit(5, "mm"),
                  pos = 0),
  title,
  1,1,1,ncol(d))



figmake <- function(df, variable, yaxis, title){
  ggboxplot(df, y = variable,
            outlier.shape = 1,
            palette = c("#00AFBB", "#E7B800"),
            fill = "WFIRRISK",
            line.color = "WFIRRISK") +
    # stat_compare_means() +
    stat_compare_means(method = "t.test",
                       label = "p.signif",
                       aes(group = WFIRRISK)
                       # aes(label = ifelse( p < 2.e-5, p.format, ifelse( p < 1.e-2, sprintf("p = %2.1e", as.numeric(..p.format..)), sprintf("p = %5.4f", as.numeric(..p.format..)))))                       # aes(label = ifelse(
                       #   p < 1.e-5,
                       #   sprintf("p = %2.1e", as.numeric(..p.format..)),
                       #   sprintf("p = %5.6f", as.numeric(..p.format..))))
                       # aes(label = ifelse(p < 0.05, sprintf("p = %2.1e", as.numeric(..p.format..)), ..p.format..))
    ) +
    theme_bw() +
    theme(legend.position = "right",
          legend.title = element_blank(),
          axis.text=element_text(color="black")) +
    # annotate("text",-Inf, Inf, label = paste0("p = ", t.test(variable ~ region, data = get(df))$p.value)
    #          , hjust = 0.01, vjust = 1) +
    labs(x = "",
         y = paste0(yaxis))
}

figmake(alldat, "pov_percent", "% Poverty", "% Poverty") +
  coord_cartesian(ylim = c(0,0.50))


sum(alldat$POPULATION[which(alldat$WFIRRISK== 1)])