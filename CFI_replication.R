# CFI_replication.R
# Max Gillet, Chicago Fed, 2024
library(data.table)
library(lubridate)
library(ggplot2)
library(alfred)
library(keyring)

# functions used ----
spearman_ccf <- function(x, y, n) {
  all_lags <- seq(-n, n)
  res_lags <- sapply(all_lags, function(l) {
    cor(data.table::shift(x, n = l, type = "lead"),
      y,
      method = "spearman",
      use = "pairwise.complete.obs"
    )
  })
  return(data.table(lag = all_lags, ccf = res_lags))
}


run_ccf <- function(cols_to_run, df, right_col) {
  result_df <- lapply(cols_to_run, function(x) {
    result <- ccf(unlist(df[, x, with = F]),
      unlist(df[, right_col, with = F]),
      plot = F,
      na.action = na.omit
    )

    data.table(
      lag = as.numeric(result$lag),
      ccf = as.numeric(result$acf),
      model = x
    )
  })
  result_df <- rbindlist(result_df)
  result_df[, c("index", "normalization") := tstrsplit(model, ".", fixed = T)]

  return(result_df)
}


run_spearman_ccf <- function(cols_to_run, df, right_col, max_n = 13) {
  result_df <- lapply(cols_to_run, function(x) {
    result <- spearman_ccf(unlist(df[, x, with = F]), unlist(df[, right_col, with = F]), max_n)

    dt <- data.table(
      lag = as.numeric(result$lag),
      ccf = as.numeric(result$ccf),
      model = x
    )

    return(dt)
  })
  result_df <- rbindlist(result_df)
  result_df[, c("index", "normalization") := tstrsplit(model, ".", fixed = T)]

  return(result_df)
}

# Read in current NFCI and ANFCI----
tryCatch(
  fred_key <<- keyring::key_get("fred_api"),
  error = function(e) {
    keyring::key_set("fred_api", prompt = "Type your FRED API key:")
    fred_key <<- keyring::key_get("fred_api")
  }
)

current_nfci <- alfred::get_fred_series("NFCI", api_key = fred_key)
current_anfci <- alfred::get_fred_series("ANFCI", api_key = fred_key)
all_nfcis_w <- merge(current_nfci, current_anfci, by = "date", all = T)
setDT(all_nfcis_w)
all_nfcis_w[, nfci_date := 1]

# Calculate renormalized NFCI ----

all_nfcis_w[, RenormalizedNFCI := NFCI]
all_nfcis_w[, RenormalizedNFCI := (RenormalizedNFCI - median(RenormalizedNFCI)) /
  mad(RenormalizedNFCI)]

# Get NFCI as reported ----
asreported_nfci <- alfred::get_alfred_series(
  "NFCI",
  observation_start = "1990-01-01",
  observation_end = "1990-01-31",
  api_key = fred_key
)
setDT(asreported_nfci)
all_realtimes <- unique(asreported_nfci$realtime_period)
all_realtimes <- all_realtimes[order(all_realtimes)]
chunked_realtimes <- split(all_realtimes, cut(seq_along(all_realtimes), 26, labels = FALSE))

all_realtime_dfs <- lapply(chunked_realtimes, function(x) {
  t1 <- x[1]
  tn <- x[length(x)]
  print(paste0(t1, " - ", tn))
  alfred::get_alfred_series(
    "NFCI",
    realtime_start = t1,
    realtime_end = tn,
    api_key = fred_key
  )
})
all_realtime_df <- rbindlist(all_realtime_dfs)
all_realtime_df[, first_rt_period := min(realtime_period), by = "date"]
as_released_nfci <- all_realtime_df[realtime_period == first_rt_period]
as_released_nfci <- as_released_nfci[order(date)]


as_released_nfci <- as_released_nfci[date >= min(realtime_period)]
as_released_nfci[, date := first_rt_period]
as_released_nfci <- as_released_nfci[, c("date", "NFCI")]
colnames(as_released_nfci)[2] <- "AsReleasedNFCI"
as_released_nfci[, nfci_release_date := 1]
as_released_nfci[, date := as.Date(date)]

# Get FCI-G ----
fcig_df <- fread(
  "https://www.federalreserve.gov/econres/notes/feds-notes/fci_g_public_monthly_3yr.csv"
)
fcig_df[, date := as.Date(date)]
colnames(fcig_df)[colnames(fcig_df) == "FCI-G Index (baseline)"] <- "fcig"
fcig_df[, headwind := ifelse(fcig >= 0, 1, 0)]
fcig_df <- fcig_df[, c("date", "fcig", "headwind")]
fcig_df[, headwind_date := 1]

# Create Figure 1 and 2 ----
palcols <- RColorBrewer::brewer.pal(3, "Set2")


nfci_alone_figure1 <- ggplot() +
  geom_hline(yintercept = 0, color = "black") +
  geom_line(aes(x = date, y = fcig, color = "2FCI-G"),
    data = fcig_df,
    size = 1
  ) +
  geom_line(aes(x = date, y = NFCI, color = "1NFCI"),
    data = all_nfcis_w,
    size = 1
  ) +
  scale_color_manual(
    values = c("1NFCI" = palcols[2], "2FCI-G" = palcols[1]),
    labels = c("1NFCI" = "NFCI", "2FCI-G" = "FCI-G")
  ) +
  theme_minimal() +
  labs(
    title = NULL,
    x = NULL,
    y = NULL,
    subtitle = "index value",
    color = NULL
  ) +
  theme(
    legend.position = "bottom",
    text = element_text(family = "sans", size = 14),
    plot.subtitle = element_text(family = "sans", size = 10),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.line.x = element_line(),
    axis.line.y = element_line(),
    axis.ticks.x = element_line(),
    axis.ticks.y = element_line(),
    axis.ticks.length.x = ggplot2::unit(-1, "mm"),
    axis.ticks.length.y = ggplot2::unit(-1, "mm"),
    legend.box.margin = margin(t = -15)
  )

print(nfci_alone_figure1)

ggsave(
  "nfci_alone_figure1.png",
  nfci_alone_figure1,
  width = 6,
  height = 4,
  units = "in",
  dpi = 320,
  bg = "white"
)


nfci_figure2 <- ggplot() +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "black"
  ) +
  geom_line(aes(x = date, y = NFCI, color = "NFCI"),
    data = all_nfcis_w,
    size = 1
  ) +
  geom_line(
    aes(x = date, y = RenormalizedNFCI, color = "Renormalized NFCI"),
    data = all_nfcis_w,
    size = 1
  ) +
  scale_color_manual(values = c("Renormalized NFCI" = palcols[3], "NFCI" = palcols[2])) +
  theme_minimal() +
  labs(
    title = NULL,
    x = NULL,
    y = NULL,
    subtitle = "index value",
    color = NULL
  ) +
  theme(
    legend.position = "bottom",
    text = element_text(family = "sans", size = 14),
    plot.subtitle = element_text(family = "sans", size = 10),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.line.x = element_line(),
    axis.line.y = element_line(),
    axis.ticks.x = element_line(),
    axis.ticks.y = element_line(),
    axis.ticks.length.x = ggplot2::unit(-1, "mm"),
    axis.ticks.length.y = ggplot2::unit(-1, "mm"),
    legend.box.margin = margin(t = -15)
  )
print(nfci_figure2)

ggsave(
  "nfci_figure2.png",
  nfci_figure2,
  width = 6,
  height = 4,
  units = "in",
  dpi = 320,
  bg = "white"
)

# Set up lead/lags ----
# fci-g is monthly, nfci is quarterly

combined_fcig_nfci <- merge(fcig_df, all_nfcis_w, by = c("date"), all = T)
combined_fcig_nfci <- combined_fcig_nfci[order(date)]

nfci_cols <- colnames(all_nfcis_w)
nfci_cols <- nfci_cols[!(nfci_cols %in% c("date", "nfci_date"))]

headwinds_cols <- colnames(fcig_df)
headwinds_cols <- headwinds_cols[!(headwinds_cols %in% c("date", "headwind_date"))]

combined_fcig_nfci[, (nfci_cols) := nafill(.SD, type = "locf"), .SDcols = nfci_cols]
combined_fcig_nfci[, (headwinds_cols) := nafill(.SD, type = "locf"), .SDcols = headwinds_cols]

# for realtimes
realtime_fcig_df <- copy(fcig_df)
colnames(realtime_fcig_df)[2] <- "fcig_realtime"
combined_realtime_fcig_nfci <- merge(realtime_fcig_df,
  as_released_nfci,
  by = c("date"),
  all = T
)

combined_realtime_fcig_nfci[, fcig_realtime := nafill(fcig_realtime, type = "locf")]
combined_realtime_fcig_nfci[, headwind := nafill(headwind, type = "locf")]
combined_realtime_fcig_nfci[, AsReleasedNFCI := nafill(AsReleasedNFCI, type = "locf")]

# two ways to set up. This one, use last of month NFCI to project FCI-G
monthly_nfci_proj_fcig <- combined_fcig_nfci[headwind_date == 1, c("date", nfci_cols, "headwind", "fcig"), with = F]

monthly_rt_nfci_proj_fcig <- combined_realtime_fcig_nfci[headwind_date == 1, c("date", "AsReleasedNFCI", "headwind", "fcig_realtime")]
monthly_rt_nfci_proj_fcig <- monthly_rt_nfci_proj_fcig[!is.na(AsReleasedNFCI)]


# CCF on FCI-G ----

monthly_nfci_fcig_ccf <- run_ccf(nfci_cols, monthly_nfci_proj_fcig, "fcig")
# spearman ccf ----

monthly_nfci_fcig_spearmanccf <- run_spearman_ccf(nfci_cols, monthly_nfci_proj_fcig, "fcig")
# TODO: Add significance line (testcorr package?)
# qnorm((1 + ci)/2)/sqrt(x$n.used)

monthly_nfci_fcig_ccf[, corr_type := "1Pearson"]
monthly_nfci_fcig_spearmanccf[, corr_type := "2Rank"]
monthly_nfci_fcig_combinedccf <- rbind(monthly_nfci_fcig_ccf, monthly_nfci_fcig_spearmanccf)

figure3_plot <- ggplot(monthly_nfci_fcig_combinedccf[abs(lag) <= 3][model %in% c("NFCI", "AsReleasedNFCI")]) +
  geom_col(aes(x = lag, y = ccf, fill = corr_type),
    position = "dodge",
    width = .5
  ) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_x_continuous(breaks = seq(-3, 3), minor_breaks = NULL) +
  scale_fill_brewer(
    palette = "Set2",
    labels = c("1Pearson" = "Pearson", "2Rank" = "Rank")
  ) +
  theme_minimal() +
  labs(
    title = NULL,
    x = "lag in months",
    y = NULL,
    fill = NULL,
    subtitle = "correlation"
  ) +
  theme(
    legend.position = "bottom",
    text = element_text(family = "sans", size = 14),
    plot.subtitle = element_text(family = "sans", size = 10),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.line.x = element_line(),
    axis.line.y = element_line(),
    axis.ticks.x = element_line(),
    axis.ticks.y = element_line(),
    axis.ticks.length.x = ggplot2::unit(-1, "mm"),
    axis.ticks.length.y = ggplot2::unit(-1, "mm"),
    legend.box.margin = margin(t = -5)
  )
print(figure3_plot)
ggsave(
  "figure3_plot.png",
  figure3_plot,
  width = 6,
  height = 4,
  units = "in",
  dpi = 320,
  bg = "white"
)

# Realtime: CCF on FCI-G ----

monthly_rt_nfci_fcig_ccf <- run_ccf(
  c("AsReleasedNFCI"),
  monthly_rt_nfci_proj_fcig,
  "fcig_realtime"
)
# spearman ccf ----

monthly_rt_nfci_fcig_spearmanccf <- run_spearman_ccf(
  c("AsReleasedNFCI"),
  monthly_rt_nfci_proj_fcig,
  "fcig_realtime"
)

monthly_rt_nfci_fcig_ccf[, corr_type := "1Pearson"]
monthly_rt_nfci_fcig_spearmanccf[, corr_type := "2Rank"]
monthly_rt_nfci_fcig_combinedccf <- rbind(monthly_rt_nfci_fcig_ccf, monthly_rt_nfci_fcig_spearmanccf)


figure4_plot <- ggplot(monthly_rt_nfci_fcig_combinedccf[abs(lag) <= 3][model %in% c("NFCI", "AsReleasedNFCI")]) +
  geom_col(aes(x = lag, y = ccf, fill = corr_type),
    position = "dodge",
    width = .5
  ) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_fill_brewer(
    palette = "Set2",
    labels = c(
      "1Pearson" = "Pearson",
      "2Rank" = "Rank",
      "3Median-based" = "Median-based"
    )
  ) +
  scale_x_continuous(breaks = seq(-3, 3), minor_breaks = NULL) +
  theme_minimal() +
  labs(
    title = NULL,
    x = "lag in months",
    subtitle = "correlation",
    fill = NULL,
    y = NULL
  ) +
  theme(
    legend.position = "bottom",
    text = element_text(family = "sans", size = 14),
    plot.subtitle = element_text(family = "sans", size = 10),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.line.x = element_line(),
    axis.line.y = element_line(),
    axis.ticks.x = element_line(),
    axis.ticks.y = element_line(),
    axis.ticks.length.x = ggplot2::unit(-1, "mm"),
    axis.ticks.length.y = ggplot2::unit(-1, "mm"),
    legend.box.margin = margin(t = -5)
  )
print(figure4_plot)
ggsave(
  "figure4_plot.png",
  figure4_plot,
  width = 6,
  height = 4,
  units = "in",
  dpi = 320,
  bg = "white"
)
