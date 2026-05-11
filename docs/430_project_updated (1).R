
# ?????? 0. PACKAGE BOOTSTRAP ???????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????

required_pkgs <- c("quantmod", "tidyverse", "lubridate", "patchwork", "scales")

invisible(lapply(required_pkgs, function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message("Installing: ", pkg)
    install.packages(pkg, repos = "https://cloud.r-project.org", quiet = TRUE)
  }
}))

suppressPackageStartupMessages({
  library(quantmod)
  library(tidyverse)
  library(lubridate)
  library(patchwork)
  library(scales)
})

cat("All packages loaded.\n")


# ?????? 1. PARAMETERS ????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????

START_DATE <- as.Date("2006-01-01")
END_DATE   <- Sys.Date()

TICKERS <- c(
  "TRY=X",    # USD/TRY exchange rate
  "XU100.IS", # BIST 100
  "XBANK.IS", # BIST Banking
  "XUSIN.IS"  # BIST Industrials
)

LABELS <- c(
  "TRY=X"    = "USD/TRY",
  "XU100.IS" = "BIST 100",
  "XBANK.IS" = "BIST Banking",
  "XUSIN.IS" = "BIST Industrials"
)

# 5-year analysis windows. Last period runs to END_DATE dynamically.
PERIODS <- list(
  list(label = "2006-2010", start = as.Date("2006-01-01"), end = as.Date("2010-12-31")),
  list(label = "2011-2015", start = as.Date("2011-01-01"), end = as.Date("2015-12-31")),
  list(label = "2016-2020", start = as.Date("2016-01-01"), end = as.Date("2020-12-31")),
  list(label = "2021-Present", start = as.Date("2021-01-01"), end = END_DATE)
)

# Shared color palette (one color per asset, consistent across all plots)
PAL <- c(
  "USD/TRY"          = "#E74C3C",
  "BIST 100"         = "#2C3E50",
  "BIST Banking"     = "#3498DB",
  "BIST Industrials" = "#27AE60"
)

cat(sprintf("Period : %s to %s\n", START_DATE, END_DATE))
cat(sprintf("Tickers: %s\n", paste(TICKERS, collapse = ", ")))


# ?????? 2. DATA FETCHING ???????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????

fetch_one <- function(ticker, from, to) {
  env <- new.env()
  result <- suppressWarnings(
    tryCatch(
      getSymbols(ticker, src = "yahoo", from = from, to = to,
                 env = env, auto.assign = TRUE, warnings = FALSE),
      error = function(e) NULL
    )
  )
  if (is.null(result)) {
    warning(sprintf("Could not fetch %s. Skipping.", ticker))
    return(NULL)
  }
  xts_obj <- get(ticker, envir = env)
  adj_col  <- grep("Adjusted", colnames(xts_obj), value = TRUE)
  if (length(adj_col) == 0) return(NULL)
  
  col_name <- gsub("[^A-Za-z0-9]", "_", ticker)
  tibble(Date = as.Date(index(xts_obj)),
         !!col_name := as.numeric(xts_obj[, adj_col]))
}

cat("\nFetching full history from Yahoo Finance...\n")

raw_list <- lapply(TICKERS, function(tk) {
  cat(sprintf("  Fetching %-12s ... ", tk))
  df <- fetch_one(tk, START_DATE, END_DATE)
  if (!is.null(df)) cat(sprintf("%d rows\n", nrow(df))) else cat("FAILED\n")
  df
})
names(raw_list) <- TICKERS
raw_list <- Filter(Negate(is.null), raw_list)

if (length(raw_list) < 2)
  stop("Fewer than 2 assets fetched. Cannot proceed.")


# ?????? 3. MERGE & CLEAN ???????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????

price_data <- purrr::reduce(raw_list, inner_join, by = "Date") |>
  arrange(Date) |>
  drop_na()

# Rename columns to friendly ASCII labels using LABELS map
ticker_cols  <- gsub("[^A-Za-z0-9]", "_", names(LABELS))   # e.g. "TRY_X"
friendly_names <- unname(LABELS)                             # e.g. "USD/TRY"
# Only rename cols that actually exist after merge
for (i in seq_along(ticker_cols)) {
  if (ticker_cols[i] %in% colnames(price_data)) {
    colnames(price_data)[colnames(price_data) == ticker_cols[i]] <- friendly_names[i]
  }
}

price_cols   <- setdiff(colnames(price_data), "Date")
usd_try_col  <- price_cols[grepl("USD/TRY", price_cols, fixed = TRUE)]
bist_cols    <- price_cols[price_cols != usd_try_col]

cat(sprintf("Full merged data: %d rows x %d columns (%s to %s)\n",
            nrow(price_data), ncol(price_data),
            min(price_data$Date), max(price_data$Date)))


# ?????? 4. LOG RETURNS (full dataset) ????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????

return_data <- price_data |>
  mutate(across(all_of(price_cols),
                ~ log(. / lag(.)),
                .names = "Ret_{.col}")) |>
  slice(-1) |>
  drop_na()

ret_cols     <- grep("^Ret_", colnames(return_data), value = TRUE)
usd_ret_col  <- ret_cols[grepl("USD.TRY", ret_cols)][1]
bist_ret_cols <- ret_cols[ret_cols != usd_ret_col]


# ?????? 5. QUALITY CHECKS ????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????

stopifnot(
  "price_data non-empty"    = nrow(price_data) > 0,
  "return_data non-empty"   = nrow(return_data) > 0,
  "Date is Date class"      = inherits(price_data$Date, "Date"),
  "No NAs in price_data"    = !anyNA(price_data),
  "No NAs in return_data"   = !anyNA(return_data),
  "At least 200 rows"       = nrow(price_data) >= 200
)
cat("Quality checks passed.\n")


# ?????? 6. PREVIEW ?????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????

cat("\n?????? Price Data (first 6 rows) ??????\n"); print(head(price_data, 6))
cat("\n?????? Return Data (first 6 rows) ??????\n")
print(head(return_data |> select(Date, all_of(ret_cols)), 6))


# ?????? 7. CORRELATION MATRIX (full period) ??????????????????????????????????????????????????????????????????????????????????????????????????????????????????

cat("\n?????? Full-Period Log-Return Correlation Matrix ??????\n")
cor_mat <- cor(return_data[, ret_cols], use = "complete.obs")
rownames(cor_mat) <- colnames(cor_mat) <- gsub("^Ret_", "", rownames(cor_mat))
print(round(cor_mat, 4))

usd_cor_row <- grep("USD.TRY", rownames(cor_mat), value = TRUE)[1]
cat("\n>> USD/TRY correlations with BIST indices:\n")
usd_cors <- sort(cor_mat[usd_cor_row, ], decreasing = TRUE)
print(round(usd_cors[names(usd_cors) != usd_cor_row], 4))


# ?????? 8. HELPER: slice data to a period window ???????????????????????????????????????????????????????????????????????????????????????????????????
# Returns price_data and return_data filtered to [start, end].
# Resets cumulative base to the FIRST row of each window.

slice_period <- function(p_data, r_data, start, end) {
  pd <- p_data |> filter(Date >= start, Date <= end)
  rd <- r_data |> filter(Date >= start, Date <= end)
  list(prices = pd, returns = rd)
}


# ?????? 9. PLOT FACTORY FUNCTIONS ???????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????
# Each function takes a period slice and a period label, returns a ggplot.

# 9a. Cumulative % Growth
plot_cumulative_fn <- function(pd, label) {
  p_cols <- setdiff(colnames(pd), "Date")
  cum <- pd |>
    mutate(across(all_of(p_cols),
                  ~ ((. / first(.)) - 1) * 100,
                  .names = "Cum_{.col}")) |>
    select(Date, starts_with("Cum_")) |>
    pivot_longer(-Date, names_to = "Asset", values_to = "Pct") |>
    mutate(Asset = gsub("^Cum_", "", Asset))
  
  ggplot(cum, aes(Date, Pct, color = Asset)) +
    geom_line(linewidth = 0.85, alpha = 0.9) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.35) +
    scale_color_manual(values = PAL) +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    theme_minimal(base_size = 11) +
    theme(plot.title = element_text(face = "bold"),
          legend.position = "bottom", legend.title = element_blank(),
          axis.text.x = element_text(angle = 30, hjust = 1),
          panel.grid.minor = element_blank()) +
    labs(title    = paste("Cumulative % Growth (TRY-Denominated / Nominal) ???", label),
         subtitle = paste0("Base = 0% on ", format(min(pd$Date), "%d %b %Y"),
                           "  |  Unadjusted local-currency growth (pre-inflation/currency effects)"),
         x = "Date", y = "Cumulative Growth (%)")
}

# 9b. Log Return Time Series ??? Volatility & Shocks
plot_returns_fn <- function(rd, label) {
  r_c <- grep("^Ret_", colnames(rd), value = TRUE)
  long <- rd |>
    select(Date, all_of(r_c)) |>
    pivot_longer(-Date, names_to = "Asset", values_to = "LogRet") |>
    mutate(Asset = gsub("^Ret_", "", Asset))
  
  # Crisis event annotations: only draw if the event date falls in this window
  crisis_events <- tibble(
    event_date = as.Date(c("2018-08-10")),
    label      = c("2018 Currency Shock")
  ) |> filter(event_date >= min(rd$Date), event_date <= max(rd$Date))
  
  p <- ggplot(long, aes(Date, LogRet, color = Asset)) +
    geom_line(linewidth = 0.35, alpha = 0.8, show.legend = FALSE) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey60", linewidth = 0.3) +
    facet_wrap(~ Asset, ncol = 2, scales = "free_y") +
    scale_color_manual(values = PAL) +
    scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    theme_minimal(base_size = 10) +
    theme(plot.title = element_text(face = "bold"),
          strip.text = element_text(face = "bold", size = 8),
          axis.text.x = element_text(angle = 30, hjust = 1, size = 7),
          panel.grid.minor = element_blank()) +
    labs(title    = paste("Daily Volatility and Market Shocks ???", label),
         subtitle = "Volatility clustering and structural break events",
         x = "Date", y = "Log Return")
  
  # Overlay crisis vlines + labels only when relevant to this period
  if (nrow(crisis_events) > 0) {
    p <- p +
      geom_vline(data = crisis_events,
                 aes(xintercept = event_date),
                 color = "#C0392B", linetype = "dashed",
                 linewidth = 0.6, inherit.aes = FALSE) +
      geom_text(data = crisis_events,
                aes(x = event_date, y = Inf, label = label),
                inherit.aes = FALSE,
                hjust = -0.05, vjust = 1.4,
                size = 2.8, color = "#C0392B", fontface = "bold")
  }
  p
}

# 9c. Scatter ??? USD/TRY vs each BIST index, with outlier clipping + r/R?? annotation
plot_scatter_fn <- function(rd, label) {
  r_c    <- grep("^Ret_", colnames(rd), value = TRUE)
  u_col  <- r_c[grepl("USD.TRY", r_c)][1]
  b_cols <- r_c[r_c != u_col]
  
  # Dynamic x-axis limits: clip at 1st/99th percentile of USD/TRY returns
  x_lo <- quantile(rd[[u_col]], 0.01, na.rm = TRUE)
  x_hi <- quantile(rd[[u_col]], 0.99, na.rm = TRUE)
  
  plots <- lapply(b_cols, function(col) {
    lbl <- gsub("^Ret_", "", col)
    
    # Pearson r and R??
    r_val  <- cor(rd[[u_col]], rd[[col]], use = "complete.obs")
    r2_val <- r_val^2
    # "R^2" written as "R2" to avoid Windows UTF-8 encoding issues with superscripts
    annot  <- sprintf("r = %.3f\nR2 = %.3f", r_val, r2_val)
    
    # x-clipped data for geom_point (keeps all data for OLS fit)
    rd_clip <- rd |> filter(.data[[u_col]] >= x_lo, .data[[u_col]] <= x_hi)
    
    ggplot(rd, aes(x = .data[[u_col]], y = .data[[col]])) +
      # OLS uses full unclipped data
      geom_smooth(method = "lm", color = "#E74C3C",
                  se = TRUE, linewidth = 1, formula = y ~ x) +
      # Points clipped to focused range
      geom_point(data = rd_clip, alpha = 0.25, size = 0.7, color = "#2C3E50") +
      # r / R2 annotation in top-left corner
      # annotate("text") used instead of ("label") ??? label.size is not a valid
      # parameter for annotate(); using a background rect via geom is unnecessary here
      annotate("text",
               x = x_lo + (x_hi - x_lo) * 0.02,
               y = Inf,
               label = annot,
               hjust = 0, vjust = 1.3,
               size = 3, fontface = "bold",
               color = "#2C3E50") +
      coord_cartesian(xlim = c(x_lo, x_hi)) +
      theme_minimal(base_size = 10) +
      theme(plot.title = element_text(size = 9, face = "bold")) +
      labs(title = paste("USD/TRY vs", lbl),
           x = "USD/TRY Log Return (clipped 1%-99%)",
           y = paste(lbl, "Log Return"))
  })
  
  wrap_plots(plots, ncol = 2) +
    plot_annotation(
      title    = paste("USD/TRY vs BIST Indices ??? Scatter ???", label),
      subtitle = "OLS fit on full data  |  Points clipped to 1st-99th percentile  |  Red band = 95% CI",
      theme    = theme(plot.title    = element_text(face = "bold", size = 12),
                       plot.subtitle = element_text(color = "grey40", size = 9))
    )
}

# 9d. Faceted USD-denominated BIST vs USD/TRY Parity
plot_faceted_usd_fn <- function(pd, label) {
  p_cols  <- setdiff(colnames(pd), "Date")
  u_col   <- p_cols[grepl("USD/TRY", p_cols, fixed = TRUE)]
  b_cols  <- p_cols[p_cols != u_col]
  
  # USD-denominated BIST = TRY price / USD_TRY rate, then indexed to 100
  usd_bist <- pd |>
    mutate(across(all_of(b_cols),
                  ~ (. / .data[[u_col]]),
                  .names = "USD_{.col}")) |>
    select(Date, starts_with("USD_")) |>
    mutate(across(-Date, ~ (. / first(.)) * 100)) |>
    pivot_longer(-Date, names_to = "Asset", values_to = "Value") |>
    mutate(Asset = gsub("^USD_", "", Asset),
           Panel = "BIST Indices (USD-Denominated, Base = 100)")
  
  parity <- pd |>
    select(Date, all_of(u_col)) |>
    mutate(Value = (.data[[u_col]] / first(.data[[u_col]])) * 100,
           Asset = "USD/TRY",
           Panel = "USD/TRY Parity (Base = 100)")
  
  combined <- bind_rows(usd_bist, parity) |>
    mutate(Panel = factor(Panel, levels = c(
      "BIST Indices (USD-Denominated, Base = 100)",
      "USD/TRY Parity (Base = 100)"
    )))
  
  b_labels <- unique(usd_bist$Asset)
  a_colors <- c(setNames(unname(PAL[b_labels]), b_labels), "USD/TRY" = "#E74C3C")
  
  ggplot(combined, aes(Date, Value, color = Asset, group = Asset)) +
    geom_line(linewidth = 0.85, alpha = 0.9) +
    geom_hline(yintercept = 100, linetype = "dashed", color = "grey60", linewidth = 0.3) +
    facet_wrap(~ Panel, ncol = 1, scales = "free_y") +
    scale_color_manual(values = a_colors) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    theme_minimal(base_size = 11) +
    theme(plot.title       = element_text(face = "bold", size = 12),
          plot.subtitle    = element_text(color = "grey40", size = 9),
          strip.text       = element_text(face = "bold", size = 9),
          strip.background = element_rect(fill = "grey95", color = NA),
          legend.position  = "bottom", legend.title = element_blank(),
          axis.text.x      = element_text(angle = 30, hjust = 1),
          panel.grid.minor = element_blank(),
          panel.spacing    = unit(1, "lines")) +
    labs(title    = paste("Faceted USD vs. Parity Plot ???", label),
         subtitle = paste0("Base = 100 on ", format(min(pd$Date), "%d %b %Y"),
                           "  |  USD-denominated BIST vs USD/TRY parity"),
         x = "Date", y = "Indexed Value (Base = 100)")
}


# ?????? 10. MAIN LOOP: generate all 4 plots for each 5-year period ?????????????????????????????????????????????

cat("\n\nGenerating plots for each 5-year period...\n")
cat(rep("=", 60), "\n", sep = "")

for (per in PERIODS) {
  cat(sprintf("\n>> Period: %s  (%s to %s)\n",
              per$label, per$start, per$end))
  
  slc <- slice_period(price_data, return_data, per$start, per$end)
  pd  <- slc$prices
  rd  <- slc$returns
  
  if (nrow(pd) < 20) {
    cat("   Skipping ??? insufficient data (<20 rows)\n")
    next
  }
  
  cat(sprintf("   %d trading days in this window\n", nrow(pd)))
  
  # Per-period correlation matrix (printed to console)
  rc <- grep("^Ret_", colnames(rd), value = TRUE)
  cm <- cor(rd[, rc], use = "complete.obs")
  rownames(cm) <- colnames(cm) <- gsub("^Ret_", "", rownames(cm))
  cat(sprintf("   Correlation matrix (%s):\n", per$label))
  print(round(cm, 4))
  
  # Generate and print the 4 plot types
  print(plot_cumulative_fn(pd, per$label))
  print(plot_returns_fn(rd, per$label))
  print(plot_scatter_fn(rd, per$label))
  print(plot_faceted_usd_fn(pd, per$label))
  
  cat(sprintf("   Plots rendered for %s\n", per$label))
}

cat("\n", rep("=", 60), "\n", sep = "")
cat("Done. All periods processed.\n")

