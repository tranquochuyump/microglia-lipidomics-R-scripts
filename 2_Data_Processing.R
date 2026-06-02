pkgs <- c("dplyr", "ggplot2", "readxl")
need <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(need)) install.packages(need)
invisible(lapply(pkgs, library, character.only = TRUE))

# =========================
# USER SETTINGS
# =========================
input_file <- "~/Desktop/Master thesis/manuscript/Code/Data_Processing/POS_PreprocessedData_sample.csv"
raw_file_format <- "two_header_rows"

use_injection_file <- TRUE
inj_file <- "~/Desktop/Master thesis/manuscript/Code/Data_Processing/POS_InjectionOrder.xlsx"
inj_sheet <- 1
inj_skip_nrows <- 0
inj_sample_col <- "Sample"
inj_order_col  <- "InjectionOrder"

outdir <- "~/Desktop/Master thesis/manuscript/Code/Data_Processing/output"
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

blank_factor_threshold <- 4.0
bio_detection_threshold <- 0.50
qc_detection_threshold  <- 0.70
qc_rsd_threshold <- 30
d_ratio_threshold <- 50
top_n_loadings <- 30
min_qc_for_loess <- 5
loess_span <- 0.80
pseudo_count <- 1

# Draw plot before + after LOESS

feature_to_plot <- "843.5269__248.1"   # <-- choose m/z_RT wanted to see plot
plot_loess_diag <- TRUE               # <-- turn on/off the plot generated

# =========================
# HELPERS
# =========================
write_out <- function(x, name, quote = FALSE) {
  write.csv(x, file.path(outdir, name), row.names = FALSE, quote = quote)
}

read_data <- function(file, skip = 0, sheet = 1) {
  ext <- tolower(tools::file_ext(file))
  if (ext %in% c("csv", "txt")) {
    read.csv(file, skip = skip, check.names = FALSE, stringsAsFactors = FALSE)
  } else if (ext %in% c("xlsx", "xls")) {
    as.data.frame(readxl::read_excel(file, skip = skip, sheet = sheet), check.names = FALSE)
  } else {
    stop("Unsupported file format: ", ext)
  }
}

norm_text <- function(x) trimws(as.character(x))

norm_match <- function(x) {
  x <- basename(trimws(as.character(x)))
  x <- gsub("\\.(raw|mzml|mzxml|cdf)$", "", x, ignore.case = TRUE)
  x <- gsub("[[:space:]_\\-]+", "", x)
  toupper(x)
}

unique_label <- function(x, empty = "feature", sep = "__dup") {
  x <- trimws(as.character(x))
  x[is.na(x) | x == ""] <- empty
  make.unique(x, sep = sep)
}

calc_rsd <- function(x) {
  x <- as.numeric(x)
  x <- x[is.finite(x) & !is.na(x)]
  if (length(x) < 2 || mean(x) <= 0) return(NA_real_)
  100 * sd(x) / mean(x)
}

safe_median <- function(x) {
  x <- x[is.finite(x) & !is.na(x)]
  if (!length(x)) return(NA_real_)
  median(x)
}

is_present <- function(x) is.finite(x) & !is.na(x) & x > 0

impute_halfmin <- function(x) {
  x <- as.numeric(x)
  pos <- x[is_present(x)]
  if (!length(pos)) return(rep(NA_real_, length(x)))
  x[!is_present(x)] <- min(pos) / 2
  x
}

pareto_scale <- function(mat) {
  mu <- colMeans(mat, na.rm = TRUE)
  sdv <- apply(mat, 2, sd, na.rm = TRUE)
  sdv[!is.finite(sdv) | sdv == 0] <- 1
  sweep(sweep(mat, 2, mu, "-"), 2, sqrt(sdv), "/")
}

group_apply <- function(mat, info, groups, fun) {
  out <- lapply(groups, function(g) apply(mat[info$group == g, , drop = FALSE], 2, fun))
  out <- as.data.frame(out, check.names = FALSE)
  names(out) <- groups
  rownames(out) <- colnames(mat)
  out
}

get_best_group <- function(stat_df) {
  vals <- as.matrix(stat_df)
  idx <- apply(vals, 1, function(x) if (all(is.na(x))) NA_integer_ else which.max(x))
  out <- rep(NA_character_, nrow(stat_df))
  out[!is.na(idx)] <- colnames(stat_df)[idx[!is.na(idx)]]
  names(out) <- rownames(stat_df)
  out
}

loess_correct <- function(y, order, is_qc, span = 0.75, min_qc = 5) {
  y <- as.numeric(y)
  qc_idx <- which(is_qc & is_present(y))
  
  if (length(qc_idx) < min_qc) {
    return(list(corrected = y, pred = rep(NA_real_, length(y)), status = "too_few_qc"))
  }
  
  fit_df <- data.frame(ord = order[qc_idx], val = y[qc_idx])
  
  if (length(unique(fit_df$val)) < 3) {
    return(list(corrected = y, pred = rep(NA_real_, length(y)), status = "low_variation"))
  }
  
  fit <- try(
    loess(val ~ ord, data = fit_df, span = span, degree = 2,
          control = loess.control(surface = "direct")),
    silent = TRUE
  )
  if (inherits(fit, "try-error")) {
    return(list(corrected = y, pred = rep(NA_real_, length(y)), status = "loess_fail"))
  }
  
  pred <- try(predict(fit, newdata = data.frame(ord = order)), silent = TRUE)
  if (inherits(pred, "try-error") || all(is.na(pred))) {
    return(list(corrected = y, pred = rep(NA_real_, length(y)), status = "predict_fail"))
  }
  
  pred <- as.numeric(pred)
  
  if (anyNA(pred)) {
    ok <- which(!is.na(pred))
    pred <- if (length(ok) >= 2) {
      approx(order[ok], pred[ok], xout = order, rule = 2)$y
    } else {
      rep(median(fit_df$val, na.rm = TRUE), length(order))
    }
  }
  
  pred[!is.finite(pred) | pred <= 0] <- median(fit_df$val, na.rm = TRUE)
  target <- median(fit_df$val, na.rm = TRUE)
  
  list(corrected = y / pred * target, pred = pred, status = "corrected")
}

save_pca_plot <- function(scores, pca, file) {
  var_exp <- summary(pca)$importance[2, 1:2] * 100
  p <- ggplot(scores, aes(PC1, PC2, color = group, shape = sample_type)) +
    geom_point(size = 3, alpha = 0.9) +
    theme_bw(base_size = 12) +
    labs(
      title = "PCA scores plot",
      x = paste0("PC1 (", round(var_exp[1], 1), "%)"),
      y = paste0("PC2 (", round(var_exp[2], 1), "%)")
    )
  ggsave(file, p, width = 8, height = 6)
}

plot_feature_diag <- function(feat, info, raw_mat, corr_mat) {
  df <- transform(
    info,
    raw_intensity = raw_mat[, feat],
    corrected_intensity = corr_mat[, feat]
  )
  qc_df <- df[df$sample_type == "QC", , drop = FALSE]
  
  list(
    ggplot(df, aes(injection_order, raw_intensity, color = group)) +
      geom_point(size = 2) +
      theme_bw(11) +
      labs(title = paste("Raw:", feat), x = "Injection order", y = "Raw intensity"),
    
    ggplot(df, aes(injection_order, corrected_intensity, color = group)) +
      geom_point(size = 2) +
      theme_bw(11) +
      labs(title = paste("Corrected:", feat), x = "Injection order", y = "Corrected intensity"),
    
    ggplot(df, aes(sample_type, corrected_intensity, fill = sample_type)) +
      geom_boxplot(outlier.shape = NA) +
      geom_jitter(width = 0.15, alpha = 0.6, size = 1.8) +
      theme_bw(11) +
      labs(title = paste("Blank/QC/Sample:", feat), x = NULL, y = "Corrected intensity"),
    
    ggplot(qc_df, aes(injection_order, corrected_intensity)) +
      geom_point(size = 2, color = "purple") +
      geom_smooth(method = "loess", se = FALSE, color = "black") +
      theme_bw(11) +
      labs(title = paste("QC only:", feat), x = "Injection order", y = "Corrected intensity")
  )
}

sanitize_sample_id <- function(x) {
  x <- gsub("[^A-Za-z0-9_]", "_", as.character(x))
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)
  x[x == ""] <- "X"
  make.unique(x, sep = "_")
}

sanitize_feature_id <- function(x) {
  x <- trimws(as.character(x))
  x[is.na(x) | x == ""] <- "feature"
  x <- gsub("[^A-Za-z0-9_.]", "_", x)
  make.unique(x, sep = "__dup")
}

# =========================
# LOESS DIAGNOSTIC PLOT HELPERS
# =========================
calc_sd_band <- function(x) {
  x <- as.numeric(x)
  x <- x[is.finite(x) & !is.na(x)]
  if (!length(x)) {
    return(c(mean = NA_real_, plus2 = NA_real_, minus2 = NA_real_))
  }
  mu <- mean(x)
  s  <- if (length(x) >= 2) sd(x) else 0
  c(mean = mu, plus2 = mu + 2 * s, minus2 = mu - 2 * s)
}

add_sd_band_lines <- function(p, sample_stats, qc_stats) {
  ref_lines <- list(
    list(y = sample_stats["plus2"],  label = "+2SD (Sample)", color = "grey40", vjust = -0.3),
    list(y = sample_stats["minus2"], label = "-2SD (Sample)", color = "grey40", vjust =  1.3),
    list(y = qc_stats["plus2"],      label = "+2SD (QC)",     color = "red3",   vjust = -0.3),
    list(y = qc_stats["minus2"],     label = "-2SD (QC)",     color = "red3",   vjust =  1.3)
  )
  
  p <- p +
    scale_x_continuous(expand = expansion(mult = c(0.02, 0.18))) +
    coord_cartesian(clip = "off") +
    theme(
      plot.margin = margin(5.5, 95, 5.5, 5.5),
      panel.grid.minor = element_blank()
    )
  
  for (ln in ref_lines) {
    if (is.finite(ln$y)) {
      p <- p +
        geom_hline(yintercept = ln$y, linetype = "dashed", color = ln$color, linewidth = 0.5) +
        annotate(
          "text", x = Inf, y = ln$y, label = ln$label,
          hjust = -0.05, vjust = ln$vjust, size = 3.2, color = ln$color
        )
    }
  }
  
  p
}

plot_one_feature_loess <- function(feat, info, raw_mat, corr_mat, pred_mat = NULL,
                                   pseudo_count = 1, outdir = NULL) {
  if (!feat %in% colnames(raw_mat)) {
    stop("Feature not found after blank/prevalence filtering: ", feat)
  }
  if (!feat %in% colnames(corr_mat)) {
    stop("Feature not found in corrected matrix: ", feat)
  }
  
  df <- info
  df$raw_value  <- as.numeric(raw_mat[, feat])
  df$corr_value <- as.numeric(corr_mat[, feat])
  df$pred_value <- if (!is.null(pred_mat) && feat %in% colnames(pred_mat)) {
    as.numeric(pred_mat[, feat])
  } else {
    NA_real_
  }
  
  # Maintain the signals of QC and samples for the chosen feature
  df <- df[df$sample_type %in% c("Sample", "QC"), , drop = FALSE]
  df <- df[order(df$injection_order), , drop = FALSE]
  
  df$log2_raw  <- log2(df$raw_value  + pseudo_count)
  df$log2_corr <- log2(df$corr_value + pseudo_count)
  df$log2_pred <- log2(df$pred_value + pseudo_count)
  
  sample_stats_raw  <- calc_sd_band(df$log2_raw[df$sample_type == "Sample"])
  qc_stats_raw      <- calc_sd_band(df$log2_raw[df$sample_type == "QC"])
  sample_stats_corr <- calc_sd_band(df$log2_corr[df$sample_type == "Sample"])
  qc_stats_corr     <- calc_sd_band(df$log2_corr[df$sample_type == "QC"])
  
  # BEFORE LOESS
  p_before <- ggplot(df, aes(x = injection_order, y = log2_raw)) +
    geom_point(
      data = df[df$sample_type == "Sample", , drop = FALSE],
      color = "#6ECFF6", size = 2.8, alpha = 0.9
    ) +
    geom_point(
      data = df[df$sample_type == "QC", , drop = FALSE],
      color = "red3", shape = 15, size = 2.6
    ) +
    theme_bw(base_size = 11) +
    labs(
      title = paste0("Peak ", feat, " - Before LOESS Correction"),
      x = "Injection order",
      y = "log2(Peak Area)"
    )
  
  # Add modeled LOESS line
  if (any(is.finite(df$log2_pred))) {
    p_before <- p_before +
      geom_line(aes(y = log2_pred), color = "black", linewidth = 0.5, na.rm = TRUE)
  }
  
  p_before <- add_sd_band_lines(p_before, sample_stats_raw, qc_stats_raw)
  
  # AFTER LOESS
  p_after <- ggplot(df, aes(x = injection_order, y = log2_corr)) +
    geom_point(
      data = df[df$sample_type == "Sample", , drop = FALSE],
      color = "#6ECFF6", size = 2.8, alpha = 0.9
    ) +
    geom_point(
      data = df[df$sample_type == "QC", , drop = FALSE],
      color = "red3", shape = 15, size = 2.6
    ) +
    theme_bw(base_size = 11) +
    labs(
      title = paste0("Peak ", feat, " - After LOESS Correction"),
      x = "Injection order",
      y = "log2(Peak Area)"
    )
  
  p_after <- add_sd_band_lines(p_after, sample_stats_corr, qc_stats_corr)
  
  # Save file
  if (!is.null(outdir)) {
    feat_tag <- sanitize_feature_id(feat)
    
    ggsave(
      file.path(outdir, paste0("Peak_", feat_tag, "_Before_LOESS.png")),
      p_before, width = 9.5, height = 4.8, dpi = 300
    )
    ggsave(
      file.path(outdir, paste0("Peak_", feat_tag, "_After_LOESS.png")),
      p_after, width = 9.5, height = 4.8, dpi = 300
    )
    
    pdf(file.path(outdir, paste0("Peak_", feat_tag, "_Before_After_LOESS.pdf")),
        width = 10.5, height = 5.2)
    print(p_before)
    print(p_after)
    dev.off()
  }
  
  invisible(list(before = p_before, after = p_after, data = df))
}

# =========================
# READ RAW
# =========================
if (raw_file_format != "two_header_rows") stop("raw_file_format must be 'two_header_rows'.")

raw_full <- read.csv(input_file, header = FALSE, check.names = FALSE, stringsAsFactors = FALSE)
if (nrow(raw_full) < 3 || ncol(raw_full) < 2) {
  stop("Raw file must contain 2 header rows and at least 1 sample column.")
}

sample_idx <- 2:ncol(raw_full)
raw_data <- raw_full[-c(1, 2), , drop = FALSE]

feature_labels <- unique_label(raw_data[[1]])
sample_names_raw <- norm_text(raw_full[1, sample_idx])
sample_groups <- norm_text(raw_full[2, sample_idx])

if (any(sample_names_raw == "" | is.na(sample_names_raw))) stop("Empty sample names found in header row 1.")
if (any(sample_groups == "" | is.na(sample_groups))) stop("Empty group labels found in header row 2.")

raw_num <- raw_data[, sample_idx, drop = FALSE]
colnames(raw_num) <- sample_names_raw
raw_num[] <- lapply(raw_num, function(x) as.numeric(as.character(x)))

raw_mat <- t(as.matrix(raw_num))
rownames(raw_mat) <- sample_names_raw
colnames(raw_mat) <- feature_labels

# =========================
# METADATA + INJECTION ORDER
# =========================
sample_info <- data.frame(
  sample_id = sample_names_raw,
  raw_name = sample_names_raw,
  raw_name_match = norm_match(sample_names_raw),
  group = sample_groups,
  injection_order_default = seq_along(sample_names_raw),
  injection_order = seq_along(sample_names_raw),
  stringsAsFactors = FALSE
)

sample_info$sample_type <- ifelse(
  toupper(sample_info$group) == "QC", "QC",
  ifelse(toupper(sample_info$group) == "BLANK", "Blank", "Sample")
)

if (use_injection_file) {
  inj_raw <- read_data(inj_file, skip = inj_skip_nrows, sheet = inj_sheet)
  
  if (!all(c(inj_sample_col, inj_order_col) %in% names(inj_raw))) {
    stop("Cannot find injection columns in file.")
  }
  
  inj_map <- data.frame(
    Sample = norm_text(inj_raw[[inj_sample_col]]),
    InjectionOrder = as.numeric(as.character(inj_raw[[inj_order_col]])),
    stringsAsFactors = FALSE
  )
  inj_map <- inj_map[!is.na(inj_map$Sample) & inj_map$Sample != "", , drop = FALSE]
  inj_map$sample_match <- norm_match(inj_map$Sample)
  
  if (anyDuplicated(inj_map$sample_match)) {
    dup <- inj_map[duplicated(inj_map$sample_match) | duplicated(inj_map$sample_match, fromLast = TRUE), ]
    write_out(dup, "duplicated_sample_names_in_injection_file.csv")
    stop("Duplicate sample names found in injection file.")
  }
  
  if (anyDuplicated(inj_map$InjectionOrder)) {
    dup <- inj_map[duplicated(inj_map$InjectionOrder) | duplicated(inj_map$InjectionOrder, fromLast = TRUE), ]
    write_out(dup, "duplicated_injection_orders_in_injection_file.csv")
    stop("Duplicate injection orders found in injection file.")
  }
  
  hit <- match(sample_info$raw_name_match, inj_map$sample_match)
  
  if (anyNA(hit)) {
    miss <- sample_info[is.na(hit), ]
    write_out(miss, "samples_missing_injection_order.csv")
    stop("Some raw samples could not be matched to injection file.")
  }
  
  sample_info$injection_order <- inj_map$InjectionOrder[hit]
  
  extra_inj <- inj_map[!(inj_map$sample_match %in% sample_info$raw_name_match), ]
  if (nrow(extra_inj)) write_out(extra_inj, "injection_samples_not_found_in_raw.csv")
}

write_out(sample_info, "sample_metadata_auto.csv")
write_out(sample_info[order(sample_info$injection_order), ], "sample_metadata_sorted_by_injection_order.csv")

# =========================
# BASIC CHECKS
# =========================
message("Matrix: ", nrow(raw_mat), " samples x ", ncol(raw_mat), " features")
print(table(sample_info$sample_type, useNA = "ifany"))
print(table(sample_info$group, useNA = "ifany"))

if (!sum(sample_info$sample_type == "QC")) stop("No QC samples found.")
if (!sum(sample_info$sample_type == "Sample")) stop("No biological samples found.")
if (anyNA(sample_info$injection_order) || anyDuplicated(sample_info$injection_order)) {
  stop("Injection order contains NA or duplicates.")
}
if (!sum(sample_info$sample_type == "Blank")) warning("No Blank samples found. Blank filtering will be skipped.")
if (any(!is.finite(raw_mat), na.rm = TRUE)) warning("Non-finite values detected in raw matrix.")

idx_blank <- sample_info$sample_type == "Blank"
idx_qc <- sample_info$sample_type == "QC"
idx_bio <- sample_info$sample_type == "Sample"

# =========================
# BLANK + PREVALENCE FILTER
# =========================
qc_detect <- colMeans(is_present(raw_mat[idx_qc, , drop = FALSE]), na.rm = TRUE)

bio_groups <- sort(unique(sample_info$group[idx_bio]))
if (!length(bio_groups)) stop("No biological groups found.")

bio_info <- sample_info[idx_bio, , drop = FALSE]
bio_mat <- raw_mat[idx_bio, , drop = FALSE]

bio_med_by_group <- group_apply(bio_mat, bio_info, bio_groups, safe_median)
bio_detect_by_group <- group_apply(bio_mat, bio_info, bio_groups, function(x) mean(is_present(x), na.rm = TRUE))

bio_med_max <- apply(as.matrix(bio_med_by_group), 1, function(x) if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE))
bio_detect_max <- apply(as.matrix(bio_detect_by_group), 1, function(x) if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE))
best_group <- get_best_group(bio_med_by_group)

bio_detect_best <- setNames(rep(NA_real_, ncol(raw_mat)), colnames(raw_mat))
for (feat in names(bio_detect_best)) {
  g <- best_group[feat]
  if (!is.na(g)) bio_detect_best[feat] <- bio_detect_by_group[feat, g]
}

if (sum(idx_blank)) {
  blank_med <- apply(raw_mat[idx_blank, , drop = FALSE], 2, safe_median)
  blank_ratio <- bio_med_max / pmax(blank_med, 1e-12)
  blank_keep <- is.na(blank_med) | is.na(bio_med_max) | blank_ratio >= blank_factor_threshold
} else {
  blank_med <- rep(NA_real_, ncol(raw_mat))
  blank_ratio <- rep(NA_real_, ncol(raw_mat))
  blank_keep <- rep(TRUE, ncol(raw_mat))
}

prevalence_keep <- bio_detect_best >= bio_detection_threshold | qc_detect >= qc_detection_threshold
early_keep <- blank_keep & prevalence_keep

feature_qc_summary <- data.frame(
  label = colnames(raw_mat),
  blank_median = blank_med,
  bio_median_max = bio_med_max,
  blank_ratio = blank_ratio,
  best_group = best_group,
  bio_detect_best = bio_detect_best,
  bio_detect_max = bio_detect_max,
  qc_detection = qc_detect,
  keep_blank = blank_keep,
  keep_prevalence = prevalence_keep,
  keep_early = early_keep,
  stringsAsFactors = FALSE
)

write_out(feature_qc_summary, "feature_qc_summary_step1_blank_prevalence.csv")
write_out(transform(bio_med_by_group, label = rownames(bio_med_by_group))[, c("label", bio_groups)], "bio_median_by_group.csv")
write_out(transform(bio_detect_by_group, label = rownames(bio_detect_by_group))[, c("label", bio_groups)], "bio_detection_by_group.csv")
write_out(subset(feature_qc_summary, !keep_blank), "features_removed_by_blank_filter.csv")
write_out(subset(feature_qc_summary, !keep_prevalence), "features_removed_by_prevalence_filter.csv")
write_out(subset(feature_qc_summary, !keep_early), "features_removed_after_early_filters.csv")

raw_mat_f1 <- raw_mat[, early_keep, drop = FALSE]
if (ncol(raw_mat_f1) < 100) warning("Few features remain after early filtering: ", ncol(raw_mat_f1))

# =========================
# QC-LOESS CORRECTION
# =========================
corr_mat <- raw_mat_f1
pred_mat <- matrix(NA_real_, nrow(raw_mat_f1), ncol(raw_mat_f1), dimnames = dimnames(raw_mat_f1))
loess_status <- character(ncol(raw_mat_f1))

for (j in seq_len(ncol(raw_mat_f1))) {
  res <- loess_correct(
    y = raw_mat_f1[, j],
    order = sample_info$injection_order,
    is_qc = idx_qc,
    span = loess_span,
    min_qc = min_qc_for_loess
  )
  
  corr_mat[, j] <- res$corrected
  pred_mat[, j] <- res$pred
  loess_status[j] <- res$status
  
  if (j %% 500 == 0) message("LOESS: ", j, "/", ncol(raw_mat_f1))
}

loess_status_df <- data.frame(label = colnames(raw_mat_f1), loess_status = loess_status)
write_out(loess_status_df, "loess_status_by_feature.csv")
print(table(loess_status, useNA = "ifany"))

# =========================
# PLOT CHOSEN FEATURE BEFORE/AFTER LOESS DIAGNOSTIC
# =========================
if (plot_loess_diag) {
  loess_diag <- plot_one_feature_loess(
    feat = feature_to_plot,
    info = sample_info,
    raw_mat = raw_mat_f1,   
    corr_mat = corr_mat,   
    pred_mat = pred_mat,  
    pseudo_count = pseudo_count,
    outdir = outdir
  )
  
  print(loess_diag$before)
  print(loess_diag$after)
}

# =========================
# QC RSD FILTER
# =========================
qc_rsd_post <- apply(corr_mat[idx_qc, , drop = FALSE], 2, calc_rsd)
qc_rsd_keep <- !is.na(qc_rsd_post) & qc_rsd_post <= qc_rsd_threshold

bio_sd <- apply(corr_mat[idx_bio, , drop = FALSE], 2, sd, na.rm = TRUE)
qc_sd  <- apply(corr_mat[idx_qc, , drop = FALSE], 2, sd, na.rm = TRUE)
d_ratio <- 100 * qc_sd / pmax(bio_sd, 1e-12)

d_ratio_keep <- is.finite(d_ratio) & !is.na(d_ratio) & d_ratio <= d_ratio_threshold
post_qc_keep <- qc_rsd_keep & d_ratio_keep

feature_qc_summary2 <- data.frame(
  label = colnames(corr_mat),
  loess_status = loess_status,
  qc_rsd_post = qc_rsd_post,
  d_ratio = d_ratio,
  keep_qc_rsd = qc_rsd_keep,
  keep_d_ratio = d_ratio_keep,
  keep_post_qc = post_qc_keep,
  stringsAsFactors = FALSE
)

write_out(feature_qc_summary2, "feature_qc_summary_step2_qc_rsd_dratio.csv")
write_out(subset(feature_qc_summary2, !keep_qc_rsd), "features_removed_by_qc_rsd_filter.csv")
write_out(subset(feature_qc_summary2, !keep_d_ratio), "features_removed_by_d_ratio_filter.csv")
write_out(subset(feature_qc_summary2, !keep_post_qc), "features_removed_after_post_qc_filters.csv")

corr_mat_f2 <- corr_mat[, post_qc_keep, drop = FALSE]
raw_mat_f2  <- raw_mat_f1[, post_qc_keep, drop = FALSE]
pred_mat_f2 <- pred_mat[, post_qc_keep, drop = FALSE]

# =========================
# IMPUTE MISSING VALUES
# =========================
corr_mat_imp <- sapply(seq_len(ncol(corr_mat_f2)), function(j) impute_halfmin(corr_mat_f2[, j]))
corr_mat_imp <- as.matrix(corr_mat_imp)
rownames(corr_mat_imp) <- rownames(corr_mat_f2)
colnames(corr_mat_imp) <- colnames(corr_mat_f2)

if (anyNA(corr_mat_imp)) warning("NA values remain after imputation.")

# =========================
# SAVE MATRICES + REPORT
# =========================
write_out(
  cbind(sample_id = rownames(raw_mat), as.data.frame(raw_mat, check.names = FALSE)),
  "raw_matrix_samples_x_features.csv"
)

write_out(
  cbind(sample_id = rownames(corr_mat_f2), as.data.frame(corr_mat_f2, check.names = FALSE)),
  "corrected_matrix_filtered.csv"
)

final_report <- data.frame(
  metric = c(
    "n_samples_total",
    "n_blank",
    "n_qc",
    "n_bio",
    "n_features_raw",
    "n_features_after_early_filters",
    "n_features_after_qc_rsd",
    "blank_factor_threshold",
    "bio_detection_threshold",
    "qc_detection_threshold",
    "qc_rsd_threshold",
    "loess_span"
  ),
  value = c(
    nrow(raw_mat),
    sum(idx_blank),
    sum(idx_qc),
    sum(idx_bio),
    ncol(raw_mat),
    ncol(raw_mat_f1),
    ncol(corr_mat_f2),
    blank_factor_threshold,
    bio_detection_threshold,
    qc_detection_threshold,
    qc_rsd_threshold,
    loess_span
  )
)

write_out(final_report, "final_qc_report.csv")
writeLines(capture.output(sessionInfo()), file.path(outdir, "sessionInfo.txt"))

# =========================
# METABOANALYST EXPORT
# =========================
sample_info_ma <- sample_info
sample_info_ma$ma_sample_id <- sanitize_sample_id(sample_info_ma$sample_id)

sample_info_ma$ma_sample_id[idx_qc] <- paste0("QC_", seq_len(sum(idx_qc)))
sample_info_ma$ma_sample_id[idx_blank] <- paste0("BLANK_", seq_len(sum(idx_blank)))
sample_info_ma$ma_sample_id <- make.unique(sample_info_ma$ma_sample_id, sep = "_")

ma_feature_id <- sanitize_feature_id(colnames(corr_mat_f2))

feature_mapping_ma <- data.frame(
  original_label = colnames(corr_mat_f2),
  metaboanalyst_feature_id = ma_feature_id
)

write_out(feature_mapping_ma, "metaboanalyst_feature_mapping.csv")
write_out(sample_info_ma, "metaboanalyst_sample_mapping.csv")

ma_imp <- t(corr_mat_imp)
rownames(ma_imp) <- ma_feature_id
colnames(ma_imp) <- sample_info_ma$ma_sample_id
ma_imp_df <- data.frame(
  Feature = rownames(ma_imp),
  as.data.frame(ma_imp, check.names = FALSE),
  check.names = FALSE
)
write_out(ma_imp_df, "POS_DataProcessed_Sample.csv")

ma_metadata <- sample_info_ma[idx_bio, c("ma_sample_id", "group", "injection_order"), drop = FALSE]
colnames(ma_metadata) <- c("SampleID", "Group", "InjectionOrder")
write_out(ma_metadata, "metaboanalyst_metadata.csv")

if (anyDuplicated(ma_noimp_df$Feature)) warning("Duplicated feature IDs remain in MetaboAnalyst export.")
if (anyDuplicated(colnames(ma_noimp_df)[-1])) warning("Duplicated sample IDs remain in MetaboAnalyst export.")
if (any(ma_imp_df[, -1] <= 0, na.rm = TRUE)) warning("Non-positive values detected in imputed export.")
if (!all(ma_metadata$SampleID %in% colnames(ma_noimp_df)[-1])) {
  warning("Some metadata SampleID are missing from exported data.")
}

message("Done. Check folder: ", outdir)