#rq1 energy charts quick draft

#Install tidyverse when missing
req <- c("tidyverse")
new <- req[!req %in% installed.packages()[, "Package"]]
if (length(new)) install.packages(new, repos = "https://cloud.r-project.org")
library(tidyverse)

#paths you probably tweak if files move
path_bench <- "results_benchmarks.csv"     
path_apps  <- "maven_ant_jabref_results.csv"     
out_dir    <- "reportTemplate/figures/rq1viz"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

pick_col <- function(df, cands) {
  nms <- names(df); low <- tolower(nms)
  hit <- which(low %in% tolower(cands))
  if (length(hit)) nms[hit[1]] else NA_character_
}

load_one <- function(path) {
  raw <- readr::read_csv(path, show_col_types = FALSE)
  suite <- pick_col(raw, c("suite","category","workload_type"))
  bench <- pick_col(raw, c("bench","workload_name","program_id"))
  gc    <- pick_col(raw, c("gc","gc_flag","collector"))
  rep   <- pick_col(raw, c("rep","run_id"))
  energy<- pick_col(raw, c("energy_pkg_j","energy_j","pkg_energy_j"))

  tibble(
    suite     = if (!is.na(suite))  raw[[suite]]  else "unknown",
    workload  = if (!is.na(bench))  raw[[bench]]  else "unknown",
    gc_raw    = if (!is.na(gc))     raw[[gc]]     else "unknown",
    rep       = if (!is.na(rep))    raw[[rep]]    else seq_len(nrow(raw)),
    energy_j  = if (!is.na(energy)) as.numeric(raw[[energy]]) else NA_real_
  )
}

normalize_gc <- function(x) {
  s <- gsub("[^A-Za-z]", "", tolower(as.character(x)))
  s <- sub("^xx", "", s)  #handle "-XX:"
  dplyr::case_when(
    s %in% c("useserialgc","serial")     ~ "Serial",
    s %in% c("useparallelgc","parallel") ~ "Parallel",
    s %in% c("useg1gc","g1")             ~ "G1",
    s %in% c("usezgc","zgc")             ~ "ZGC",
    TRUE ~ as.character(x)
  )
}
workload_type_of <- function(suite) {
  ss <- tolower(as.character(suite))
  ifelse(ss %in% c("maven","ant","jabref"), "Application", "Benchmark")
}

df <- bind_rows(load_one(path_bench), load_one(path_apps)) |>
  mutate(
    gc            = normalize_gc(gc_raw),
    workload_type = workload_type_of(suite)
  ) |>
  filter(!is.na(energy_j), is.finite(energy_j))

#keep only these four gcs
gc_target  <- c("Serial","Parallel","G1","ZGC")
gc_present <- intersect(gc_target, unique(df$gc))
df <- df |> filter(gc %in% gc_present) |> mutate(gc = factor(gc, levels = gc_present))

gc_palette <- c(
  Serial = "#1F77B4",
  Parallel = "#FF7F0E",
  G1 = "#2CA02C",
  ZGC = "#9467BD"
)
workload_palette <- c(
  Benchmark = "#1E88E5",
  Application = "#D81B60"
)
workload_present <- intersect(names(workload_palette), unique(df$workload_type))

viz_theme <- function() {
  theme_minimal(base_size = 12) +
    theme(
      panel.grid.major = element_line(color = "#d0d5db", linewidth = 0.4),
      panel.grid.major.x = element_line(color = "#d0d5db", linewidth = 0.4),
      panel.grid.minor = element_line(color = "#e7ebf0", linewidth = 0.3),
      panel.grid.minor.x = element_line(color = "#e7ebf0", linewidth = 0.3),
      plot.title = element_text(face = "bold"),
      legend.position = "bottom",
      legend.title = element_text(face = "bold"),
      axis.title = element_text(face = "bold")
    )
}

#mean_ci helper keep it simple
mean_ci <- function(x, conf = 0.95) {
  x <- x[is.finite(x)]
  n <- length(x); m <- mean(x)
  if (n > 1) {
    s <- sd(x); tcrit <- qt(1 - (1 - conf)/2, df = n - 1); half <- tcrit * s / sqrt(n)
    tibble(mean = m, lo = m - half, hi = m + half, n = n)
  } else {
    tibble(mean = m, lo = NA_real_, hi = NA_real_, n = n)
  }
}

#Plot images drop in rq1viz
save_density <- function(wt) {
  g <- df |> filter(workload_type == wt)
  if (!nrow(g)) return(invisible(NULL))
  gc_cols <- gc_palette[gc_present]
  p <- ggplot(g, aes(x = energy_j, colour = gc, fill = gc)) +
    geom_density(linewidth = 0.9, alpha = 0.25, na.rm = TRUE) +
    labs(x = "Energy (J)", y = "Density", title = paste("Energy Distributions by GC —", wt)) +
    scale_x_continuous(labels = scales::label_comma(), breaks = scales::pretty_breaks(n = 7),
                       expand = expansion(mult = c(0.02, 0.05))) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 6), expand = expansion(mult = c(0, 0.05))) +
    scale_colour_manual(values = gc_cols, drop = FALSE, guide = guide_legend(title = "GC")) +
    scale_fill_manual(values = gc_cols, guide = "none") +
    viz_theme()
  fn <- file.path(out_dir, paste0("density_energy_", wt, ".png"))
  ggsave(fn, p, width = 7, height = 4.5, dpi = 300)
}
save_density("Benchmark")
save_density("Application")

save_box <- function(wt) {
  g <- df |> filter(workload_type == wt)
  if (!nrow(g)) return(invisible(NULL))
  gc_cols <- gc_palette[gc_present]
  p <- ggplot(g, aes(x = gc, y = energy_j, fill = gc)) +
    geom_boxplot(outlier.shape = NA, width = 0.65, alpha = 0.9, colour = "#424242", linewidth = 0.4) +
    geom_jitter(colour = "#424242", alpha = 0.35, width = 0.15, height = 0, size = 1.4) +
    labs(x = "GC", y = "Energy (J)", title = paste("Energy by GC —", wt)) +
    scale_fill_manual(values = gc_cols, drop = FALSE) +
    scale_y_continuous(labels = scales::label_comma(), breaks = scales::pretty_breaks(n = 6),
                       expand = expansion(mult = c(0.02, 0.08))) +
    viz_theme() +
    guides(fill = guide_legend(title = "GC"))
  fn <- file.path(out_dir, paste0("box_energy_", wt, ".png"))
  ggsave(fn, p, width = 7, height = 4.5, dpi = 300)
}
save_box("Benchmark")
save_box("Application")

summ_ci <- df |>
  group_by(workload_type, gc) |>
  summarise(mean_ci(energy_j), .groups = "drop")

save_means <- function(wt) {
  d <- summ_ci |> filter(workload_type == wt)
  if (!nrow(d)) return(invisible(NULL))
  gc_cols <- gc_palette[gc_present]
  p <- ggplot(d, aes(x = gc, y = mean)) +
    geom_line(group = 1, colour = "#636363", linewidth = 0.6) +
    geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.18, colour = "#636363") +
    geom_point(aes(colour = gc, fill = gc), size = 3.2, shape = 21, stroke = 0.5) +
    labs(x = "GC", y = "Mean Energy (J)", title = paste("Mean Energy with 95% CI —", wt)) +
    scale_colour_manual(values = gc_cols, drop = FALSE, guide = "none") +
    scale_fill_manual(values = gc_cols, drop = FALSE, guide = guide_legend(title = "GC")) +
    scale_y_continuous(labels = scales::label_comma(), breaks = scales::pretty_breaks(n = 6),
                       expand = expansion(mult = c(0.04, 0.08))) +
    viz_theme()
  fn <- file.path(out_dir, paste0("means_ci_", wt, ".png"))
  ggsave(fn, p, width = 7, height = 4.5, dpi = 300)
}
save_means("Benchmark")
save_means("Application")

means_wide <- df |>
  group_by(workload_type, gc) |>
  summarise(mean_energy = mean(energy_j), .groups = "drop")
p_inter <- ggplot(means_wide, aes(x = gc, y = mean_energy,
                                  colour = workload_type, group = workload_type)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 3) +
  labs(x = "GC", y = "Mean Energy (J)", title = "Interaction: GC × WorkloadType") +
  scale_colour_manual(values = workload_palette[workload_present],
                      drop = FALSE, guide = guide_legend(title = "Workload Type")) +
  scale_y_continuous(labels = scales::label_comma(),
                     breaks = scales::pretty_breaks(n = 6),
                     expand = expansion(mult = c(0.04, 0.08))) +
  scale_x_discrete(drop = FALSE) +
  viz_theme()
ggsave(file.path(out_dir, "interaction_gc_x_workloadtype.png"),
       p_inter, width = 7.5, height = 4.5, dpi = 300)

message("Saved to: ", normalizePath(out_dir))
message("GCs present in this run: ", paste(gc_present, collapse = ", "))
