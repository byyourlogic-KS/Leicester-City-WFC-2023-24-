# Player dashboard v1: squad page + player summary page. StatsBomb open data, women's leagues 2023/24 (NWSL 2023).
# Run scripts 11 to 14 first (they create data/players_master.csv, metric_registry.csv, metric_reliability.csv).
# Start from the project folder:  setwd("~/Desktop/i2i_dashboard_trial"); shiny::runApp("app_players.R")
# Needs: shiny, bslib, dplyr, tidyr, purrr, ggplot2, DT. Optional: ggrepel.

library(shiny); library(bslib); library(dplyr); library(tidyr); library(purrr); library(ggplot2); library(DT)
HAS_REPEL <- requireNamespace("ggrepel", quietly = TRUE)

# ---- Branding. Placeholder colours until the Leicester City Women kit hex codes are confirmed. Change them here only. ----
BRAND <- list(primary = "#003090", accent = "#FDBE11", mid = "#5D79B8", dark = "#1B1F2A",
              grey = "#8A90A0", light = "#EEF1F7", muted = "#C5CBDA")
APP_TITLE <- "Leicester City Women: player dashboard"
DEFAULT_TEAM <- "Leicester City WFC"
FOOTER <- "Created by Kannan Suresh. Illustrative mockup built on StatsBomb open data, not affiliated with Leicester City or the WSL."
if (file.exists("theme_leicester.R")) source("theme_leicester.R", local = TRUE)   # branding lives there

# ---- Fonts: only fonts already installed (no downloads). Body Avenir Next, headings DIN Alternate, with safe fallbacks. ----
pick_font <- function(cands) {
  have <- tryCatch(unique(systemfonts::system_fonts()$family), error = function(e) character(0))
  hit <- cands[cands %in% have]; if (length(hit)) hit[1] else "sans"
}
FONT_BODY <- pick_font(c("Avenir Next", "Helvetica Neue", "Helvetica", "Arial"))
FONT_HEAD <- pick_font(c("DIN Alternate", "Avenir Next Condensed", "Avenir Next", "Helvetica Neue"))
theme_lcw <- function(base_size = 13) {
  theme_minimal(base_size = base_size, base_family = FONT_BODY) +
    theme(text = element_text(colour = BRAND$dark), panel.grid.major = element_line(colour = "#E8ECF4"),
          plot.subtitle = element_text(colour = "#4A5163"), legend.text = element_text(colour = BRAND$dark))
}
for (g in c("text", "label")) ggplot2::update_geom_defaults(g, list(family = FONT_BODY))
try(ggplot2::update_geom_defaults("text_repel", list(family = FONT_BODY)), silent = TRUE)

# ---- Data ----
need <- c("data/players_master.csv", "data/metric_registry.csv", "data/metric_reliability.csv")
if (any(!file.exists(need))) stop("Missing: ", paste(need[!file.exists(need)], collapse = ", "),
                                  ". Run scripts 11 to 14 first, from this folder (now: ", getwd(), ").")
M   <- read.csv("data/players_master.csv", check.names = FALSE, stringsAsFactors = FALSE)
REG <- read.csv("data/metric_registry.csv", check.names = FALSE, stringsAsFactors = FALSE)
REL <- read.csv("data/metric_reliability.csv", check.names = FALSE, stringsAsFactors = FALSE)
if (!"domain_label" %in% names(REG)) REG$domain_label <- REG$domain
GROUPS <- c("CB", "FB", "MF", "AM", "W", "FW")
GROUP_NAMES <- c(CB = "Centre-back", FB = "Full-back", MF = "Midfielder", AM = "Attacking midfielder",
                 W = "Winger", FW = "Forward", GK = "Goalkeeper")
DOM_ORDER <- c("final", "shooting", "creation", "dribbling", "receiving", "progression", "passing", "aerial", "defending")
DOM <- REG %>% distinct(domain, domain_label); DOM <- DOM[match(DOM_ORDER, DOM$domain), ]; DOM <- DOM[!is.na(DOM$domain), ]
REG$domain <- factor(REG$domain, levels = DOM$domain)
LEAGUES <- sort(unique(M$league))
PCT_KEYS <- c(grep("pct$|share$", REG$key, value = TRUE), "drib_att_press", "rec_pressure_arriving", "rec_press_share",
              "rec_passer_press_share", "prog_press_share", "prog_own_half_share")
SC <- REG %>% filter(role != "context") %>% distinct(key, .keep_all = TRUE)
SC_CHOICES <- split(setNames(SC$key, SC$label), factor(SC$domain_label, levels = DOM$domain_label))
for (sub in c("players", "crests", "sponsors")) dir.create(file.path("assets", sub), recursive = TRUE, showWarnings = FALSE)
addResourcePath("assets", "assets")

`%||%` <- function(a, b) if (is.null(a)) b else a
fmt_num <- function(x) ifelse(is.na(x), "n/a", ifelse(abs(x) >= 100, sprintf("%.0f", x), ifelse(abs(x) >= 10, sprintf("%.1f", x), sprintf("%.2f", x))))
fmt_val <- function(key, v) ifelse(is.na(v), "n/a", ifelse(key %in% PCT_KEYS, sprintf("%.0f%%", 100 * v), fmt_num(v)))

# ---- Percentile and profile logic ----
pctl <- function(x, pool) {
  pool <- sort(pool[is.finite(pool)]); n <- length(pool)
  if (n < 20) return(rep(NA_real_, length(x)))
  lo <- findInterval(x, pool, left.open = TRUE); hi <- findInterval(x, pool)
  out <- 100 * (lo + hi) / 2 / n; out[!is.finite(x)] <- NA_real_; out
}
group_set <- function(pos, merge) if (isTRUE(merge) && pos %in% c("AM", "W")) c("AM", "W") else pos
tier_tbl <- function(gset) {
  REL %>% filter(pos_group %in% gset) %>% group_by(key, domain) %>%
    summarise(tier = if (any(tier == "D")) "D" else if (any(tier == "C")) "C" else if (any(tier == "B")) "B" else if (any(tier == "A")) "A" else tier[1],
              .groups = "drop") %>% mutate(domain = factor(domain, levels = DOM$domain))
}
build_profile <- function(p, merge, leagues, min_min) {
  gset <- group_set(p$pos_group, merge)
  pool <- M %>% filter(pos_group %in% gset, league %in% leagues, minutes >= min_min)
  d <- REG %>% left_join(tier_tbl(gset), by = c("key", "domain")) %>% mutate(tier = coalesce(tier, "n/a"))
  d$value <- vapply(d$key, function(k) as.numeric(p[[k]]), numeric(1))
  d$pct <- NA_real_
  for (i in which(d$role != "context")) {
    v <- pctl(d$value[i], pool[[d$key[i]]]); d$pct[i] <- if (d$dir[i] < 0) 100 - v else v
  }
  d$excluded <- d$role == "headline" & d$tier == "D"
  list(d = d, n_pool = nrow(pool), gset = gset)
}
bars_from <- function(d) {
  d %>% filter(role == "headline", !excluded) %>% group_by(domain) %>%
    summarise(n_ok = sum(!is.na(pct)), n_all = n(),
              bar = if (n_ok >= max(1, ceiling(n_all / 2))) mean(pct, na.rm = TRUE) else NA_real_, .groups = "drop")
}

# ---- Small visual helpers ----
# Finds assets/<folder>/<stem>.<png|jpg|jpeg|webp> and returns the file name, or NULL
find_asset <- function(folder, stem) {
  for (ext in c("png", "jpg", "jpeg", "webp", "svg")) {
    fn <- paste0(stem, ".", ext)
    if (file.exists(file.path("assets", folder, fn))) return(fn)
  }
  NULL
}
# Photos are found by player id first. For our own team they can also be named by surname or full name (Tierney.png).
norm_name <- function(x) tolower(gsub("[^A-Za-z]", "", iconv(x, to = "ASCII//TRANSLIT")))
find_player_photo <- function(id, name, team = NULL) {
  fn <- find_asset("players", id); if (!is.null(fn)) return(fn)
  if (!identical(team, DEFAULT_TEAM)) return(NULL)
  fs <- list.files(file.path("assets", "players"), pattern = "\\.(png|jpg|jpeg|webp)$", ignore.case = TRUE)
  if (!length(fs)) return(NULL)
  stems <- norm_name(sub("\\.[A-Za-z]+$", "", fs)); full <- norm_name(name); last <- norm_name(sub("^.* ", "", name))
  inside <- vapply(stems, function(s) nchar(s) >= 4 && grepl(s, full, fixed = TRUE), logical(1))
  hit <- which(stems == full | stems == last | inside)
  if (length(hit)) fs[hit[1]] else NULL
}
photo_tag <- function(id, name, size = 96, team = NULL) {
  fn <- find_player_photo(id, name, team)
  if (!is.null(fn)) {
    tags$img(src = paste0("assets/players/", fn), style = sprintf("height:%dpx;width:%dpx;border-radius:50%%;object-fit:cover;", size, size))
  } else {
    w <- strsplit(name, " ")[[1]]; ini <- toupper(paste0(substr(w[1], 1, 1), substr(w[length(w)], 1, 1)))
    div(ini, style = sprintf("height:%dpx;width:%dpx;border-radius:50%%;background:%s;color:white;display:flex;align-items:center;justify-content:center;font-weight:700;font-size:%dpx;",
                             size, size, BRAND$primary, round(size / 3)))
  }
}
crest_tag <- function(team, size = 40) {
  fn <- find_asset("crests", team)
  if (!is.null(fn)) tags$img(src = paste0("assets/crests/", utils::URLencode(fn)), style = sprintf("height:%dpx;", size)) else NULL
}
league_tag <- function(league, size = 40) {
  fn <- find_asset("leagues", league)
  if (!is.null(fn)) tags$img(src = paste0("assets/leagues/", utils::URLencode(fn)), style = sprintf("height:%dpx;", size)) else NULL
}
# Sponsor logos are normalised into a temp folder (originals untouched): trimmed, and the adidas JPEG's black box becomes transparent
# (brightness used as opacity, so the white logo stays white). King Power is shown as a white silhouette so it reads on the blue strip.
SPONSOR_DIR <- file.path(tempdir(), "sponsors_cache"); dir.create(SPONSOR_DIR, showWarnings = FALSE)
addResourcePath("sponsors_cache", SPONSOR_DIR)
prep_sponsor <- function(f) {
  im <- magick::image_read(file.path("assets", "sponsors", f)); inf <- magick::image_info(im)
  corner <- as.character(magick::image_data(magick::image_scale(im, "3x3!"), channels = "rgb")[, 1, 1])
  if (!isTRUE(inf$matte) && all(corner %in% c("00", "01", "02", "03", "04", "05"))) {
    gray <- magick::image_convert(im, colorspace = "Gray")
    im <- magick::image_composite(magick::image_blank(inf$width, inf$height, "white"), gray, operator = "CopyOpacity")
  }
  im <- magick::image_trim(im, fuzz = 6)
  dest <- paste0(tools::file_path_sans_ext(f), ".png"); magick::image_write(im, file.path(SPONSOR_DIR, dest), format = "png"); dest
}
sponsor_strip <- function() {
  fs <- list.files(file.path("assets", "sponsors"), pattern = "\\.(png|jpg|jpeg|webp|svg)$", ignore.case = TRUE)
  if (!length(fs)) return(NULL)
  div(class = "sponsor-strip",
      lapply(fs, function(f) {
        ok <- requireNamespace("magick", quietly = TRUE) && !inherits(try(dest <- prep_sponsor(f), silent = TRUE), "try-error")
        src <- if (ok) paste0("sponsors_cache/", utils::URLencode(dest)) else paste0("assets/sponsors/", utils::URLencode(f))
        tags$img(src = src, alt = tools::file_path_sans_ext(f), class = if (grepl("king", f, ignore.case = TRUE)) "sponsor-logo sponsor-white" else "sponsor-logo")
      }))
}
pitch_plot <- function(ax, ay, label) {
  lc <- BRAND$muted; th <- seq(0, 2 * pi, length.out = 100)
  ggplot() +
    annotate("rect", xmin = 0, xmax = 80, ymin = 0, ymax = 120, fill = "#F4F7F2", colour = lc) +
    annotate("segment", x = 0, xend = 80, y = 60, yend = 60, colour = lc) +
    annotate("path", x = 40 + 10 * cos(th), y = 60 + 10 * sin(th), colour = lc) +
    annotate("rect", xmin = 18, xmax = 62, ymin = 102, ymax = 120, fill = NA, colour = lc) +
    annotate("rect", xmin = 30, xmax = 50, ymin = 114, ymax = 120, fill = NA, colour = lc) +
    annotate("rect", xmin = 18, xmax = 62, ymin = 0, ymax = 18, fill = NA, colour = lc) +
    annotate("rect", xmin = 30, xmax = 50, ymin = 0, ymax = 6, fill = NA, colour = lc) +
    annotate("segment", x = 40, xend = 40, y = 66, yend = 78, colour = BRAND$muted, arrow = arrow(length = unit(0.15, "cm"))) +
    annotate("point", x = ay, y = ax, size = 6, colour = BRAND$primary) +
    annotate("text", x = ay, y = ax + 7, label = label, colour = BRAND$dark, fontface = "bold", size = 4) +
    coord_fixed(xlim = c(-2, 82), ylim = c(-2, 122), expand = FALSE) + theme_void(base_family = FONT_BODY)
}

# Square, transparent-padded PNG copies of every crest for ggimage (handles svg/webp and mislabelled extensions). Assets stay as they are.
CREST_PNG <- local({
  out <- character(0)
  if (requireNamespace("ggimage", quietly = TRUE) && requireNamespace("magick", quietly = TRUE)) {
    fs <- list.files(file.path("assets", "crests"), pattern = "\\.(png|jpg|jpeg|webp|svg)$", ignore.case = TRUE, full.names = TRUE)
    td <- file.path(tempdir(), "crest_png"); dir.create(td, showWarnings = FALSE)
    for (f in fs) try({
      im <- magick::image_trim(magick::image_read(f), fuzz = 8)
      im <- magick::image_scale(im, "160x160")
      im <- magick::image_extent(im, "160x160", gravity = "center", color = "none")
      dest <- file.path(td, paste0(tools::file_path_sans_ext(basename(f)), ".png")); magick::image_write(im, dest, format = "png")
      out[[tools::file_path_sans_ext(basename(f))]] <- dest
    }, silent = TRUE)
  }
  out
})
HAS_CRESTS <- length(CREST_PNG) > 0

# ---- Maps: pitch and data loader ----
maps_cache <- new.env()
get_maps <- function(lg) {
  f <- file.path("data", paste0("maps_", gsub("[^A-Za-z]", "", lg), ".rds"))
  if (!file.exists(f)) return(NULL)
  if (is.null(maps_cache[[lg]])) maps_cache[[lg]] <- readRDS(f)
  maps_cache[[lg]]
}
pitch_base <- function(half = FALSE, dark = FALSE) {
  fill <- if (dark) "#14213D" else "#F4F7F2"; lc <- if (dark) "#6B7A99" else "#B9C0CF"
  y0 <- if (half) 60 else 0; th <- seq(0, 2 * pi, length.out = 100)
  a <- seq(0, 2 * pi, length.out = 200); ax <- 108 + 10 * cos(a); ay <- 40 + 10 * sin(a); keep <- ax <= 102
  g <- ggplot() +
    annotate("rect", xmin = 0, xmax = 80, ymin = y0, ymax = 120, fill = fill, colour = lc) +
    annotate("rect", xmin = 18, xmax = 62, ymin = 102, ymax = 120, fill = NA, colour = lc) +
    annotate("rect", xmin = 30, xmax = 50, ymin = 114, ymax = 120, fill = NA, colour = lc) +
    annotate("rect", xmin = 36, xmax = 44, ymin = 120, ymax = 122, fill = NA, colour = lc) +
    annotate("path", x = ay[keep], y = ax[keep], colour = lc) +
    annotate("point", x = 40, y = 108, size = 0.8, colour = lc)
  if (!half) {
    g <- g + annotate("segment", x = 0, xend = 80, y = 60, yend = 60, colour = lc) +
      annotate("path", x = 40 + 10 * cos(th), y = 60 + 10 * sin(th), colour = lc) +
      annotate("rect", xmin = 18, xmax = 62, ymin = 0, ymax = 18, fill = NA, colour = lc) +
      annotate("rect", xmin = 30, xmax = 50, ymin = 0, ymax = 6, fill = NA, colour = lc) +
      annotate("rect", xmin = 36, xmax = 44, ymin = -2, ymax = 0, fill = NA, colour = lc)
  }
  g + coord_fixed(xlim = c(-3, 83), ylim = c(if (half) 58 else -3, 124), expand = FALSE) + theme_void(base_family = FONT_BODY) +
    theme(plot.title = element_text(face = "bold", colour = BRAND$dark, size = 14), plot.subtitle = element_text(colour = "#4A5163", size = 12, margin = margin(b = 6)),
          plot.background = element_rect(fill = "white", colour = NA))
}
arrow_head <- arrow(length = unit(0.14, "cm"), type = "closed")

team_display <- function(t) trimws(sub("\\s+(WFC|FCW|LFC|W|Women)$", "", t))
fmt_formation <- function(f) vapply(f, function(x) paste(strsplit(x, "")[[1]], collapse = "-"), character(1), USE.NAMES = FALSE)
LEAGUE_FULL <- c(WSL = "Women's Super League", `Frauen-Bundesliga` = "Frauen-Bundesliga", `Liga F` = "Liga F", `Serie A Women` = "Serie A Women", NWSL = "NWSL")
season_label <- function(lg) {
  f <- file.path("data", paste0("match_dates_", gsub("[^A-Za-z]", "", lg), ".rds")); if (!file.exists(f)) return("")
  d <- readRDS(f)$match_date; y1 <- format(min(d), "%Y"); y2 <- format(max(d), "%Y")
  if (y1 == y2) y1 else paste0(y1, "/", substr(y2, 3, 4))
}
ord_suffix <- function(n) paste0(n, ifelse(n %% 100 %in% 11:13, "th", c("th", "st", "nd", "rd", "th", "th", "th", "th", "th", "th")[n %% 10 + 1]))
spark_svg <- function(v, w = 130, h = 34) {
  v <- v[is.finite(v)]; if (length(v) < 3) return(NULL)
  r <- range(v); if (diff(r) == 0) r <- r + c(-1, 1)
  xs <- seq(2, w - 2, length.out = length(v)); ys <- h - 3 - (v - r[1]) / diff(r) * (h - 6)
  pts <- paste(sprintf("%.1f,%.1f", xs, ys), collapse = " ")
  HTML(sprintf('<svg viewBox="0 0 %d %d" width="%d" height="%d"><polyline points="%s" fill="none" stroke="%s" stroke-width="2.2" stroke-linejoin="round" stroke-linecap="round"/><circle cx="%.1f" cy="%.1f" r="3" fill="%s"/></svg>',
               w, h, w, h, pts, BRAND$primary, tail(xs, 1), tail(ys, 1), BRAND$accent))
}
CMP_CHOICES <- {
  d <- M %>% filter(pos_group %in% GROUPS, minutes > 0) %>% arrange(league, team, player)
  setNames(paste(d$league, d$player.id, sep = "|"), paste0(d$player, " (", team_display(d$team), ", ", d$pos_group, ", ", d$league, ")"))
}
LINEUP_XY <- list(
  "Goalkeeper" = c(40, 8), "Right Back" = c(68, 28), "Right Center Back" = c(53, 24), "Center Back" = c(40, 24), "Left Center Back" = c(27, 24), "Left Back" = c(12, 28),
  "Right Wing Back" = c(72, 46), "Left Wing Back" = c(8, 46), "Right Defensive Midfield" = c(52, 46), "Center Defensive Midfield" = c(40, 46), "Left Defensive Midfield" = c(28, 46),
  "Right Midfield" = c(70, 62), "Right Center Midfield" = c(53, 60), "Center Midfield" = c(40, 60), "Left Center Midfield" = c(27, 60), "Left Midfield" = c(10, 62),
  "Right Attacking Midfield" = c(53, 80), "Center Attacking Midfield" = c(40, 80), "Left Attacking Midfield" = c(27, 80), "Right Wing" = c(68, 92), "Left Wing" = c(12, 92),
  "Right Center Forward" = c(50, 100), "Center Forward" = c(40, 102), "Left Center Forward" = c(30, 100), "Secondary Striker" = c(40, 92))
shape_of <- function(pos) {
  pos <- pos[pos != "Goalkeeper"]; d <- sum(grepl("Back", pos)); a <- sum(grepl("Wing$|Forward|Striker", pos)); paste(d, length(pos) - d - a, a, sep = "-")
}
zone_of <- function(ex, ey) {
  z <- rep("Other", length(ex))
  z[ex >= 94 & ex < 102 & ey >= 18 & ey <= 62] <- "Edge of the box"
  z[ex >= 102 & ey >= 18 & ey < 30] <- "Box, left"; z[ex >= 102 & ey > 50 & ey <= 62] <- "Box, right"
  z[ex >= 102 & ex < 114 & ey >= 30 & ey <= 50] <- "Penalty spot area"; z[ex >= 114 & ey >= 30 & ey <= 50] <- "Six yard box"
  factor(z, levels = c("Six yard box", "Penalty spot area", "Box, left", "Box, right", "Edge of the box", "Other"))
}
radar_overlay <- function(va, vb, na, nb) {
  n <- nrow(DOM); a <- 2 * pi * (seq_len(n) - 1) / n
  poly <- function(v, who) tibble(x = ifelse(is.na(v), 0, v) / 100 * sin(a), y = ifelse(is.na(v), 0, v) / 100 * cos(a), who = who)
  dd <- bind_rows(poly(va, na), poly(vb, nb)); cols <- setNames(c(BRAND$primary, BRAND$accent), c(na, nb))
  ring <- function(r) { t <- seq(0, 2 * pi, length.out = 200); tibble(x = r * sin(t), y = r * cos(t)) }
  lab <- tibble(x = 1.22 * sin(a), y = 1.22 * cos(a), l = gsub(" ", "\n", DOM$domain_label, fixed = TRUE))
  ggplot() + geom_path(data = ring(0.25), aes(x, y), colour = "#E3E6EE") + geom_path(data = ring(0.75), aes(x, y), colour = "#E3E6EE") + geom_path(data = ring(1), aes(x, y), colour = BRAND$muted) +
    geom_path(data = ring(0.5), aes(x, y), colour = "#9AA3B5", linewidth = 0.8, linetype = "dashed") + geom_segment(data = tibble(a = a), aes(x = 0, y = 0, xend = sin(a), yend = cos(a)), colour = "#E3E6EE") +
    geom_polygon(data = dd, aes(x, y, fill = who, colour = who), alpha = 0.2, linewidth = 1.3) + geom_point(data = dd, aes(x, y, colour = who), size = 2.8) +
    geom_text(data = lab, aes(x, y, label = l), size = 3.8, colour = BRAND$dark, lineheight = 0.9) + scale_fill_manual(values = cols, name = NULL) + scale_colour_manual(values = cols, name = NULL) +
    coord_fixed(xlim = c(-1.6, 1.6), ylim = c(-1.45, 1.45)) + theme_void(base_size = 13, base_family = FONT_BODY) + theme(legend.position = "bottom")
}
CMP_META <- {
  d <- M %>% filter(pos_group %in% GROUPS, minutes > 0)
  tibble(key = paste(d$league, d$player.id, sep = "|"), pos_group = d$pos_group, minutes = d$minutes, league = d$league, player.id = d$player.id)
}
role_family <- function(g) ifelse(g %in% c("AM", "W"), "AM/W", g)   # wingers and attacking midfielders are compared together
# Segmented "pill" toggle: a radio group styled by CSS (.pill-radio)
pills <- function(inputId, label, choices, selected = NULL) {
  tagAppendAttributes(radioButtons(inputId, label, choices, selected = selected, inline = TRUE), class = "pill-radio")
}
nav_icon <- function(kind) {
  d <- switch(kind,
    overview = '<path d="M12 3l2.7 5.6 6.1.9-4.4 4.3 1 6.1L12 17l-5.4 2.9 1-6.1L3.2 9.5l6.1-.9z"/>',
    profile = '<path d="M4 20V10M10 20V4M16 20v-7M22 20V8"/>',
    compare = '<path d="M7 7h12M15 3l4 4-4 4M17 17H5M9 13l-4 4 4 4"/>',
    matchrep = '<rect x="5" y="4" width="14" height="17" rx="2"/><path d="M9 4v2h6V4M9 11h6M9 15h6"/>',
    setp   = '<path d="M5 21V4M5 4h11l-2 4 2 4H5"/>',
    similar = '<circle cx="10" cy="10" r="6"/><path d="M15 15l5 5"/>',
    squad  = '<rect x="3" y="3" width="7" height="7" rx="1.5"/><rect x="14" y="3" width="7" height="7" rx="1.5"/><rect x="3" y="14" width="7" height="7" rx="1.5"/><rect x="14" y="14" width="7" height="7" rx="1.5"/>',
    player = '<circle cx="12" cy="8" r="4"/><path d="M4 21c0-4.4 3.6-7 8-7s8 2.6 8 7"/>',
    maps   = '<rect x="3" y="4" width="18" height="16" rx="2"/><path d="M3 12h18"/><circle cx="12" cy="12" r="3"/>',
    team   = '<circle cx="9" cy="8" r="3"/><circle cx="17" cy="9" r="2.5"/><path d="M3 20c0-3.5 2.7-6 6-6s6 2.5 6 6"/><path d="M15 14.2c3 0 6 1.8 6 5.8"/>',
    about  = '<circle cx="12" cy="12" r="9"/><path d="M12 11v6"/><circle cx="12" cy="7.5" r=".6"/>')
  HTML(sprintf('<svg viewBox="0 0 24 24" stroke-linecap="round" stroke-linejoin="round">%s</svg>', d))
}
side_nav <- function() {
  item <- function(page, label, kind, active = FALSE)
    tags$a(class = paste("side-link", if (active) "active"), `data-page` = page, href = "#", nav_icon(kind), span(label))
  div(class = "side-nav", item("Overview", "Overview", "overview", TRUE), item("Team profile", "Team profile", "profile"), item("Squad", "Squad", "squad"), item("Team maps", "Team maps", "team"), item("Match report", "Match report", "matchrep"), item("Set pieces", "Set pieces", "setp"), item("Player", "Player", "player"), item("Compare", "Compare", "compare"), item("Similar players", "Similar players", "similar"), item("Maps", "Maps", "maps"), item("About", "About", "about"))
}
side_js <- "
$(document).on('click', '.side-link', function(e) { e.preventDefault(); var v = $(this).data('page'); $('.navbar a.nav-link[data-value=\"' + v + '\"]')[0].click(); });
$(document).on('shown.bs.tab', 'a[data-bs-toggle=\"tab\"][data-value]', function(e) {
  var v = $(e.target).data('value'); $('.side-link').removeClass('active').filter('[data-page=\"' + v + '\"]').addClass('active'); });
"

# Share-as-image player card (PNG): header band, radar, area scores
square_photo <- function(fn) {
  im <- magick::image_read(file.path("assets", "players", fn)); inf <- magick::image_info(im); sz <- min(inf$width, inf$height)
  im <- tryCatch(magick::image_crop(im, magick::geometry_area(sz, sz), gravity = "north"), error = function(e) im)
  im <- magick::image_scale(im, "300x300!")
  as.raster(im)
}
make_card <- function(p, pr, pool_note) {
  b <- bars_from(pr$d); v <- b$bar[match(DOM$domain, b$domain)]; n <- nrow(DOM); vv <- ifelse(is.na(v), 0, v)
  cx <- 4.4; cy <- 3.35; R <- 2.3; a <- 2 * pi * (seq_len(n) - 1) / n
  ring <- function(r) { t <- seq(0, 2 * pi, length.out = 200); tibble(x = cx + R * r * sin(t), y = cy + R * r * cos(t)) }
  poly <- tibble(x = cx + R * vv / 100 * sin(a), y = cy + R * vv / 100 * cos(a))
  lab <- tibble(x = cx + 1.22 * R * sin(a), y = cy + 1.2 * R * cos(a), l = gsub(" ", "\n", DOM$domain_label, fixed = TRUE))
  rows <- tibble(l = DOM$domain_label, v = v, y = 6.2 - (seq_len(n) - 1) * 0.58)
  fn <- find_player_photo(p$player.id, p$player, p$team)
  crest <- CREST_PNG[p$team]
  g <- ggplot() +
    annotate("rect", xmin = 0, xmax = 16, ymin = 7.1, ymax = 9, fill = BRAND$primary) +
    annotate("rect", xmin = 0, xmax = 16, ymin = 7.02, ymax = 7.1, fill = BRAND$accent) +
    geom_path(data = ring(0.25), aes(x, y), colour = "#E3E6EE") + geom_path(data = ring(0.75), aes(x, y), colour = "#E3E6EE") +
    geom_path(data = ring(1), aes(x, y), colour = BRAND$muted) +
    geom_path(data = ring(0.5), aes(x, y), colour = BRAND$accent, linewidth = 1, linetype = "dashed") +
    geom_segment(data = tibble(a = a), aes(x = cx, y = cy, xend = cx + R * sin(a), yend = cy + R * cos(a)), colour = "#E3E6EE") +
    geom_polygon(data = poly, aes(x, y), fill = BRAND$primary, colour = BRAND$primary, alpha = 0.25, linewidth = 1.3) +
    geom_point(data = poly, aes(x, y), colour = BRAND$primary, size = 3.2) +
    geom_text(data = lab, aes(x, y, label = l), size = 3.6, colour = BRAND$dark, lineheight = 0.9, family = FONT_BODY) +
    annotate("text", x = cx, y = 6.78, label = "PROFILE SHAPE", size = 4.2, fontface = "bold", colour = BRAND$primary, family = FONT_HEAD) +
    annotate("text", x = 9.0, y = 6.78, label = "AREA SCORES (PERCENTILE)", size = 4.2, fontface = "bold", colour = BRAND$primary, hjust = 0, family = FONT_HEAD) +
    geom_rect(data = rows, aes(xmin = 11.3, xmax = 11.3 + 3.6, ymin = y - 0.19, ymax = y + 0.19), fill = "#EEF1F7") +
    geom_rect(data = rows %>% filter(!is.na(v)), aes(xmin = 11.3, xmax = 11.3 + 3.6 * v / 100, ymin = y - 0.19, ymax = y + 0.19), fill = BRAND$primary) +
    geom_text(data = rows, aes(x = 11.15, y = y, label = l), hjust = 1, size = 3.9, colour = BRAND$dark, family = FONT_BODY) +
    geom_text(data = rows, aes(x = 15.05, y = y, label = ifelse(is.na(v), "n/a", sprintf("%.0f", v))), hjust = 0, size = 4, fontface = "bold", colour = BRAND$primary, family = FONT_HEAD) +
    annotate("segment", x = 11.3 + 1.8, xend = 11.3 + 1.8, y = 1.25, yend = 6.5, colour = BRAND$accent, linetype = "dashed", linewidth = 0.8) +
    annotate("text", x = 0.5, y = 0.35, label = paste0(pool_note, ". Gold ring and dashed line: 50th percentile, the average player in the pool. StatsBomb open data, illustrative mockup."),
             hjust = 0, size = 3, colour = BRAND$grey, family = FONT_BODY) +
    annotate("text", x = 2.35, y = 8.15, label = p$player, hjust = 0, size = 11, fontface = "bold", colour = "white", family = FONT_HEAD) +
    annotate("text", x = 2.35, y = 7.55, label = paste0(unname(GROUP_NAMES[p$pos_group]), "  |  ", p$team, "  |  ", round(p$minutes), " minutes, ", p$matches, " matches"),
             hjust = 0, size = 4.6, colour = "#DCE6F6", family = FONT_BODY) +
    coord_fixed(xlim = c(0, 16), ylim = c(0, 9), expand = FALSE) + theme_void(base_family = FONT_BODY) +
    theme(plot.background = element_rect(fill = "white", colour = NA))
  if (!is.null(fn)) g <- g + annotate("rect", xmin = 0.5, xmax = 2.1, ymin = 7.3, ymax = 8.9, fill = BRAND$accent) +
      annotation_raster(square_photo(fn), xmin = 0.56, xmax = 2.04, ymin = 7.36, ymax = 8.84, interpolate = TRUE)
  else g <- g + annotate("point", x = 1.3, y = 8.1, size = 26, colour = "white") +
    annotate("text", x = 1.3, y = 8.1, label = toupper(paste0(substr(strsplit(p$player, " ")[[1]][1], 1, 1), substr(tail(strsplit(p$player, " ")[[1]], 1), 1, 1))), size = 11, colour = BRAND$primary, fontface = "bold", family = FONT_HEAD)
  if (!is.na(crest)) g <- g + annotation_raster(as.raster(magick::image_read(crest)), xmin = 14.1, xmax = 15.6, ymin = 7.3, ymax = 8.8, interpolate = TRUE)
  g
}


# ---- About page: definitions, references, acknowledgements ----
cite <- function(...) { n <- c(...); tags$sup(class = "cite", lapply(seq_along(n), function(i) tagList(if (i > 1) ",", tags$a(href = paste0("#ref-", n[i]), n[i])))) }
GLOSS <- list(
  list(g = "Chances and shooting", t = "xG (expected goals)", d = "The chance that a shot becomes a goal, judged from where and how it was taken.",
       b = "StatsBomb's own model, supplied with every shot in the open data. We use it as given and do not retrain it. The idea of rating shots by quality rather than counting them is the same one behind published shot models.", r = c(1, 12)),
  list(g = "Chances and shooting", t = "npxG", d = "xG with penalties removed.", b = "Sum of xG over all non-penalty shots."),
  list(g = "Chances and shooting", t = "xA (expected assists)", d = "The xG of the shot that followed a player's pass.", b = "Taken from the shot's linked key pass. Includes set piece passes unless the label says open play."),
  list(g = "Chances and shooting", t = "xGOT / npxGOT (post-shot xG)", d = "How likely an on-target shot is to be a goal once you know where in the goal it ended up. Rewards placement, not only the chance. Some providers call this post-shot xG (PSxG).",
       b = "Our own model: gradient boosted trees trained on every goal and saved shot in the five leagues (penalties excluded). It uses where the ball ended in the goal, the pre-shot xG, shot position, distance, body part and shot type. Fitted in 5 folds by match so no shot is scored by a model that saw its own match. Placement gain is xGOT minus xG on shots on target. We do not cite a single paper for the idea; this is our own model.", r = 10),
  list(g = "Chances and shooting", t = "Big chance", d = "A clear-cut opportunity.", b = "This dashboard's own rule: an open play shot with xG of 0.33 or more. It is not the StatsBomb or Opta definition."),
  list(g = "Possession value", t = "xT (expected threat)", d = "How dangerous it is to have the ball in each part of the pitch. A pass or carry earns the difference in value between where it ends and where it starts.",
       b = "The pitch is cut into a 16 by 12 grid. Each cell's value is the chance of scoring within the next few actions, found by repeating the move-or-shoot probabilities five times, fitted from open play passes, carries and shots in all five leagues. Defensive actions and context (opponents, space) are not included.", r = c(2, 3)),
  list(g = "Possession value", t = "VAEP-style value (as used here)", d = "A value for every on-ball action: how much it changes the chance that the team scores and the chance that it concedes soon after.",
       b = "A VAEP-style adaptation, not the original. Each action is scored on how it changes the modelled xG of shots by both teams in the next 10 actions. The original labels goals; we label shot xG, which is less noisy. After a shot the value resets to zero, because the chance has already been counted (otherwise the defending team would be credited for removing it). Gradient boosted trees, fitted in 5 folds by match.", r = c(4, 5, 10)),
  list(g = "Possession value", t = "Progressive pass / carry", d = "A move that takes the ball clearly towards goal.", b = "Our rule: a completed open play pass, or a carry of at least 5 m, that finishes at least 25% closer to the opposition goal than it started. Other sites (FBref, for example) use different rules, so counts are not comparable across sites."),
  list(g = "Possession value", t = "Deep zone involvement", d = "How often a player is part of moves that reach the area near the opposition goal.",
       b = "A move counts when it gets within 20 yards of the goal centre through a completed open play pass or a carry. A player is involved if he or she touches the ball in such a move. The share is the player's involvements divided by the team's. The idea is similar to the \"deep completion\" statistic data providers publish; ours counts moves (possessions), not individual passes."),
  list(g = "Passing", t = "xPass and passes completed over expected", d = "How hard a pass was, and whether a player completes more than a typical player would from the same situation.",
       b = "Chance that a pass is completed from its start and end position, length, height, pressure and so on. Over expected is completed minus expected, per 100 passes, shrunk towards zero by adding 200 passes of prior weight so small samples do not look extreme. Gradient boosted trees, fitted in 5 folds by match. Related to, but not the same as, the risk and reward idea in the paper cited; ours is an event-based completion probability.", r = c(6, 10)),
  list(g = "Passing", t = "Share of passes", d = "A stand-in for possession.", b = "The team's passes divided by both teams' passes in the same matches. True possession time is not in the open data."),
  list(g = "Defending and pressing", t = "PPDA (passes per defensive action)", d = "How many passes the opponent is allowed before we make a defensive action. Lower means a more intense press.",
       b = "Opponent open play passes in their own 60% of the pitch, divided by our tackles, interceptions and fouls in that area. Interpret with care: it measures intensity, not success.", r = 7),
  list(g = "Defending and pressing", t = "PAdj (possession adjusted)", d = "Defensive counts evened out for how much of the ball a team has.",
       b = "Counts are multiplied by 0.5 divided by the opponent's share of possession time, so teams are compared as if possession were 50 to 50. This is a widely used convention among analysts with event data; we do not have a single original source for it. Aerial duels are not adjusted."),
  list(g = "Defending and pressing", t = "High regain", d = "Winning the ball back far up the pitch.", b = "A ball recovery, interception or tackle won in the final 40% of the pitch (72 yards or more from our goal). We also check whether a shot follows within 15 seconds."),
  list(g = "Defending and pressing", t = "Defensive line height", d = "How far from our own goal we typically defend.", b = "Average distance from our goal line of tackles, interceptions, recoveries, blocks and clearances."),
  list(g = "Defending and pressing", t = "Turnover", d = "Losing the ball.", b = "A failed open play pass, a lost take-on, or a miscontrol or dispossession."),
  list(g = "Defending and pressing", t = "Defensive responsibility index", d = "How much defending a player does compared with a typical player in the same position group.", b = "Defensive points per 90 (possession adjusted, with editable weights in script 14) divided by the group median. 100 is the median."),
  list(g = "Ratings and reading the charts", t = "Percentile", d = "Where a player sits in the comparison pool. 50 is the average player, 90 is better than 90% of the pool.", b = "Share of the pool with a lower value, ties counted as half. The bars follow the style of public scouting reports. The pool is players in the same position group with at least the chosen minutes. Where lower is better (for example being dribbled past) the scale is flipped."),
  list(g = "Ratings and reading the charts", t = "Area bar", d = "One number per area (Final product, Shooting, Creation and so on).", b = "The equal-weight average of that area's headline KPI percentiles. KPIs that fail the reliability check for the position group are shown grey and left out."),
  list(g = "Ratings and reading the charts", t = "Reliability (tiers A to D)", d = "How much of the difference between players is real skill or role rather than luck.",
       b = "1 minus the average luck variance divided by the variance seen across players in the same position group, with luck from a Poisson or binomial model. Tier A is 0.8 or more, B 0.6 to 0.8, C 0.4 to 0.6, D below 0.4. This is an upper bound. xT and VAEP sums are not assessed yet. The signal-versus-noise idea is inspired by work on metric stability and discrimination, but it is not an exact copy of that method.", r = 17),
  list(g = "Ratings and reading the charts", t = "Minutes-adjusted score", d = "A score pulled gently towards average when a player has played little.", b = "50 + (score - 50) x minutes / (minutes + 600). This is a rule of thumb in the spirit of shrinkage estimators; the 600 is chosen, not fitted.", r = 11),
  list(g = "Ratings and reading the charts", t = "Best area (on the key player cards)", d = "The area where the player ranks highest against positional peers, shown as a percentile (81% means better than 81% of players in the same position group).", b = "The highest of the player's area bars. The three best areas are listed under it. The cards no longer show an average of all areas, because an average hides big differences between areas."),
  list(g = "Ratings and reading the charts", t = "Network centrality", d = "How much of the team's passing runs through a player.", b = "PageRank on the graph of completed open play passes between teammates, scaled so the most central player is 100. The method comes from web search and has been used to study football passing networks.", r = c(8, 9)),
  list(g = "Ratings and reading the charts", t = "xG build-up (xGBuildup)", d = "Not used in this dashboard.", b = "Public sites commonly define it as the xG of the moves a player takes part in, excluding the player's own shots and assists. The closest idea here is deep zone involvement.")
)
REFS <- list(
  list(id = 1, txt = "StatsBomb. Open Data (event data and data specification).", url = "https://github.com/statsbomb/open-data", note = "Please credit StatsBomb as the data source, as their terms request."),
  list(id = 2, txt = "Singh, K. (2018). Introducing Expected Threat (xT). Blog post.", url = "https://karun.in/blog/expected-threat.html"),
  list(id = 3, txt = "Rudd, S. (2011). A framework for tactical analysis and individual offensive production assessment in soccer using Markov chains. Presentation, New England Symposium on Statistics in Sports.", url = "https://nessis.org/nessis11.html"),
  list(id = 4, txt = "Decroos, T., Bransen, L., Van Haaren, J. and Davis, J. (2019). Actions speak louder than goals: valuing player actions in soccer. Proceedings of the 25th ACM SIGKDD Conference (KDD '19), pp. 1851 to 1861.", url = "https://doi.org/10.1145/3292500.3330758"),
  list(id = 5, txt = "Fernandez, J., Bornn, L. and Cervone, D. (2019). Decomposing the immeasurable sport: a deep learning expected possession value framework for soccer. MIT Sloan Sports Analytics Conference. Extended in Machine Learning (2021), doi 10.1007/s10994-021-05989-6.", url = "https://www.sloansportsconference.com/research-papers/decomposing-the-immeasurable-sport-a-deep-learning-expected-possession-value-framework-for-soccer"),
  list(id = 6, txt = "Power, P., Ruiz, H., Wei, X. and Lucey, P. (2017). Not all passes are created equal: objectively measuring the risk and reward of passes in soccer from tracking data. Proceedings of KDD '17, pp. 1605 to 1613.", url = "https://doi.org/10.1145/3097983.3098051"),
  list(id = 7, txt = "Trainor, C. (2014). Defensive metrics: measuring the intensity of a high press. StatsBomb.", url = "https://statsbomb.com/?p=10118"),
  list(id = 8, txt = "Lopez Pena, J. and Touchette, H. (2012). A network theory analysis of football strategies. arXiv:1206.6904.", url = "https://arxiv.org/abs/1206.6904"),
  list(id = 9, txt = "Brin, S. and Page, L. (1998). The anatomy of a large-scale hypertextual web search engine. Computer Networks and ISDN Systems, 30(1 to 7), 107 to 117.", url = "https://research.google/pubs/the-anatomy-of-a-large-scale-hypertextual-web-search-engine/"),
  list(id = 10, txt = "Chen, T. and Guestrin, C. (2016). XGBoost: a scalable tree boosting system. Proceedings of the 22nd ACM SIGKDD Conference (KDD '16). arXiv:1603.02754.", url = "https://doi.org/10.1145/2939672.2939785"),
  list(id = 11, txt = "Efron, B. and Morris, C. (1977). Stein's paradox in statistics. Scientific American, May 1977.", url = "https://www.scientificamerican.com/article/steins-paradox-in-statistics"),
  list(id = 12, txt = "Lucey, P., Bialkowski, A., Monfort, M., Carr, P. and Matthews, I. (2015). 'Quality vs quantity': improved shot prediction in soccer using strategic features from spatiotemporal data. MIT Sloan Sports Analytics Conference.", url = "https://www.sloansportsconference.com/research-papers/quality-vs-quantity-improved-shot-prediction-in-soccer-using-strategic-features-from-spatiotemporal-data"),
  list(id = 17, txt = "Franks, A., D'Amour, A., Cervone, D. and Bornn, L. (2016). Meta-analytics: tools for understanding the statistical properties of sports metrics. Journal of Quantitative Analysis in Sports, 12(4), 151 to 165.", url = "https://doi.org/10.1515/jqas-2016-0098"),
  list(id = 14, txt = "Smith, K. L. et al. (2018). Relative age effects across and within female sport contexts: a systematic review and meta-analysis. Sports Medicine, 48(6), 1451 to 1478.", url = "https://doi.org/10.1007/s40279-018-0890-8"),
  list(id = 15, txt = "Barreira, J., Bueno, B. and Chiminazzo, J. G. C. (2021). Relative age effect and age of peak performance: an analysis of women's football players in the Olympic games (1996-2016). Motriz: Revista de Educacao Fisica, 27.", url = "https://doi.org/10.1590/S1980-65742021006921"),
  list(id = 16, txt = "Baptista, I., Winther, A. K., Pedersen, S., Johansen, D. and Pettersen, S. A. (2023). The influence of age on the match-to-match variability of physical performance in women's elite football. Frontiers in Physiology, 14.", url = "https://doi.org/10.3389/fphys.2023.1193501"),
  list(id = 13, txt = "Carrasquilla, L. (2025). Complete guide on working with the StatsBomb Open Data dataset (with applications). Medium, 28 December 2025.", url = "https://medium.com/@lucascarrasquillaparra/complete-guide-on-working-with-the-statsbomb-open-data-dataset-a57c26d5852b"))

about_page <- function() {
  groups <- unique(vapply(GLOSS, function(x) x$g, character(1)))
  gloss_ui <- lapply(groups, function(g) tagList(
    div(class = "gl-group", g),
    div(class = "gl-grid", lapply(Filter(function(x) x$g == g, GLOSS), function(x)
      div(class = "gl-item", div(class = "gl-term", x$t, if (!is.null(x$r)) do.call(cite, as.list(x$r))), div(class = "gl-def", x$d),
          div(class = "gl-how", tags$b("How it is built here: "), x$b))))))
  refs_ui <- tags$ol(class = "ref-list", lapply(REFS[order(vapply(REFS, function(r) r$id, numeric(1)))], function(r) tags$li(id = paste0("ref-", r$id), value = r$id, r$txt, if (!is.null(r$url)) tagList(" ", tags$a(href = r$url, target = "_blank", rel = "noopener", "Link")),
                                                         if (!is.null(r$note)) span(class = "ref-note", " ", r$note))))
  tagList(
    card(fill = FALSE, card_header("About this dashboard"),
      div(class = "about-text",
        p("This is an illustrative internal-style mock-up of a player and team analysis tool for Leicester City Women, built on free event data. It is a starting point for video review and discussion, not a verdict on any player."),
        p(tags$b("Created by. "), "Kannan Suresh."),
        p(tags$b("Data. "), "StatsBomb open data: WSL, Frauen-Bundesliga, Liga F and Serie A Women 2023/24, NWSL 2023. Women's football only. Event data, no tracking or 360 data. One season, so small samples are common.", cite(1)),
        p(tags$b("Not affiliated. "), "Not affiliated with, or endorsed by, Leicester City, the WSL, StatsBomb or any sponsor shown. Club crests, league logos, sponsor logos and any player photos belong to their owners and appear here only to make this illustrative mock-up look realistic. No affiliation or endorsement is implied, and they will be removed on request. Leicester blue (#003090) and gold (#FDBE11) are used as the club's colours."))),
    card(fill = FALSE, card_header("Definitions"),
      div(class = "gl-search", tags$input(id = "gl_filter", type = "search", placeholder = "Search a term, for example xT, PPDA, percentile", class = "form-control")),
      div(id = "gl_list", gloss_ui),
      tags$script(HTML("$(document).on('input', '#gl_filter', function() { var q = this.value.toLowerCase(); $('.gl-item').each(function() { $(this).toggle($(this).text().toLowerCase().indexOf(q) > -1); }); $('.gl-group').each(function() { var any = $(this).next('.gl-grid').find('.gl-item:visible').length > 0; $(this).toggle(any); }); });"))),
    card(fill = FALSE, card_header("How we checked the models"),
      div(class = "about-text",
        p("Every model and score here was tested before use. Two plain-language terms help: ", tags$b("AUC"), " is the chance that the model ranks a random goal (or completed pass) above a random miss; 0.5 is a coin flip and 1 is perfect. ",
          tags$b("Log loss"), " penalises confident wrong answers; lower is better, and the baseline is a model that always guesses the average."),
        tags$table(class = "chk-table",
          tags$thead(tags$tr(tags$th("What"), tags$th("Check"), tags$th("Result"), tags$th("What it tells us"))),
          tags$tbody(
            tags$tr(tags$td("xPass"), tags$td("AUC, log loss, average prediction vs actual"), tags$td("AUC 0.912. Log loss 0.309 against 0.546 for the baseline. Predicted completion 0.764, actual 0.764."), tags$td("Separates completed from failed passes well, and is right on average.")),
            tags$tr(tags$td("xGOT"), tags$td("AUC against pre-shot xG, on 7,123 on-target shots"), tags$td("AUC 0.911 against 0.769 for pre-shot xG. Average xGOT 0.299, actual goal rate 0.300."), tags$td("Where the ball ends in the goal adds real information beyond the chance, and the model is calibrated.")),
            tags$tr(tags$td("VAEP (our version)"), tags$td("Average prediction vs label, AUC on moments that led to a real chance, team totals against xG difference"), tags$td("Average 0.0105 predicted, 0.0105 actual (scoring); 0.0025 predicted, 0.0026 actual (conceding). AUC 0.852 and 0.838. Team VAEP per match correlates 0.81 with xG difference."), tags$td("Calibrated and good at ranking dangerous moments. One check missed: the average team total per match came out at 1.2, not near zero as expected, so we treat VAEP as relative (percentiles inside a position group), not as an absolute value.")),
            tags$tr(tags$td("Reliability of each KPI"), tags$td("Noise model: how much of the spread between players is more than luck"), tags$td("Of 335 assessed KPI and position group combinations: 33% tier A, 33% B, 18% C, 16% D. Median 0.71. 16 headline KPIs in tier D are greyed out and left out of the area bars. 150 (xT and VAEP sums) are not assessed yet."), tags$td("Most KPIs carry real signal; the weak ones are flagged instead of hidden or trusted.")),
            tags$tr(tags$td("xG (StatsBomb)"), tags$td("Not tested by us"), tags$td("Used as supplied."), tags$td("Treat as StatsBomb's model, not ours.")))),
        p(class = "chk-note", "All model numbers are from re-running script 13 on 11 October 2026 with its fixed random seed, cross-fitted by match. They test the models on matches they did not train on, not on another season or league. Reliability numbers come from data/metric_reliability.csv."))),
    card(fill = FALSE, card_header("Age and maturation: what we can and cannot say"),
      tags$ul(class = "about-text",
        tags$li("Ages are not in the StatsBomb open data. The age chart on the Overview page is descriptive only: share of minutes by age band against the league. The bands are for reading, not research-based cut-offs."),
        tags$li("We do not copy age or development models built on men's football. Research on women's football is thinner and does not simply mirror the men's findings."),
        tags$li("Squad ages at the women's Olympic tournaments from 1996 to 2016 averaged about 25 and rose by roughly 1.4 years, with goalkeepers oldest and forwards youngest. That describes who is selected, not how well players perform.", cite(15)),
        tags$li("In one study of 59 elite female club players over 150 matches, physical match output and its match-to-match variability did not differ significantly between players under 23, aged 23 to 27, and over 27. The authors warn the sample was small, and that men's reference values should not be assumed to apply.", cite(16)),
        tags$li("Relative age effects (players born early in the selection year being over-represented) are common across female sport, with an odds ratio of about 1.25 for the oldest against the youngest quartile, stronger in adolescence and at higher levels. The same Olympic squad analysis found no such effect among adult internationals. Testing it needs birth dates, which this dashboard only has if you supply them.", cite(14, 15)),
        tags$li("Biological maturation (timing of the growth spurt, bio-banding) needs height and maturity measures that are not in event data. We make no maturation claims. Most bio-banding research has been done on boys, so female-specific evidence should be read from female-specific studies."))),
    card(fill = FALSE, card_header("Method notes and limits"),
      tags$ul(class = "about-text",
        tags$li("Percentiles are within position group, so a centre-back is never ranked against a winger. Goalkeeper bars are not built yet."),
        tags$li("Several thresholds are our choices, not standards: big chance 0.33 xG, deep zone 20 yards, high regain 72 yards, 15 second shot window, 600 minute shrinkage. Change them in the scripts and the app follows."),
        tags$li("xT and VAEP are not yet tested for reliability (see the model checks above). They are shown, but read them with extra care."),
        tags$li("xPass, xGOT and VAEP are fitted in 5 folds by match, so no action is predicted by a model that saw its own match."),
        tags$li("Match order uses real dates where they exist (all five leagues have them after script 18). Formation minutes exist for the WSL only (script 19). Squad ages are not in the data and need a file you supply."),
        tags$li("Share of passes stands in for possession. PPDA measures how often we act, not how well."))),
    card(fill = FALSE, card_header("Acknowledgements and inspiration"),
      div(class = "about-text",
        p(tags$b("Data and methods. "), "Thanks to StatsBomb for the open data, and to the authors listed below whose ideas this dashboard builds on. The code implementing them is our own and any mistakes in it are ours, not theirs."),
        p(tags$b("Ideas we took, not code. ")),
        tags$ul(
          tags$li("Passing-network centrality and the idea of shrinking small samples towards the average came from reading Lucas Carrasquilla's guide to StatsBomb open data.", cite(13, 8, 11)),
          tags$li("The skill-card layout and quick-search idea were inspired by Gradient Sports' Gradient FC site (fc.gradientsports.com)."),
          tags$li("The side navigation, pill toggles and share-as-image button were inspired by Teams Lab (teamslab.streamlit.app). The look and the content are our own."),
          tags$li("Chart types (radar, dumbbell, lollipop) were suggested by the R Graph Gallery (r-graph-gallery.com)."),
          tags$li("The colour discipline (use colour sparingly, at most two accent colours, white text on dark fills) follows ETH Zurich's published corporate design guidance on colours."))  ,
        p(tags$b("Software. "), "R, shiny, bslib, ggplot2, dplyr, tidyr, purrr, DT, ggimage, ggrepel, magick, ragg, xgboost and StatsBombR.", cite(10)))),
    card(fill = FALSE, card_header("References"), refs_ui))
}

# PNG device for the share-as-image buttons: ragg when installed, otherwise base png
open_png <- function(file) if (requireNamespace("ragg", quietly = TRUE)) ragg::agg_png(file, width = 1600, height = 900, res = 150) else grDevices::png(file, width = 1600, height = 900, res = 150)
make_cmp_card <- function(pa, pb, ba, bb) {
  n <- nrow(DOM); a <- 2 * pi * (seq_len(n) - 1) / n; cx <- 4.4; cy <- 3.3; R <- 2.25
  poly <- function(v, who) tibble(x = cx + R * ifelse(is.na(v), 0, v) / 100 * sin(a), y = cy + R * ifelse(is.na(v), 0, v) / 100 * cos(a), who = who)
  dd <- bind_rows(poly(ba, "A"), poly(bb, "B")); cols <- c(A = BRAND$primary, B = BRAND$accent)
  ring <- function(r) { t <- seq(0, 2 * pi, length.out = 200); tibble(x = cx + R * r * sin(t), y = cy + R * r * cos(t)) }
  lab <- tibble(x = cx + 1.22 * R * sin(a), y = cy + 1.2 * R * cos(a), l = gsub(" ", "\n", DOM$domain_label, fixed = TRUE))
  rows <- tibble(l = DOM$domain_label, va = ba, vb = bb, y = 6.2 - (seq_len(n) - 1) * 0.58)
  bx <- 11.3; bw <- 3.4
  g <- ggplot() +
    annotate("rect", xmin = 0, xmax = 16, ymin = 7.1, ymax = 9, fill = BRAND$primary) + annotate("rect", xmin = 0, xmax = 16, ymin = 7.02, ymax = 7.1, fill = BRAND$accent) +
    geom_path(data = ring(0.25), aes(x, y), colour = "#E3E6EE") + geom_path(data = ring(0.75), aes(x, y), colour = "#E3E6EE") + geom_path(data = ring(1), aes(x, y), colour = BRAND$muted) +
    geom_path(data = ring(0.5), aes(x, y), colour = "#9AA3B5", linewidth = 0.9, linetype = "dashed") +
    geom_segment(data = tibble(a = a), aes(x = cx, y = cy, xend = cx + R * sin(a), yend = cy + R * cos(a)), colour = "#E3E6EE") +
    geom_polygon(data = dd, aes(x, y, fill = who, colour = who), alpha = 0.22, linewidth = 1.3) + geom_point(data = dd, aes(x, y, colour = who), size = 3) +
    scale_fill_manual(values = cols, guide = "none") + scale_colour_manual(values = cols, guide = "none") +
    geom_text(data = lab, aes(x, y, label = l), size = 3.5, colour = BRAND$dark, lineheight = 0.9, family = FONT_BODY) +
    geom_rect(data = rows, aes(xmin = bx, xmax = bx + bw, ymin = y + 0.02, ymax = y + 0.2), fill = "#EEF1F7") + geom_rect(data = rows, aes(xmin = bx, xmax = bx + bw, ymin = y - 0.2, ymax = y - 0.02), fill = "#EEF1F7") +
    geom_rect(data = rows %>% filter(!is.na(va)), aes(xmin = bx, xmax = bx + bw * va / 100, ymin = y + 0.02, ymax = y + 0.2), fill = BRAND$primary) +
    geom_rect(data = rows %>% filter(!is.na(vb)), aes(xmin = bx, xmax = bx + bw * vb / 100, ymin = y - 0.2, ymax = y - 0.02), fill = BRAND$accent) +
    geom_text(data = rows, aes(x = bx - 0.15, y = y, label = l), hjust = 1, size = 3.8, colour = BRAND$dark, family = FONT_BODY) +
    geom_text(data = rows, aes(x = bx + bw + 0.12, y = y + 0.11, label = ifelse(is.na(va), "n/a", sprintf("%.0f", va))), hjust = 0, size = 3.2, fontface = "bold", colour = BRAND$primary, family = FONT_HEAD) +
    geom_text(data = rows, aes(x = bx + bw + 0.12, y = y - 0.11, label = ifelse(is.na(vb), "n/a", sprintf("%.0f", vb))), hjust = 0, size = 3.2, fontface = "bold", colour = "#B58A00", family = FONT_HEAD) +
    annotate("text", x = cx, y = 6.78, label = "PROFILE SHAPE", size = 4.2, fontface = "bold", colour = BRAND$primary, family = FONT_HEAD) +
    annotate("text", x = 9.0, y = 6.78, label = "AREA SCORES (PERCENTILE)", size = 4.2, fontface = "bold", colour = BRAND$primary, hjust = 0, family = FONT_HEAD) +
    annotate("text", x = 2.35, y = 8.1, label = pa$player, hjust = 0, size = 9, fontface = "bold", colour = "white", family = FONT_HEAD) +
    annotate("text", x = 13.65, y = 8.1, label = pb$player, hjust = 1, size = 9, fontface = "bold", colour = BRAND$accent, family = FONT_HEAD) +
    annotate("text", x = 2.35, y = 7.55, label = paste0(team_display(pa$team), "  |  ", unname(GROUP_NAMES[pa$pos_group]), "  |  ", round(pa$minutes), " min"), hjust = 0, size = 4, colour = "#DCE6F6", family = FONT_BODY) +
    annotate("text", x = 13.65, y = 7.55, label = paste0(team_display(pb$team), "  |  ", unname(GROUP_NAMES[pb$pos_group]), "  |  ", round(pb$minutes), " min"), hjust = 1, size = 4, colour = "#DCE6F6", family = FONT_BODY) +
    annotate("label", x = 8, y = 8.05, label = "VS", size = 6, fontface = "bold", colour = BRAND$primary, fill = "white", family = FONT_HEAD) +
    annotate("text", x = 0.5, y = 0.35, label = "Percentiles are within each player's own position group. Gold dashed ring: the average player. StatsBomb open data, illustrative mockup.", hjust = 0, size = 3, colour = BRAND$grey, family = FONT_BODY) +
    coord_fixed(xlim = c(0, 16), ylim = c(0, 9), expand = FALSE) + theme_void(base_family = FONT_BODY) + theme(plot.background = element_rect(fill = "white", colour = NA))
  face <- function(p, xmin, xmax, frame) {
    fn <- find_player_photo(p$player.id, p$player, p$team)
    if (!is.null(fn)) list(annotate("rect", xmin = xmin - 0.06, xmax = xmax + 0.06, ymin = 7.24, ymax = 8.86, fill = frame), annotation_raster(square_photo(fn), xmin = xmin, xmax = xmax, ymin = 7.3, ymax = 8.8, interpolate = TRUE))
    else { w <- strsplit(p$player, " ")[[1]]; list(annotate("point", x = (xmin + xmax) / 2, y = 8.05, size = 22, colour = "white"),
      annotate("text", x = (xmin + xmax) / 2, y = 8.05, label = toupper(paste0(substr(w[1], 1, 1), substr(w[length(w)], 1, 1))), size = 9, fontface = "bold", colour = BRAND$primary, family = FONT_HEAD)) }
  }
  g + face(pa, 0.55, 2.05, BRAND$primary) + face(pb, 13.95, 15.45, BRAND$accent)
}

# ---- UI ----
ui <- page_navbar(
  id = "page",
  fillable = FALSE,   # pages scroll at natural height instead of squeezing every card into the window
  header = tags$head(tags$script(HTML(side_js)), tags$style(HTML(sprintf("
    .navbar { border-bottom: 3px solid %1$s; }
    .card-header { font-weight: 600; color: %2$s; }
    .dom-box { border: 1px solid #DCE1EC; border-left: 5px solid %2$s; border-radius: 10px; background: #fff; padding: 10px 14px 6px; margin-bottom: 14px; box-shadow: 0 1px 3px rgba(27,31,42,.06); }
    .dom-head { display: flex; align-items: center; justify-content: space-between; margin-bottom: 4px; }
    .dom-title { font-weight: 700; font-size: 15px; color: %2$s; }
    .dom-badge { background: %2$s; color: #fff; font-weight: 700; font-size: 13px; min-width: 38px; text-align: center; padding: 2px 10px; border-radius: 999px; border-bottom: 3px solid %1$s; }
    .sponsor-strip { background: linear-gradient(90deg, %2$s 0%%, #0A3FB0 100%%); border-top: 3px solid %1$s; display: flex; align-items: center; justify-content: center; gap: 72px; flex-wrap: wrap; padding: 26px 24px; margin-top: 20px; }
    .sponsor-logo { height: 60px; max-width: 200px; width: auto; object-fit: contain; }
    .sponsor-white { filter: brightness(0) invert(1); }
    table.dataTable { font-size: 13px; }
    table.dataTable thead th { vertical-align: bottom; white-space: normal; line-height: 1.15; padding: 6px 14px 6px 6px !important; }
    table.dataTable td { padding: 5px 8px !important; }
    table.dataTable td { white-space: nowrap; }
    .shiny-input-container > label { font-weight: 600; }
    #context table { font-size: 12.5px; } #context td, #context th { padding: 3px 6px; }
    .help-block { font-size: 12.5px; }
  ", BRAND$accent, BRAND$primary))),
    tags$style(HTML(paste0("
    h1, h2, h3, h4, h5, .navbar-brand, .card-header, .dom-title, .dom-badge { font-family: '", FONT_HEAD, "', '", FONT_BODY, "', sans-serif; }
    .card { border: 1px solid #E1E5F0; border-radius: 12px; box-shadow: 0 1px 4px rgba(27,31,42,.06); }
    .card-header { background: #fff; border-bottom: 1px solid #EEF1F7; text-transform: uppercase; letter-spacing: .07em; font-size: 14px; padding: 12px 16px; }
    .navbar { background: linear-gradient(90deg, ", BRAND$primary, " 0%, #0A3FB0 100%) !important; box-shadow: 0 2px 10px rgba(0,30,90,.25); }
    .navbar-brand { font-size: 21px; letter-spacing: .03em; color: #fff !important; display: flex; align-items: center; }
    .league-chip { background: #fff; border-radius: 8px; padding: 3px 7px; margin-left: 14px; }
    .navbar .nav-link { font-weight: 600; color: rgba(255,255,255,.78) !important; padding: 16px 18px; border-bottom: 3px solid transparent; letter-spacing: .03em; }
    .navbar .nav-link:hover { color: #fff !important; }
    .navbar .nav-link.active { color: #fff !important; border-bottom-color: ", BRAND$accent, "; background: rgba(255,255,255,.08); }
    .navbar-toggler { border-color: rgba(255,255,255,.5); }
    .bslib-sidebar-layout > .sidebar { border-right: 1px solid #E1E5F0; }
    .shiny-input-container > label, .control-label { text-transform: uppercase; letter-spacing: .06em; font-size: 11.5px; color: #5A6070; font-weight: 700; margin-bottom: 5px; }
    .selectize-input, .form-select, .form-control { border: 1px solid #CBD2E3 !important; border-radius: 8px !important; padding: 8px 12px !important; box-shadow: none !important; background-color: #fff; min-height: 40px; color: ", BRAND$dark, "; }
    .selectize-input.focus, .form-select:focus, .form-control:focus { border-color: ", BRAND$primary, " !important; box-shadow: 0 0 0 3px rgba(0,48,144,.15) !important; }
    .selectize-dropdown { border: 1px solid #CBD2E3; border-radius: 8px; box-shadow: 0 10px 28px rgba(27,31,42,.16); margin-top: 4px; overflow: hidden; }
    .selectize-dropdown .option { padding: 8px 12px; }
    .selectize-dropdown .active { background: ", BRAND$primary, " !important; color: #fff !important; }
    .navbar .navbar-nav, .navbar ul.nav, .navbar .shiny-tab-input, .navbar-toggler { display: none !important; visibility: hidden !important; }
    .side-nav { display: flex; flex-direction: column; gap: 4px; margin-bottom: 6px; }
    .side-link { display: flex; align-items: center; gap: 12px; padding: 11px 12px; border-radius: 10px; color: #3A4152; font-weight: 600; font-size: 15px; text-decoration: none; border-left: 4px solid transparent; }
    .side-link:hover { background: #EEF2FA; color: ", BRAND$primary, "; }
    .side-link.active { background: #E4ECFA; color: ", BRAND$primary, "; border-left-color: ", BRAND$accent, "; }
    .side-link svg { width: 20px; height: 20px; stroke: currentColor; fill: none; stroke-width: 1.8; flex: none; }
    .side-label { text-transform: uppercase; letter-spacing: .08em; font-size: 11px; font-weight: 700; color: #8A90A0; margin: 10px 0 12px; padding-top: 14px; border-top: 1px solid #E1E5F0; }
    .pill-radio .shiny-options-group { display: flex; flex-wrap: wrap; gap: 6px; }
    .pill-radio .radio-inline { padding: 0 !important; margin: 0 !important; }
    .pill-radio input[type=radio] { position: absolute; opacity: 0; pointer-events: none; margin: 0; }
    .pill-radio .radio-inline span { display: inline-block; padding: 6px 13px; border: 1px solid #CBD2E3; border-radius: 999px; background: #fff; cursor: pointer; font-size: 13px; font-weight: 600; color: #3A4152; transition: all .12s; }
    .pill-radio .radio-inline:hover span { border-color: ", BRAND$primary, "; color: ", BRAND$primary, "; }
    .pill-radio input[type=radio]:checked + span { background: ", BRAND$primary, "; border-color: ", BRAND$primary, "; color: #fff; box-shadow: inset 0 -3px 0 ", BRAND$accent, "; }
    .sk-head { display: flex; align-items: baseline; gap: 14px; flex-wrap: wrap; margin: 4px 2px 10px; font-family: '", FONT_HEAD, "', sans-serif; text-transform: uppercase; letter-spacing: .07em; font-size: 14px; color: ", BRAND$primary, "; font-weight: 600; }
    .sk-hint { text-transform: none; letter-spacing: 0; font-family: inherit; font-size: 12.5px; color: #6A7185; font-weight: 400; }
    .skill-grid { display: grid; grid-template-columns: repeat(auto-fill, minmax(168px, 1fr)); gap: 12px; margin-bottom: 18px; }
    .skill-card { background: #fff; border: 1px solid #E1E5F0; border-top: 4px solid #DCE1EC; border-radius: 12px; padding: 12px 14px 10px; cursor: pointer; box-shadow: 0 1px 4px rgba(27,31,42,.06); transition: transform .12s, box-shadow .12s; }
    .skill-card:hover { transform: translateY(-2px); box-shadow: 0 6px 16px rgba(0,48,144,.16); }
    .sk-strength { border-top-color: ", BRAND$accent, "; }
    .sk-work { border-top-color: #8FA6D6; }
    .sk-top { display: flex; justify-content: space-between; align-items: center; gap: 6px; min-height: 22px; }
    .sk-name { font-family: '", FONT_HEAD, "', sans-serif; font-weight: 600; font-size: 14px; color: #3A4152; letter-spacing: .02em; }
    .sk-tag { white-space: nowrap; flex: none; font-size: 10px; font-weight: 700; text-transform: uppercase; letter-spacing: .06em; padding: 2px 7px; border-radius: 999px; background: #EEF1F7; color: ", BRAND$primary, "; }
    .sk-strength .sk-tag { background: ", BRAND$accent, "; }
    .sk-score { font-family: '", FONT_HEAD, "', sans-serif; font-size: 40px; line-height: 1.05; font-weight: 700; color: ", BRAND$primary, "; margin: 4px 0 6px; }
    .sk-bar { position: relative; height: 8px; border-radius: 999px; background: #EEF1F7; margin-bottom: 8px; }
    .sk-fill { height: 100%; border-radius: 999px; background: ", BRAND$primary, "; }
    .sk-mid { position: absolute; left: 50%; top: -3px; width: 2px; height: 14px; background: ", BRAND$accent, "; }
    .sk-desc { font-weight: 600; font-size: 13px; color: ", BRAND$dark, "; }
    .sk-sub { font-size: 11.5px; color: #6A7185; margin-top: 2px; }
    .ov-band { background: linear-gradient(100deg, ", BRAND$primary, " 0%, #0A3FB0 100%); color: #fff; border-radius: 16px; padding: 24px 32px; display: flex; align-items: center; justify-content: space-between; gap: 20px; flex-wrap: wrap; border-bottom: 5px solid ", BRAND$accent, "; box-shadow: 0 4px 16px rgba(0,30,90,.25); margin-bottom: 20px; }
    .ov-name { font-family: '", FONT_HEAD, "', sans-serif; font-size: 44px; font-weight: 700; line-height: 1.05; letter-spacing: .01em; }
    .ov-sub { font-size: 16px; color: #DCE6F6; margin-top: 6px; letter-spacing: .03em; }
    .ov-logos { display: flex; align-items: center; gap: 18px; }
    .ov-chip { background: #fff; border-radius: 14px; padding: 8px 12px; display: flex; align-items: center; }
    .ov-keypill { margin: 4px auto 18px; max-width: 700px; background: #fff; border: 2px solid ", BRAND$primary, "; border-radius: 999px; text-align: center; padding: 12px 30px; box-shadow: 0 2px 10px rgba(0,48,144,.12); }
    .ov-keypill .t { font-family: '", FONT_HEAD, "', sans-serif; font-size: 26px; letter-spacing: .14em; color: ", BRAND$primary, "; font-weight: 700; line-height: 1.1; }
    .ov-keypill .s { font-size: 13px; color: #6A7185; margin-top: 2px; }
    .key-grid { display: grid; grid-template-columns: repeat(auto-fit, minmax(320px, 1fr)); gap: 16px; margin-bottom: 22px; }
    .key-card { background: #fff; border: 1px solid #E1E5F0; border-top: 4px solid ", BRAND$accent, "; border-radius: 14px; padding: 16px 18px; cursor: pointer; box-shadow: 0 1px 4px rgba(27,31,42,.06); transition: transform .12s, box-shadow .12s; }
    .key-card:hover { transform: translateY(-3px); box-shadow: 0 8px 20px rgba(0,48,144,.18); }
    .key-top { display: flex; align-items: center; gap: 14px; }
    .key-name { font-family: '", FONT_HEAD, "', sans-serif; font-size: 21px; font-weight: 700; color: ", BRAND$primary, "; line-height: 1.1; }
    .key-meta { color: #6A7185; font-size: 12.5px; margin-top: 3px; }
    .key-overall { margin-left: auto; text-align: center; padding-left: 6px; }
    .key-overall .n { font-family: '", FONT_HEAD, "', sans-serif; font-size: 38px; font-weight: 700; color: ", BRAND$primary, "; line-height: 1; }
    .key-overall .l { font-size: 10px; text-transform: uppercase; letter-spacing: .08em; color: #8A90A0; font-weight: 700; }
    .key-sub { text-transform: uppercase; letter-spacing: .06em; font-size: 11px; color: #8A90A0; font-weight: 700; margin: 14px 0 2px; }
    .key-rows .r { display: grid; grid-template-columns: 120px 1fr 30px; gap: 10px; align-items: center; font-size: 13px; margin-top: 8px; }
    .key-rows .bar { position: relative; height: 8px; border-radius: 999px; background: #EEF1F7; }
    .key-rows .fill { height: 100%; border-radius: 999px; background: ", BRAND$primary, "; }
    .key-rows .tick { position: absolute; left: 50%; top: -3px; width: 2px; height: 14px; background: ", BRAND$accent, "; }
    .key-rows .v { font-weight: 700; color: ", BRAND$primary, "; text-align: right; }
    .ov-note { font-size: 13.5px; color: #3A4152; margin-bottom: 12px; }
    .tp-bar { display: flex; align-items: center; gap: 20px; flex-wrap: wrap; margin: 2px 2px 14px; }
    .tp-bar .form-group { margin-bottom: 0; }
    .tp-note { font-size: 12.5px; color: #6A7185; max-width: 520px; }
    .tp-tiles { display: grid; grid-template-columns: repeat(auto-fit, minmax(190px, 1fr)); gap: 14px; margin-bottom: 18px; }
    .tp-tile { background: #fff; border: 1px solid #E1E5F0; border-top: 4px solid ", BRAND$primary, "; border-radius: 14px; padding: 12px 16px 8px; box-shadow: 0 1px 4px rgba(27,31,42,.06); }
    .tp-tile .l { font-size: 11.5px; text-transform: uppercase; letter-spacing: .07em; color: #6A7185; font-weight: 700; }
    .tp-tile .v { font-family: '", FONT_HEAD, "', sans-serif; font-size: 34px; font-weight: 700; color: ", BRAND$primary, "; line-height: 1.1; margin-top: 2px; }
    .tp-tile .u { font-size: 12px; color: #6A7185; font-weight: 400; margin-left: 4px; font-family: inherit; }
    .tp-tile .rk { display: inline-block; margin-top: 2px; font-size: 12px; font-weight: 700; padding: 2px 9px; border-radius: 999px; background: #EEF1F7; color: ", BRAND$primary, "; }
    .tp-tile .rk.good { background: ", BRAND$accent, "; }
    .tp-tile .sp { margin-top: 4px; height: 36px; }
    .kpi-row { display: flex; justify-content: space-between; align-items: center; padding: 6px 0; border-bottom: 1px solid #EEF1F7; font-size: 14px; }
    .kpi-row:last-child { border-bottom: 0; }
    .kpi-row .kv { font-weight: 700; color: ", BRAND$primary, "; }
    .kpi-row .kr { font-size: 11.5px; font-weight: 700; padding: 1px 8px; border-radius: 999px; background: #EEF1F7; color: ", BRAND$primary, "; margin-left: 8px; }
    .kpi-row .kr.good { background: ", BRAND$accent, "; }
    .tp-spacer { height: 10px; }
    .about-text { font-size: 15px; line-height: 1.55; color: #2B3042; max-width: 980px; }
    .about-text li { margin-bottom: 6px; }
    .gl-search { margin-bottom: 14px; max-width: 460px; }
    .gl-group { font-family: '", FONT_HEAD, "', sans-serif; text-transform: uppercase; letter-spacing: .08em; font-size: 14px; font-weight: 700; color: ", BRAND$primary, "; margin: 18px 0 8px; padding-bottom: 4px; border-bottom: 2px solid ", BRAND$accent, "; }
    .gl-grid { display: grid; grid-template-columns: repeat(auto-fill, minmax(340px, 1fr)); gap: 12px; }
    .gl-item { border: 1px solid #E1E5F0; border-left: 4px solid ", BRAND$primary, "; border-radius: 10px; padding: 11px 14px; background: #fff; }
    .gl-term { font-family: '", FONT_HEAD, "', sans-serif; font-weight: 700; font-size: 17px; color: ", BRAND$primary, "; }
    .gl-def { font-size: 14px; color: #2B3042; margin-top: 3px; }
    .gl-how { font-size: 12.5px; color: #5A6070; margin-top: 6px; line-height: 1.45; }
    .cite { font-size: 11px; margin-left: 3px; } .cite a { color: ", BRAND$primary, "; font-weight: 700; text-decoration: none; padding: 0 1px; }
    .ref-list { font-size: 13.5px; line-height: 1.5; color: #2B3042; padding-left: 26px; } .ref-list li { margin-bottom: 8px; scroll-margin-top: 80px; }
    .ref-list a { color: ", BRAND$primary, "; font-weight: 600; } .ref-note { color: #6A7185; font-style: italic; }
    li:target { background: #FFF4CC; border-radius: 6px; }
    .cmp-pickers { display: grid; grid-template-columns: 1fr 1fr; gap: 16px; margin-bottom: 4px; }
    .cmp-head { display: grid; grid-template-columns: 1fr auto 1fr; gap: 16px; align-items: center; margin: 8px 0 18px; }
    .cmp-card { background: #fff; border: 1px solid #E1E5F0; border-radius: 14px; padding: 14px 18px; box-shadow: 0 1px 4px rgba(27,31,42,.06); display: flex; align-items: center; gap: 14px; }
    .cmp-a { border-top: 5px solid ", BRAND$primary, "; } .cmp-b { border-top: 5px solid ", BRAND$accent, "; }
    .cmp-name { font-family: '", FONT_HEAD, "', sans-serif; font-size: 22px; font-weight: 700; color: ", BRAND$primary, "; line-height: 1.1; }
    .cmp-meta { font-size: 12.5px; color: #6A7185; margin-top: 3px; }
    .cmp-ovr { margin-left: auto; text-align: center; } .cmp-ovr .n { font-family: '", FONT_HEAD, "', sans-serif; font-size: 36px; font-weight: 700; line-height: 1; } .cmp-ovr .l { font-size: 10px; text-transform: uppercase; letter-spacing: .08em; color: #8A90A0; font-weight: 700; }
    .cmp-a .n { color: ", BRAND$primary, "; } .cmp-b .n { color: #C99A00; }
    .cmp-vs { background: ", BRAND$primary, "; color: #fff; font-family: '", FONT_HEAD, "', sans-serif; font-weight: 700; letter-spacing: .1em; border-radius: 999px; padding: 10px 18px; border-bottom: 4px solid ", BRAND$accent, "; }
    .cmp-note { font-size: 12.5px; color: #6A7185; grid-column: 1 / -1; text-align: center; }
    @media (max-width: 900px) { .cmp-head, .cmp-pickers { grid-template-columns: 1fr; } .cmp-vs { justify-self: center; } }
    .chk-table { width: 100%; border-collapse: collapse; font-size: 13.5px; margin: 10px 0; }
    .chk-table th { text-align: left; background: #F1F4FA; color: ", BRAND$primary, "; padding: 8px 10px; border-bottom: 2px solid ", BRAND$accent, "; font-family: '", FONT_HEAD, "', sans-serif; letter-spacing: .03em; }
    .chk-table td { padding: 9px 10px; border-bottom: 1px solid #EEF1F7; vertical-align: top; } .chk-table td:first-child { font-weight: 700; color: ", BRAND$primary, "; white-space: nowrap; }
    .chk-note { font-size: 12.5px; color: #6A7185; }
    .btn-compare { background: #fff; color: ", BRAND$primary, "; border: 2px solid ", BRAND$primary, "; border-radius: 999px; font-weight: 700; padding: 6px 16px; white-space: nowrap; flex: none; }
    .btn-compare:hover { background: ", BRAND$primary, "; color: #fff; }
    .cmp-share { display: flex; justify-content: flex-end; margin: 2px 0 6px; }
    .mr-side { text-align: center; min-width: 170px; } .mr-team { font-family: '", FONT_HEAD, "', sans-serif; font-size: 22px; font-weight: 700; margin-top: 6px; } .mr-xg { color: #DCE6F6; font-size: 14px; }
    .mr-score { font-family: '", FONT_HEAD, "', sans-serif; font-size: 64px; font-weight: 700; line-height: 1; letter-spacing: .04em; }
    .cmp-any { grid-column: 1 / -1; font-size: 12.5px; color: #6A7185; margin-top: -8px; }
    .btn-share { white-space: nowrap; flex: none; background: ", BRAND$accent, "; color: ", BRAND$primary, "; border: 0; border-radius: 999px; font-weight: 700; padding: 8px 18px; letter-spacing: .02em; }
    .btn-share:hover { background: #F0AE00; color: ", BRAND$primary, "; }
    .form-check-input:checked { background-color: ", BRAND$primary, "; border-color: ", BRAND$primary, "; }
    .form-check-label { font-size: 14px; }
    table.dataTable thead th { background: #F1F4FA; color: ", BRAND$primary, "; border-bottom: 2px solid ", BRAND$accent, " !important; font-family: '", FONT_HEAD, "', sans-serif; letter-spacing: .02em; }
    .help-block, .shiny-input-container .help-block { color: #6A7185; }
    ")))),
  title = tagList(
    if (!is.null(find_asset("crests", DEFAULT_TEAM))) tags$img(src = paste0("assets/crests/", utils::URLencode(find_asset("crests", DEFAULT_TEAM))), height = 38, style = "margin-right:10px;"),
    span(APP_TITLE),
    if (!is.null(find_asset(".", "wsl"))) tags$img(src = paste0("assets/", find_asset(".", "wsl")), height = 26, class = "league-chip")),
  theme = bs_theme(version = 5, primary = BRAND$primary, secondary = BRAND$accent, bg = "#E9EFF9", fg = BRAND$dark,
                   base_font = font_collection(FONT_BODY, "Helvetica Neue", "system-ui", "sans-serif"),
                   heading_font = font_collection(FONT_HEAD, FONT_BODY, "sans-serif"), "card-bg" = "#FFFFFF",
                   "font-size-base" = "0.95rem", "border-radius" = "0.6rem"),
  footer = tagList(sponsor_strip(), div(FOOTER, style = sprintf("padding:8px 16px;color:%s;font-size:12px;", BRAND$grey))),
  sidebar = sidebar(width = 300, bg = "#FFFFFF",
    side_nav(), div(class = "side-label", "Filters"),
    selectInput("league", "League", LEAGUES, selected = if ("WSL" %in% LEAGUES) "WSL" else LEAGUES[1]),
    selectInput("team", "Team", choices = NULL),
    selectInput("player", "Player (Player and Maps pages)", choices = NULL),
    pills("pool", "Compare against", c("All leagues" = "all", setNames(LEAGUES, LEAGUES))),
    checkboxInput("merge", "Merge attacking mid + winger (AM+W)", FALSE),
    sliderInput("min_min", "Minimum minutes for the comparison pool", 0, 1500, 450, step = 50),
    helpText("Percentiles are within position group. Bars are the equal-weight average of the headline KPIs.",
             "Context in brackets never feeds a score.")),
  nav_panel("Overview",
    uiOutput("ov_head"), uiOutput("ov_keyhead"), uiOutput("ov_cards"),
    layout_columns(fill = FALSE, col_widths = breakpoints(sm = 12, lg = c(6, 6)),
      card(fill = FALSE, card_header("Age profile: share of minutes vs the league"), plotOutput("ov_age", height = 420)),
      card(fill = FALSE, card_header("Goal contribution: npxG + xA"), plotOutput("ov_goals", height = 420))),
    card(fill = FALSE, card_header("Most used 11"),
      layout_columns(fill = FALSE, col_widths = breakpoints(sm = 12, lg = c(7, 5)),
        plotOutput("ov_xi", height = 680), div(uiOutput("ov_shape_txt"), DTOutput("ov_shapes", fill = FALSE))))),
  nav_panel("Team profile",
    uiOutput("tp_head"),
    div(class = "tp-bar", pills("tp_range", "Matches", c("Whole season" = "season", "Last 10" = "last10", "Last 5" = "last5")),
        div(class = "tp-note", "Ranks compare our chosen matches with every team's whole season. Sparklines show every match in date order.")),
    uiOutput("tp_tiles"),
    layout_columns(fill = FALSE, col_widths = breakpoints(sm = 12, xl = c(4, 4, 4)),
      card(fill = FALSE, card_header("Build-up"), uiOutput("tp_k_build"), plotOutput("tp_build", height = 430)),
      card(fill = FALSE, card_header("Defending: where we press"), uiOutput("tp_k_def"), plotOutput("tp_press", height = 430)),
      card(fill = FALSE, card_header("Creating chances"), uiOutput("tp_k_chance"), plotOutput("tp_shots", height = 430))),
    layout_columns(fill = FALSE, col_widths = breakpoints(sm = 12, lg = c(7, 5)),
      card(fill = FALSE, card_header("League style map"), pills("tp_sc", "View", c("Style" = "style", "Chances" = "chances", "Pressing" = "pressing", "Territory" = "terr")),
           plotOutput("tp_scatter", height = 520)),
      card(fill = FALSE, card_header("Threat trend (xT)"), plotOutput("tp_xt", height = 520))),
    layout_columns(fill = FALSE, col_widths = breakpoints(sm = 12, lg = c(6, 6)),
      card(fill = FALSE, card_header("After we win the ball high"), uiOutput("tp_k_regain"), plotOutput("tp_regain", height = 330)),
      card(fill = FALSE, card_header("Shape: share of minutes by formation"), plotOutput("tp_shape", height = 200),
           helpText("Formation minutes are stored for the WSL only (script 19)."))),
    layout_columns(fill = FALSE, col_widths = breakpoints(sm = 12, lg = c(6, 6)),
      card(fill = FALSE, card_header("How our attacks end in a shot"), plotOutput("tp_origin", height = 420),
           helpText("Origin is StatsBomb's play pattern: open play (positional attack), counter, or the set piece that started the move. Penalties excluded.")),
      card(fill = FALSE, card_header("Sequence style: passes before the shot"), plotOutput("tp_seq", height = 420),
           helpText("Open play and counter shots only. Bars: share of shots. Labels above: xG per shot in that band."))),
    div(class = "tp-spacer")),
  nav_panel("Squad",
    layout_columns(fill = FALSE, col_widths = c(12),
      card(fill = FALSE, card_header("How our players rate, by position group (percentile bars)"),
           helpText("Each cell is the headline bar for that area, 50 is the average player in the comparison pool."),
           DTOutput("squad_tbl", fill = FALSE)),
      layout_columns(fill = FALSE, col_widths = c(6, 6),
        card(fill = FALSE, card_header("What each player contributes to the team"),
             selectInput("contrib", "Measure", c("npxG" = "npxg_p90", "Shots" = "shots_p90", "Open play xA" = "xa_open_p90",
                                                  "Deep zone involvement" = "deep_inv_p90", "Progressive passes" = "prog_passes_p90",
                                                  "Progressive carries" = "prog_carries_p90", "Final third entries" = "f3_entries_p90",
                                                  "Successful dribbles" = "dribbles_won_p90", "xT received" = "xt_received_p90",
                                                  "VAEP" = "vaep_total_p90", "Tackles + interceptions" = "tackles_int_p90",
                                                  "Recoveries" = "recoveries_p90")),
             plotOutput("contrib_plot", height = 480)),
        card(fill = FALSE, card_header("Do the areas go together? (exploratory)"),
             selectInput("cor_grp", "Players", c("All outfield players" = "all", setNames(GROUPS, GROUP_NAMES[GROUPS]))),
             helpText("Spearman correlation between headline bars across players in the pool (default groups and pool).",
                      "Players are not teams, so this shows which skills tend to come together, not what drives results."),
             plotOutput("cor_plot", height = 430))),
      card(fill = FALSE, card_header("Where we enter the final third (left, centre, right)"),
           helpText("Completed open play passes and carries that start outside the final third and end inside it, split by the channel where they end. Left and right are from the attacker's point of view."),
           layout_columns(fill = FALSE, col_widths = c(5, 7), plotOutput("flank_plot", height = 300), DTOutput("flank_tbl", fill = FALSE))))),
  nav_panel("Team maps",
    layout_columns(fill = FALSE, col_widths = breakpoints(sm = 12, xl = c(3, 9)),
      card(fill = FALSE, card_header("What do you want to see?"),
        pills("tm_view", "Question", c("Defending" = "def", "Losing the ball" = "lose", "Our attacks" = "att", "Midfield" = "mid", "Season rhythm" = "rhy")),
        conditionalPanel("input.tm_view == 'def'", pills("tm_def", "Show", c("Heatmap" = "heat", "Actions" = "act"))),
        conditionalPanel("input.tm_view == 'att'", pills("tm_att", "Show", c("Shots" = "shots", "Final third entries" = "entries", "Threat (xT)" = "threat"))),
        conditionalPanel("input.tm_view == 'mid'", pills("tm_mid", "Show", c("Passing network" = "net", "Touches" = "touch", "Progression" = "prog"))),
        conditionalPanel("input.tm_view == 'rhy'", pills("tm_rhy", "Show", c("Form (xG difference)" = "form", "One metric" = "met", "Inside a match" = "inm"))),
        conditionalPanel("input.tm_view == 'rhy' && input.tm_rhy == 'met'", selectInput("tm_rmet", "Metric", c("xG" = "xg", "Shots" = "shots", "xT created" = "xt", "Final third entries" = "entries", "High regains" = "regains", "Turnovers" = "tov"))),
        conditionalPanel("input.tm_view != 'mid' && input.tm_view != 'rhy'", pills("tm_pos", "Players", c("Everyone" = "all", "Defenders" = "def", "Midfield" = "mid", "Attackers" = "att"))),
        selectInput("tm_match", "Match", c("All matches" = "all")),
        conditionalPanel("input.tm_view != 'rhy'", pills("tm_half", "Half", c("Both" = "both", "First" = "1", "Second" = "2"))),
        conditionalPanel("input.tm_view != 'rhy'", pills("tm_state", "Game state", c("All" = "all", "Trailing" = "-1", "Level" = "0", "Leading" = "1"))),
        helpText("Team and league come from the sidebar. Attacking up the page. Midfield means midfielders and attacking midfielders.")),
      card(fill = FALSE, card_header(textOutput("tm_title")), plotOutput("tm_plot", height = 700))),
    layout_columns(fill = FALSE, col_widths = breakpoints(sm = 12, lg = c(5, 7)),
      card(fill = FALSE, card_header("Us vs the league (per match)"), DTOutput("tm_cmp", fill = FALSE),
           helpText("League average is the mean of all teams in this league. Same filters and players for both.")),
      card(fill = FALSE, card_header("Who is involved"), DTOutput("tm_pl", fill = FALSE)))),
  nav_panel("Match report",
    div(class = "cmp-pickers", selectInput("mr_match", "Match", choices = NULL)), uiOutput("mr_head"),
    layout_columns(fill = FALSE, col_widths = breakpoints(sm = 12, lg = c(6, 6)),
      card(fill = FALSE, card_header("xG timeline"), plotOutput("mr_xg", height = 420)),
      card(fill = FALSE, card_header("Momentum (threat created per 5 minutes)"), plotOutput("mr_mom", height = 420))),
    card(fill = FALSE, card_header("Shot maps"), layout_columns(fill = FALSE, col_widths = breakpoints(sm = 12, lg = c(6, 6)), plotOutput("mr_shots_a", height = 480), plotOutput("mr_shots_b", height = 480))),
    card(fill = FALSE, card_header("Lineups"), layout_columns(fill = FALSE, col_widths = breakpoints(sm = 12, lg = c(6, 6)), plotOutput("mr_lu_a", height = 560), plotOutput("mr_lu_b", height = 560)),
         helpText("Starting XI at their listed positions. Arrow: minute substituted off. Shape inferred from the positions."), DTOutput("mr_subs", fill = FALSE)),
    card(fill = FALSE, card_header("Match stats"), DTOutput("mr_stats", fill = FALSE))),
  nav_panel("Set pieces",
    div(class = "tp-bar", pills("sp_type", "Type", c("Corners" = "Corner", "Free kicks" = "Free Kick")), pills("sp_side", "Whose", c("Our attack" = "for", "Against us" = "against")),
        conditionalPanel("input.sp_type == 'Corner'", pills("sp_lr", "Corner from", c("Both" = "both", "Left" = "L", "Right" = "R")))),
    uiOutput("sp_tiles"),
    layout_columns(fill = FALSE, col_widths = breakpoints(sm = 12, xl = c(7, 5)),
      card(fill = FALSE, card_header("Where the deliveries land"), plotOutput("sp_map", height = 600),
           helpText("Gold: a shot followed in the same possession (dot size is its xG). Blue: reached a teammate without a shot. Grey: not completed. Boxes show the share of deliveries per zone.")),
      card(fill = FALSE, card_header("Zones"), DTOutput("sp_zones", fill = FALSE), helpText("Left and right are from the attacker's point of view."))),
    layout_columns(fill = FALSE, col_widths = breakpoints(sm = 12, lg = c(6, 6)),
      card(fill = FALSE, card_header(textOutput("sp_who_title")), plotOutput("sp_who", height = 420), DTOutput("sp_who_tbl", fill = FALSE)),
      card(fill = FALSE, card_header("Who takes them"), DTOutput("sp_takers", fill = FALSE), uiOutput("sp_note"))),
    div(class = "tp-spacer")),
  nav_panel("Similar players",
    div(class = "cmp-pickers", selectizeInput("sim_player", "Find players similar to", choices = NULL, options = list(placeholder = "Search any player", dropdownParent = "body")),
        div(pills("sim_scope", "Search in", c("All leagues" = "all", "Same league" = "same")), pills("sim_group", "Roles", c("Same position group" = "same", "Include AM + W" = "amw")),
            checkboxInput("sim_excl", "Leave out players from my club", TRUE))),
    helpText("Similarity is 100 minus the typical gap in percentile points across the headline KPIs that are reliable for the position group. Uses the minimum minutes from the sidebar."),
    layout_columns(fill = FALSE, col_widths = breakpoints(sm = 12, xl = c(7, 5)),
      card(fill = FALSE, card_header("Most similar players"), DTOutput("sim_tbl", fill = FALSE)),
      card(fill = FALSE, card_header("Profile shape: target vs selected"), plotOutput("sim_radar", height = 420), uiOutput("sim_why"),
           actionButton("sim_open", "Open in Compare", class = "btn-compare"))),
    div(class = "tp-spacer")),
  nav_panel("Player",
    card(fill = FALSE, uiOutput("header")),
    uiOutput("skill_cards"),
    layout_columns(fill = FALSE, col_widths = breakpoints(sm = 12, xl = c(5, 7)),
      card(fill = FALSE, card_header("Profile shape (area bars)"),
           plotOutput("radar", height = 440),
           helpText("Each spoke is an area bar. The gold ring is the 50th percentile, the average player in the pool.")),
      card(fill = FALSE, card_header("Head to head on the headline KPIs"),
           selectInput("cmp", "Compare with", c("Pool average (50th percentile)" = "pool")),
           plotOutput("dumbbell", height = 780),
           helpText("Dots are percentiles within position group. Low-reliability KPIs are left out."))),
    layout_columns(fill = FALSE, col_widths = breakpoints(sm = 12, xxl = c(7, 5)),
      card(fill = FALSE, card_header("Percentile bars vs position group"),
           layout_columns(fill = FALSE, col_widths = c(4, 4, 4),
             checkboxInput("show_detail", "Show underlying KPIs", TRUE),
             selectInput("dom", "Area", c("All areas" = "all", setNames(DOM$domain, DOM$domain_label))),
             helpText("Grey bars: low reliability, not in the area bar.")),
           uiOutput("bars_ui")),
      layout_columns(fill = FALSE, col_widths = c(12),
        card(fill = FALSE, card_header("Average position"), plotOutput("pitch", height = 360),
             helpText("Heatmaps and action maps are on the Maps page.")),
        card(fill = FALSE, card_header("Context (not scored)"), tableOutput("context")))),
    card(fill = FALSE, card_header("Scatterplot vs peers"),
         layout_columns(fill = FALSE, col_widths = c(3, 3, 6),
           selectInput("sx", "X axis", SC_CHOICES, selected = "npxg_p90"),
           selectInput("sy", "Y axis", SC_CHOICES, selected = "xa_open_p90"),
           helpText("Same comparison pool as the bars. Crests are WSL players. Blue ring: our team, gold halo: selected player. Grey dots: players from leagues without crests.")),
         plotOutput("scatter", height = 620))),
  nav_panel("Compare",
    div(class = "cmp-pickers",
        selectizeInput("cmpA", "Player A (blue)", choices = NULL, options = list(placeholder = "Search any player", dropdownParent = "body")),
        selectizeInput("cmpB", "Player B (gold)", choices = NULL, options = list(placeholder = "Search any player", dropdownParent = "body")),
        div(class = "cmp-any", checkboxInput("cmp_any", "Allow players from other positions (percentiles are within each position group, so read the gaps with care)", FALSE))),
    div(class = "cmp-share", downloadButton("dl_cmp", "Share comparison as image", class = "btn-share")),
    uiOutput("cmp_head"),
    layout_columns(fill = FALSE, col_widths = breakpoints(sm = 12, xl = c(5, 7)),
      card(fill = FALSE, card_header("Profile shape"), plotOutput("cmp_radar", height = 460)),
      card(fill = FALSE, card_header("Area scores, side by side"), plotOutput("cmp_fly", height = 460))),
    div(class = "tp-bar", pills("cmp_basis", "View", c("Percentiles" = "pct", "Values" = "raw")), pills("cmp_padj", "Defending counts", c("Possession adjusted (PAdj)" = "padj", "Raw counts" = "raw")), pills("cmp_vol", "Volume", c("Per 90" = "p90", "Total" = "tot")),
        div(class = "tp-note", "Percentiles are always against the player's position group and are calculated from possession adjusted counts. \"Defending counts\" and \"Volume\" change the values shown in the Values chart and the numbers table. Raw counts exist for tackles, interceptions, recoveries, clearances and blocks; every other KPI is unchanged. Rate KPIs (completion %, shares, the defensive index) have no total, so they stay as rates.")),
    card(fill = FALSE, card_header(textOutput("cmp_dumb_title")), plotOutput("cmp_dumb", height = 1500),
         helpText("Position adjusted: percentiles within each player's own position group. Raw: the actual numbers, each KPI on its own scale. Low-reliability KPIs are left out. Defending counts are possession adjusted.")),
    card(fill = FALSE, card_header("The numbers"), DTOutput("cmp_tbl", fill = FALSE)),
    card(fill = FALSE, card_header("Where they play"), pills("cmp_map", "Map", c("Touch heatmap" = "heat", "Shot map" = "shot", "Progressive actions" = "prog")),
         layout_columns(fill = FALSE, col_widths = breakpoints(sm = 12, lg = c(6, 6)), plotOutput("cmp_map_a", height = 600), plotOutput("cmp_map_b", height = 600)))),
  nav_panel("Maps",
    layout_columns(fill = FALSE, col_widths = c(3, 9),
      card(fill = FALSE, card_header("Controls"),
        selectInput("map_type", "Map", c("Touch heatmap" = "heat", "Shot map" = "shot", "Pass map" = "pass",
                                         "Progressive actions" = "prog", "Threat map (xT)" = "threat",
                                         "Synergy map (passing partners)" = "syn", "Pass sonar" = "sonar",
                                         "Take-ons" = "takeon", "Defensive actions" = "def")),
        selectInput("mp_match", "Match", c("All matches" = "all")),
        pills("mp_half", "Half", c("Both" = "both", "First half" = "1", "Second half" = "2")),
        pills("mp_state", "Game state", c("All" = "all", "Trailing" = "-1", "Level" = "0", "Leading" = "1")),
        conditionalPanel("input.map_type == 'shot'",
          pills("shot_style", "Style", c("StatsBomb style" = "sb", "Outcome colours" = "out"))),
        conditionalPanel("input.map_type == 'pass'",
          selectInput("pass_f", "Show", c("All passes" = "all", "Key passes and assists" = "key", "Into the final third" = "f3",
                                           "Into the box" = "box", "Through balls" = "through"))),
        conditionalPanel("input.map_type == 'threat'",
          pills("thr_basis", "Heat shows xT created at", c("Where actions end" = "end", "Where actions start" = "start"))),
        conditionalPanel("input.map_type == 'sonar'",
          pills("sonar_col", "Colour by", c("Average pass length" = "len", "Completion %" = "cmp"))),
        conditionalPanel("input.map_type == 'syn'",
          sliderInput("syn_n", "Partners shown", 3, 15, 10, step = 1)),
        helpText("Attacking up the page. Filters apply to every map.")),
      card(fill = FALSE, card_header(textOutput("map_title")), plotOutput("map", height = 680), DTOutput("map_tbl", fill = FALSE)))),
  nav_panel("About", about_page()))

# ---- Server ----
server <- function(input, output, session) {
  pool_leagues <- reactive(if (identical(input$pool, "all")) LEAGUES else input$pool)

  observeEvent(input$league, {
    tm <- sort(unique(M$team[M$league == input$league]))
    updateSelectInput(session, "team", choices = tm, selected = if (DEFAULT_TEAM %in% tm) DEFAULT_TEAM else tm[1])
  })
  squad <- reactive({
    req(input$team, input$league)
    M %>% filter(league == input$league, team == input$team) %>% arrange(desc(minutes))
  })
  observeEvent(list(squad(), input$player), {
    req(input$player); me <- squad() %>% filter(as.character(player.id) == input$player); req(nrow(me) == 1)
    sq2 <- squad() %>% filter(pos_group %in% GROUPS, role_family(pos_group) == role_family(me$pos_group), player.id != me$player.id)
    updateSelectInput(session, "cmp", choices = c("Pool average (50th percentile)" = "pool",
      setNames(as.character(sq2$player.id), paste0(sq2$player, " (", sq2$pos_group, ")"))))
  })
  observeEvent(squad(), {
    sq <- squad()
    updateSelectInput(session, "player", choices = setNames(as.character(sq$player.id),
                      paste0(sq$player, " (", sq$pos_group, ", ", round(sq$minutes), " min)")))
  })
  sel <- reactive({ req(input$player); M %>% filter(league == input$league, as.character(player.id) == input$player) %>% head(1) })
  prof <- reactive({
    p <- sel(); req(nrow(p) == 1, p$pos_group %in% GROUPS)
    build_profile(p, input$merge, pool_leagues(), input$min_min)
  })

  # ---- Match report ----
  names_lg <- reactive({ M %>% filter(league == input$league) %>% distinct(player.id, player) })
  lineups_lg <- reactive({ f <- file.path("data", paste0("lineups_", gsub("[^A-Za-z]", "", input$league), ".rds")); validate(need(file.exists(f), "Lineup data not found. Run 22_lineups_setpieces.R first.")); readRDS(f)$players })
  observeEvent(list(input$team, input$league), {
    req(input$team, !is.null(get_maps(input$league))); d <- tm_season()$me %>% arrange(desc(idx)); req(nrow(d) > 0)
    lab <- paste0(ifelse(is.na(d$match_date), "", format(d$match_date, "%d %b")), "  |  ", ifelse(is.na(d$ven), "", paste0(d$ven, " ")), "vs ", team_display(d$team_opp), "  |  ", d$goals, "-", d$goals_opp)
    updateSelectInput(session, "mr_match", choices = setNames(as.character(d$match_id), lab))
  })
  mr_row <- reactive({ req(nzchar(input$mr_match %||% "")); d <- tm_season()$me %>% filter(match_id == as.numeric(input$mr_match)); req(nrow(d) == 1); d })
  mr_ev <- reactive({ lg_maps() %>% filter(match_id == as.numeric(input$mr_match)) })
  output$mr_head <- renderUI({
    r <- mr_row(); home_first <- is.na(r$ven) || r$ven == "H"
    lt <- if (home_first) input$team else r$team_opp; rt <- if (home_first) r$team_opp else input$team
    ls <- if (home_first) r$goals else r$goals_opp; rs <- if (home_first) r$goals_opp else r$goals
    lx <- if (home_first) r$xg else r$xg_opp; rx <- if (home_first) r$xg_opp else r$xg
    div(class = "ov-band", div(class = "mr-side", crest_tag(lt, 70), div(class = "mr-team", team_display(lt)), div(class = "mr-xg", sprintf("xG %.2f", lx))),
        div(style = "text-align:center;", div(class = "mr-score", paste(ls, "-", rs)), div(class = "ov-sub", paste0(if (!is.na(r$match_date)) format(r$match_date, "%d %b %Y") else "", if (!is.na(r$match_week)) paste0("  |  Matchweek ", r$match_week) else ""))),
        div(class = "mr-side", crest_tag(rt, 70), div(class = "mr-team", team_display(rt)), div(class = "mr-xg", sprintf("xG %.2f", rx))))
  })
  output$mr_xg <- renderPlot({
    ev <- mr_ev(); r <- mr_row(); nm <- names_lg()
    sh <- ev %>% filter(kind == "shot") %>% mutate(t = minute + second / 60, who = ifelse(team == input$team, "us", "them")) %>% left_join(nm, by = "player.id"); validate(need(nrow(sh) > 0, "No shots in this match."))
    endt <- max(95, max(ev$minute, na.rm = TRUE)); labs_w <- c(us = team_display(input$team), them = team_display(r$team_opp))
    curve <- bind_rows(lapply(c("us", "them"), function(w) { d <- sh %>% filter(who == w, !tf(pen)) %>% arrange(t); tibble(t = c(0, d$t, endt), cum = c(0, cumsum(coalesce(d$xg, 0)), sum(coalesce(d$xg, 0))), who = w) }))
    gl <- sh %>% filter(tf(ok)) %>% rowwise() %>% mutate(cum = { cv <- curve[curve$who == who, ]; cv$cum[max(findInterval(t, cv$t), 1)] }) %>% ungroup() %>% mutate(lab = paste0(surname(player), " ", minute, "'"))
    g <- ggplot(curve, aes(t, cum, colour = who)) + geom_vline(xintercept = 45, linetype = "dashed", colour = "#9AA3B5") + geom_step(linewidth = 1.8) +
      scale_colour_manual(values = c(us = BRAND$primary, them = "#D9A300"), labels = labs_w, name = NULL) + scale_x_continuous(breaks = seq(0, 90, 15)) +
      labs(x = "Minute", y = "Cumulative xG (penalties excluded)", subtitle = "Circles: goals, labelled with scorer and minute.") + theme_lcw(base_size = 13) + theme(legend.position = "top", panel.grid.minor = element_blank())
    if (nrow(gl)) g <- g + geom_point(data = gl, aes(t, cum), shape = 21, size = 5, fill = "white", stroke = 1.6, show.legend = FALSE) +
      (if (HAS_REPEL) ggrepel::geom_text_repel(data = gl, aes(t, cum, label = lab), size = 3.8, fontface = "bold", show.legend = FALSE, nudge_y = 0.12, segment.colour = NA) else geom_text(data = gl, aes(t, cum, label = lab), size = 3.6, vjust = -1, show.legend = FALSE))
    g
  })
  output$mr_mom <- renderPlot({
    ev <- mr_ev(); r <- mr_row()
    bins <- ev %>% mutate(bin = floor(minute / 5) * 5, w = ifelse(!is.na(xt) & xt > 0, xt, 0)) %>% group_by(bin, who = ifelse(team == input$team, "us", "them")) %>% summarise(v = sum(w), .groups = "drop") %>% mutate(v = ifelse(who == "us", v, -v))
    validate(need(nrow(bins) > 0, "No data for this match."))
    gl <- ev %>% filter(kind == "shot", tf(ok)) %>% transmute(minute, who = ifelse(team == input$team, "us", "them")); top <- max(abs(bins$v)) * 1.25; gl$y <- ifelse(gl$who == "us", top, -top)
    ggplot(bins, aes(bin + 2.5, v, fill = who)) + geom_hline(yintercept = 0, colour = BRAND$muted) + geom_vline(xintercept = 45, linetype = "dashed", colour = "#9AA3B5") + geom_col(width = 4.4) +
      scale_fill_manual(values = c(us = BRAND$primary, them = BRAND$accent), labels = c(us = team_display(input$team), them = team_display(r$team_opp)), name = NULL) +
      geom_point(data = gl, aes(minute, y), inherit.aes = FALSE, shape = 21, size = 6, fill = "white", colour = BRAND$primary, stroke = 1.5) + geom_text(data = gl, aes(minute, y, label = "G"), inherit.aes = FALSE, size = 3.2, fontface = "bold", colour = BRAND$primary) +
      scale_x_continuous(breaks = seq(0, 120, 15)) + labs(x = "Minute", y = "xT per 5 minutes", subtitle = "Above the line: our threat. Below: theirs. G: goal.") +
      theme_lcw(base_size = 13) + theme(panel.grid.minor = element_blank(), legend.position = "top")
  })
  mr_shot_map <- function(team_name, col) {
    d <- mr_ev() %>% filter(kind == "shot", team == team_name, !is.na(xg)); validate(need(nrow(d) > 0, "No shots."))
    pitch_base(TRUE, TRUE) + geom_point(data = d, aes(y, x, size = xg, fill = tf(ok)), shape = 21, colour = "white", stroke = 0.8, alpha = 0.92) +
      scale_fill_manual(values = c(`TRUE` = BRAND$accent, `FALSE` = col), guide = "none") + scale_size_area(max_size = 14, guide = "none") +
      labs(title = team_display(team_name), subtitle = sprintf("%d shots, %d goals, %.2f xG. Gold: goal.", nrow(d), sum(tf(d$ok)), sum(d$xg)))
  }
  output$mr_shots_a <- renderPlot({ mr_shot_map(input$team, BRAND$mid) })
  output$mr_shots_b <- renderPlot({ mr_shot_map(mr_row()$team_opp, "#D9A300") })
  mr_lineup <- function(team_name) {
    p <- lineups_lg() %>% filter(match_id == as.numeric(input$mr_match), team == team_name, start); validate(need(nrow(p) > 0, "No lineup stored for this match."))
    xy <- t(vapply(p$position, function(z) { v <- LINEUP_XY[[z]]; if (is.null(v)) c(40, 60) else v }, numeric(2))); p$px <- xy[, 1]; p$py <- xy[, 2]
    p <- p %>% group_by(px, py) %>% mutate(px = px + (row_number() - 1) * 6) %>% ungroup()
    p$off <- ifelse(p$off_min < max(p$off_min, na.rm = TRUE) & p$minutes < max(p$minutes), paste0("\n", intToUtf8(8595), " ", p$off_min, "'"), "")
    pitch_base() + geom_point(data = p, aes(px, py), size = 12, shape = 21, fill = BRAND$primary, colour = "white", stroke = 1.5) +
      geom_text(data = p, aes(px, py, label = jersey), colour = "white", fontface = "bold", size = 4.2) + geom_text(data = p, aes(px, py - 5.5, label = paste0(surname(player), off)), size = 3.6, fontface = "bold", colour = BRAND$dark, lineheight = 0.9, vjust = 1) +
      labs(title = team_display(team_name), subtitle = paste0("Shape ", shape_of(p$position)))
  }
  output$mr_lu_a <- renderPlot({ mr_lineup(input$team) })
  output$mr_lu_b <- renderPlot({ mr_lineup(mr_row()$team_opp) })
  output$mr_subs <- renderDT({
    p <- lineups_lg() %>% filter(match_id == as.numeric(input$mr_match)); on <- p %>% filter(!start) %>% arrange(on_min)
    validate(need(nrow(on) > 0, "No substitutions."))
    off <- p %>% filter(off_min < max(off_min)); t <- on %>% rowwise() %>% mutate(replaced = { o <- off[off$team == team & off$off_min == on_min, ]; if (nrow(o)) o$player[1] else "" }) %>% ungroup()
    datatable(transmute(t, Minute = on_min, Team = team_display(team), `Comes on` = player, `Goes off` = replaced), rownames = FALSE, class = "compact stripe", options = list(dom = "t", ordering = FALSE, pageLength = 20))
  })
  output$mr_stats <- renderDT({
    id <- as.numeric(input$mr_match); r <- mr_row(); T <- tp_T() %>% filter(match_id == id)
    f <- function(tm) { x <- T[T$team == tm, ]; c(sprintf("%.2f", x$xg), x$shots, if (tm == input$team) r$goals else r$goals_opp, sprintf("%.0f%%", 100 * x$passes / max(x$passes + x$passes_against, 1)), sprintf("%.1f", x$p60_against / max(x$da60, 1)), x$entries, x$tov, x$regains_high, sprintf("%.2f", x$xt)) }
    t <- tibble(Stat = c("xG", "Shots", "Goals", "Share of passes", "PPDA (lower = more pressing)", "Final third entries", "Turnovers", "High regains", "Threat created (xT)"), a = f(input$team), b = f(r$team_opp))
    names(t)[2:3] <- c(team_display(input$team), team_display(r$team_opp)); datatable(t, rownames = FALSE, class = "compact stripe", options = list(dom = "t", ordering = FALSE, pageLength = 12))
  })

  # ---- Set pieces ----
  sp_all <- reactive({ f <- file.path("data", paste0("setpieces_", gsub("[^A-Za-z]", "", input$league), ".rds")); validate(need(file.exists(f), "Set piece data not found. Run 22_lineups_setpieces.R first.")); readRDS(f) })
  sp_set <- reactive({
    d <- sp_all()$deliv %>% filter(type == (input$sp_type %||% "Corner")); side <- input$sp_side %||% "for"
    d <- if (side == "for") d %>% filter(team == input$team) else d %>% filter(opp == input$team)
    if (identical(input$sp_type, "Corner") && !identical(input$sp_lr %||% "both", "both")) d <- d %>% filter(if (input$sp_lr == "L") y < 40 else y >= 40)
    d
  })
  sp_team_tbl <- reactive({
    typ <- input$sp_type %||% "Corner"; nm <- tm_season()$all %>% count(team, name = "n_m"); d <- sp_all()$deliv %>% filter(type == typ)
    f <- function(g, key) d %>% group_by(.data[[g]]) %>% summarise(n = n(), ok = mean(ok), shot = mean(!is.na(shot_xg)), xg = sum(coalesce(shot_xg, 0)) / n(), goals = sum(tf(shot_goal)), .groups = "drop") %>% rename(team = 1) %>% left_join(nm, by = "team") %>% mutate(per_m = n / n_m)
    list(`for` = f("team"), against = f("opp"))
  })
  output$sp_tiles <- renderUI({
    d <- sp_set(); side <- input$sp_side %||% "for"; lg <- sp_team_tbl()[[side]]; me <- lg[lg$team == input$team, ]; req(nrow(me) == 1)
    one <- function(label, key, fmt, mult = 1, hib = TRUE, unit = "") {
      v <- me[[key]] * mult; others <- lg[[key]][lg$team != input$team] * mult; rk <- 1 + sum(if (hib) others > v else others < v)
      div(class = "tp-tile", div(class = "l", label), div(class = "v", sprintf(fmt, v), span(class = "u", unit)), span(class = paste("rk", if (rk <= 3) "good"), paste0(ord_suffix(rk), " of ", nrow(lg))))
    }
    good_hi <- side == "for"
    div(class = "tp-tiles", one("Deliveries per match", "per_m", "%.1f", hib = good_hi), one("Completed", "ok", "%.0f", 100, hib = good_hi, unit = "%"), one("Led to a shot", "shot", "%.0f", 100, hib = good_hi, unit = "%"),
        one("xG per delivery", "xg", "%.3f", hib = good_hi), one("Goals", "goals", "%.0f", hib = good_hi))
  })
  output$sp_map <- renderPlot({
    d <- sp_set(); validate(need(nrow(d) >= 5, "Not enough deliveries.")); d$z <- zone_of(d$ex, d$ey)
    d$cls <- ifelse(!is.na(d$shot_xg), "Shot", ifelse(d$ok, "Reached a teammate", "Not completed"))
    zs <- d %>% count(z, .drop = FALSE) %>% mutate(sh = n / sum(n))
    boxes <- tibble(z = factor(c("Six yard box", "Penalty spot area", "Box, left", "Box, right", "Edge of the box"), levels = levels(d$z)), x0 = c(30, 30, 18, 50, 18), x1 = c(50, 50, 30, 62, 62), y0 = c(114, 102, 102, 102, 94), y1 = c(120, 114, 120, 120, 102)) %>% left_join(zs, by = "z")
    pitch_base(TRUE) + geom_rect(data = boxes, aes(xmin = x0, xmax = x1, ymin = y0, ymax = y1), fill = NA, colour = BRAND$primary, linewidth = 0.6, linetype = "dashed", inherit.aes = FALSE) +
      geom_point(data = d %>% filter(cls != "Shot"), aes(ey, ex, colour = cls), size = 2.6, alpha = 0.55) +
      geom_point(data = d %>% filter(cls == "Shot"), aes(ey, ex, size = shot_xg), shape = 21, fill = BRAND$accent, colour = BRAND$primary, stroke = 1.1, alpha = 0.95) +
      geom_label(data = boxes %>% filter(z != "Edge of the box"), aes(x = (x0 + x1) / 2, y = (y0 + y1) / 2, label = sprintf("%.0f%%", 100 * sh)), size = 3.4, fill = alpha("white", 0.85), label.size = 0, colour = BRAND$primary, fontface = "bold", inherit.aes = FALSE) +
      scale_colour_manual(values = c(`Reached a teammate` = BRAND$mid, `Not completed` = "#9AA3B5"), name = NULL) + scale_size_area(max_size = 9, guide = "none") +
      labs(subtitle = sprintf("%d %s deliveries. %d led to a shot, %d goals.", nrow(d), tolower(if (input$sp_type == "Corner") "corner" else "free kick"), sum(!is.na(d$shot_xg)), sum(tf(d$shot_goal)))) + theme(legend.position = "bottom")
  })
  output$sp_zones <- renderDT({
    d <- sp_set(); req(nrow(d) >= 5); d$z <- zone_of(d$ex, d$ey)
    t <- d %>% group_by(Zone = z, .drop = FALSE) %>% summarise(Deliveries = n(), `Share (%)` = round(100 * n() / nrow(d)), `Completed (%)` = round(100 * mean(ok)), `Led to a shot` = sum(!is.na(shot_xg)), xG = round(sum(coalesce(shot_xg, 0)), 2), Goals = sum(tf(shot_goal)), .groups = "drop") %>% mutate(Zone = as.character(Zone))
    datatable(t, rownames = FALSE, class = "compact stripe", options = list(dom = "t", ordering = FALSE, pageLength = 8))
  })
  output$sp_who_title <- renderText(if ((input$sp_side %||% "for") == "for") "Who attacks the ball" else "Who defends the ball")
  sp_who_data <- reactive({
    d <- sp_set(); nm <- names_lg(); side <- input$sp_side %||% "for"
    if (side == "for") {
      a <- d %>% filter(ok, !is.na(recipient)) %>% count(player.id = recipient, name = "Contacts")
      b <- d %>% filter(!is.na(shooter)) %>% group_by(player.id = shooter) %>% summarise(Shots = n(), Headers = sum(shot_body %in% "Head"), Goals = sum(tf(shot_goal)), xG = round(sum(shot_xg), 2), .groups = "drop")
      full_join(a, b, by = "player.id") %>% mutate(across(where(is.numeric), ~ coalesce(.x, 0))) %>% left_join(nm, by = "player.id") %>% arrange(desc(xG), desc(Contacts))
    } else {
      d %>% filter(!is.na(def_player)) %>% group_by(player.id = def_player) %>% summarise(Clearances = sum(def_type == "Clearance"), Blocks = sum(def_type == "Block"), Duels = sum(def_type == "Duel"), Interceptions = sum(def_type == "Interception"), `Goalkeeper` = sum(def_type == "Goal Keeper"), Total = n(), .groups = "drop") %>%
        left_join(nm, by = "player.id") %>% arrange(desc(Total))
    }
  })
  output$sp_who <- renderPlot({
    t <- sp_who_data() %>% head(10); validate(need(nrow(t) > 0, "Nothing to show for these filters.")); side <- input$sp_side %||% "for"
    if (side == "for") {
      t$player <- reorder(t$player, t$xG + t$Contacts / 100)
      ggplot(t, aes(xG, player)) + geom_col(fill = BRAND$primary, width = 0.7) + geom_text(aes(label = sprintf("%d shots, %d headers, %d contacts", Shots, Headers, Contacts)), hjust = -0.05, size = 3.6, colour = BRAND$dark) +
        scale_x_continuous(expand = expansion(mult = c(0, 0.65))) + labs(x = "xG from shots after these deliveries", y = NULL, subtitle = "First contacts are completed deliveries received by the player.") + theme_lcw(base_size = 13) + theme(panel.grid.major.y = element_blank())
    } else {
      t$player <- reorder(t$player, t$Total)
      ggplot(t, aes(Total, player)) + geom_col(fill = BRAND$primary, width = 0.7) + geom_text(aes(label = sprintf("%d clearances, %d blocks", Clearances, Blocks)), hjust = -0.05, size = 3.6, colour = BRAND$dark) +
        scale_x_continuous(expand = expansion(mult = c(0, 0.6))) + labs(x = "First defensive touches after the delivery", y = NULL, subtitle = "First touch by our team within 6 events of the delivery.") + theme_lcw(base_size = 13) + theme(panel.grid.major.y = element_blank())
    }
  })
  output$sp_who_tbl <- renderDT({ t <- sp_who_data() %>% head(12); req(nrow(t) > 0); datatable(t %>% select(Player = player, everything(), -player.id), rownames = FALSE, class = "compact stripe", options = list(dom = "t", ordering = TRUE, pageLength = 12)) })
  output$sp_takers <- renderDT({
    d <- sp_set(); nm <- names_lg(); req(nrow(d) > 0)
    t <- d %>% group_by(player.id = taker) %>% summarise(Deliveries = n(), `Completed (%)` = round(100 * mean(ok)), `Led to a shot` = sum(!is.na(shot_xg)), xG = round(sum(coalesce(shot_xg, 0)), 2), `xG per delivery` = round(sum(coalesce(shot_xg, 0)) / n(), 3), .groups = "drop") %>%
      left_join(nm, by = "player.id") %>% arrange(desc(Deliveries)) %>% head(10)
    datatable(t %>% select(Player = player, everything(), -player.id), rownames = FALSE, class = "compact stripe", options = list(dom = "t", ordering = TRUE, pageLength = 10))
  })
  output$sp_note <- renderUI({
    if (!identical(input$sp_type, "Free Kick")) return(NULL)
    d <- sp_all()$dfk %>% filter(team == input$team)
    if (!identical(input$sp_side %||% "for", "for")) d <- sp_all()$dfk %>% filter(match_id %in% tm_season()$me$match_id, team != input$team)
    div(class = "ov-note", sprintf("Direct free kick shots are separate from deliveries: %d shots, %d goals, %.2f xG.", nrow(d), sum(tf(d$goal)), sum(d$xg)))
  })

  # ---- Similar players ----
  observeEvent(sel(), { p <- sel(); if (nrow(p) == 1 && p$pos_group %in% GROUPS) updateSelectizeInput(session, "sim_player", choices = CMP_CHOICES, selected = paste(p$league, p$player.id, sep = "|"), server = TRUE) })
  sim_calc <- reactive({
    req(nzchar(input$sim_player %||% "")); p <- get_pl(input$sim_player); req(nrow(p) == 1, p$pos_group %in% GROUPS)
    gset <- group_set(p$pos_group, identical(input$sim_group, "amw")); lgs <- if (identical(input$sim_scope, "same")) p$league else LEAGUES
    pool <- M %>% filter(pos_group %in% gset, league %in% lgs, minutes >= input$min_min)
    keys <- REG %>% filter(role == "headline") %>% distinct(key, domain, dir) %>% inner_join(tier_tbl(gset) %>% select(key, tier), by = "key") %>% filter(tier != "D", key %in% names(pool))
    P <- vapply(seq_len(nrow(keys)), function(i) { k <- keys$key[i]; v <- pctl(pool[[k]], pool[[k]]); if (keys$dir[i] < 0) 100 - v else v }, numeric(nrow(pool))); colnames(P) <- keys$key
    tv <- vapply(seq_len(nrow(keys)), function(i) { k <- keys$key[i]; v <- pctl(as.numeric(p[[k]]), pool[[k]]); if (keys$dir[i] < 0) 100 - v else v }, numeric(1))
    isme <- pool$player.id == p$player.id & pool$league == p$league
    D <- sweep(P, 2, tv, "-"); ok <- rowSums(!is.na(D)); rmsd <- sqrt(rowMeans(D^2, na.rm = TRUE))
    dom_gap <- sapply(unique(keys$domain), function(dm) rowMeans(D[, keys$domain == dm, drop = FALSE], na.rm = TRUE)); colnames(dom_gap) <- unique(keys$domain)
    big <- apply(dom_gap, 1, function(r) if (all(is.na(r))) NA_character_ else names(r)[which.max(abs(r))]); sgn <- vapply(seq_len(nrow(dom_gap)), function(i) if (is.na(big[i])) 0 else sign(dom_gap[i, big[i]]), numeric(1))
    out <- pool %>% mutate(sim = 100 - rmsd, n_keys = ok, gap_dom = big, gap_dir = sgn, avgp = rowMeans(P, na.rm = TRUE)) %>% filter(!isme, n_keys >= 0.6 * nrow(keys), is.finite(sim))
    if (isTRUE(input$sim_excl)) out <- out %>% filter(!(team == input$team & league == input$league))
    out <- out %>% arrange(desc(sim)) %>% head(15)
    out$gap_txt <- ifelse(is.na(out$gap_dom), "", paste0(DOM$domain_label[match(out$gap_dom, DOM$domain)], ifelse(out$gap_dir > 0, ": higher", ": lower")))
    list(target = p, out = out, gset = gset, lgs = lgs)
  })
  output$sim_tbl <- renderDT({
    r <- sim_calc(); o <- r$out; req(nrow(o) > 0)
    t <- tibble(Player = o$player, Club = team_display(o$team), League = o$league, Role = o$pos_group, Minutes = round(o$minutes), Similarity = round(o$sim, 1), `Avg percentile` = round(o$avgp), `Biggest difference` = o$gap_txt)
    datatable(t, rownames = FALSE, class = "compact stripe", selection = list(mode = "single", selected = 1), options = list(dom = "t", ordering = FALSE, pageLength = 15)) %>% formatStyle("Similarity", background = styleColorBar(c(50, 100), "#A9C0E8"), backgroundSize = "100% 70%", backgroundRepeat = "no-repeat", backgroundPosition = "center")
  })
  sim_pick <- reactive({ r <- sim_calc(); i <- input$sim_tbl_rows_selected %||% 1; req(nrow(r$out) >= i); r$out[i, ] })
  output$sim_radar <- renderPlot({
    r <- sim_calc(); c2 <- sim_pick(); amw <- identical(input$sim_group, "amw")
    pa <- build_profile(r$target, amw, r$lgs, input$min_min); pb <- build_profile(c2, amw, r$lgs, input$min_min)
    radar_overlay(cmp_bars(pa), cmp_bars(pb), r$target$player, c2$player)
  })
  output$sim_why <- renderUI({ c2 <- sim_pick(); div(class = "ov-note", tags$b(paste0(c2$player, " (", team_display(c2$team), ", ", c2$league, ")")), sprintf(": similarity %.0f. Biggest difference: %s.", c2$sim, if (nzchar(c2$gap_txt)) tolower(c2$gap_txt) else "none")) })
  observeEvent(input$sim_open, {
    r <- sim_calc(); c2 <- sim_pick()
    updateSelectizeInput(session, "cmpA", choices = CMP_CHOICES, selected = paste(r$target$league, r$target$player.id, sep = "|"), server = TRUE)
    updateSelectizeInput(session, "cmpB", choices = CMP_CHOICES, selected = paste(c2$league, c2$player.id, sep = "|"), server = TRUE); nav_select("page", "Compare")
  })

  # ---- Compare page ----
  observeEvent(input$go_compare, nav_select("page", "Compare"))
  get_pl <- function(key) { kk <- strsplit(key, "|", fixed = TRUE)[[1]]; M %>% filter(league == kk[1], as.character(player.id) == kk[2]) %>% head(1) }
  observeEvent(sel(), {
    p <- sel(); if (nrow(p) == 1 && p$pos_group %in% GROUPS) updateSelectizeInput(session, "cmpA", choices = CMP_CHOICES, selected = paste(p$league, p$player.id, sep = "|"), server = TRUE)
  })
  # Player B can only be from the same role as Player A unless the tick box is on (wingers and attacking midfielders count as one role)
  observeEvent(list(input$cmpA, input$cmp_any), {
    req(nzchar(input$cmpA %||% "")); a <- get_pl(input$cmpA); req(nrow(a) == 1)
    ok_keys <- if (isTRUE(input$cmp_any)) CMP_META$key else CMP_META$key[role_family(CMP_META$pos_group) == role_family(a$pos_group)]
    ch <- CMP_CHOICES[CMP_CHOICES %in% ok_keys]; cur <- input$cmpB %||% ""
    keep <- nzchar(cur) && cur %in% ch && cur != input$cmpA
    sel_b <- if (keep) cur else {
      cand <- M %>% filter(league == a$league, role_family(pos_group) == role_family(a$pos_group), player.id != a$player.id, minutes >= 450) %>% arrange(desc(minutes)) %>% head(1)
      if (nrow(cand) == 1) paste(cand$league, cand$player.id, sep = "|") else ""
    }
    updateSelectizeInput(session, "cmpB", choices = ch, selected = sel_b, server = TRUE)
  })
  cmp_a <- reactive({ req(nzchar(input$cmpA %||% "")); p <- get_pl(input$cmpA); req(nrow(p) == 1); p })
  cmp_b <- reactive({ req(nzchar(input$cmpB %||% "")); p <- get_pl(input$cmpB); req(nrow(p) == 1); p })
  cmp_pa <- reactive({ build_profile(cmp_a(), input$merge, pool_leagues(), input$min_min) })
  cmp_pb <- reactive({ build_profile(cmp_b(), input$merge, pool_leagues(), input$min_min) })
  cmp_bars <- function(pr) { b <- bars_from(pr$d); b$bar[match(DOM$domain, b$domain)] }
  output$cmp_head <- renderUI({
    pa <- cmp_a(); pb <- cmp_b(); ba <- cmp_bars(cmp_pa()); bb <- cmp_bars(cmp_pb())
    card_of <- function(p, b, cls) div(class = paste("cmp-card", cls), photo_tag(p$player.id, p$player, size = 76, team = p$team),
      div(div(class = "cmp-name", p$player), div(style = "margin:2px 0;", crest_tag(p$team, 26)), div(class = "cmp-meta", paste0(unname(GROUP_NAMES[p$pos_group]), "  |  ", team_display(p$team), "  |  ", p$league)),
          div(class = "cmp-meta", paste0(round(p$minutes), " min  |  ", p$matches, " matches"))))
    div(class = "cmp-head", card_of(pa, ba, "cmp-a"), div(class = "cmp-vs", "VS"), card_of(pb, bb, "cmp-b"),
        if (pa$pos_group != pb$pos_group) div(class = "cmp-note", "Different position groups: each player's percentiles are against their own position group, so read the gaps with care."))
  })
  output$dl_cmp <- downloadHandler(
    filename = function() paste0(gsub("[^A-Za-z0-9]+", "_", cmp_a()$player), "_vs_", gsub("[^A-Za-z0-9]+", "_", cmp_b()$player), ".png"), contentType = "image/png",
    content = function(file) {
      open_png(file); on.exit(dev.off())
      print(make_cmp_card(cmp_a(), cmp_b(), cmp_bars(cmp_pa()), cmp_bars(cmp_pb())))
    })
  output$cmp_radar <- renderPlot({
    pa <- cmp_a(); pb <- cmp_b(); n <- nrow(DOM); a <- 2 * pi * (seq_len(n) - 1) / n
    poly <- function(v, who) tibble(x = ifelse(is.na(v), 0, v) / 100 * sin(a), y = ifelse(is.na(v), 0, v) / 100 * cos(a), who = who)
    dd <- bind_rows(poly(cmp_bars(cmp_pa()), pa$player), poly(cmp_bars(cmp_pb()), pb$player))
    cols <- setNames(c(BRAND$primary, BRAND$accent), c(pa$player, pb$player))
    ring <- function(r) { t <- seq(0, 2 * pi, length.out = 200); tibble(x = r * sin(t), y = r * cos(t)) }
    lab <- tibble(x = 1.22 * sin(a), y = 1.22 * cos(a), l = gsub(" ", "\n", DOM$domain_label, fixed = TRUE))
    ggplot() + geom_path(data = ring(0.25), aes(x, y), colour = "#E3E6EE") + geom_path(data = ring(0.75), aes(x, y), colour = "#E3E6EE") + geom_path(data = ring(1), aes(x, y), colour = BRAND$muted) +
      geom_path(data = ring(0.5), aes(x, y), colour = "#9AA3B5", linewidth = 0.8, linetype = "dashed") +
      geom_segment(data = tibble(a = a), aes(x = 0, y = 0, xend = sin(a), yend = cos(a)), colour = "#E3E6EE") +
      geom_polygon(data = dd, aes(x, y, fill = who, colour = who), alpha = 0.2, linewidth = 1.3) + geom_point(data = dd, aes(x, y, colour = who), size = 2.8) +
      geom_text(data = lab, aes(x, y, label = l), size = 3.8, colour = BRAND$dark, lineheight = 0.9) +
      scale_fill_manual(values = cols, name = NULL) + scale_colour_manual(values = cols, name = NULL) +
      coord_fixed(xlim = c(-1.6, 1.6), ylim = c(-1.45, 1.45)) + theme_void(base_size = 13, base_family = FONT_BODY) + theme(legend.position = "bottom")
  })
  output$cmp_fly <- renderPlot({
    pa <- cmp_a(); pb <- cmp_b(); va <- cmp_bars(cmp_pa()); vb <- cmp_bars(cmp_pb())
    d <- tibble(lab = factor(DOM$domain_label, levels = rev(DOM$domain_label)), a = va, b = vb)
    long <- bind_rows(tibble(lab = d$lab, v = -ifelse(is.na(d$a), 0, d$a), who = pa$player, s = d$a), tibble(lab = d$lab, v = ifelse(is.na(d$b), 0, d$b), who = pb$player, s = d$b))
    long$who <- factor(long$who, levels = c(pa$player, pb$player))
    ggplot(long, aes(v, lab, fill = who)) + geom_col(width = 0.65) + geom_vline(xintercept = 0, colour = BRAND$dark) +
      geom_vline(xintercept = c(-50, 50), colour = "#9AA3B5", linetype = "dashed") +
      geom_text(aes(label = ifelse(is.na(s), "n/a", sprintf("%.0f", s)), x = v + ifelse(v < 0, -4, 4)), hjust = ifelse(long$v < 0, 1, 0), size = 4.2, fontface = "bold", colour = BRAND$dark) +
      scale_fill_manual(values = setNames(c(BRAND$primary, BRAND$accent), levels(long$who)), name = NULL) +
      scale_x_continuous(limits = c(-118, 118), breaks = c(-100, -50, 0, 50, 100), labels = abs) + labs(x = "Percentile", y = NULL, subtitle = "Dashed lines: the average player in each pool (50).") +
      theme_lcw(base_size = 13) + theme(panel.grid.major.y = element_blank(), legend.position = "bottom")
  })
  cmp_join <- reactive({
    da <- cmp_pa()$d %>% filter(role == "headline", !excluded) %>% select(key, label, domain, domain_label, va = value, pa = pct)
    db <- cmp_pb()$d %>% filter(role == "headline") %>% select(key, vb = value, pb = pct)
    da %>% inner_join(db, by = "key") %>% mutate(domain = factor(as.character(domain), levels = DOM$domain)) %>% arrange(domain)
  })
  # raw value in the chosen volume: totals only for per 90 KPIs (value x minutes / 90); rates stay rates
  cmp_conv <- function(key, v, minutes) if (identical(input$cmp_vol, "tot")) ifelse(grepl("_p90$", key), v * minutes / 90, v) else v
  cmp_unit <- function(key) if (identical(input$cmp_vol, "tot") && grepl("_p90$", key)) " (total)" else if (grepl("_p90$", key)) " (per 90)" else " (rate)"
  RAWMAP <- c(tackles_int_p90 = "tackles_int_raw_p90", recoveries_p90 = "recoveries_raw_p90", clearances_p90 = "clearances_raw_p90", blocks_p90 = "blocks_raw_p90",
              tackles_won_p90 = "tackles_won_raw_p90", dribbled_past_p90 = "dribbled_past_raw_p90")
  raw_def <- reactive({ f <- "data/defending_raw.csv"; if (file.exists(f)) read.csv(f, check.names = FALSE, stringsAsFactors = FALSE) else NULL })
  use_raw <- reactive(identical(input$cmp_padj, "raw") && !is.null(raw_def()))
  raw_p90 <- function(p, key) { rd <- raw_def(); r <- rd[rd$league == p$league & rd$player.id == p$player.id, ]; if (nrow(r) == 1) r[[RAWMAP[[key]]]] else NA_real_ }
  cmp_vals <- function(d) {
    A <- d$va; B <- d$vb
    if (use_raw()) { m <- d$key %in% names(RAWMAP); A[m] <- vapply(d$key[m], function(k) raw_p90(cmp_a(), k), numeric(1)); B[m] <- vapply(d$key[m], function(k) raw_p90(cmp_b(), k), numeric(1)) }
    list(A = cmp_conv(d$key, A, cmp_a()$minutes), B = cmp_conv(d$key, B, cmp_b()$minutes))
  }
  cmp_lab <- function(d) if (use_raw()) ifelse(d$key %in% names(RAWMAP), sub("\\(PAdj\\)", "(raw)", d$label), d$label) else d$label
  output$cmp_dumb_title <- renderText(if (identical(input$cmp_basis, "raw")) paste0("Head to head on the headline KPIs: values, ", if (identical(input$cmp_vol, "tot")) "season totals" else "per 90", if (use_raw()) ", raw defending counts" else ", possession adjusted defending") else "Head to head on the headline KPIs: percentiles within position group")
  output$cmp_dumb <- renderPlot({
    nm_a <- cmp_a()$player; nm_b <- cmp_b()$player; d <- cmp_join(); req(nrow(d) > 0)
    cols <- setNames(c(BRAND$primary, BRAND$accent), c(nm_a, nm_b))
    if (identical(input$cmp_basis, "raw")) {
      d <- d %>% filter(!is.na(va) | !is.na(vb)); ma <- cmp_a()$minutes; mb <- cmp_b()$minutes
      v <- cmp_vals(d); d$A <- v$A; d$B <- v$B
      d$lab <- paste0(cmp_lab(d), ifelse(grepl("_p90$", d$key), if (identical(input$cmp_vol, "tot")) " (total)" else " (per 90)", ""))
      d$lab <- factor(d$lab, levels = unique(d$lab))
      long <- bind_rows(tibble(lab = d$lab, v = d$A, who = nm_a, key = d$key), tibble(lab = d$lab, v = d$B, who = nm_b, key = d$key)); long$who <- factor(long$who, levels = c(nm_a, nm_b))
      long$txt <- vapply(seq_len(nrow(long)), function(i) fmt_val(long$key[i], long$v[i]), character(1))
      ggplot(long, aes(v, who, fill = who)) + geom_vline(xintercept = 0, colour = BRAND$dark, linewidth = 0.4) + geom_col(width = 0.7) +
        geom_label(aes(label = txt, hjust = ifelse(v >= 0, -0.1, 1.1)), size = 3.4, fontface = "bold", colour = BRAND$dark, fill = alpha("white", 0.85), label.size = 0, label.padding = unit(0.12, "lines"), show.legend = FALSE) +
        scale_fill_manual(values = cols, name = NULL) + scale_x_continuous(expand = expansion(mult = 0.32)) + scale_y_discrete(labels = NULL, limits = rev) + expand_limits(x = 0) +
        facet_wrap(~lab, scales = "free_x", ncol = 3, labeller = label_wrap_gen(26)) + labs(x = NULL, y = NULL, subtitle = "Bars start at zero, so lengths are honest. Each KPI has its own scale.") + theme_lcw(base_size = 12) +
        theme(strip.text = element_text(face = "bold", colour = BRAND$primary, size = 10.5, lineheight = 0.95), panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(), axis.text.y = element_blank(),
              axis.text.x = element_blank(), axis.ticks = element_blank(), panel.grid.major.x = element_blank(), legend.position = "top", panel.spacing = unit(1.1, "lines"))
    } else {
      d <- d %>% filter(!is.na(pa), !is.na(pb)); req(nrow(d) > 0)
      d$domain_label <- factor(d$domain_label, levels = DOM$domain_label); d$id <- factor(seq_len(nrow(d)), levels = rev(seq_len(nrow(d))))
      pts <- bind_rows(tibble(id = d$id, domain_label = d$domain_label, pct = d$pa, who = nm_a), tibble(id = d$id, domain_label = d$domain_label, pct = d$pb, who = nm_b))
      pts$who <- factor(pts$who, levels = c(nm_a, nm_b))
      ggplot(d) + geom_vline(xintercept = 50, colour = BRAND$muted, linetype = "dashed") +
        geom_segment(aes(x = pa, xend = pb, y = id, yend = id), colour = BRAND$muted, linewidth = 1.6) + geom_point(data = pts, aes(pct, id, colour = who), size = 4.2) +
        scale_colour_manual(values = cols, name = NULL) +
        scale_x_continuous(limits = c(0, 100), breaks = c(0, 25, 50, 75, 100)) + scale_y_discrete(labels = setNames(d$label, d$id)) +
        facet_grid(domain_label ~ ., scales = "free_y", space = "free_y", switch = "y") + labs(x = "Percentile", y = NULL) + theme_lcw(base_size = 13) +
        theme(strip.placement = "outside", strip.text.y.left = element_text(angle = 0, hjust = 0, face = "bold", colour = BRAND$primary), panel.grid.major.y = element_blank(),
              panel.grid.minor = element_blank(), legend.position = "top", axis.text.y = element_text(colour = BRAND$dark), panel.spacing.y = unit(0.6, "lines"))
    }
  })
  output$cmp_tbl <- renderDT({
    d <- cmp_join(); req(nrow(d) > 0); a <- cmp_a()$player; b <- cmp_b()$player; raw <- identical(input$cmp_basis, "raw")
    v <- cmp_vals(d); d <- d %>% mutate(A = v$A, B = v$B, unit = vapply(key, cmp_unit, character(1)), label = cmp_lab(d))
    t <- if (raw) {
      tibble(Area = d$domain_label, KPI = paste0(d$label, d$unit), A = vapply(seq_len(nrow(d)), function(i) fmt_val(d$key[i], d$A[i]), character(1)), B = vapply(seq_len(nrow(d)), function(i) fmt_val(d$key[i], d$B[i]), character(1)),
             `B minus A` = ifelse(is.na(d$A) | is.na(d$B), "", vapply(seq_len(nrow(d)), function(i) fmt_num(d$B[i] - d$A[i]), character(1))),
             `B vs A (%)` = ifelse(is.na(d$A) | is.na(d$B) | d$A == 0, "", sprintf("%+.0f%%", 100 * (d$B - d$A) / abs(d$A))))
    } else {
      tibble(Area = d$domain_label, KPI = paste0(d$label, d$unit), A = vapply(seq_len(nrow(d)), function(i) fmt_val(d$key[i], d$A[i]), character(1)), `A pct` = round(d$pa),
             B = vapply(seq_len(nrow(d)), function(i) fmt_val(d$key[i], d$B[i]), character(1)), `B pct` = round(d$pb),
             Edge = ifelse(is.na(d$pa) | is.na(d$pb), "", ifelse(d$pa - d$pb >= 10, "A", ifelse(d$pb - d$pa >= 10, "B", "even"))))
    }
    names(t)[names(t) == "A"] <- a; names(t)[names(t) == "B"] <- b; names(t)[names(t) == "A pct"] <- paste(a, "pct"); names(t)[names(t) == "B pct"] <- paste(b, "pct")
    names(t)[names(t) == "B minus A"] <- paste(b, "minus", a); names(t)[names(t) == "B vs A (%)"] <- paste(b, "vs", a, "(%)")
    dt <- datatable(t, rownames = FALSE, class = "compact stripe", options = list(dom = "t", pageLength = 60, ordering = TRUE, scrollY = "460px", paging = FALSE))
    if (!raw) dt <- dt %>% formatStyle(c(4, 6), backgroundColor = styleInterval(c(25, 50, 75), c("#FBDF85", "#FEF1C7", "#DCE6F6", "#A9C0E8")))
    dt
  })
  cmp_map_plot <- function(p, type, cols) {
    mp <- get_maps(p$league); validate(need(!is.null(mp), "Maps data not found for this league."))
    ev <- mp %>% filter(player.id == p$player.id)
    if (type == "heat") {
      d <- ev %>% filter(touch); validate(need(nrow(d) >= 10, "Not enough touches."))
      pitch_base() + stat_density_2d(data = d, aes(y, x, fill = after_stat(density), alpha = after_stat(density)), geom = "raster", contour = FALSE, n = 110) +
        scale_fill_gradientn(colours = cols, guide = "none") + scale_alpha(range = c(0, 0.9), guide = "none") + annotate("point", x = mean(d$y), y = mean(d$x), size = 4, colour = BRAND$dark) +
        labs(title = p$player, subtitle = paste0(nrow(d), " touches. Dark dot: average position."))
    } else if (type == "shot") {
      d <- ev %>% filter(kind == "shot", !tf(pen), !is.na(xg)); validate(need(nrow(d) >= 1, "No shots."))
      pitch_base(TRUE, TRUE) + geom_point(data = d, aes(y, x, size = xg, fill = tf(ok)), shape = 21, colour = "white", stroke = 0.7, alpha = 0.9) +
        scale_fill_manual(values = c(`TRUE` = BRAND$accent, `FALSE` = cols[2]), guide = "none") + scale_size_area(max_size = 12, guide = "none") +
        labs(title = p$player, subtitle = sprintf("%d shots, %d goals, %.2f xG. Gold: goal.", nrow(d), sum(tf(d$ok)), sum(d$xg)))
    } else {
      d <- ev %>% filter(tf(prog), kind %in% c("pass", "carry"), !is.na(ex)); validate(need(nrow(d) >= 3, "No progressive actions."))
      n_all <- nrow(d); d <- d %>% arrange(desc(xt)) %>% head(80)
      pitch_base() + geom_segment(data = d, aes(y, x, xend = ey, yend = ex, colour = kind), arrow = arrow_head, alpha = 0.8, linewidth = 0.5) +
        scale_colour_manual(values = c(pass = cols[3], carry = "#D9A300"), labels = c(pass = "Pass", carry = "Carry"), name = NULL) +
        labs(title = p$player, subtitle = sprintf("%d progressive actions, 80 highest xT shown.", n_all)) + theme(legend.position = "bottom")
    }
  }
  output$cmp_map_a <- renderPlot({ cmp_map_plot(cmp_a(), input$cmp_map %||% "heat", c("#DCE5F5", BRAND$mid, BRAND$primary)) })
  output$cmp_map_b <- renderPlot({ cmp_map_plot(cmp_b(), input$cmp_map %||% "heat", c("#FFF4CC", BRAND$accent, "#C97F00")) })

  # ---- Overview page ----
  observeEvent(input$key_pick, { updateSelectInput(session, "player", selected = input$key_pick); nav_select("page", "Player") })
  output$ov_head <- renderUI({
    req(input$team, input$league)
    div(class = "ov-band",
        div(div(class = "ov-name", team_display(input$team)),
            div(class = "ov-sub", paste0(LEAGUE_FULL[[input$league]] %||% input$league, if (nzchar(season_label(input$league))) paste0("  |  ", season_label(input$league)) else ""))),
        div(class = "ov-logos", div(class = "ov-chip", league_tag(input$league, 56)), crest_tag(input$team, 84)))
  })
  output$ov_keyhead <- renderUI({
    div(class = "ov-keypill", div(class = "t", "KEY PLAYERS"),
        div(class = "s", paste0("Team profile vs positional peers  |  minimum ", input$min_min, " minutes  |  ",
                                if (identical(input$pool, "all")) "all five leagues" else input$pool)))
  })
  output$ov_cards <- renderUI({
    sq <- squad() %>% filter(pos_group %in% GROUPS) %>% arrange(desc(minutes)) %>% head(6); req(nrow(sq) > 0)
    cards <- lapply(seq_len(nrow(sq)), function(i) {
      p <- sq[i, ]; b <- bars_from(build_profile(p, input$merge, pool_leagues(), input$min_min)$d)
      b <- b %>% mutate(domain = as.character(domain)) %>% left_join(DOM %>% mutate(domain = as.character(domain)), by = "domain") %>% filter(!is.na(bar)) %>% arrange(desc(bar))
      top <- head(b, 3)
      rows <- lapply(seq_len(nrow(top)), function(j) div(class = "r", span(top$domain_label[j]), div(class = "bar", div(class = "fill", style = sprintf("width:%d%%;", round(top$bar[j]))), div(class = "tick")), span(class = "v", sprintf("%.0f", top$bar[j]))))
      div(class = "key-card", onclick = sprintf("Shiny.setInputValue('key_pick', '%s', {priority: 'event'});", p$player.id),
          div(class = "key-top", photo_tag(p$player.id, p$player, size = 76, team = p$team),
              div(div(class = "key-name", p$player), div(class = "key-meta", paste0(unname(GROUP_NAMES[p$pos_group]), "  |  ", round(p$minutes), " min  |  ", p$matches, " matches"))),
              div(class = "key-overall", title = "Best area, as a percentile against players in the same position group", div(class = "n", if (!nrow(top)) "n/a" else sprintf("%.0f%%", top$bar[1])), div(class = "l", if (!nrow(top)) "" else paste0("Best: ", top$domain_label[1])))),
          div(class = "key-sub", "Best areas vs positional peers"), div(class = "key-rows", rows))
    })
    div(class = "key-grid", cards)
  })
  output$ov_goals <- renderPlot({
    d <- squad() %>% mutate(thr = npxg + xa, ga = goals + assists) %>% filter(thr > 0) %>% arrange(desc(thr)) %>% head(12); validate(need(nrow(d) > 0, "No chance data for this squad."))
    d$player <- reorder(d$player, d$thr)
    long <- bind_rows(transmute(d, player, n = npxg, what = "npxG"), transmute(d, player, n = xa, what = "xA")); long$what <- factor(long$what, levels = c("xA", "npxG"))
    ggplot() + geom_col(data = long, aes(n, player, fill = what), width = 0.7) +
      geom_text(data = d, aes(x = thr, y = player, label = sprintf("%.1f", thr)), hjust = -0.35, size = 4, fontface = "bold", colour = BRAND$primary) +
      geom_point(data = d, aes(x = ga, y = player, shape = "Actual goals + assists"), size = 3.4, colour = BRAND$dark, fill = "white", stroke = 1.1) +
      scale_shape_manual(values = c(`Actual goals + assists` = 23), name = NULL) +
      scale_fill_manual(values = c(npxG = BRAND$primary, xA = BRAND$accent), name = NULL, breaks = c("npxG", "xA")) +
      scale_x_continuous(expand = expansion(mult = c(0, 0.14))) +
      labs(x = NULL, y = NULL, subtitle = "Non-penalty xG plus xA this season (top 12). Diamond: actual goals + assists, so right of the bar means overperforming.") +
      theme_lcw(base_size = 13) + theme(panel.grid.major.y = element_blank(), legend.position = "bottom")
  })
  output$ov_age <- renderPlot({
    f <- "assets/player_ages.csv"
    validate(need(file.exists(f), "Ages are not in the StatsBomb open data.\nFill in assets/player_ages_template.csv (every player in the league),\nsave it as assets/player_ages.csv, and the age chart appears here."))
    a <- read.csv(f, check.names = FALSE, stringsAsFactors = FALSE)
    ref <- { fd <- file.path("data", paste0("match_dates_", gsub("[^A-Za-z]", "", input$league), ".rds")); if (file.exists(fd)) max(readRDS(fd)$match_date) else Sys.Date() }
    if ("birth_date" %in% names(a)) a$age <- ifelse(nzchar(a$birth_date), as.numeric(ref - as.Date(a$birth_date)) / 365.25, NA_real_)
    validate(need("age" %in% names(a), "player_ages.csv needs a birth_date or an age column."))
    lg <- M %>% filter(league == input$league, minutes > 0)
    lg <- if ("player.id" %in% names(a) && any(!is.na(a$player.id))) lg %>% left_join(a %>% filter(!is.na(player.id)) %>% distinct(player.id, age), by = "player.id")
          else lg %>% mutate(k = norm_name(player)) %>% left_join(a %>% mutate(k = norm_name(player)) %>% distinct(k, age), by = "k")
    lg <- lg %>% filter(is.finite(age)); mine <- lg %>% filter(team == input$team)
    validate(need(nrow(mine) >= 5, "Not enough ages filled in for this squad yet."))
    brk <- c(-Inf, 20, 23, 26, 29, Inf); labs_b <- c("20 and under", "21 to 23", "24 to 26", "27 to 29", "30 and over")
    share <- function(d) { b <- cut(floor(d$age), brk, labels = labs_b); tapply(d$minutes, b, sum) %>% replace(is.na(.), 0) %>% (function(x) x / sum(x)) }
    out <- tibble(band = factor(labs_b, levels = labs_b), us = as.numeric(share(mine)))
    cover <- nrow(lg) / nrow(M %>% filter(league == input$league, minutes > 0)); have_lg <- cover >= 0.5 && n_distinct(lg$team) >= 6
    long <- tibble(band = out$band, v = out$us, who = team_display(input$team))
    if (have_lg) long <- bind_rows(long, tibble(band = out$band, v = as.numeric(share(lg)), who = "League"))
    sub <- sprintf("Share of minutes by age band. %s median age %.1f, minutes-weighted %.1f.%s", team_display(input$team), median(mine$age), weighted.mean(mine$age, mine$minutes),
                   if (have_lg) sprintf(" League: ages known for %.0f%% of players.", 100 * cover) else " League comparison appears once ages are filled in for most of the league.")
    ggplot(long, aes(band, v, fill = who)) + geom_col(position = position_dodge(width = 0.75), width = 0.7) +
      geom_text(aes(label = sprintf("%.0f%%", 100 * v)), position = position_dodge(width = 0.75), vjust = -0.5, size = 3.9, colour = BRAND$dark) +
      scale_fill_manual(values = setNames(c(BRAND$primary, BRAND$accent), c(team_display(input$team), "League")), name = NULL) +
      scale_y_continuous(labels = scales::percent, expand = expansion(mult = c(0, 0.15))) + scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
      labs(x = NULL, y = "Share of minutes", subtitle = sub) + theme_lcw(base_size = 13) + theme(panel.grid.major.x = element_blank(), legend.position = "bottom")
  })
  tm_shapes <- reactive({
    f <- file.path("data", paste0("team_formations_", gsub("[^A-Za-z]", "", input$league), ".rds")); if (!file.exists(f)) return(NULL)
    readRDS(f) %>% filter(team == input$team) %>% arrange(desc(share))
  })
  # The 11 players with most minutes, laid out in the team's most used formation (when stored), otherwise a shape inferred from positions.
  # Players are sorted into lines by how deep they play (average x), then spread across the pitch by average y.
  xi_data <- reactive({
    sq <- squad() %>% filter(!is.na(avg_x), !is.na(avg_y))
    gk <- sq %>% filter(pos_group == "GK") %>% arrange(desc(minutes)) %>% head(1)
    out <- sq %>% filter(pos_group != "GK") %>% arrange(desc(minutes)) %>% head(10)
    games <- tryCatch(n_distinct(lg_maps()$match_id[lg_maps()$team == input$team]), error = function(e) max(sq$matches))
    sh <- tm_shapes(); lines_n <- NULL; src <- "inferred from positions"
    if (!is.null(sh) && nrow(sh)) { dg <- as.integer(strsplit(sh$formation[1], "")[[1]]); if (sum(dg) == 10) { lines_n <- dg; src <- "most used formation" } }
    if (is.null(lines_n)) { d <- sum(out$pos_group %in% c("CB", "FB")); a <- sum(out$pos_group %in% c("W", "FW")); lines_n <- c(d, 10 - d - a, a) }
    out <- out %>% arrange(avg_x); out$line <- rep(seq_along(lines_n), lines_n)[seq_len(nrow(out))]
    nl <- length(lines_n); lx <- seq(26, 104, length.out = nl)
    out <- out %>% group_by(line) %>% arrange(avg_y, .by_group = TRUE) %>% mutate(k = row_number(), n = n(), px = lx[line], py = if (n() == 1) 40 else seq(12, 68, length.out = n())[k]) %>% ungroup()
    gk <- gk %>% mutate(line = 0, k = 1, n = 1, px = 9, py = 40)
    xi <- bind_rows(gk, out); xi$pct <- pmin(100, 100 * xi$minutes / (max(1, games) * 90))
    list(xi = xi, shape = paste(lines_n, collapse = "-"), src = src)
  })
  output$ov_xi <- renderPlot({
    x <- xi_data(); xi <- x$xi; req(nrow(xi) > 5)
    g <- pitch_base() +
      geom_point(data = xi, aes(py, px, fill = pos_group == "GK"), shape = 21, size = 15, colour = "white", stroke = 1.6) +
      geom_text(data = xi, aes(py, px, label = ifelse(is.na(match(player, player)), "", sprintf("%.0f", pct))), size = 3.6, fontface = "bold", colour = ifelse(xi$pos_group == "GK", BRAND$primary, "white")) +
      scale_fill_manual(values = c(`TRUE` = BRAND$accent, `FALSE` = BRAND$primary), guide = "none") +
      labs(subtitle = paste0("Shape ", x$shape, " (", x$src, "). Number in the dot: % of the team's available minutes played."))
    g + geom_text(data = xi, aes(py, px - 6.5, label = surname(player)), size = 3.9, fontface = "bold", colour = BRAND$dark)
  })
  output$ov_shape_txt <- renderUI({
    sh <- tm_shapes()
    if (is.null(sh) || !nrow(sh)) div(class = "ov-note", "Formation minutes are only stored for the WSL (script 19). For this league the shape on the pitch is inferred from average positions.")
    else div(class = "ov-note", tags$b(paste0("Most used formation: ", fmt_formation(sh$formation[1]))), paste0(" (", round(100 * sh$share[1]), "% of minutes). Shapes change in games, so this is minutes in each formation across the season."))
  })
  output$ov_shapes <- renderDT({
    sh <- tm_shapes(); req(!is.null(sh), nrow(sh) > 0)
    t <- sh %>% head(8) %>% transmute(Formation = fmt_formation(formation), `Share of minutes` = paste0(round(100 * share), "%"), Matches = matches)
    datatable(t, rownames = FALSE, class = "compact stripe", options = list(dom = "t", ordering = FALSE, pageLength = 8))
  })

  # ---- Team profile page ----
  tp_prof <- reactive({
    req(input$league); f <- file.path("data", paste0("team_prof_", gsub("[^A-Za-z]", "", input$league), ".rds"))
    validate(need(file.exists(f), "Team profile data not found. Run 20_team_profile_data.R first (from this folder)."))
    readRDS(f)
  })
  tp_T <- reactive({
    st <- tm_season()$all; pf <- tp_prof()
    pf$tm %>% left_join(st %>% select(match_id, team, xg, shots, goals, xt, entries, tov), by = c("match_id", "team")) %>%
      left_join(st %>% transmute(match_id, opp = team, xga = xg, xt_against = xt), by = c("match_id", "opp")) %>% mutate(across(c(xg, shots, goals, xt, entries, tov, xga, xt_against), ~ coalesce(.x, 0)))
  })
  tp_ids <- reactive({ ids <- tm_season()$me$match_id; switch(input$tp_range %||% "season", season = ids, last10 = tail(ids, 10), last5 = tail(ids, 5)) })
  summ <- function(d) d %>% group_by(team) %>% summarise(n = n(), xg = mean(xg), xga = mean(xga), xt = mean(xt), shots = mean(shots), xg_shot = sum(xg) / max(sum(shots), 1),
      pass_share = sum(passes) / max(sum(passes) + sum(passes_against), 1), long = sum(long_open) / max(sum(passes_open), 1), plen = sum(len_open) / max(sum(passes_open), 1),
      ppda = sum(p60_against) / max(sum(da60), 1), hr = mean(regains_high), shot_rate = sum(shots_after) / max(sum(regains_high), 1), xg_hr = sum(xg_after) / max(sum(regains_high), 1),
      line = sum(def_x_sum) / max(sum(def_n), 1) * 0.9144, tov = mean(tov), entries = mean(entries), f3 = sum(f3_passes) / max(sum(passes_open), 1), .groups = "drop")
  tp_league <- reactive({ summ(tp_T()) })
  tp_ours <- reactive({ d <- tp_T() %>% filter(team == input$team, match_id %in% tp_ids()); req(nrow(d) > 0); summ(d) })
  # value, rank among the league's whole-season values, formatted
  tp_val <- function(key, hib = TRUE, fmt = "%.2f", mult = 1) {
    lg <- tp_league(); o <- tp_ours(); v <- o[[key]] * mult; others <- lg[[key]][lg$team != input$team] * mult
    rk <- 1 + sum(if (hib) others > v else others < v)
    list(val = sprintf(fmt, v), rank = rk, n = nrow(lg), good = rk <= 3)
  }
  kpi_html <- function(label, k, unit = "") div(class = "kpi-row", span(label), span(span(class = "kv", paste0(k$val, unit)), span(class = paste("kr", if (k$good) "good"), paste0(ord_suffix(k$rank), " of ", k$n))))
  output$tp_head <- renderUI({
    req(input$team, input$league)
    div(class = "ov-band", div(div(class = "ov-name", paste0(team_display(input$team), ": team profile")),
        div(class = "ov-sub", paste0(LEAGUE_FULL[[input$league]] %||% input$league, if (nzchar(season_label(input$league))) paste0("  |  ", season_label(input$league)) else ""))),
        div(class = "ov-logos", div(class = "ov-chip", league_tag(input$league, 56)), crest_tag(input$team, 84)))
  })
  output$tp_tiles <- renderUI({
    T <- tp_T(); mine <- T %>% filter(team == input$team) %>% arrange(match(match_id, tm_season()$me$match_id))
    defs <- list(list("xG for", "xg", TRUE, "%.2f", "per match", mine$xg), list("xG against", "xga", FALSE, "%.2f", "per match", mine$xga),
                 list("Threat created (xT)", "xt", TRUE, "%.1f", "per match", mine$xt), list("Share of passes", "pass_share", TRUE, "%.0f", "%", NULL),
                 list("PPDA (lower = more pressing)", "ppda", FALSE, "%.1f", "", NULL), list("High regains", "hr", TRUE, "%.1f", "per match", mine$regains_high))
    tiles <- lapply(defs, function(d) {
      k <- tp_val(d[[2]], d[[3]], d[[4]], mult = if (d[[2]] == "pass_share") 100 else 1)
      sp <- if (!is.null(d[[6]])) d[[6]] else if (d[[2]] == "pass_share") 100 * mine$passes / (mine$passes + mine$passes_against) else mine$p60_against / pmax(mine$da60, 1)
      div(class = "tp-tile", div(class = "l", d[[1]]), div(class = "v", k$val, span(class = "u", d[[5]])),
          span(class = paste("rk", if (k$good) "good"), paste0(ord_suffix(k$rank), " of ", k$n)), div(class = "sp", spark_svg(sp)))
    })
    div(class = "tp-tiles", tiles)
  })
  output$tp_k_build <- renderUI({ tagList(kpi_html("Share of passes", tp_val("pass_share", TRUE, "%.0f", 100), "%"), kpi_html("Average pass length", tp_val("plen", TRUE, "%.1f"), " m"), kpi_html("Final third entries", tp_val("entries", TRUE, "%.1f"), " per match")) })
  output$tp_k_def <- renderUI({ tagList(kpi_html("PPDA (lower = more pressing)", tp_val("ppda", FALSE, "%.1f")), kpi_html("Defensive line height", tp_val("line", TRUE, "%.0f"), " m"), kpi_html("High regains", tp_val("hr", TRUE, "%.1f"), " per match")) })
  output$tp_k_chance <- renderUI({ tagList(kpi_html("xG", tp_val("xg", TRUE, "%.2f"), " per match"), kpi_html("Shots", tp_val("shots", TRUE, "%.1f"), " per match"), kpi_html("xG per shot", tp_val("xg_shot", TRUE, "%.3f"))) })
  output$tp_k_regain <- renderUI({ tagList(kpi_html("High regains (final 40% of pitch)", tp_val("hr", TRUE, "%.1f"), " per match"), kpi_html("Shot within 15 s of a high regain", tp_val("shot_rate", TRUE, "%.0f", 100), "%"), kpi_html("xG created per high regain", tp_val("xg_hr", TRUE, "%.3f"))) })
  output$tp_build <- renderPlot({
    d <- lg_maps() %>% filter(team == input$team, match_id %in% tp_ids(), tf(prog), kind %in% c("pass", "carry"), !is.na(ex)); validate(need(nrow(d) >= 5, "Not enough progressive actions."))
    n_all <- nrow(d); d <- d %>% arrange(desc(xt)) %>% head(70)
    pitch_base() + geom_segment(data = d, aes(y, x, xend = ey, yend = ex, colour = kind), arrow = arrow_head, alpha = 0.75, linewidth = 0.5) +
      scale_colour_manual(values = c(pass = BRAND$primary, carry = "#D9A300"), labels = c(pass = "Pass", carry = "Carry"), name = NULL) +
      labs(subtitle = sprintf("Progressive passes and carries (%d). The 70 highest xT shown.", n_all)) + theme(legend.position = "bottom")
  })
  output$tp_press <- renderPlot({
    pr <- tp_prof()$press; me <- pr %>% filter(team == input$team, match_id %in% tp_ids()); validate(need(nrow(me) >= 30, "Not enough pressures."))
    zs <- function(d) tibble(z = zone3(d$x)) %>% count(z, .drop = FALSE) %>% mutate(sh = n / sum(n))
    lg <- pr %>% group_by(team) %>% group_modify(~ zs(.x)) %>% group_by(z) %>% summarise(sh = mean(sh), .groups = "drop"); m <- zs(me)
    lab <- tibble(x = c(20, 60, 100), z = levels(zone3(1))); lab$txt <- sprintf("%s\n%.0f%% (league %.0f%%)", c("Own third", "Middle", "High press"), 100 * m$sh[match(lab$z, m$z)], 100 * lg$sh[match(lab$z, lg$z)])
    pitch_base() + stat_density_2d(data = me, aes(y, x, fill = after_stat(density), alpha = after_stat(density)), geom = "raster", contour = FALSE, n = 110) +
      scale_fill_gradientn(colours = c("#DCE5F5", BRAND$mid, BRAND$primary), guide = "none") + scale_alpha(range = c(0, 0.9), guide = "none") +
      geom_hline(yintercept = c(40, 80), linetype = "dashed", colour = "#9AA3B5") +
      geom_label(data = lab, aes(x = 78, y = x, label = txt), hjust = 1, size = 3.3, fill = alpha("white", 0.85), label.size = 0, colour = BRAND$dark, lineheight = 0.95) +
      labs(subtitle = sprintf("%d pressures. Share of our pressing in each third, with the league average.", nrow(me)))
  })
  output$tp_shots <- renderPlot({
    s <- tp_prof()$shots %>% filter(team == input$team, match_id %in% tp_ids()); validate(need(nrow(s) >= 3, "Not enough shots."))
    s$set <- s$origin %in% c("Corner", "Free kick", "Throw-in")
    pitch_base(TRUE, TRUE) +
      geom_point(data = s %>% filter(!goal), aes(y, x, size = xg, shape = set), fill = BRAND$mid, colour = "white", stroke = 0.6, alpha = 0.85) +
      geom_point(data = s %>% filter(goal), aes(y, x, size = xg, shape = set), fill = BRAND$accent, colour = BRAND$primary, stroke = 1.6) +
      scale_shape_manual(values = c(`FALSE` = 21, `TRUE` = 24), labels = c(`FALSE` = "Open play and counter", `TRUE` = "From a set piece"), name = NULL) + scale_size_area(max_size = 12, guide = "none") +
      labs(subtitle = sprintf("%d shots, %d goals, %.1f xG. Gold: goal. Triangle: set piece.", nrow(s), sum(s$goal), sum(s$xg))) + theme(legend.position = "bottom")
  })
  output$tp_scatter <- renderPlot({
    lg <- tp_league(); o <- tp_ours()
    cfg <- switch(input$tp_sc %||% "style",
      style = list(x = "pass_share", y = "plen", xl = "Share of passes (possession proxy)", yl = "Average open play pass length (m)", xm = 100, ym = 1, xr = FALSE, yr = FALSE, tl = "Direct, lower possession", tr = "Direct, high possession", bl = "Short, lower possession", br = "Patient possession"),
      chances = list(x = "xg", y = "xga", xl = "xG for per match", yl = "xG against per match (flipped: up is better)", xm = 1, ym = 1, xr = FALSE, yr = TRUE, tl = "Solid, few chances", tr = "Dominant", bl = "Poor", br = "Open games"),
      pressing = list(x = "ppda", y = "hr", xl = "PPDA (flipped: right is more pressing)", yl = "High regains per match", xm = 1, ym = 1, xr = TRUE, yr = FALSE, tl = "Wins it high without pressing much", tr = "Intense high press", bl = "Passive", br = "Presses, wins little high"),
      terr = list(x = "f3", y = "line", xl = "Share of passes in the final third", yl = "Defensive line height (m)", xm = 100, ym = 1, xr = FALSE, yr = FALSE, tl = "High line, plays deep", tr = "High and advanced", bl = "Deep and cautious", br = "Low block, plays high"))
    d <- lg %>% mutate(X = .data[[cfg$x]] * cfg$xm, Y = .data[[cfg$y]] * cfg$ym, me = team == input$team)
    mine <- o %>% mutate(X = .data[[cfg$x]] * cfg$xm, Y = .data[[cfg$y]] * cfg$ym)
    d$X[d$me] <- mine$X; d$Y[d$me] <- mine$Y
    d$crest <- unname(CREST_PNG[d$team]); has <- !is.na(d$crest) & input$league == "WSL"
    xr <- range(d$X); yr <- range(d$Y); px <- diff(xr) * 0.08; py <- diff(yr) * 0.1
    g <- ggplot(d, aes(X, Y)) + geom_vline(xintercept = mean(d$X), colour = BRAND$muted, linetype = "dashed") + geom_hline(yintercept = mean(d$Y), colour = BRAND$muted, linetype = "dashed") +
      annotate("text", x = xr[1] - px, y = yr[2] + py, label = cfg$tl, hjust = 0, size = 3.5, colour = "#8A90A0", fontface = "italic") + annotate("text", x = xr[2] + px, y = yr[2] + py, label = cfg$tr, hjust = 1, size = 3.5, colour = "#8A90A0", fontface = "italic") +
      annotate("text", x = xr[1] - px, y = yr[1] - py, label = cfg$bl, hjust = 0, size = 3.5, colour = "#8A90A0", fontface = "italic") + annotate("text", x = xr[2] + px, y = yr[1] - py, label = cfg$br, hjust = 1, size = 3.5, colour = "#8A90A0", fontface = "italic")
    if (any(has)) {
      g <- g + geom_point(data = d[d$me, ], shape = 21, size = 17, fill = BRAND$accent, colour = BRAND$primary, stroke = 2) +
        ggimage::geom_image(data = d[has, ], aes(image = crest), size = 0.07, asp = 1.4)
    } else g <- g + geom_point(aes(colour = me), size = 5) + scale_colour_manual(values = c(`TRUE` = BRAND$accent, `FALSE` = BRAND$primary), guide = "none")
    g <- g + (if (HAS_REPEL) ggrepel::geom_text_repel(aes(label = team_display(team)), size = 3.6, colour = BRAND$dark, nudge_y = -py * 0.7, box.padding = 0.5, segment.colour = NA) else geom_text(aes(label = team_display(team)), size = 3.4, vjust = 2.2)) +
      labs(x = cfg$xl, y = cfg$yl, subtitle = "Every team's whole season; our team uses the matches chosen above.") +
      scale_x_continuous(expand = expansion(mult = 0.18), trans = if (cfg$xr) "reverse" else "identity") + scale_y_continuous(expand = expansion(mult = 0.18), trans = if (cfg$yr) "reverse" else "identity") +
      theme_lcw(base_size = 13) + theme(panel.grid.minor = element_blank())
    g
  })
  output$tp_xt <- renderPlot({
    T <- tp_T(); sn <- tm_season(); mine <- T %>% filter(team == input$team) %>% left_join(sn$me %>% select(match_id, idx, match_date), by = "match_id") %>% arrange(idx); validate(need(nrow(mine) >= 3, "Not enough matches."))
    mine$r_for <- roll(mine$xt); mine$r_ag <- roll(mine$xt_against); lgm <- mean(T$xt)
    long <- bind_rows(tibble(idx = mine$idx, v = mine$xt, r = mine$r_for, who = "Threat created"), tibble(idx = mine$idx, v = mine$xt_against, r = mine$r_ag, who = "Threat against"))
    ggplot(long, aes(idx, colour = who)) + geom_hline(yintercept = lgm, colour = BRAND$accent, linetype = "dashed", linewidth = 1) +
      geom_point(aes(y = v), alpha = 0.3, size = 2.4) + geom_line(aes(y = r), linewidth = 1.7) +
      scale_colour_manual(values = c(`Threat created` = BRAND$primary, `Threat against` = "#D9A300"), name = NULL) +
      scale_x_continuous(breaks = mine$idx, labels = if (sn$dated) format(mine$match_date, "%d %b") else mine$idx) +
      labs(x = NULL, y = "xT per match", subtitle = "Dots: each match. Lines: rolling 5 match average. Gold dashed: league average for a team in a match.") +
      theme_lcw(base_size = 13) + theme(panel.grid.minor = element_blank(), legend.position = "top", axis.text.x = element_text(angle = if (sn$dated) 45 else 0, hjust = if (sn$dated) 1 else 0.5))
  })
  output$tp_regain <- renderPlot({
    rg <- tp_prof()$regains %>% filter(x >= 72); me <- rg %>% filter(team == input$team, match_id %in% tp_ids()); validate(need(nrow(me) >= 10, "Not enough high regains."))
    lgr <- rg %>% group_by(team) %>% summarise(r = mean(shot15), .groups = "drop")
    d <- tibble(who = c(team_display(input$team), "League average"), r = c(mean(me$shot15), mean(lgr$r)), n = c(nrow(me), NA))
    d$who <- factor(d$who, levels = d$who)
    ggplot(d, aes(who, r, fill = who)) + geom_col(width = 0.55) + geom_text(aes(label = sprintf("%.0f%%", 100 * r)), vjust = -0.5, size = 5, fontface = "bold", colour = BRAND$primary) +
      scale_fill_manual(values = setNames(c(BRAND$primary, BRAND$accent), levels(d$who)), guide = "none") + scale_y_continuous(labels = scales::percent, expand = expansion(mult = c(0, 0.2))) +
      labs(x = NULL, y = "High regains followed by a shot within 15 s", subtitle = sprintf("%d high regains (ball won in the final 40%% of the pitch).", nrow(me))) + theme_lcw(base_size = 13) + theme(panel.grid.major.x = element_blank())
  })
  output$tp_shape <- renderPlot({
    sh <- tm_shapes(); validate(need(!is.null(sh) && nrow(sh) > 0, "No formation data for this league."))
    d <- sh %>% head(6); other <- 1 - sum(d$share); d <- bind_rows(d %>% transmute(f = fmt_formation(formation), share), if (other > 0.005) tibble(f = "Other", share = other))
    d$f <- factor(d$f, levels = rev(d$f)); d$y <- ""
    cols <- c(BRAND$accent, BRAND$primary, "#2D55B3", BRAND$mid, "#8FA6D6", "#BCC9E6", "#DCE3F2"); cols <- cols[seq_len(nrow(d))]
    d$tc <- ifelse(seq_len(nrow(d)) %in% 2:4 & as.character(d$f) != "Other", "white", BRAND$dark)
    ggplot(d, aes(share, y, fill = f)) + geom_col(width = 0.8, colour = "white", position = position_stack(reverse = FALSE)) +
      geom_text(aes(label = ifelse(share >= 0.08, paste0(as.character(f), "\n", round(100 * share), "%"), ""), colour = I(tc)), position = position_stack(vjust = 0.5), size = 4.2, lineheight = 0.9, fontface = "bold") +
      scale_fill_manual(values = setNames(rev(cols), levels(d$f)), guide = "none") + scale_x_continuous(labels = scales::percent, expand = c(0, 0)) + labs(x = NULL, y = NULL,
        subtitle = paste0("Smaller shapes: ", paste(paste0(as.character(d$f[d$share < 0.08]), " ", round(100 * d$share[d$share < 0.08]), "%"), collapse = ", "))) +
      theme_lcw(base_size = 13) + theme(panel.grid = element_blank(), axis.text.y = element_blank())
  })
  output$tp_origin <- renderPlot({
    s <- tp_prof()$shots; lv <- c("Open play", "Counter", "Corner", "Free kick", "Throw-in", "Other")
    sh <- function(d) { x <- tibble(origin = factor(d$origin, levels = lv)) %>% count(origin, .drop = FALSE); x$sh <- x$n / sum(x$n); x }
    me <- s %>% filter(team == input$team, match_id %in% tp_ids()); validate(need(nrow(me) >= 5, "Not enough shots."))
    lgs <- s %>% group_by(team) %>% group_modify(~ sh(.x)) %>% group_by(origin) %>% summarise(sh = mean(sh), .groups = "drop")
    xgs <- me %>% group_by(origin) %>% summarise(xgps = sum(xg) / n(), .groups = "drop")
    d <- bind_rows(sh(me) %>% transmute(origin, sh, who = team_display(input$team)), lgs %>% transmute(origin, sh, who = "League average"))
    d$origin <- factor(d$origin, levels = lv); d$who <- factor(d$who, levels = c(team_display(input$team), "League average"))
    ggplot(d, aes(origin, sh, fill = who)) + geom_col(position = position_dodge(width = 0.75), width = 0.7) +
      geom_text(aes(label = sprintf("%.0f%%", 100 * sh)), position = position_dodge(width = 0.75), vjust = -0.5, size = 3.8, colour = BRAND$dark) +
      scale_fill_manual(values = setNames(c(BRAND$primary, BRAND$accent), levels(d$who)), name = NULL) + scale_y_continuous(labels = scales::percent, expand = expansion(mult = c(0, 0.15))) +
      labs(x = NULL, y = "Share of shots", subtitle = sprintf("%d shots. xG per shot for us: %s.", nrow(me), paste0(xgs$origin, " ", sprintf("%.2f", xgs$xgps), collapse = ", "))) +
      theme_lcw(base_size = 13) + theme(panel.grid.major.x = element_blank(), legend.position = "bottom")
  })
  output$tp_seq <- renderPlot({
    s <- tp_prof()$shots %>% filter(origin %in% c("Open play", "Counter")); lv <- c("0 to 2 passes", "3 to 5", "6 to 9", "10 or more")
    band <- function(d) factor(cut(d$n_pass, c(-1, 2, 5, 9, Inf), labels = lv), levels = lv)
    me <- s %>% filter(team == input$team, match_id %in% tp_ids()); validate(need(nrow(me) >= 5, "Not enough shots."))
    sh <- function(d) { x <- tibble(b = band(d)) %>% count(b, .drop = FALSE); x$sh <- x$n / sum(x$n); x }
    lgs <- s %>% group_by(team) %>% group_modify(~ sh(.x)) %>% group_by(b) %>% summarise(sh = mean(sh), .groups = "drop")
    xg <- me %>% mutate(b = band(me)) %>% group_by(b) %>% summarise(x = sum(xg) / n(), .groups = "drop")
    d <- bind_rows(sh(me) %>% transmute(b, sh, who = team_display(input$team)), lgs %>% transmute(b, sh, who = "League average")); d$who <- factor(d$who, levels = c(team_display(input$team), "League average"))
    xl <- xg %>% mutate(sh = max(d$sh) * 1.25, lab = sprintf("xG/shot %.2f", x))
    ggplot(d, aes(b, sh, fill = who)) + geom_col(position = position_dodge(width = 0.75), width = 0.7) +
      geom_text(aes(label = sprintf("%.0f%%", 100 * sh)), position = position_dodge(width = 0.75), vjust = -0.5, size = 3.8, colour = BRAND$dark) +
      geom_text(data = xl, aes(b, sh, label = lab), inherit.aes = FALSE, size = 3.7, colour = BRAND$primary, fontface = "bold") +
      scale_fill_manual(values = setNames(c(BRAND$primary, BRAND$accent), levels(d$who)), name = NULL) + scale_y_continuous(labels = scales::percent, expand = expansion(mult = c(0, 0.2))) +
      labs(x = "Passes by us in the move before the shot", y = "Share of open play and counter shots") + theme_lcw(base_size = 13) + theme(panel.grid.major.x = element_blank(), legend.position = "bottom")
  })

  # ---- Squad page ----
  output$squad_tbl <- renderDT({
    sq <- squad() %>% filter(pos_group %in% GROUPS, minutes >= 270)
    req(nrow(sq) > 0)
    rows <- map_dfr(seq_len(nrow(sq)), function(i) {
      p <- sq[i, ]; b <- bars_from(build_profile(p, input$merge, pool_leagues(), input$min_min)$d)
      v <- setNames(round(b$bar), as.character(b$domain))
      tibble(Player = p$player, Position = GROUP_NAMES[[p$pos_group]], Minutes = round(p$minutes),
             pg = factor(p$pos_group, levels = GROUPS)) %>%
        bind_cols(as_tibble(as.list(setNames(v[DOM$domain], DOM$domain_label))))
    }) %>% arrange(pg, desc(Minutes)) %>% select(-pg)
    bar_cols <- DOM$domain_label
    datatable(rows, rownames = FALSE, class = "compact stripe", options = list(pageLength = 30, dom = "t", ordering = TRUE, scrollX = TRUE, autoWidth = FALSE)) %>%
      formatStyle(bar_cols, backgroundColor = styleInterval(c(25, 50, 75), c("#FBDF85", "#FEF1C7", "#DCE6F6", "#A9C0E8")))
  })
  output$contrib_plot <- renderPlot({
    k <- input$contrib; sq <- squad()
    d <- sq %>% mutate(tot = .data[[k]] * minutes / 90)
    team_tot <- sum(d$tot, na.rm = TRUE)
    d <- d %>% filter(tot > 0) %>% mutate(share = tot / team_tot, pos = factor(pos_group, levels = c(GROUPS, "GK"))) %>% arrange(desc(share)) %>% head(14)
    req(nrow(d) > 0)
    ggplot(d, aes(share, reorder(player, share), colour = pos)) +
      geom_segment(aes(x = 0, xend = share, yend = reorder(player, share)), linewidth = 1.3) + geom_point(size = 4.5) +
      geom_text(aes(label = sprintf("%.0f%%", 100 * share)), hjust = -0.7, size = 3.8, colour = BRAND$dark) +
      scale_x_continuous(labels = scales::percent, expand = expansion(mult = c(0, 0.15))) +
      scale_colour_manual(values = c(CB = "#1B1F2A", FB = "#5D79B8", MF = "#003090", AM = "#8FA6D6", W = "#FDBE11", FW = "#C99A00", GK = "#8A90A0"), name = NULL) +
      labs(x = paste0("Share of team total (", input$team, ")"), y = NULL, subtitle = paste("Team total:", fmt_num(team_tot))) +
      theme_lcw(base_size = 13) + theme(panel.grid.major.y = element_blank(), legend.position = "bottom")
  })
  output$cor_plot <- renderPlot({
    cc <- paste0("comp_", DOM$domain); q <- M %>% filter(pos_group %in% GROUPS, minutes >= input$min_min)
    if (input$cor_grp != "all") q <- q %>% filter(pos_group == input$cor_grp)
    req(nrow(q) > 30)
    cm <- cor(q[, cc], method = "spearman", use = "pairwise.complete.obs")
    dimnames(cm) <- list(DOM$domain_label, DOM$domain_label)
    d <- as.data.frame(as.table(cm)); names(d) <- c("a", "b", "rho")
    ggplot(d, aes(a, b, fill = rho)) + geom_tile(colour = "white") +
      geom_text(aes(label = sub("^(-?)0\\.", "\\1.", sprintf("%.2f", rho))), size = 3, colour = BRAND$dark) +
      scale_fill_gradient2(low = BRAND$accent, mid = "white", high = BRAND$primary, midpoint = 0, limits = c(-1, 1), name = NULL) +
      labs(x = NULL, y = NULL, subtitle = paste(nrow(q), "players")) +
      theme_lcw(base_size = 12) + theme(axis.text.x = element_text(angle = 40, hjust = 1), panel.grid = element_blank())
  })

  # ---- Squad: entries by flank ----
  flank_data <- reactive({
    e <- lg_maps() %>% filter(kind %in% c("pass", "carry"), ok, !setp, !is.na(ex), x < 80, ex >= 80)
    e$ch <- cut(e$ey, c(-Inf, 80 / 3, 160 / 3, Inf), labels = c("Left", "Centre", "Right"))
    e
  })
  output$flank_plot <- renderPlot({
    e <- flank_data(); req(input$team)
    tm <- e %>% group_by(team, ch) %>% summarise(n = n(), .groups = "drop") %>% group_by(team) %>% mutate(share = n / sum(n)) %>% ungroup()
    validate(need(input$team %in% tm$team, "No entries found for this team."))
    lg <- tm %>% group_by(ch) %>% summarise(share = mean(share), .groups = "drop") %>% mutate(who = "League average")
    me <- tm %>% filter(team == input$team) %>% transmute(ch, share, who = input$team)
    ggplot(bind_rows(lg, me), aes(ch, share, fill = who)) + geom_col(position = position_dodge(width = 0.75), width = 0.7) +
      geom_text(aes(label = sprintf("%.0f%%", 100 * share)), position = position_dodge(width = 0.75), vjust = -0.4, size = 3.5, colour = BRAND$dark) +
      scale_fill_manual(values = setNames(c(BRAND$muted, BRAND$primary), c("League average", input$team)), name = NULL) +
      scale_y_continuous(labels = scales::percent, expand = expansion(mult = c(0, 0.15))) +
      labs(x = NULL, y = "Share of final third entries") +
      theme_lcw(base_size = 12) + theme(legend.position = "bottom", panel.grid.major.x = element_blank())
  })
  output$flank_tbl <- renderDT({
    e <- flank_data() %>% filter(team == input$team); req(nrow(e) > 0)
    nm <- M %>% filter(league == input$league) %>% distinct(player.id, player)
    t <- e %>% count(player.id, ch) %>% pivot_wider(names_from = ch, values_from = n, values_fill = 0)
    for (cc in c("Left", "Centre", "Right")) if (!cc %in% names(t)) t[[cc]] <- 0L
    t <- t %>% mutate(Total = Left + Centre + Right, `Share of team` = round(100 * Total / nrow(e))) %>%
      left_join(nm, by = "player.id") %>% arrange(desc(Total)) %>% head(12) %>% select(Player = player, Left, Centre, Right, Total, `Share of team`)
    datatable(t, rownames = FALSE, options = list(dom = "t", pageLength = 12))
  })

  # ---- Maps page ----
  lg_maps <- reactive({
    req(input$league); mp <- get_maps(input$league)
    validate(need(!is.null(mp), "Maps data not found. Run 15_players_maps.R first (from this folder)."))
    mp
  })
  p_ev <- reactive({ p <- sel(); req(nrow(p) == 1); lg_maps() %>% filter(player.id == p$player.id) })
  observeEvent(p_ev(), {
    d <- p_ev() %>% distinct(match_id, opp) %>% arrange(match_id)
    updateSelectInput(session, "mp_match", choices = c("All matches" = "all", setNames(as.character(d$match_id), paste0("vs ", d$opp, " (", d$match_id, ")"))))
  })
  apply_f <- function(df) {
    if (!identical(input$mp_match, "all")) df <- df %>% filter(match_id == as.numeric(input$mp_match))
    if (!identical(input$mp_half, "both")) df <- df %>% filter(period == as.numeric(input$mp_half))
    if (!identical(input$mp_state, "all")) df <- df %>% filter(state == as.numeric(input$mp_state))
    df
  }
  output$map_title <- renderText({
    p <- sel(); req(nrow(p) == 1)
    paste0(p$player, ": ", names(which(c("Touch heatmap" = "heat", "Shot map" = "shot", "Pass map" = "pass", "Progressive actions" = "prog",
                                         "Threat map (xT)" = "threat", "Synergy map (passing partners)" = "syn", "Pass sonar" = "sonar",
                                         "Take-ons" = "takeon", "Defensive actions" = "def") == input$map_type)))
  })
  output$map <- renderPlot({
    p <- sel(); req(nrow(p) == 1)
    d0 <- apply_f(p_ev()); typ <- input$map_type
    if (typ == "heat") {
      d <- d0 %>% filter(touch); validate(need(nrow(d) >= 10, "Not enough touches for these filters."))
      pitch_base() +
        stat_density_2d(data = d, aes(y, x, fill = after_stat(density), alpha = after_stat(density)), geom = "raster", contour = FALSE, n = 110) +
        scale_fill_gradientn(colours = c("#DCE5F5", BRAND$mid, BRAND$primary), guide = "none") +
        scale_alpha(range = c(0, 0.9), guide = "none") +
        annotate("point", x = mean(d$y), y = mean(d$x), size = 4, colour = BRAND$accent) +
        labs(subtitle = paste0(nrow(d), " touches. Gold dot: average position. Most frequent position: ", ifelse(is.na(p$main_pos), "n/a", p$main_pos)))
    } else if (typ == "shot") {
      d <- d0 %>% filter(kind == "shot", !pen, !is.na(xg)); validate(need(nrow(d) >= 1, "No shots for these filters."))
      sub <- sprintf("%d shots, %d goals, %.2f xG, %.2f xG per shot (penalties excluded)", nrow(d), sum(d$ok), sum(d$xg), mean(d$xg))
      if (input$shot_style == "sb") {
        d$body2 <- ifelse(d$body %in% c("Head", "Right Foot", "Left Foot"), d$body, "Other")
        pitch_base(TRUE, TRUE) +
          geom_point(data = d %>% filter(!ok), aes(y, x, fill = xg, shape = body2), colour = "#1B1F2A", stroke = 0.4, size = 6, alpha = 0.85) +
          geom_point(data = d %>% filter(ok), aes(y, x, fill = xg, shape = body2), colour = "white", stroke = 2, size = 6, alpha = 0.95) +
          scale_fill_gradientn(colours = c("#2C7BB6", "#ABD9E9", "#FFFFBF", "#FDAE61", "#D7191C"), limits = c(0, 0.8), oob = scales::squish, name = "xG") +
          scale_shape_manual(values = c(Head = 21, `Right Foot` = 23, `Left Foot` = 24, Other = 22), name = NULL) +
          labs(subtitle = paste0(sub, ". White outline: goal.")) + theme(legend.position = "bottom", legend.text = element_text(colour = BRAND$dark))
      } else {
        d$oc <- case_when(d$sout == "Goal" ~ "Goal", d$sout %in% c("Saved", "Saved to Post") ~ "Saved", d$sout == "Post" ~ "Post",
                          d$sout == "Blocked" ~ "Blocked", TRUE ~ "Off target")
        pitch_base(TRUE, TRUE) +
          geom_point(data = d, aes(y, x, colour = oc, size = xg), alpha = 0.9) +
          geom_point(data = d %>% filter(bc), aes(y, x, size = xg + 0.06), shape = 21, fill = NA, colour = "white", stroke = 1.2) +
          scale_colour_manual(values = c(Goal = "#2ECC71", Saved = "#4C8DFF", Post = "#F39C12", Blocked = "#9AA3B5", `Off target` = "#E74C3C"), name = NULL) +
          scale_size_area(max_size = 12, guide = "none") +
          labs(subtitle = paste0(sub, ". Ring: big chance.")) + theme(legend.position = "bottom", legend.text = element_text(colour = BRAND$dark))
      }
    } else if (typ == "pass") {
      d <- d0 %>% filter(kind == "pass", !is.na(ex))
      d <- switch(input$pass_f, all = d, key = d %>% filter(key | assist), f3 = d %>% filter(ok, ex >= 80, x < 80),
                  box = d %>% filter(ok, ex >= 102, ey >= 18, ey <= 62, !(x >= 102 & y >= 18 & y <= 62)), through = d %>% filter(through))
      validate(need(nrow(d) >= 1, "No passes for these filters."))
      d$cls <- ifelse(d$key | d$assist, "Key pass or assist", ifelse(d$ok, "Completed", "Incomplete"))
      pitch_base() +
        geom_segment(data = d, aes(y, x, xend = ey, yend = ex, colour = cls, linewidth = cls), arrow = arrow_head, alpha = 0.75) +
        scale_colour_manual(values = c(Completed = BRAND$mid, Incomplete = "#E8A5A7", `Key pass or assist` = BRAND$accent), name = NULL) +
        scale_linewidth_manual(values = c(Completed = 0.45, Incomplete = 0.35, `Key pass or assist` = 1.2), guide = "none") +
        labs(subtitle = sprintf("%d passes, %.0f%% completed, %d key passes or assists", nrow(d), 100 * mean(d$ok), sum(d$key | d$assist))) +
        theme(legend.position = "bottom")
    } else if (typ == "prog") {
      d <- d0 %>% filter(prog); validate(need(nrow(d) >= 1, "No progressive actions for these filters."))
      d$kk <- ifelse(d$kind == "pass", "Progressive pass", "Progressive carry")
      pitch_base() +
        geom_segment(data = d, aes(y, x, xend = ey, yend = ex, colour = kk), arrow = arrow_head, alpha = 0.8, linewidth = 0.5) +
        scale_colour_manual(values = c(`Progressive pass` = BRAND$primary, `Progressive carry` = "#D9A300"), name = NULL) +
        labs(subtitle = sprintf("%d progressive passes, %d progressive carries (25%% closer to goal)", sum(d$kind == "pass"), sum(d$kind == "carry"))) +
        theme(legend.position = "bottom")
    } else if (typ == "threat") {
      d <- d0 %>% filter(kind %in% c("pass", "carry"), !is.na(xt), xt > 0); validate(need(nrow(d) >= 5, "Not enough threatening actions."))
      d$bx <- if (input$thr_basis == "end") d$ey else d$y; d$by <- if (input$thr_basis == "end") d$ex else d$x
      top <- d %>% arrange(desc(xt)) %>% head(30)
      pitch_base() +
        geom_bin_2d(data = d, aes(bx, by, weight = xt), binwidth = c(10, 10), alpha = 0.6) +
        scale_fill_gradient(low = "#FFF4CC", high = "#E8A200", name = "xT created") +
        geom_segment(data = top, aes(y, x, xend = ey, yend = ex, colour = xt), arrow = arrow_head, linewidth = 0.6) +
        scale_colour_gradient(low = "#8FA6D6", high = "#BF2227", name = "xT of action") +
        labs(subtitle = sprintf("%d actions with positive xT, total %.2f. Arrows: the 30 most threatening.", nrow(d), sum(d$xt))) +
        theme(legend.position = "bottom")
    } else if (typ == "syn") {
      syn_data()$plot
    } else if (typ == "sonar") {
      d <- d0 %>% filter(kind == "pass", !setp, !is.na(ex)); validate(need(nrow(d) >= 20, "Not enough passes for a sonar."))
      d$ang <- (atan2(d$ey - d$y, d$ex - d$x) * 180 / pi + 360) %% 360
      a <- d %>% mutate(bin = floor(ang / 20) * 20 + 10) %>% group_by(bin) %>%
        summarise(n = n(), len = mean(plen * 0.9144, na.rm = TRUE), cmp = mean(ok), .groups = "drop") %>% mutate(share = n / sum(n))
      a$val <- if (input$sonar_col == "len") a$len else 100 * a$cmp
      ggplot(a, aes(bin, share, fill = val)) + geom_col(width = 20, colour = "white") +
        coord_polar(start = 0, direction = 1) +
        scale_x_continuous(limits = c(0, 360), breaks = c(0, 90, 180, 270), labels = c("Forward", "Right", "Back", "Left")) +
        scale_y_continuous(labels = scales::percent) +
        scale_fill_gradient(low = "#DCE5F5", high = BRAND$primary, name = if (input$sonar_col == "len") "Avg length (m)" else "Completion %") +
        labs(x = NULL, y = NULL, subtitle = sprintf("%d open play passes. Bar length: share of passes in that direction (forward is up).", nrow(d))) +
        theme_lcw(base_size = 12) + theme(panel.grid.minor = element_blank())
    } else if (typ == "takeon") {
      d <- d0 %>% filter(kind == "dribble"); validate(need(nrow(d) >= 1, "No take-ons for these filters."))
      d$res <- ifelse(d$ok, "Won", "Lost")
      pitch_base() +
        geom_point(data = d, aes(y, x, colour = res, shape = res), size = 4.5, alpha = 0.85, stroke = 1.3) +
        scale_colour_manual(values = c(Won = BRAND$primary, Lost = "#BF2227"), name = NULL) +
        scale_shape_manual(values = c(Won = 16, Lost = 4), name = NULL) +
        labs(subtitle = sprintf("%d take-ons, %d won (%.0f%%)", nrow(d), sum(d$ok), 100 * mean(d$ok))) + theme(legend.position = "bottom")
    } else {
      d <- d0 %>% filter(kind == "def"); validate(need(nrow(d) >= 3, "Not enough defensive actions for these filters."))
      h <- mean(d$x) * 0.9144
      pitch_base() +
        geom_hline(yintercept = mean(d$x), linetype = "dashed", colour = BRAND$accent, linewidth = 0.9) +
        geom_point(data = d, aes(y, x, shape = dtype, colour = ok), size = 3.8, alpha = 0.85, stroke = 1.1) +
        scale_shape_manual(values = c(tackle = 16, interception = 17, recovery = 15, block = 18, clearance = 4), name = NULL) +
        scale_colour_manual(values = c(`TRUE` = BRAND$primary, `FALSE` = "#BF2227"), labels = c(`TRUE` = "Successful", `FALSE` = "Lost tackle"), name = NULL) +
        labs(subtitle = sprintf("%d defensive actions. Dashed line: average height, %.0f m from own goal line.", nrow(d), h)) + theme(legend.position = "bottom")
    }
  })
  syn_data <- reactive({
    p <- sel(); req(nrow(p) == 1)
    pass <- apply_f(p_ev()) %>% filter(kind == "pass", !setp, !is.na(recip))
    validate(need(nrow(pass) >= 5, "Not enough passes for these filters."))
    pr <- pass %>% group_by(recip) %>% summarise(passes = n(), completed = sum(ok), xT = sum(xt, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(passes)) %>% head(input$syn_n)
    team_ev <- apply_f(lg_maps() %>% filter(team == p$team, touch, player.id %in% c(pr$recip, p$player.id)))
    pos <- team_ev %>% group_by(player.id) %>% summarise(ax = mean(x), ay = mean(y), .groups = "drop")
    nm <- M %>% filter(league == input$league) %>% distinct(player.id, player)
    pr <- pr %>% left_join(pos, by = c("recip" = "player.id")) %>% left_join(nm, by = c("recip" = "player.id")) %>% filter(!is.na(ax))
    me <- pos %>% filter(player.id == p$player.id)
    validate(need(nrow(me) == 1 && nrow(pr) > 0, "Not enough data for the synergy map."))
    pr <- pr %>% mutate(lab = paste0(sub("^.* ", "", player), "\n", passes, " | ", sprintf("%.2f", xT)))
    g <- pitch_base() +
      geom_segment(data = pr, aes(x = me$ay, y = me$ax, xend = ay, yend = ax, linewidth = passes), colour = BRAND$mid, alpha = 0.55, lineend = "round") +
      scale_linewidth(range = c(0.5, 5), guide = "none") +
      geom_point(data = pr, aes(ay, ax), size = 5, colour = BRAND$primary) +
      geom_text(data = pr, aes(ay, ax, label = lab), size = 3.1, vjust = -0.9, lineheight = 0.9, colour = BRAND$dark) +
      annotate("point", x = me$ay, y = me$ax, size = 7, colour = BRAND$accent) +
      annotate("text", x = me$ay, y = me$ax, label = sub("^.* ", "", p$player), size = 3.2, vjust = 2.3, fontface = "bold", colour = BRAND$dark) +
      labs(subtitle = "Top passing partners at their average positions. Line width: passes. Label: passes | xT gained.")
    list(plot = g, tbl = pr %>% transmute(Partner = player, Passes = passes, `Completed %` = round(100 * completed / passes), `xT gained` = round(xT, 2)))
  })
  output$map_tbl <- renderDT({
    req(input$map_type == "syn")
    datatable(syn_data()$tbl, rownames = FALSE, options = list(dom = "t", pageLength = 15))
  })

  # ---- Team maps page ----
  tf <- function(x) x %in% TRUE
  lg_pos <- reactive({ req(input$league); M %>% filter(league == input$league) %>% distinct(player.id, pos_group) })
  tm_lg <- reactive({
    d <- lg_maps() %>% left_join(lg_pos(), by = "player.id")
    if (!identical(input$tm_half, "both")) d <- d %>% filter(period == as.numeric(input$tm_half))
    if (!identical(input$tm_state, "all")) d <- d %>% filter(state == as.numeric(input$tm_state))
    d
  })
  tm_pgs <- reactive({
    if (identical(input$tm_view, "mid")) return(c("MF", "AM"))
    switch(input$tm_pos %||% "all", all = NULL, def = c("CB", "FB"), mid = c("MF", "AM"), att = c("W", "FW"))
  })
  tm_ev <- reactive({
    req(input$team); d <- tm_lg() %>% filter(team == input$team)
    if (!identical(input$tm_match, "all")) d <- d %>% filter(match_id == as.numeric(input$tm_match))
    g <- tm_pgs(); if (!is.null(g)) d <- d %>% filter(pos_group %in% g)
    d
  })
  observeEvent(list(input$team, input$league), {
    req(input$team, !is.null(get_maps(input$league)))
    d <- lg_maps() %>% filter(team == input$team) %>% distinct(match_id, opp) %>% arrange(match_id)
    updateSelectInput(session, "tm_match", choices = c("All matches" = "all", setNames(as.character(d$match_id), paste0("vs ", d$opp))))
  })
  # PageRank on the team's completed open play pass graph: how much of the passing runs through each player
  pass_central <- function(team_all) {
    ps <- team_all %>% filter(kind == "pass", tf(ok), !tf(setp), !is.na(recip), player.id != recip)
    ids <- sort(unique(c(ps$player.id, ps$recip))); n <- length(ids); if (n < 3) return(NULL)
    A <- matrix(0, n, n); ix <- function(v) match(v, ids)
    cnt <- ps %>% count(player.id, recip); A[cbind(ix(cnt$player.id), ix(cnt$recip))] <- cnt$n
    rs <- rowSums(A); P <- A / ifelse(rs == 0, 1, rs); P[rs == 0, ] <- 1 / n
    r <- rep(1 / n, n); for (i in 1:200) r <- 0.15 / n + 0.85 * as.numeric(r %*% P)
    inv <- tibble(player.id = ids, central = 100 * r / max(r), involved = rowSums(A) + colSums(A))
    inv$pass_share <- inv$involved / sum(inv$involved) * 2
    inv
  }
  tm_team_all <- reactive({
    req(input$team); d <- tm_lg() %>% filter(team == input$team)
    if (!identical(input$tm_match, "all")) d <- d %>% filter(match_id == as.numeric(input$tm_match))
    d
  })
  is_tov <- function(d) (d$kind == "pass" & !tf(d$ok) & !tf(d$setp)) | (d$kind == "dribble" & !tf(d$ok)) | d$kind == "loss"
  is_entry <- function(d) d$kind %in% c("pass", "carry") & tf(d$ok) & !tf(d$setp) & !is.na(d$ex) & d$x < 80 & d$ex >= 80
  zone3 <- function(x) cut(x, c(-Inf, 40, 80, Inf), labels = c("Own third", "Middle third", "Final third"))
  third_share <- function(d) d %>% count(team, z = zone3(x)) %>% group_by(team) %>% mutate(sh = n / sum(n)) %>% ungroup()
  surname <- function(x) sub("^.* ", "", x)
  tm_metrics <- reactive({
    switch(input$tm_view,
      def = list("Defensive actions" = function(d) d$kind == "def", "Tackles" = function(d) d$dtype %in% "tackle",
                 "Interceptions" = function(d) d$dtype %in% "interception", "Recoveries" = function(d) d$dtype %in% "recovery",
                 "Blocks and clearances" = function(d) d$dtype %in% c("block", "clearance"),
                 "Actions in the attacking third" = function(d) d$kind == "def" & d$x >= 80),
      lose = list("Turnovers" = is_tov, "In own third" = function(d) is_tov(d) & d$x < 40, "In middle third" = function(d) is_tov(d) & d$x >= 40 & d$x < 80,
                  "In final third" = function(d) is_tov(d) & d$x >= 80),
      att = , rhy = list("Shots (no penalties)" = function(d) d$kind == "shot" & !tf(d$pen), "xG" = function(d) ifelse(d$kind == "shot" & !tf(d$pen), coalesce(d$xg, 0), 0),
                 "Final third entries" = is_entry, "xT created" = function(d) ifelse(!is.na(d$xt) & d$xt > 0, d$xt, 0)),
      mid = list("Touches" = function(d) tf(d$touch), "Progressive passes" = function(d) d$kind == "pass" & tf(d$prog), "Progressive carries" = function(d) d$kind == "carry" & tf(d$prog),
                 "Entries into the final third" = is_entry, "Turnovers" = is_tov, "xT created" = function(d) ifelse(!is.na(d$xt) & d$xt > 0, d$xt, 0)))
  })
  # ---- Season rhythm: per match stats for every team (match order follows match id) ----
  tm_season <- reactive({
    req(input$team)
    st <- lg_maps() %>% mutate(.xg = ifelse(kind == "shot" & !tf(pen), coalesce(xg, 0), 0), .sh = kind == "shot" & !tf(pen), .gl = kind == "shot" & tf(ok),
                               .xt = ifelse(!is.na(xt) & xt > 0, xt, 0), .en = is_entry(.), .rg = kind == "def" & x >= 80, .tv = is_tov(.)) %>%
      group_by(match_id, team) %>% summarise(xg = sum(.xg), shots = sum(.sh), goals = sum(.gl), xt = sum(.xt), entries = sum(.en), regains = sum(.rg), tov = sum(.tv), .groups = "drop")
    me <- st %>% filter(team == input$team)
    vs <- st %>% rename_with(~ paste0(.x, "_opp"), -match_id)
    out <- me %>% inner_join(vs, by = "match_id") %>% filter(team != team_opp)
    # Real dates and official scores when data/match_dates_<league>.rds exists (script 18); otherwise match id order and event goals
    fd <- file.path("data", paste0("match_dates_", gsub("[^A-Za-z]", "", input$league), ".rds"))
    if (file.exists(fd)) {
      out <- out %>% left_join(readRDS(fd), by = "match_id") %>% arrange(match_date, match_id) %>%
        mutate(ven = ifelse(home == input$team, "H", "A"), gf = ifelse(home == input$team, home_score, away_score), ga = ifelse(home == input$team, away_score, home_score),
               goals = coalesce(as.integer(gf), as.integer(goals)), goals_opp = coalesce(as.integer(ga), as.integer(goals_opp)))
    } else out <- out %>% arrange(match_id) %>% mutate(match_date = as.Date(NA), ven = NA_character_)
    out <- out %>% mutate(idx = row_number())
    list(me = out, all = st, dated = file.exists(fd))
  })
  roll <- function(x, k = 5) vapply(seq_along(x), function(i) mean(x[max(1, i - k + 1):i]), numeric(1))
  season_log <- reactive({
    d <- tm_season()$me
    d %>% transmute(`#` = idx, Date = ifelse(is.na(match_date), "", format(match_date, "%d %b")), `H/A` = coalesce(ven, ""), Opponent = team_opp, Result = paste0(ifelse(goals > goals_opp, "W ", ifelse(goals == goals_opp, "D ", "L ")), goals, "-", goals_opp),
                    xG = round(xg, 2), `xG against` = round(xg_opp, 2), Shots = shots, `xT` = round(xt, 2))
  })
  output$tm_cmp <- renderDT({
    req(input$team); lgd <- tm_lg(); g <- tm_pgs(); if (!is.null(g)) lgd <- lgd %>% filter(pos_group %in% g)
    den <- lg_maps() %>% distinct(team, match_id); mets <- tm_metrics()
    one <- !identical(input$tm_match, "all")
    rows <- bind_rows(lapply(names(mets), function(nm) {
      tmv <- lgd %>% mutate(w = as.numeric(mets[[nm]](lgd))) %>% group_by(team, match_id) %>% summarise(n = sum(w, na.rm = TRUE), .groups = "drop")
      per <- den %>% left_join(tmv, by = c("team", "match_id")) %>% mutate(n = coalesce(n, 0))
      ours <- if (one) per$n[per$team == input$team & per$match_id == as.numeric(input$tm_match)] else mean(per$n[per$team == input$team])
      lgt <- per %>% group_by(team) %>% summarise(m = mean(n), .groups = "drop")
      tibble(Metric = nm, `Per match` = fmt_num(ours), `League average` = fmt_num(mean(lgt$m)),
             Rank = paste0(rank(-lgt$m, ties.method = "min")[lgt$team == input$team], " of ", nrow(lgt)))
    }))
    datatable(rows, rownames = FALSE, class = "compact stripe", options = list(dom = "t", ordering = FALSE, pageLength = 10))
  })
  output$tm_pl <- renderDT({
    ev <- tm_ev(); nm <- M %>% filter(league == input$league) %>% distinct(player.id, player)
    v <- input$tm_view
    if (v == "rhy") {
      lg <- season_log() %>% arrange(desc(`#`))
      return(datatable(lg, rownames = FALSE, class = "compact stripe", options = list(dom = "t", ordering = TRUE, pageLength = 12, scrollY = "320px", paging = FALSE)))
    }
    t <- if (v == "def") {
      d <- ev %>% filter(kind == "def"); req(nrow(d) > 0)
      d %>% count(player.id, dtype) %>% pivot_wider(names_from = dtype, values_from = n, values_fill = 0) %>%
        mutate(Total = rowSums(across(where(is.numeric) & !player.id)))
    } else if (v == "lose") {
      d <- ev %>% filter(is_tov(.)); req(nrow(d) > 0)
      d %>% count(player.id, z = zone3(x)) %>% pivot_wider(names_from = z, values_from = n, values_fill = 0) %>%
        mutate(Total = rowSums(across(where(is.numeric) & !player.id)))
    } else if (v == "att") {
      if (identical(input$tm_att, "shots")) {
        d <- ev %>% filter(kind == "shot", !tf(pen)); req(nrow(d) > 0)
        d %>% group_by(player.id) %>% summarise(Shots = n(), Goals = sum(tf(ok)), xG = round(sum(xg, na.rm = TRUE), 2), .groups = "drop") %>% arrange(desc(Shots))
      } else if (identical(input$tm_att, "entries")) {
        d <- ev %>% filter(is_entry(.)); req(nrow(d) > 0)
        d %>% group_by(player.id) %>% summarise(Passes = sum(kind == "pass"), Carries = sum(kind == "carry"), Total = n(), .groups = "drop") %>% arrange(desc(Total))
      } else {
        d <- ev %>% filter(!is.na(xt), xt > 0); req(nrow(d) > 0)
        d %>% group_by(player.id) %>% summarise(Actions = n(), `xT created` = round(sum(xt), 2), .groups = "drop") %>% arrange(desc(`xT created`))
      }
    } else if (identical(input$tm_mid, "net")) {
      cen <- pass_central(tm_team_all()); req(!is.null(cen))
      pg <- tm_team_all() %>% filter(!is.na(pos_group)) %>% count(player.id, pos_group) %>% group_by(player.id) %>% slice_max(n, n = 1, with_ties = FALSE) %>% ungroup() %>% select(player.id, Position = pos_group)
      cen %>% left_join(pg, by = "player.id") %>% transmute(player.id, Position, `Passes sent + received` = involved, `Share of team passing (%)` = round(100 * pass_share, 1), `Centrality (100 = most)` = round(central)) %>% arrange(desc(`Centrality (100 = most)`))
    } else {
      req(nrow(ev) > 0)
      ev %>% mutate(tov = is_tov(.)) %>% group_by(player.id) %>% summarise(Touches = sum(tf(touch)), `Prog. passes` = sum(kind == "pass" & tf(prog)), `Prog. carries` = sum(kind == "carry" & tf(prog)),
                                              Turnovers = sum(tov), `xT created` = round(sum(pmax(xt, 0), na.rm = TRUE), 2), .groups = "drop") %>% arrange(desc(Touches))
    }
    t <- t %>% left_join(nm, by = "player.id") %>% select(Player = player, everything(), -player.id) %>% head(12)
    datatable(t, rownames = FALSE, class = "compact stripe", options = list(dom = "t", ordering = TRUE, pageLength = 12))
  })
  output$tm_title <- renderText({
    req(input$team)
    q <- switch(input$tm_view, def = if (identical(input$tm_def, "heat")) "defensive action heatmap" else "defensive actions",
                lose = "where we lose the ball", att = switch(input$tm_att, shots = "shot map", entries = "final third entries", threat = "threat map (xT)"),
                mid = switch(input$tm_mid, net = "midfield passing network", touch = "midfield touches", prog = "midfield progression"),
                rhy = switch(input$tm_rhy, form = "season form and momentum", met = "match by match", inm = "momentum inside a match"))
    paste0(input$team, ": ", q)
  })
  output$tm_plot <- renderPlot({
    req(input$team); v <- input$tm_view
    if (v == "rhy") {
      sn <- tm_season(); d <- sn$me; validate(need(nrow(d) >= 3, "Not enough matches."))
      mode <- input$tm_rhy %||% "form"
      d$res <- ifelse(d$goals > d$goals_opp, "W", ifelse(d$goals == d$goals_opp, "D", "L"))
      rescol <- c(W = BRAND$primary, D = "#9AA3B5", L = BRAND$accent)
      if (mode == "form") {
        d$xgd <- d$xg - d$xg_opp; d$r5 <- roll(d$xgd); top <- max(abs(c(d$xgd, d$r5))) * 1.35
        g <- ggplot(d, aes(idx, xgd)) + geom_hline(yintercept = 0, colour = BRAND$muted) +
          geom_col(aes(fill = xgd >= 0), width = 0.7, alpha = 0.85) +
          scale_fill_manual(values = c(`TRUE` = BRAND$mid, `FALSE` = BRAND$accent), guide = "none") +
          geom_line(aes(y = r5), colour = BRAND$primary, linewidth = 1.6) + geom_point(aes(y = r5), colour = BRAND$primary, size = 2.6) +
          geom_point(aes(y = top, colour = res), size = 8) + geom_text(aes(y = top, label = res), colour = "white", fontface = "bold", size = 3.8) +
          scale_colour_manual(values = rescol, guide = "none") +
          scale_x_continuous(breaks = d$idx, labels = if (sn$dated) format(d$match_date, "%d %b") else d$idx, expand = expansion(add = 0.7)) +
          scale_y_continuous(expand = expansion(mult = c(0.18, 0.08))) +
          labs(x = if (sn$dated) "Match date" else "Match number (match id order, no dates for this league)", y = "xG for minus xG against",
               subtitle = sprintf("Bars: each match. Blue line: rolling 5 match average, now %+.2f. Circles: result. Last 5 matches %+.2f vs season %+.2f.", tail(d$r5, 1), mean(tail(d$xgd, 5)), mean(d$xgd))) +
          theme_lcw(base_size = 14) + theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(), axis.text.x = element_text(angle = if (sn$dated) 45 else 0, hjust = if (sn$dated) 1 else 0.5))
        if (all(d$team_opp %in% names(CREST_PNG))) {
          lo <- min(c(d$xgd, d$r5)); d$cr <- unname(CREST_PNG[d$team_opp]); yy <- lo - (top - lo) * 0.12
          g <- g + ggimage::geom_image(data = d, aes(y = yy, image = cr), size = 0.035, asp = 1.6)
        }
        g
      } else if (mode == "met") {
        k <- input$tm_rmet %||% "xg"; lab <- c(xg = "xG", shots = "Shots", xt = "xT created", entries = "Final third entries", regains = "High regains", tov = "Turnovers")[[k]]
        d$a <- d[[k]]; d$b <- d[[paste0(k, "_opp")]]; lgm <- mean(sn$all[[k]])
        dl <- bind_rows(tibble(idx = d$idx, v = d$a, r = roll(d$a), who = input$team), tibble(idx = d$idx, v = d$b, r = roll(d$b), who = "Opponents"))
        ggplot(dl, aes(idx, colour = who)) + geom_hline(yintercept = lgm, colour = BRAND$accent, linetype = "dashed", linewidth = 1) +
          geom_point(aes(y = v), alpha = 0.35, size = 2.6) + geom_line(aes(y = r), linewidth = 1.6) +
          scale_colour_manual(values = setNames(c(BRAND$primary, "#D9A300"), c(input$team, "Opponents")), name = NULL) +
          scale_x_continuous(breaks = d$idx, labels = if (sn$dated) format(d$match_date, "%d %b") else d$idx) +
          labs(x = if (sn$dated) "Match date" else "Match number (match id order)", y = lab, subtitle = paste0(lab, " per match. Dots: each match. Lines: rolling 5 match average. Gold dashed: league average for a team in a match.")) +
          theme_lcw(base_size = 14) + theme(panel.grid.minor = element_blank(), legend.position = "top", axis.text.x = element_text(angle = if (sn$dated) 45 else 0, hjust = if (sn$dated) 1 else 0.5))
      } else {
        m <- if (identical(input$tm_match, "all")) tail(d$match_id, 1) else as.numeric(input$tm_match)
        info <- d[d$match_id == m, ]; validate(need(nrow(info) == 1, "Pick a match from the Match list."))
        ev <- lg_maps() %>% filter(match_id == m)
        bins <- ev %>% mutate(bin = floor(minute / 5) * 5, w = ifelse(!is.na(xt) & xt > 0, xt, 0)) %>% group_by(bin, who = ifelse(team == input$team, "us", "them")) %>% summarise(v = sum(w), .groups = "drop") %>%
          mutate(v = ifelse(who == "us", v, -v))
        gl <- ev %>% filter(kind == "shot", tf(ok)) %>% transmute(minute, who = ifelse(team == input$team, "us", "them"))
        top <- max(abs(bins$v)) * 1.25
        gl$y <- ifelse(gl$who == "us", top, -top)
        ggplot(bins, aes(bin + 2.5, v, fill = who)) + geom_hline(yintercept = 0, colour = BRAND$muted) + geom_vline(xintercept = 45, linetype = "dashed", colour = "#9AA3B5") +
          geom_col(width = 4.4) + scale_fill_manual(values = c(us = BRAND$primary, them = BRAND$accent), labels = c(us = input$team, them = info$team_opp), name = NULL) +
          geom_point(data = gl, aes(minute, y), inherit.aes = FALSE, shape = 21, size = 6, fill = "white", colour = BRAND$primary, stroke = 1.5) +
          geom_text(data = gl, aes(minute, y, label = "G"), inherit.aes = FALSE, size = 3.2, fontface = "bold", colour = BRAND$primary) +
          scale_x_continuous(breaks = seq(0, 120, 15)) +
          labs(x = "Minute", y = "Threat created (xT) per 5 minutes", subtitle = sprintf("Match %d vs %s: %s %d-%d. Bars above the line are our threat, below the line theirs. G: goal.", info$idx, info$team_opp, input$team, info$goals, info$goals_opp)) +
          theme_lcw(base_size = 14) + theme(panel.grid.minor = element_blank(), legend.position = "top")
      }
    } else {
    ev <- tm_ev()
    lgd <- tm_lg(); g <- tm_pgs(); if (!is.null(g)) lgd <- lgd %>% filter(pos_group %in% g)
    share_txt <- function(dd, keep_fn) {
      d <- dd[keep_fn(dd), ]; ours <- third_share(d); me <- ours %>% filter(team == input$team); lg <- ours %>% group_by(z) %>% summarise(sh = mean(sh), .groups = "drop")
      paste0(sprintf("%s %.0f%% (league %.0f%%)", me$z, 100 * me$sh, 100 * lg$sh[match(me$z, lg$z)]), collapse = "   |   ")
    }
    heat <- function(d, cols) stat_density_2d(data = d, aes(y, x, fill = after_stat(density), alpha = after_stat(density)), geom = "raster", contour = FALSE, n = 110)
    if (v == "def") {
      d <- ev %>% filter(kind == "def"); validate(need(nrow(d) >= 5, "Not enough defensive actions for these filters."))
      sub <- paste0(nrow(d), " defensive actions. Line height ", sprintf("%.0f m", mean(d$x) * 0.9144), " from our goal line. ", share_txt(ev, function(z) z$kind == "def"))
      if (identical(input$tm_def, "heat")) {
        pitch_base() + heat(d) + scale_fill_gradientn(colours = c("#DCE5F5", BRAND$mid, BRAND$primary), guide = "none") + scale_alpha(range = c(0, 0.9), guide = "none") +
          geom_hline(yintercept = mean(d$x), linetype = "dashed", colour = BRAND$accent, linewidth = 1) + labs(subtitle = sub)
      } else {
        pitch_base() + geom_hline(yintercept = mean(d$x), linetype = "dashed", colour = BRAND$accent, linewidth = 1) +
          geom_vline(xintercept = c(26.7, 53.3), colour = "#E3E6EE") +
          geom_point(data = d, aes(y, x, shape = dtype, colour = ok), size = 2, alpha = 0.5, stroke = 0.9) +
          scale_shape_manual(values = c(tackle = 16, interception = 17, recovery = 15, block = 18, clearance = 4), name = NULL) +
          scale_colour_manual(values = c(`TRUE` = BRAND$primary, `FALSE` = BRAND$accent), labels = c(`TRUE` = "Successful", `FALSE` = "Lost tackle"), name = NULL) +
          labs(subtitle = sub) + theme(legend.position = "bottom")
      }
    } else if (v == "lose") {
      d <- ev %>% filter(is_tov(.)); validate(need(nrow(d) >= 10, "Not enough turnovers for these filters."))
      shares <- third_share(d) %>% filter(team == input$team)
      lgs <- third_share(lgd %>% filter(is_tov(.))) %>% group_by(z) %>% summarise(sh = mean(sh), .groups = "drop")
      lab <- tibble(x = c(20, 60, 100), z = levels(zone3(1)))
      lab$txt <- sprintf("%s\n%.0f%% (league %.0f%%)", lab$z, 100 * shares$sh[match(lab$z, shares$z)], 100 * lgs$sh[match(lab$z, lgs$z)])
      pitch_base() + heat(d) + scale_fill_gradientn(colours = c("#FFF4CC", BRAND$accent, "#C97F00"), guide = "none") + scale_alpha(range = c(0, 0.95), guide = "none") +
        geom_point(data = d, aes(y, x), colour = BRAND$primary, size = 0.9, alpha = 0.35) +
        geom_hline(yintercept = c(40, 80), linetype = "dashed", colour = "#9AA3B5") +
        geom_label(data = lab, aes(x = 77, y = x, label = txt), hjust = 1, size = 3.6, colour = BRAND$dark, fill = alpha("white", 0.85), label.size = 0, lineheight = 0.95) +
        labs(subtitle = paste0(nrow(d), " turnovers: failed passes, lost take-ons and heavy touches or dispossessions (set pieces excluded)."))
    } else if (v == "att") {
      mode <- input$tm_att
      if (mode == "shots") {
        d <- ev %>% filter(kind == "shot", !tf(pen), !is.na(xg)); validate(need(nrow(d) >= 1, "No shots for these filters."))
        pitch_base(TRUE, TRUE) +
          geom_point(data = d %>% filter(!tf(ok)), aes(y, x, size = xg), shape = 21, fill = BRAND$mid, colour = "white", stroke = 0.6, alpha = 0.85) +
          geom_point(data = d %>% filter(tf(ok)), aes(y, x, size = xg), shape = 21, fill = BRAND$accent, colour = BRAND$primary, stroke = 1.6) +
          scale_size_area(max_size = 13, guide = "none") +
          labs(subtitle = sprintf("%d shots, %d goals, %.2f xG, %.2f xG per shot. Gold: goal. Dot size: xG (penalties excluded).", nrow(d), sum(tf(d$ok)), sum(d$xg), mean(d$xg)))
      } else if (mode == "entries") {
        d <- ev %>% filter(is_entry(.)); validate(need(nrow(d) >= 3, "Not enough entries for these filters."))
        d$ch <- cut(d$ey, c(-Inf, 80 / 3, 160 / 3, Inf), labels = c("Left", "Centre", "Right")); cn <- table(d$ch)
        n_all <- nrow(d); if (n_all > 150) d <- d %>% arrange(desc(xt)) %>% head(150)
        pitch_base() + geom_vline(xintercept = c(80 / 3, 160 / 3), linetype = "dotted", colour = "#9AA3B5") + geom_hline(yintercept = 80, linetype = "dashed", colour = BRAND$accent, linewidth = 0.9) +
          geom_segment(data = d, aes(y, x, xend = ey, yend = ex, colour = kind), arrow = arrow_head, alpha = 0.7, linewidth = 0.5) +
          scale_colour_manual(values = c(pass = BRAND$primary, carry = "#D9A300"), labels = c(pass = "Pass", carry = "Carry"), name = NULL) +
          labs(subtitle = sprintf("%d entries into the final third. Left %d, centre %d, right %d.%s Dashed gold line: start of the final third.", n_all, cn[["Left"]], cn[["Centre"]], cn[["Right"]], if (n_all > 150) " Arrows: the 150 highest xT." else "")) + theme(legend.position = "bottom")
      } else {
        d <- ev %>% filter(kind %in% c("pass", "carry"), !is.na(xt), xt > 0); validate(need(nrow(d) >= 5, "Not enough threatening actions."))
        top <- d %>% arrange(desc(xt)) %>% head(30)
        pitch_base() + geom_bin_2d(data = d, aes(ey, ex, weight = xt), binwidth = c(10, 10), alpha = 0.7) +
          scale_fill_gradient(low = "#FFF4CC", high = "#E8A200", name = "xT created") +
          geom_segment(data = top, aes(y, x, xend = ey, yend = ex), colour = BRAND$primary, arrow = arrow_head, linewidth = 0.6, alpha = 0.8) +
          labs(subtitle = sprintf("%d actions with positive xT, total %.1f. Blue arrows: the 30 most threatening.", nrow(d), sum(d$xt))) + theme(legend.position = "bottom")
      }
    } else {
      mode <- input$tm_mid
      mf <- ev
      if (mode == "net") {
        team_all <- tm_team_all(); cen <- pass_central(team_all)
        validate(need(!is.null(cen), "Not enough passes for these filters."))
        pos <- team_all %>% filter(tf(touch)) %>% group_by(player.id) %>% summarise(ax = mean(x), ay = mean(y), touches = n(), pg = first(na.omit(pos_group)), .groups = "drop")
        mids <- pos$player.id[pos$pg %in% c("MF", "AM")]
        pass <- team_all %>% filter(kind == "pass", tf(ok), !tf(setp), !is.na(recip), player.id != recip, player.id %in% mids | recip %in% mids)
        validate(need(nrow(pass) >= 10 && length(mids) > 0, "Not enough midfield passes for these filters."))
        e <- pass %>% mutate(a = pmin(player.id, recip), b = pmax(player.id, recip)) %>% count(a, b) %>% arrange(desc(n)) %>% head(18)
        e <- e %>% left_join(pos, by = c("a" = "player.id")) %>% rename(x1 = ay, y1 = ax) %>% left_join(pos, by = c("b" = "player.id")) %>% rename(x2 = ay, y2 = ax) %>% filter(!is.na(x1), !is.na(x2))
        nodes <- pos %>% filter(player.id %in% unique(c(e$a, e$b))) %>% left_join(cen, by = "player.id") %>% left_join(M %>% filter(league == input$league) %>% distinct(player.id, player), by = "player.id")
        pitch_base() + geom_segment(data = e, aes(x = x1, y = y1, xend = x2, yend = y2, linewidth = n), colour = BRAND$mid, alpha = 0.5, lineend = "round") + scale_linewidth(range = c(0.5, 6), guide = "none") +
          geom_point(data = nodes, aes(ay, ax, size = central, fill = pg %in% c("MF", "AM")), shape = 21, colour = "white", stroke = 1.2) +
          scale_fill_manual(values = c(`TRUE` = BRAND$primary, `FALSE` = BRAND$muted), guide = "none") + scale_size_area(max_size = 12, guide = "none") +
          geom_text(data = nodes, aes(ay, ax, label = surname(player)), vjust = -1.9, size = 3.6, fontface = "bold", colour = BRAND$dark) +
          labs(subtitle = paste0("Connections between midfielders and teammates (completed open play passes). Line width: passes. Node size: network centrality (how much of our passing runs through the player). Blue: midfielders. ", nrow(pass), " passes."))
      } else if (mode == "touch") {
        d <- mf %>% filter(tf(touch)); validate(need(nrow(d) >= 30, "Not enough midfield touches for these filters."))
        avg <- d %>% group_by(player.id) %>% summarise(ax = mean(x), ay = mean(y), n = n(), .groups = "drop") %>% filter(n >= 30) %>%
          left_join(M %>% filter(league == input$league) %>% distinct(player.id, player), by = "player.id")
        pitch_base() + heat(d) + scale_fill_gradientn(colours = c("#DCE5F5", BRAND$mid, BRAND$primary), guide = "none") + scale_alpha(range = c(0, 0.9), guide = "none") +
          geom_point(data = avg, aes(ay, ax), colour = BRAND$accent, size = 4) + geom_text(data = avg, aes(ay, ax, label = surname(player)), vjust = -1.2, size = 3.4, fontface = "bold", colour = BRAND$dark) +
          labs(subtitle = paste0(nrow(d), " midfield touches. Gold dots: each midfielder's average position."))
      } else {
        d <- mf %>% filter(tf(prog)); validate(need(nrow(d) >= 3, "No progressive actions for these filters."))
        np <- sum(d$kind == "pass"); nc <- sum(d$kind == "carry"); n_all <- nrow(d); if (n_all > 150) d <- d %>% arrange(desc(xt)) %>% head(150)
        d$kk <- ifelse(d$kind == "pass", "Progressive pass", "Progressive carry")
        pitch_base() + geom_segment(data = d, aes(y, x, xend = ey, yend = ex, colour = kk), arrow = arrow_head, alpha = 0.75, linewidth = 0.5) +
          scale_colour_manual(values = c(`Progressive pass` = BRAND$primary, `Progressive carry` = "#D9A300"), name = NULL) +
          labs(subtitle = sprintf("%d progressive passes and %d progressive carries by our midfielders (25%% closer to goal).%s", np, nc, if (n_all > 150) " Arrows: the 150 highest xT." else "")) + theme(legend.position = "bottom")
      }
    }
    }
  })

  # ---- Player page ----
  output$header <- renderUI({
    p <- sel(); req(nrow(p) == 1)
    note <- if (p$pos_group %in% GROUPS) {
      pr <- prof(); paste0("Compared with ", pr$n_pool, " players (", paste(GROUP_NAMES[pr$gset], collapse = " + "), ", ",
                           if (identical(input$pool, "all")) "five leagues" else input$pool, ", ", input$min_min, "+ minutes)")
    } else "Goalkeepers: bars not built yet."
    div(style = "display:flex;align-items:center;gap:16px;",
        photo_tag(p$player.id, p$player, size = 112, team = p$team),
        div(tags$h3(p$player, style = sprintf("margin:0 0 2px 0;color:%s;font-weight:700;", BRAND$primary)),
            div(paste0(unname(GROUP_NAMES[p$pos_group]), if (!is.na(p$main_pos)) paste0(" (most frequent: ", p$main_pos, ")") else "", ", ", p$team)),
            div(paste0(round(p$minutes), " minutes, ", p$matches, " matches"), style = sprintf("color:%s;", BRAND$grey)),
            div(note, style = sprintf("color:%s;font-size:13px;", BRAND$grey))),
        div(style = "margin-left:auto;display:flex;align-items:center;gap:18px;", actionButton("go_compare", "Compare with...", class = "btn-compare"), downloadButton("dl_card", "Share as image", class = "btn-share"), crest_tag(p$team, 64), league_tag(p$league, 52)))
  })
  # Skill cards: one per area, with a descriptor, strength / work-on tags, and a minutes-adjusted score.
  # Adjusted = 50 + (bar - 50) * minutes / (minutes + 600): a Bayesian-style shrink toward the average, so thin samples count for less.
  output$skill_cards <- renderUI({
    p <- sel(); req(nrow(p) == 1, p$pos_group %in% GROUPS)
    b <- bars_from(prof()$d); b <- b[match(DOM$domain, b$domain), ]
    w <- p$minutes / (p$minutes + 600)
    ok <- which(!is.na(b$bar)); top <- ok[order(-b$bar[ok])][seq_len(min(2, length(ok)))]; low <- rev(ok[order(-b$bar[ok])])[seq_len(min(2, length(ok)))]
    desc <- function(v) if (is.na(v)) "Not enough reliable data" else if (v >= 90) "Elite" else if (v >= 75) "Strong" else if (v >= 60) "Above average" else if (v >= 40) "Average" else if (v >= 25) "Below average" else "Needs work"
    cards <- lapply(seq_len(nrow(DOM)), function(i) {
      v <- b$bar[i]; tag <- if (i %in% top && v >= 60) "Strength" else if (i %in% low && v < 60 && !(i %in% top)) "Work on" else ""
      conf <- if (is.na(v)) "" else paste0("Minutes-adjusted ", round(50 + (v - 50) * w), "  |  ", b$n_ok[i], " of ", b$n_all[i], " KPIs")
      div(class = paste("skill-card", if (tag == "Strength") "sk-strength" else if (tag == "Work on") "sk-work"),
          onclick = sprintf("var e=document.getElementById('box_%s'); if(e){e.scrollIntoView({behavior:'smooth',block:'start'});}", DOM$domain[i]),
          div(class = "sk-top", span(class = "sk-name", DOM$domain_label[i]), if (nzchar(tag)) span(class = "sk-tag", tag)),
          div(class = "sk-score", if (is.na(v)) "n/a" else sprintf("%.0f", v)),
          div(class = "sk-bar", div(class = "sk-fill", style = sprintf("width:%s%%;", if (is.na(v)) 0 else round(v))), div(class = "sk-mid")),
          div(class = "sk-desc", desc(v)), div(class = "sk-sub", conf))
    })
    div(div(class = "sk-head", span("Skill cards"), span(class = "sk-hint", "Tap a card to jump to its detail. Percentile within position group; 50 is the average player in the pool.")),
        div(class = "skill-grid", cards))
  })
  output$dl_card <- downloadHandler(
    filename = function() paste0(gsub("[^A-Za-z0-9]+", "_", sel()$player), "_player_card.png"),
    contentType = "image/png",
    content = function(file) {
      p <- sel(); req(nrow(p) == 1, p$pos_group %in% GROUPS); pr <- prof()
      note <- paste0("Compared with ", pr$n_pool, " players (", paste(GROUP_NAMES[pr$gset], collapse = " + "), ", ",
                     if (identical(input$pool, "all")) "five leagues" else input$pool, ", ", input$min_min, "+ minutes)")
      open_png(file); on.exit(dev.off())
      print(make_card(p, pr, note))
    })
  plot_df <- reactive({
    pr <- prof(); d <- pr$d; b <- bars_from(d)
    keep <- c("headline", if (isTRUE(input$show_detail)) "detail")
    rows <- d %>% filter(role %in% keep)
    if (input$dom != "all") { rows <- rows %>% filter(domain == input$dom); b <- b %>% filter(domain == input$dom) }
    barrows <- b %>% transmute(domain, label = "Area bar", role = "bar", key = "bar", value = NA_real_, pct = bar, excluded = FALSE)
    out <- bind_rows(barrows, rows %>% select(domain, label, role, key, value, pct, excluded)) %>%
      mutate(domain = as.character(domain)) %>% left_join(DOM, by = "domain") %>%
      mutate(domain = factor(domain, levels = DOM$domain), domain_label = factor(domain_label, levels = DOM$domain_label),
             ord = match(role, c("bar", "headline", "detail"))) %>% arrange(domain, ord) %>%
      mutate(id = factor(row_number()), kind = ifelse(excluded, "low", role),
             lab = ifelse(role == "bar", ifelse(is.na(pct), "n/a", sprintf("%.0f", pct)),
                          ifelse(excluded, paste0(fmt_val(key, value), "  (low reliability)"),
                                 paste0(fmt_val(key, value), "  |  ", ifelse(is.na(pct), "n/a", sprintf("%.0f", pct))))))
    out$id <- factor(out$id, levels = rev(levels(out$id)))
    out
  })
  # One bordered box per area (Final product, Shooting, ...), each with its own header, area score and bar plot
  output$bars_ui <- renderUI({
    df <- plot_df(); doms <- intersect(DOM$domain, as.character(df$domain))
    tagList(lapply(doms, function(d) {
      dd <- df[as.character(df$domain) == d, ]; b <- dd$pct[dd$role == "bar"]
      div(class = "dom-box", id = paste0("box_", d),
          div(class = "dom-head", span(class = "dom-title", DOM$domain_label[DOM$domain == d]),
              span(class = "dom-badge", if (length(b) && !is.na(b)) sprintf("%.0f", b) else "n/a")),
          plotOutput(paste0("bars_", d), height = paste0(26 * nrow(dd) + 34, "px")))
    }))
  })
  for (d in DOM$domain) local({
    dom <- d
    output[[paste0("bars_", dom)]] <- renderPlot({
      df <- plot_df(); df <- df[as.character(df$domain) == dom, ]; req(nrow(df) > 0); df$id <- droplevels(df$id)
      labs_y <- setNames(ifelse(df$role == "bar", paste0("■ ", df$label), df$label), df$id)
      ggplot(df, aes(pct, id)) +
        geom_col(aes(fill = kind), width = 0.72, na.rm = TRUE) +
        geom_vline(xintercept = 50, colour = BRAND$muted, linetype = "dashed") +
        geom_text(aes(x = 102, label = lab), hjust = 0, size = 3.8, colour = BRAND$dark) +
        scale_fill_manual(values = c(bar = BRAND$primary, headline = BRAND$mid, detail = BRAND$muted, low = "#E3E6EE"), guide = "none") +
        scale_x_continuous(limits = c(0, 150), breaks = c(0, 25, 50, 75, 100), expand = c(0, 0)) +
        scale_y_discrete(labels = labs_y) +
        labs(x = NULL, y = NULL) +
        theme_lcw(base_size = 13) +
        theme(panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(), axis.text.y = element_text(size = 11, colour = BRAND$dark),
              axis.text.x = element_text(colour = BRAND$grey, size = 10), plot.margin = margin(2, 6, 2, 2))
    })
  })
  cmp_prof <- reactive({
    req(input$cmp, input$cmp != "pool")
    p2 <- M %>% filter(league == input$league, as.character(player.id) == input$cmp) %>% head(1)
    req(nrow(p2) == 1, p2$pos_group %in% GROUPS)
    list(p = p2, prof = build_profile(p2, input$merge, pool_leagues(), input$min_min))
  })
  output$radar <- renderPlot({
    p <- sel(); b <- bars_from(prof()$d); n <- nrow(DOM)
    val <- function(bb) ifelse(is.na(bb$bar[match(DOM$domain, bb$domain)]), 0, bb$bar[match(DOM$domain, bb$domain)])
    poly <- function(v, who) { a <- 2 * pi * (seq_len(n) - 1) / n; tibble(x = v / 100 * sin(a), y = v / 100 * cos(a), who = who, k = seq_len(n)) }
    a <- 2 * pi * (seq_len(n) - 1) / n
    dA <- poly(val(b), p$player); nm <- p$player; cols <- setNames(BRAND$primary, nm)
    dd <- dA
    if (input$cmp != "pool") {
      cp <- cmp_prof(); nB <- cp$p$player; dB <- poly(val(bars_from(cp$prof$d)), nB)
      dd <- bind_rows(dA, dB); cols <- setNames(c(BRAND$primary, BRAND$accent), c(nm, nB))
    }
    ring <- function(r) { t <- seq(0, 2 * pi, length.out = 200); tibble(x = r * sin(t), y = r * cos(t)) }
    lab <- tibble(x = 1.22 * sin(a), y = 1.22 * cos(a), l = gsub(" ", "\n", DOM$domain_label, fixed = TRUE))
    g <- ggplot() +
      geom_path(data = ring(0.25), aes(x, y), colour = "#E3E6EE") + geom_path(data = ring(0.75), aes(x, y), colour = "#E3E6EE") +
      geom_path(data = ring(1), aes(x, y), colour = BRAND$muted) +
      geom_path(data = ring(0.5), aes(x, y), colour = BRAND$accent, linewidth = 1, linetype = "dashed") +
      geom_segment(data = tibble(a = a), aes(x = 0, y = 0, xend = sin(a), yend = cos(a)), colour = "#E3E6EE") +
      geom_polygon(data = dd, aes(x, y, fill = who, colour = who), alpha = 0.22, linewidth = 1.2) +
      geom_point(data = dd, aes(x, y, colour = who), size = 3) +
      geom_text(data = lab, aes(x, y, label = l), size = 3.8, colour = BRAND$dark, lineheight = 0.9) +
      scale_fill_manual(values = cols, name = NULL) + scale_colour_manual(values = cols, name = NULL) +
      coord_fixed(xlim = c(-1.6, 1.6), ylim = c(-1.45, 1.45)) + theme_void(base_size = 13, base_family = FONT_BODY) + theme(legend.position = "bottom")
    g
  })
  output$dumbbell <- renderPlot({
    p <- sel(); d <- prof()$d %>% filter(role == "headline", !excluded, !is.na(pct)) %>% select(key, label, domain, domain_label, pct)
    nA <- p$player
    if (input$cmp != "pool") {
      cp <- cmp_prof(); nB <- cp$p$player
      d2 <- cp$prof$d %>% filter(role == "headline") %>% select(key, pct2 = pct)
      d <- d %>% left_join(d2, by = "key")
    } else { nB <- "Pool average"; d$pct2 <- 50 }
    d <- d %>% filter(!is.na(pct2)) %>% mutate(domain = factor(as.character(domain), levels = DOM$domain)) %>% arrange(domain)
    d$domain_label <- factor(d$domain_label, levels = DOM$domain_label); d$id <- factor(seq_len(nrow(d)), levels = rev(seq_len(nrow(d))))
    req(nrow(d) > 0)
    pts <- bind_rows(transmute(d, id, domain_label, pct = pct2, who = nB), transmute(d, id, domain_label, pct = pct, who = nA))
    ggplot(d) +
      geom_vline(xintercept = 50, colour = BRAND$muted, linetype = "dashed") +
      geom_segment(aes(x = pct2, xend = pct, y = id, yend = id), colour = BRAND$muted, linewidth = 1.6) +
      geom_point(data = pts, aes(pct, id, colour = who), size = 4.2) +
      scale_colour_manual(values = setNames(c(BRAND$accent, BRAND$primary), c(nB, nA)), breaks = c(nA, nB), name = NULL) +
      scale_x_continuous(limits = c(0, 100), breaks = c(0, 25, 50, 75, 100)) +
      scale_y_discrete(labels = setNames(d$label, d$id)) +
      facet_grid(domain_label ~ ., scales = "free_y", space = "free_y", switch = "y") +
      labs(x = "Percentile", y = NULL) + theme_lcw(base_size = 13) +
      theme(strip.placement = "outside", strip.text.y.left = element_text(angle = 0, hjust = 0, face = "bold", colour = BRAND$primary),
            panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(), legend.position = "top",
            axis.text.y = element_text(colour = BRAND$dark), panel.spacing.y = unit(0.6, "lines"))
  })
  output$context <- renderTable({
    d <- prof()$d %>% filter(role == "context")
    data.frame(Area = as.character(d$domain_label), Metric = d$label, Value = paste0("(", fmt_val(d$key, d$value), ")"))
  }, striped = TRUE, spacing = "xs")
  output$pitch <- renderPlot({
    p <- sel(); req(nrow(p) == 1)
    pitch_plot(p$avg_x, p$avg_y, ifelse(is.na(p$main_pos), "", p$main_pos))
  })
  output$scatter <- renderPlot({
    p <- sel(); req(nrow(p) == 1, p$pos_group %in% GROUPS)
    gset <- group_set(p$pos_group, input$merge)
    pool <- M %>% filter(pos_group %in% gset, league %in% pool_leagues(), minutes >= input$min_min)
    d <- bind_rows(pool, p %>% filter(!player.id %in% pool$player.id)) %>% distinct(league, player.id, .keep_all = TRUE) %>%
      mutate(who = case_when(player.id == p$player.id & league == p$league ~ "sel",
                             team == p$team & league == p$league ~ "team", TRUE ~ "other"))
    xl <- SC$label[match(input$sx, SC$key)]; yl <- SC$label[match(input$sy, SC$key)]
    d$x <- d[[input$sx]]; d$y <- d[[input$sy]]; d <- d %>% filter(is.finite(x), is.finite(y))
    lab <- d %>% filter(who != "other")
    d$crest <- unname(CREST_PNG[d$team]); d$crest[d$league != "WSL"] <- NA
    has <- !is.na(d$crest)
    g <- ggplot(d, aes(x, y)) +
      geom_vline(xintercept = median(d$x[d$who != "sel"]), colour = BRAND$muted, linetype = "dashed") +
      geom_hline(yintercept = median(d$y[d$who != "sel"]), colour = BRAND$muted, linetype = "dashed") +
      geom_point(data = d %>% filter(who == "other", !has), colour = "#AEB5C8", size = 2.4, alpha = 0.9) +
      geom_point(data = d %>% filter(who == "team", !has), colour = BRAND$primary, size = 3.4) +
      labs(x = xl, y = yl, subtitle = paste(GROUP_NAMES[gset], collapse = " + ")) +
      theme_lcw(base_size = 13) + theme(panel.grid.minor = element_blank(), plot.subtitle = element_text(colour = "#4A5163"))
    if (HAS_CRESTS) {
      dc <- d %>% filter(has)
      g <- g +
        geom_point(data = dc %>% filter(who == "team"), shape = 21, fill = "white", colour = BRAND$primary, stroke = 2, size = 9) +
        geom_point(data = dc %>% filter(who == "sel"), shape = 21, fill = BRAND$accent, colour = BRAND$primary, stroke = 2, size = 16) +
        ggimage::geom_image(data = dc %>% filter(who == "other"), aes(image = crest), size = 0.032, asp = 1.6) +
        ggimage::geom_image(data = dc %>% filter(who == "team"), aes(image = crest), size = 0.04, asp = 1.6) +
        ggimage::geom_image(data = dc %>% filter(who == "sel"), aes(image = crest), size = 0.05, asp = 1.6)
    } else {
      g <- g + geom_point(data = d %>% filter(who == "sel"), colour = BRAND$accent, size = 5, shape = 21, fill = BRAND$accent, stroke = 1.2)
    }
    g <- g + geom_point(data = d %>% filter(who == "sel", !has), colour = BRAND$accent, size = 5, shape = 21, fill = BRAND$accent, stroke = 1.2)
    if (HAS_REPEL) g + ggrepel::geom_text_repel(data = lab, aes(label = player), size = 3.8, colour = BRAND$dark, max.overlaps = 30, box.padding = 0.6, point.padding = 0.5, min.segment.length = 0.2, segment.colour = BRAND$muted, bg.colour = "white", bg.r = 0.15)
    else g + geom_text(data = lab, aes(label = player), size = 3.2, vjust = -0.9, colour = BRAND$dark)
  })
}

shinyApp(ui, server)
