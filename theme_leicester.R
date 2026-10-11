# Branding for the dashboard. Edit this file only; app_players.R reads it at start-up.
# Official Leicester City colours: blue #003090 (PMS 7685), gold #FDBE11 (PMS 7408).
BRAND <- list(primary = "#003090",   # club blue: area bars, our players, headings
              accent  = "#FDBE11",   # club gold: selected player, highlights
              mid     = "#5D79B8",   # lighter blue for headline KPI bars (derived from the club blue)
              dark    = "#1B1F2A", grey = "#8A90A0", light = "#EEF1F7", muted = "#C5CBDA")
APP_TITLE    <- "Leicester City Women: player dashboard"
DEFAULT_TEAM <- "Leicester City WFC"
FOOTER       <- "Created by Kannan Suresh. Illustrative mockup built on StatsBomb open data, not affiliated with Leicester City or the WSL."
# Images (all optional, put them in the assets folder; missing ones fall back to initials or are skipped):
#   assets/wsl.png                     league logo, shown in the title bar
#   assets/crests/<team name>.png      e.g. assets/crests/Leicester City WFC.png
#   assets/players/<player id>.png     ids are listed in assets/photo_checklist.csv
