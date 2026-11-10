# Baraga County Commercial Forest, 2020 and 2026, with canopy change 2020 to 2025.
# Same owner-matching rules as Houghton and Keweenaw (scripts/lib_owner_2026.R).
# Reads the statewide layers in data/cfp_data/ and the existing canopy rasters.
# Writes only under baraga_county/. Run from the project root:
#   Rscript baraga_county/analyze_baraga_cfp.R

Sys.setenv(PROJ_NETWORK = "OFF")

suppressPackageStartupMessages({
  library(sf)
  library(terra)
  library(dplyr)
  library(ggplot2)
  library(tigris)
})
terraOptions(progress = 0)
options(tigris_use_cache = TRUE)

root <- if (file.exists("data/cfp_data/cfp_2020_mi.shp")) {
  "."
} else {
  stop("Run from the kee_trees project root.")
}
source(file.path(root, "scripts/lib_owner_2026.R"))

out_dir <- file.path(root, "baraga_county")
fig_dir <- file.path(out_dir, "figures")
tab_dir <- file.path(out_dir, "tables")
dir.create(fig_dir, showWarnings = FALSE)
dir.create(tab_dir, showWarnings = FALSE)

MIN_ACRES <- 20

message("Reading Baraga parcels...")
sf_use_s2(FALSE)
# The Houghton-Keweenaw canopy files are masked at the county line.
# These two are the same national product, clipped to Baraga.
tcc20 <- subst(rast(file.path(out_dir, "tcc/Baraga_TCC_2020.tif")), c(254, 255), NA)
tcc25 <- subst(rast(file.path(out_dir, "tcc/Baraga_TCC_2025.tif")), c(254, 255), NA)
# Repair boundaries in the canopy map's metre projection. A zero-width buffer
# drops the slivers that make later overlays fail.
clean_poly <- function(x) {
  x |>
    st_transform(crs(tcc20)) |>
    st_make_valid() |>
    st_buffer(0) |>
    st_make_valid()
}
p20 <- st_read(file.path(root, "data/cfp_data/cfp_2020_mi.shp"), quiet = TRUE) |>
  filter(County_Nam == "Baraga") |>
  clean_poly()
p26 <- st_read(file.path(root, "data/cfp_data/cfp_2026_mi.shp"), quiet = TRUE) |>
  filter(CountyName == "Baraga") |>
  clean_poly()

p20$parid <- as.character(p20$parid)
p26$parid <- as.character(p26$parid)
p26 <- p26[!duplicated(p26$parid), ]

p20$acres <- as.numeric(p20$acres)
p26$Acres <- as.numeric(p26$Acres)
p20$name <- trimws(p20$search_nam)
p26$name <- trimws(p26$FullLegalN)
p20$type <- trimws(p20$owner_type)
p26$type <- trimws(p26$OwnerTypeT)

# ---- Owner tables, one row per legal name, no grouping ----
owner_table <- function(name, type, acres, year_total) {
  tibble(owner = name, state_type = type, acres = acres) |>
    group_by(owner, state_type) |>
    summarise(parcels = n(), acres = sum(acres), .groups = "drop") |>
    mutate(
      share_pct = round(100 * acres / year_total, 4),
      acres = round(acres, 2),
      emily_updates = "",
      `carbon (y/n)` = "",
      `easement (y/n)` = ""
    ) |>
    arrange(desc(acres))
}

owners_2020 <- owner_table(p20$name, p20$type, p20$acres, sum(p20$acres))
owners_2026 <- owner_table(p26$name, p26$type, p26$Acres, sum(p26$Acres))
write.csv(owners_2020, file.path(tab_dir, "owners_2020.csv"), row.names = FALSE)
write.csv(owners_2026, file.path(tab_dir, "owners_2026.csv"), row.names = FALSE)

# ---- Follow each 2020 parcel to 2026 ----
message("Matching 2020 parcels to 2026 owners...")
p20_join <- p20 |>
  transmute(parid, parcel_acres = acres, name_2020 = name, owner_type = type, lastname)
p26_join <- p26 |>
  transmute(parid, FullLegalN = name, OwnerTypeT = type)

own <- owner_2026_lookup(p20_join, p26_join)
# The shared share function unions every 2026 parcel first, and that union
# fails on a few Baraga boundaries. Intersect each 2020 parcel with the 2026
# parcels it touches instead.
share_one <- function(p20, p26) {
  p20 <- p20[!duplicated(p20$parid), ]
  hits <- st_intersects(p20, p26)
  area20 <- as.numeric(st_area(p20))
  share <- numeric(nrow(p20))
  for (i in which(lengths(hits) > 0)) {
    piece <- suppressWarnings(st_intersection(p20[i, ], p26[hits[[i]], ]))
    if (!nrow(piece)) next
    share[i] <- min(1, sum(as.numeric(st_area(piece))) / area20[i])
  }
  share[share >= 0.98] <- 1
  data.frame(parid = p20$parid, share_in_cfp_2026 = share)
}
share <- share_one(p20_join, p26_join)

firm_of <- function(name) {
  n <- toupper(ifelse(is.na(name), "", name))
  case_when(
    grepl("WEYERHAEUSER|GREAT LAKES FORESTS 2", n) ~ "Weyerhaeuser and Great Lakes Forests 2",
    grepl("\\bLYME\\b", n) ~ "Lyme",
    grepl("TRG", n) ~ "TRG",
    grepl("VERDANT", n) ~ "Verdant",
    grepl("MWF|MOLPUS", n) ~ "Molpus (MWF)",
    grepl("NATURE CONSERVANCY", n) ~ "The Nature Conservancy",
    TRUE ~ "All other owners"
  )
}

parcels <- p20_join |>
  st_drop_geometry() |>
  left_join(own, by = "parid") |>
  left_join(share, by = "parid") |>
  mutate(
    same_owner = same_owner_2026(name_2020, name_2026, owner_type, lastname),
    firm_2020 = firm_of(name_2020),
    firm_2026 = if_else(is.na(name_2026), "Left the program", firm_of(name_2026)),
    went_to = case_when(
      firm_2020 == "Weyerhaeuser and Great Lakes Forests 2" & firm_2026 == "Lyme" ~
        "Weyerhaeuser and Great Lakes Forests to Lyme",
      firm_2020 == "Weyerhaeuser and Great Lakes Forests 2" ~
        "Weyerhaeuser and Great Lakes Forests to other or left",
      firm_2020 == "TRG" & firm_2026 == "Verdant" ~ "TRG to Verdant",
      firm_2020 == "TRG" ~ "TRG to other or left",
      firm_2020 == "Molpus (MWF)" & firm_2026 == "Molpus (MWF)" ~ "Molpus kept",
      firm_2020 == "Molpus (MWF)" ~ "Molpus sold or left",
      firm_2020 == "The Nature Conservancy" & same_owner ~ "The Nature Conservancy kept",
      same_owner ~ "Other owners kept",
      TRUE ~ "Other owners sold or left"
    )
  )

# ---- Canopy, parcels of 20 acres and up ----
message("Averaging canopy on each 2020 parcel...")
big <- p20[p20$acres >= MIN_ACRES, ]
z20 <- terra::extract(tcc20, vect(big), fun = mean, na.rm = TRUE, ID = FALSE)
z25 <- terra::extract(tcc25, vect(big), fun = mean, na.rm = TRUE, ID = FALSE)
n_pix <- terra::extract(tcc20, vect(big), fun = function(x) sum(!is.na(x)), ID = FALSE)
canopy <- tibble(
  parid = big$parid,
  mean_tcc_2020 = as.numeric(z20[[1]]),
  mean_tcc_2025 = as.numeric(z25[[1]]),
  n_tcc_pixels = as.integer(n_pix[[1]])
)

parcels <- parcels |>
  left_join(canopy, by = "parid") |>
  mutate(
    delta_pp = mean_tcc_2025 - mean_tcc_2020,
    heavily_cut = !is.na(delta_pp) & delta_pp <= -20 & parcel_acres >= MIN_ACRES
  )

write.csv(
  parcels |> select(-lastname),
  file.path(tab_dir, "parcel_2020_to_2026.csv"),
  row.names = FALSE
)

by_dest <- parcels |>
  group_by(went_to) |>
  summarise(
    parcels = n(),
    acres_2020 = sum(parcel_acres),
    acres_heavily_cut = sum(parcel_acres[heavily_cut], na.rm = TRUE),
    acres_with_canopy = sum(parcel_acres[!is.na(delta_pp) & parcel_acres >= MIN_ACRES]),
    .groups = "drop"
  ) |>
  mutate(
    share_of_2020 = round(100 * acres_2020 / sum(acres_2020), 1),
    heavily_cut_pct = round(100 * acres_heavily_cut / acres_with_canopy, 1)
  ) |>
  arrange(desc(acres_2020))
write.csv(by_dest, file.path(tab_dir, "acres_by_destination.csv"), row.names = FALSE)

firm_year <- bind_rows(
  parcels |> transmute(year = "2020", firm = firm_2020, acres = parcel_acres),
  tibble(year = "2026", firm = firm_of(p26$name), acres = p26$Acres)
) |>
  group_by(year, firm) |>
  summarise(acres = sum(acres), .groups = "drop")
write.csv(firm_year, file.path(tab_dir, "acres_by_firm_year.csv"), row.names = FALSE)

message("2020 acres ", round(sum(p20$acres), 1),
        "  2026 acres ", round(sum(p26$Acres), 1))
message("Matched by id ", sum(own$matched_by == "parcel id", na.rm = TRUE),
        "  by overlap ", sum(own$matched_by == "overlap", na.rm = TRUE),
        "  left ", sum(is.na(own$name_2026)))
print(by_dest)

# ---- Figures ----
font <- "sans"

theme_bar <- function() {
  theme_minimal(base_size = 12, base_family = font) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 10, colour = "grey30"),
      legend.position = "top"
    )
}

firm_levels <- c(
  "Weyerhaeuser and Great Lakes Forests 2",
  "Lyme",
  "TRG",
  "Verdant",
  "Molpus (MWF)",
  "The Nature Conservancy",
  "All other owners"
)
firm_levels <- firm_levels[firm_levels %in% firm_year$firm]

p_firms <- firm_year |>
  mutate(
    firm = factor(firm, levels = rev(firm_levels)),
    year = factor(year, levels = c("2020", "2026"))
  ) |>
  ggplot(aes(acres / 1000, firm, fill = year)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.65) +
  scale_fill_manual(values = c("2020" = "#8aa0b4", "2026" = "#1c3a2e"), name = NULL) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.05)), labels = scales::comma) +
  labs(
    title = "Who holds Baraga County Commercial Forest land",
    subtitle = "Listed acres. Lyme, Verdant, and Weyerhaeuser each combine the legal names on the state list.",
    x = "Thousand acres",
    y = NULL
  ) +
  theme_bar()
ggsave(file.path(fig_dir, "01_who_owns.png"), p_firms, width = 9.2, height = 5.4, dpi = 160, bg = "white")

dest_levels <- by_dest |> arrange(acres_2020) |> pull(went_to)
p_dest <- by_dest |>
  mutate(went_to = factor(went_to, levels = dest_levels)) |>
  ggplot(aes(acres_2020 / 1000, went_to)) +
  geom_col(fill = "#1c3a2e", width = 0.7) +
  geom_text(aes(label = paste0(share_of_2020, "%")), hjust = -0.15, size = 3.2, family = font) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15)), labels = scales::comma) +
  labs(
    title = "Where the 2020 Commercial Forest acres went",
    subtitle = "Each 2020 parcel followed to its 2026 owner. Share of the 2020 acres.",
    x = "Thousand acres in 2020",
    y = NULL
  ) +
  theme_bar()
ggsave(file.path(fig_dir, "02_where_2020_acres_went.png"), p_dest, width = 9.2, height = 5.2, dpi = 160, bg = "white")

cut_tab <- by_dest |>
  filter(acres_heavily_cut > 0) |>
  arrange(acres_2020) |>
  mutate(went_to = factor(went_to, levels = went_to))
p_cut <- ggplot(cut_tab, aes(heavily_cut_pct, went_to)) +
  geom_col(fill = "#8c4a32", width = 0.7) +
  geom_text(aes(label = paste0(format(round(acres_heavily_cut), big.mark = ","), " ac")), hjust = -0.12, size = 3.1, family = font) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.22)), limits = c(0, NA)) +
  labs(
    title = "Parcels whose average canopy fell more than 20 points",
    subtitle = "2020 to 2025, parcels of 20 acres or more. Groups with none are left off.",
    x = "Percent of acres",
    y = NULL
  ) +
  theme_bar()
ggsave(file.path(fig_dir, "03_heavily_cut.png"), p_cut, width = 9.2, height = 4.2, dpi = 160, bg = "white")

message("Drawing the map...")
county <- tigris::counties(state = "MI", year = 2023, cb = TRUE, progress_bar = FALSE) |>
  filter(NAME == "Baraga") |>
  st_transform(crs(tcc20))
map_parcels <- p20 |>
  left_join(parcels |> select(parid, went_to), by = "parid")
pal <- c(
  "Weyerhaeuser and Great Lakes Forests to Lyme" = "#1c3a2e",
  "Weyerhaeuser and Great Lakes Forests to other or left" = "#6d9484",
  "TRG to Verdant" = "#3d6756",
  "TRG to other or left" = "#a9bfb2",
  "Molpus kept" = "#8c4a32",
  "Molpus sold or left" = "#c4a484",
  "The Nature Conservancy kept" = "#2f5d8c",
  "Other owners kept" = "#d5ddd8",
  "Other owners sold or left" = "#e4d2a8"
)
bb <- st_bbox(county)
p_map <- ggplot() +
  geom_sf(data = county, fill = "#f3f1ec", colour = "#1c1c1c", linewidth = 0.3) +
  geom_sf(data = map_parcels, aes(fill = went_to), colour = NA) +
  scale_fill_manual(values = pal, name = NULL, drop = FALSE) +
  coord_sf(
    xlim = c(bb["xmin"] - 400, bb["xmax"] + 400),
    ylim = c(bb["ymin"] - 400, bb["ymax"] + 400),
    expand = FALSE
  ) +
  labs(
    title = "Baraga County Commercial Forest, 2020 parcels",
    subtitle = "Colour is where that 2020 parcel's owner stood in 2026."
  ) +
  theme_void(base_family = font, base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0, margin = margin(b = 2)),
    plot.subtitle = element_text(colour = "grey30", hjust = 0, margin = margin(b = 6)),
    legend.position = "right",
    legend.text = element_text(size = 8),
    plot.margin = margin(8, 8, 8, 8)
  )
ggsave(file.path(fig_dir, "04_map_destination.png"), p_map, width = 9.2, height = 7.2, dpi = 160, bg = "white")

message("Wrote figures to ", fig_dir)
