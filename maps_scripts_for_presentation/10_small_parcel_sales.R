# Small parcel sales (first look): how big were the transactions in which Commercial
# Forest land changed hands or left the program, 2020 -> 2026, for corporate and other
# private sellers?
#
# A "transaction" is
#   - all parcels one 2020 owner passed to one 2026 owner (sold, still enrolled),
#   - a connected block of one 2020 owner's parcels that left the program, or
#   - part of a parcel removed from the program (same owner, fewer acres in 2026).
# Owners are matched on name words, so "KAUMA PAUL T." and "Paul T. Kauma" count as
# the same owner, and an individual moving land into a trust or LLC carrying their
# surname counts as keeping it. TRG -> Verdant counts as a sale.
#
# Outputs: figures/10a_chart_small_parcel_sales.png
#          figures/10b_map_small_parcel_sales.png
#          tables/small_parcel_transactions.csv
#          tables/small_parcel_status_2020_parcels.csv

if (!exists("SETUP_LOADED")) source(if (file.exists("00_setup.R")) "00_setup.R" else "maps_scripts_for_presentation/00_setup.R")

SMALL_ACRES <- 160  # transactions under this size count as "small"
SIZE_BREAKS <- c(0, 40, 160, 640, Inf)
SIZE_LABELS <- c("Under 40 ac", "40–159 ac", "160–639 ac", "640 ac or more")
TYPE_LABELS <- c(
  sold = "Sold, stayed in the program",
  left = "Left the program",
  trim = "Part of a parcel left the program"
)
TYPE_COLS <- setNames(c("#4d7ea8", "#d1495b", "#edae49"), TYPE_LABELS)

STOP <- c("llc", "inc", "ltd", "co", "company", "corp", "corporation", "limited", "lp", "the", "of",
          "and", "trust", "trustee", "trustees", "et", "al", "etal", "jr", "sr", "ii", "iii", "iv",
          "rev", "revocable", "living", "family", "estate",
          # generic company words that would make different companies look alike
          "timber", "timberlands", "michigan", "mi", "land", "lands", "forest", "forests", "holding",
          "holdings", "resources", "properties", "investment", "investments", "management", "cub",
          "natural", "great", "lakes", "realty")
name_tokens <- function(s) {
  s <- gsub("[^a-z0-9 ]", " ", tolower(ifelse(is.na(s), "", s)))
  t <- strsplit(s, "\\s+")[[1]]
  unique(t[nzchar(t) & !t %in% STOP])
}
same_owner <- function(a, b) {
  ta <- name_tokens(a)
  tb <- name_tokens(b)
  if (!length(ta) || !length(tb)) return(FALSE)
  length(intersect(ta, tb)) / min(length(ta), length(tb)) >= 0.66
}
# Individuals passing land to a family trust or LLC with their surname count as keeping it.
same_family <- function(type, last, b) {
  isTRUE(type == "Private Individual(s)") && !is.na(last) && nchar(trimws(last)) > 2 &&
    tolower(trimws(last)) %in% name_tokens(b)
}
owner_key <- function(s) vapply(s, function(x) paste(sort(name_tokens(x)), collapse = " "), "")

p20 <- st_read(input("cfp/cfp_hk_2020.shp"), quiet = TRUE) |>
  st_make_valid() |>
  st_transform(GRID_CRS) |>
  transmute(parid = as.character(parid), owner_2020 = trimws(search_nam), acres_2020 = as.numeric(acres),
            lastname, owner_type) |>
  distinct(parid, .keep_all = TRUE)
p26 <- st_read(input("cfp/cfp_hk_2026.shp"), quiet = TRUE) |>
  st_make_valid() |>
  st_transform(GRID_CRS) |>
  transmute(parid = as.character(parid), owner_2026 = trimws(FullLegalN), acres_2026 = as.numeric(Acres))
a26 <- p26 |> st_drop_geometry() |> distinct(parid, .keep_all = TRUE)

# 2026 owner: by parcel number, or, for renumbered parcels, the 2026 parcel covering
# most of the 2020 parcel (at least half of it).
st <- p20 |> left_join(a26, by = "parid")
miss <- which(is.na(st$owner_2026))
if (length(miss)) {
  ix <- suppressWarnings(st_intersection(st[miss, "parid"], select(p26, owner_new = owner_2026)))
  ix$a <- as.numeric(st_area(ix))
  best <- st_drop_geometry(ix) |>
    group_by(parid) |>
    summarise(cover = sum(a), owner_new = owner_new[which.max(a)], .groups = "drop")
  area20 <- setNames(as.numeric(st_area(st[miss, ])), st$parid[miss])
  best <- best |> filter(cover / area20[parid] >= 0.5)
  j <- match(st$parid[miss], best$parid)
  st$owner_2026[miss] <- best$owner_new[j]
}

st <- st |>
  mutate(
    seller_class = if_else(!is.na(firm_from_name(owner_2020)), CORP, OTHER),
    same = mapply(same_owner, owner_2020, owner_2026) | mapply(same_family, owner_type, lastname, owner_2026),
    status = case_when(
      is.na(owner_2026) ~ "left",
      !same ~ "sold",
      !is.na(acres_2026) & acres_2026 < acres_2020 - pmax(1, 0.02 * acres_2020) ~ "trim",
      TRUE ~ "kept"
    ),
    seller_key = owner_key(owner_2020),
    buyer_key = if_else(status == "sold", owner_key(owner_2026), NA_character_)
  )

# Transactions.
sold <- st |>
  st_drop_geometry() |>
  filter(status == "sold") |>
  group_by(seller_class, seller_key, buyer_key) |>
  summarise(seller = first(owner_2020), buyer = first(owner_2026), n_parcels = n(),
            acres = sum(acres_2020), parids = paste(parid, collapse = ";"), .groups = "drop") |>
  mutate(type = "sold")

left_blocks <- st |> filter(status == "left")
left_blocks$block <- NA_integer_
next_id <- 0L
for (k in unique(left_blocks$seller_key)) {
  i <- which(left_blocks$seller_key == k)
  blobs <- st_cast(st_union(st_buffer(st_geometry(left_blocks)[i], 15)), "POLYGON")
  left_blocks$block[i] <- next_id + vapply(st_intersects(st_geometry(left_blocks)[i], blobs), `[`, 1L, 1)
  next_id <- next_id + length(blobs)
}
left <- left_blocks |>
  st_drop_geometry() |>
  group_by(seller_class, seller_key, block) |>
  summarise(seller = first(owner_2020), buyer = NA_character_, n_parcels = n(),
            acres = sum(acres_2020), parids = paste(parid, collapse = ";"), .groups = "drop") |>
  mutate(type = "left") |>
  select(-block)

trim <- st |>
  st_drop_geometry() |>
  filter(status == "trim") |>
  transmute(seller_class, seller_key, seller = owner_2020, buyer = NA_character_, n_parcels = 1L,
            acres = acres_2020 - acres_2026, parids = parid, type = "trim")

tx <- bind_rows(sold, left, trim) |>
  mutate(size = cut(acres, SIZE_BREAKS, SIZE_LABELS, right = FALSE),
         type_label = factor(TYPE_LABELS[type], levels = TYPE_LABELS),
         seller_class = factor(seller_class, levels = c(CORP, OTHER))) |>
  arrange(seller_class, desc(acres))

write.csv(select(tx, seller_class, seller, buyer, type = type_label, n_parcels, acres, size, parids),
          file.path(TAB_DIR, "small_parcel_transactions.csv"), row.names = FALSE)
write.csv(st_drop_geometry(select(st, parid, owner_2020, owner_2026, acres_2020, acres_2026, seller_class, status)),
          file.path(TAB_DIR, "small_parcel_status_2020_parcels.csv"), row.names = FALSE)

summ <- tx |>
  group_by(seller_class) |>
  summarise(n = n(), n_small = sum(acres < SMALL_ACRES), acres_small = sum(acres[acres < SMALL_ACRES]),
            acres = sum(acres), .groups = "drop")
print(summ)

# ---- 10a: transactions by size ----------------------------------------------

counts <- tx |> count(seller_class, size, type_label, .drop = FALSE)
totals <- tx |> count(seller_class, size, .drop = FALSE)
facet_lab <- setNames(
  paste0(summ$seller_class, " as sellers\n", summ$n, " transactions, ", fmt_n(summ$acres), " acres"),
  summ$seller_class
)

p <- ggplot(counts, aes(size, n, fill = type_label)) +
  geom_col(width = 0.7) +
  geom_text(data = totals, aes(size, n, label = n), inherit.aes = FALSE, vjust = -0.4,
            size = BASE_SIZE / 3.2, fontface = "bold") +
  facet_wrap(~seller_class, labeller = as_labeller(facet_lab)) +
  scale_fill_manual(values = TYPE_COLS, name = NULL) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(
    title = "Small parcel sales: how big was each transaction?",
    subtitle = "Commercial Forest land that changed hands or left the program, 2020 → 2026, by size of transaction",
    x = "Size of transaction", y = "Number of transactions",
    caption = paste0(
      "First look. A transaction = all parcels one 2020 owner passed to one 2026 owner, a connected block that left the program, ",
      "or part of a parcel removed from the program.\n",
      "Owners matched on name words; TRG → Verdant counts as one sale. Land that left the program may have been sold or simply withdrawn; the state lists don't say.\n",
      "Source: Michigan DNR Commercial Forest lists, 2020 and 2026. ", CORP_NOTE
    )
  ) +
  theme_chart() +
  theme(panel.grid.major.x = element_blank(), strip.text = element_text(size = BASE_SIZE - 1),
        panel.spacing = unit(0.5, "in"))

save_chart(p, "10a_chart_small_parcel_sales.png")

# ---- 10b: map of the small transactions --------------------------------------

small_ids <- unlist(strsplit(tx$parids[tx$acres < SMALL_ACRES], ";"))
small <- st |>
  filter(parid %in% small_ids) |>
  left_join(st_drop_geometry(select(tx, parids, type_label)) |>
              tidyr::separate_rows(parids, sep = ";") |>
              rename(parid = parids), by = "parid")
counties <- read_mainland()
all_cfp <- st |> summarise()
bb <- pad_bbox(st_bbox(p20), 0.04)

m <- ggplot() +
  geom_sf(data = counties, fill = "grey97", colour = NA) +
  geom_sf(data = all_cfp, fill = "grey85", colour = NA) +
  geom_sf(data = small, aes(fill = seller_class), colour = "grey15", linewidth = 0.15) +
  geom_sf(data = counties, fill = NA, colour = "grey35", linewidth = 0.4) +
  # parcels are too small to see at county scale, so each also gets a point
  geom_sf(data = suppressWarnings(st_point_on_surface(small)), aes(fill = seller_class),
          shape = 21, size = 2.6, colour = "white", stroke = 0.35, show.legend = FALSE) +
  scale_fill_manual(values = CLASS_COLS[c(CORP, OTHER)], name = "Seller", drop = FALSE,
                    labels = function(x) {
                      s <- summ[match(x, summ$seller_class), ]
                      paste0(x, ": ", s$n_small, " small transactions,\n", fmt_n(s$acres_small), " acres")
                    }) +
  scale_bar(bb$x, bb$y, pos = "bottomright") +
  north_arrow(bb$x, bb$y, pos = "topleft") +
  coord_sf(xlim = bb$x, ylim = bb$y, expand = FALSE, datum = NA) +
  theme_map() +
  theme(legend.position = "right", legend.title = element_text(size = BASE_SIZE + 1, face = "bold"),
        legend.text = element_text(size = BASE_SIZE - 1), legend.key.size = unit(0.4, "in"),
        legend.key.spacing.y = unit(0.12, "in"))

notes <- paste(strwrap(paste0(
  "Coloured parcels (marked with a dot so they show at this scale) changed hands or left the ",
  "program in a transaction under ", SMALL_ACRES,
  " acres, 2020 → 2026. Grey: other 2020 Commercial Forest land. ",
  "Parcels that lost only part of their acres are shown whole."
), 60), collapse = "\n")

save_layout(
  "10b_map_small_parcel_sales.png",
  panels = list(m + theme(legend.position = "none", plot.margin = margin(4, 4, 4, 40))),
  side = list(get_legend(m), text_block(notes, size = BASE_SIZE - 2)),
  side_heights = c(0.9, 1.1),
  side_width = 0.6,
  title_pos = "side",
  title = paste0("Small parcel sales: transactions under ", SMALL_ACRES, " acres"),
  subtitle = "Where small pieces of Commercial Forest land changed hands or left the program, 2020 → 2026",
  caption = "First look. Source: Michigan DNR Commercial Forest lists, 2020 and 2026. Owners matched on name words."
)
