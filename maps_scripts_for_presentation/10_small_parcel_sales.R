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

# ---- Settings ----
# Size classes for the chart (160 acres = a quarter section; 640 acres = one square
# mile) and the names and colours of the three kinds of transaction.
SMALL_ACRES <- 160  # transactions under this size count as "small"
SIZE_BREAKS <- c(0, 40, 160, 640, Inf)
SIZE_LABELS <- c("Under 40 ac", "40–159 ac", "160–639 ac", "640 ac or more")
TYPE_LABELS <- c(
  sold = "Sold, stayed in the program",
  left = "Left the program",
  trim = "Part of a parcel left the program"
)
TYPE_COLS <- setNames(c("#4d7ea8", "#d1495b", "#edae49"), TYPE_LABELS)

# ---- Owner name matching ----
# Words ignored when comparing owner names: legal forms, filler words and generic
# company words, so only the distinctive part of a name is compared.
STOP <- c("llc", "inc", "ltd", "co", "company", "corp", "corporation", "limited", "lp", "the", "of",
          "and", "trust", "trustee", "trustees", "et", "al", "etal", "jr", "sr", "ii", "iii", "iv",
          "rev", "revocable", "living", "family", "estate",
          # generic company words that would make different companies look alike
          "timber", "timberlands", "michigan", "mi", "land", "lands", "forest", "forests", "holding",
          "holdings", "resources", "properties", "investment", "investments", "management", "cub",
          "natural", "great", "lakes", "realty")
# Breaks a name into lower-case words, drops punctuation and the ignored words, and
# keeps each word once.
name_tokens <- function(s) {
  s <- gsub("[^a-z0-9 ]", " ", tolower(ifelse(is.na(s), "", s)))
  t <- strsplit(s, "\\s+")[[1]]
  unique(t[nzchar(t) & !t %in% STOP])
}
# Two names are the same owner if at least two-thirds of the shorter name's words also
# appear in the other name. Word order doesn't matter.
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
# A standard form of each name (its words in alphabetical order), used to group
# parcels by seller and buyer.
owner_key <- function(s) vapply(s, function(x) paste(sort(name_tokens(x)), collapse = " "), "")

# ---- Read the 2020 and 2026 parcel lists ----
# The 2020 and 2026 Michigan Department of Natural Resources (DNR) Commercial Forest
# lists, one row per parcel, with owner name and enrolled acres. The 2020 list also
# has the owner's last name and owner type, used for the family check above.
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
# A 2020 parcel with no 2026 owner after both steps is treated as having left the program.
st <- p20 |> left_join(a26, by = "parid")
miss <- which(is.na(st$owner_2026))
if (length(miss)) {
  # Overlay the unmatched 2020 parcels on the 2026 parcels and measure each overlap.
  ix <- suppressWarnings(st_intersection(st[miss, "parid"], select(p26, owner_new = owner_2026)))
  ix$a <- as.numeric(st_area(ix))
  # For each 2020 parcel: total area covered by 2026 parcels, and the owner of the
  # 2026 parcel with the largest overlap. Keep it only if at least half is covered.
  best <- st_drop_geometry(ix) |>
    group_by(parid) |>
    summarise(cover = sum(a), owner_new = owner_new[which.max(a)], .groups = "drop")
  area20 <- setNames(as.numeric(st_area(st[miss, ])), st$parid[miss])
  best <- best |> filter(cover / area20[parid] >= 0.5)
  j <- match(st$parid[miss], best$parid)
  st$owner_2026[miss] <- best$owner_new[j]
}

# ---- What happened to each 2020 parcel ----
# Seller class comes from the 2020 owner name. Status of each parcel:
#   left = no 2026 owner (left the program);
#   sold = 2026 owner is a different owner (and not the same family);
#   trim = same owner, but enrolled acres fell by more than 1 acre or 2%, whichever is larger;
#   kept = same owner, same acres.
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

# ---- Group parcels into transactions ----
# Transactions.
# Sales: all parcels going from one seller to one buyer form one transaction.
sold <- st |>
  st_drop_geometry() |>
  filter(status == "sold") |>
  group_by(seller_class, seller_key, buyer_key) |>
  summarise(seller = first(owner_2020), buyer = first(owner_2026), n_parcels = n(),
            acres = sum(acres_2020), parids = paste(parid, collapse = ";"), .groups = "drop") |>
  mutate(type = "sold")

# Exits: for each seller, parcels that left the program are grown by 15 m and merged,
# so parcels that touch (or nearly touch) form one block. Each block gets its own
# number, and each block counts as one transaction.
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

# Partial removals: each trimmed parcel is one transaction, sized by the acres removed.
trim <- st |>
  st_drop_geometry() |>
  filter(status == "trim") |>
  transmute(seller_class, seller_key, seller = owner_2020, buyer = NA_character_, n_parcels = 1L,
            acres = acres_2020 - acres_2026, parids = parid, type = "trim")

# All transactions in one table, with a size class, sorted by seller class and size.
tx <- bind_rows(sold, left, trim) |>
  mutate(size = cut(acres, SIZE_BREAKS, SIZE_LABELS, right = FALSE),
         type_label = factor(TYPE_LABELS[type], levels = TYPE_LABELS),
         seller_class = factor(seller_class, levels = c(CORP, OTHER))) |>
  arrange(seller_class, desc(acres))

# Save the transactions and the parcel-by-parcel status as tables for checking.
write.csv(select(tx, seller_class, seller, buyer, type = type_label, n_parcels, acres, size, parids),
          file.path(TAB_DIR, "small_parcel_transactions.csv"), row.names = FALSE)
write.csv(st_drop_geometry(select(st, parid, owner_2020, owner_2026, acres_2020, acres_2026, seller_class, status)),
          file.path(TAB_DIR, "small_parcel_status_2020_parcels.csv"), row.names = FALSE)

# Per seller class: number of transactions, how many were small, and their acres.
# Used in the chart panel titles and the map legend.
summ <- tx |>
  group_by(seller_class) |>
  summarise(n = n(), n_small = sum(acres < SMALL_ACRES), acres_small = sum(acres[acres < SMALL_ACRES]),
            acres = sum(acres), .groups = "drop")
print(summ)

# ---- 10a: transactions by size ----------------------------------------------

# Number of transactions per size class and type (for the stacked bars), and the total
# per size class (printed above each bar). Empty combinations are kept as zero.
counts <- tx |> count(seller_class, size, type_label, .drop = FALSE)
totals <- tx |> count(seller_class, size, .drop = FALSE)
# Panel titles give the seller class with its number of transactions and acres.
facet_lab <- setNames(
  paste0(summ$seller_class, " as sellers\n", summ$n, " transactions, ", fmt_n(summ$acres), " acres"),
  summ$seller_class
)

# Two panels (corporate and other private sellers): stacked bars of transaction counts
# by size class, coloured by what happened to the land.
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

# Parcels that took part in a small transaction, each tagged with its transaction type.
# The parcel ID lists stored per transaction are split back into one row per parcel.
small_ids <- unlist(strsplit(tx$parids[tx$acres < SMALL_ACRES], ";"))
small <- st |>
  filter(parid %in% small_ids) |>
  left_join(st_drop_geometry(select(tx, parids, type_label)) |>
              tidyr::separate_rows(parids, sep = ";") |>
              rename(parid = parids), by = "parid")
# Background: county outlines and all 2020 enrolled land merged into one grey shape.
counties <- read_mainland()
all_cfp <- st |> summarise()
bb <- pad_bbox(st_bbox(p20), 0.04)

# Map layers: pale county land, grey enrolled land, small-transaction parcels coloured
# by seller class, county borders, then a dot on each small parcel. The legend labels
# add each seller class's count and acres of small transactions.
m <- ggplot() +
  geom_sf(data = counties, fill = "grey97", colour = NA) +
  geom_sf(data = all_cfp, fill = "grey85", colour = NA) +
  geom_sf(data = small, aes(fill = seller_class), colour = "grey15", linewidth = 0.15) +
  geom_sf(data = counties, fill = NA, colour = "grey35", linewidth = 0.4) +
  # Parcels are too small to see at county scale, so each also gets a point.
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

# Side notes explaining the map, wrapped to about 60 characters per line.
notes <- paste(strwrap(paste0(
  "Coloured parcels (marked with a dot so they show at this scale) changed hands or left the ",
  "program in a transaction under ", SMALL_ACRES,
  " acres, 2020 → 2026. Grey: other 2020 Commercial Forest land. ",
  "Parcels that lost only part of their acres are shown whole."
), 60), collapse = "\n")

# Map without its legend on the left; title, legend and notes in the side column.
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
