# Who owns the Commercial Forest land? Share of 2020 enrolled acres held by the
# largest owners and by groups of smaller owners, coloured corporate vs. other private,
# with a few bullets on the largest shares.
#
# Output: figures/12_chart_who_owns_cfp.png

if (!exists("SETUP_LOADED")) source(if (file.exists("00_setup.R")) "00_setup.R" else "maps_scripts_for_presentation/00_setup.R")

# ---- Data ----
# The 2020 Michigan Department of Natural Resources (DNR) Commercial Forest Program
# (CFP) list: owner name, owner type and acres per parcel, plus the total enrolled acres.
p20 <- st_read(input("cfp/cfp_hk_2020.shp"), quiet = TRUE) |>
  st_drop_geometry() |>
  transmute(owner = trimws(search_nam), type = owner_type, acres = as.numeric(acres))
total <- sum(p20$acres)

# Owner groups: the big owners by name, everyone else by the state's owner type.
# Corporate firms are matched by name because the state's types are inconsistent
# (Lyme is listed as "Private Individual(s)", Molpus Vulcan as "Other Business").
owners <- p20 |>
  mutate(
    firm = firm_from_name(owner),
    group = case_when(
      firm == "TRG" ~ "TRG (Threshold Timber funds)",
      firm %in% c("Molpus", "Lyme") ~ "Molpus and Lyme",
      grepl("LAKE SUPERIOR TIMBERLANDS", owner) ~ "Lake Superior Timberlands",
      grepl("KEWEENAW LAND ASSOCIATION", owner) ~ "Keweenaw Land Association",
      type == "Forest Industry" ~ "Other timber companies",
      type == "Other Business" ~ "Other businesses and family companies",
      type == "Private Individual(s)" ~ "Individuals and families",
      TRUE ~ "Nonprofits, clubs and a township"
    ),
    class = if_else(!is.na(firm), CORP, OTHER)
  )

# Acres, number of owners and share of all enrolled land per group. Groups are
# ordered by share so the largest bar ends up at the top of the chart.
by_group <- owners |>
  group_by(group, class) |>
  summarise(acres = sum(acres), n_owners = n_distinct(owner), .groups = "drop") |>
  mutate(pct = 100 * acres / total, class = factor(class, levels = c(CORP, OTHER))) |>
  arrange(pct) |>
  mutate(group = factor(group, levels = group))

# Numbers for the bullets.
# top4: share held by the four largest single owners. other_timber: acres held by
# timber companies within other private land. indiv: acres per individual or family.
# pct_of() looks up a group's rounded share.
per_owner <- owners |> group_by(owner) |> summarise(acres = sum(acres), .groups = "drop") |> arrange(desc(acres))
top4 <- 100 * sum(per_owner$acres[1:4]) / total
other_acres <- sum(by_group$acres[by_group$class == OTHER])
other_timber <- sum(by_group$acres[by_group$group %in% c("Lake Superior Timberlands", "Keweenaw Land Association",
                                                          "Other timber companies")])
indiv <- owners |> filter(group == "Individuals and families") |> group_by(owner) |> summarise(a = sum(acres))
pct_of <- function(g) round(by_group$pct[by_group$group == g])

# ---- Chart ----
# Horizontal bars coloured corporate vs. other private, each labelled with its share
# ("<1%" for tiny groups) and acres. The x-axis has no labels and extends to the right
# so the bar labels fit.
p <- ggplot(by_group, aes(pct, group, fill = class)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = paste0(if_else(pct < 1, "<1", as.character(round(pct))), "%  (", fmt_n(acres), " ac)")),
            hjust = -0.06,
            size = BASE_SIZE / 3.4, family = FONT_FAMILY, colour = "grey15") +
  scale_fill_manual(values = CLASS_COLS[c(CORP, OTHER)], name = NULL) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.6)), labels = NULL) +
  labs(x = NULL, y = NULL) +
  theme_chart() +
  theme(panel.grid = element_blank(), axis.text.y = element_text(size = BASE_SIZE - 1, colour = "grey15"),
        legend.position = "top", legend.text = element_text(size = BASE_SIZE - 2),
        plot.margin = margin(10, 10, 10, 10))

# ---- Side bullets ----
# Each bullet is wrapped to 40 characters, with continuation lines indented under the text.
bullet <- function(x) paste0("•  ", paste(strwrap(x, 40, exdent = 3), collapse = "\n"))
bullets <- paste(
  bullet(paste0("Four owners hold ", round(top4), "% of the enrolled land: TRG's two funds, ",
                "Lake Superior Timberlands and Keweenaw Land Association.")),
  bullet(paste0("TRG alone held ", pct_of("TRG (Threshold Timber funds)"), "%. It sold to Verdant in 2022.")),
  bullet(paste0("Most \"other private\" land is also timber companies (", round(100 * other_timber / other_acres),
                "%), led by Lake Superior Timberlands.")),
  bullet(paste0(nrow(indiv), " individuals and families hold ", pct_of("Individuals and families"),
                "%; half own ", fmt_n(median(indiv$a)), " acres or less.")),
  sep = "\n\n"
)

# Bar chart on the left, bullets in the right-hand column, title across the top.
save_layout(
  "12_chart_who_owns_cfp.png",
  panels = list(p),
  side = list(text_block(bullets, size = BASE_SIZE - 1)),
  side_width = 0.36,
  title = "Who Owns the Commercial Forest Land?",
  subtitle = paste0("Houghton and Keweenaw counties, share of the ", fmt_n(total), " acres enrolled in 2020"),
  caption = paste0(
    "Source: Michigan DNR Commercial Forest list, 2020. Groups below the top owners follow the state's owner types; ",
    "corporate owners are matched by name.\n", CORP_NOTE
  )
)
