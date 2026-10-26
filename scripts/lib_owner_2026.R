# Shared rules for following a 2020 Commercial Forest parcel to its 2026 owner.
# Sourced by scripts/10 and scripts/11.
#
# 2026 owner: the 2026 parcel with the same parcel ID; if the ID is not on the 2026
# list (the state renumbers some parcels), the 2026 owner covering the most of the
# 2020 parcel, provided 2026 parcels cover at least half of it. Otherwise the parcel
# left the program.
#
# Same owner: names compared word by word, ignoring legal and generic words (LLC,
# trust, timber, Michigan, ...); same owner if they share at least two-thirds of the
# remaining words of the shorter name. For individuals, a 2020 last name appearing
# in the 2026 name also counts (land moved to a family trust, spouse or children).

OWNER_STOP <- c(
  "llc", "inc", "ltd", "co", "company", "corp", "corporation", "limited", "lp", "the", "of",
  "and", "trust", "trustee", "trustees", "et", "al", "etal", "jr", "sr", "ii", "iii", "iv",
  "rev", "revocable", "living", "family", "estate",
  # generic company words that would make different companies look alike
  "timber", "timberlands", "michigan", "mi", "land", "lands", "forest", "forests", "holding",
  "holdings", "resources", "properties", "investment", "investments", "management", "cub",
  "natural", "great", "lakes", "realty"
)

name_tokens <- function(s) {
  s <- gsub("[^a-z0-9 ]", " ", tolower(ifelse(is.na(s), "", s)))
  t <- strsplit(s, "\\s+")[[1]]
  unique(t[nzchar(t) & !t %in% OWNER_STOP])
}

same_owner_words <- function(a, b) {
  ta <- name_tokens(a)
  tb <- name_tokens(b)
  if (!length(ta) || !length(tb)) return(FALSE)
  length(intersect(ta, tb)) / min(length(ta), length(tb)) >= 0.66
}

same_family_surname <- function(owner_type, lastname, b) {
  isTRUE(owner_type == "Private Individual(s)") && !is.na(lastname) &&
    nchar(trimws(lastname)) > 2 && tolower(trimws(lastname)) %in% name_tokens(b)
}

# TRUE when the 2026 name is the same owner (or family) as the 2020 owner.
same_owner_2026 <- function(name_2020, name_2026, owner_type, lastname) {
  !is.na(name_2026) &
    (mapply(same_owner_words, name_2020, name_2026) |
       mapply(same_family_surname, owner_type, lastname, name_2026))
}

# One row per 2020 parcel: parid, name_2026 and owner_type_2026 (NA = left the program),
# matched_by. p20 and p26 are sf layers in the same CRS with columns parid and, for p26,
# FullLegalN and OwnerTypeT.
owner_2026_lookup <- function(p20, p26) {
  p20 <- p20[!duplicated(p20$parid), "parid"]
  a26 <- sf::st_drop_geometry(p26) |>
    dplyr::transmute(parid = as.character(parid), name_2026 = trimws(as.character(FullLegalN)),
                     owner_type_2026 = trimws(as.character(OwnerTypeT))) |>
    dplyr::distinct(parid, .keep_all = TRUE)
  out <- sf::st_drop_geometry(p20) |>
    dplyr::left_join(a26, by = "parid") |>
    dplyr::mutate(matched_by = dplyr::if_else(is.na(name_2026), NA_character_, "parcel id"))
  miss <- which(is.na(out$name_2026))
  if (length(miss)) {
    p26n <- dplyr::transmute(p26, owner_new = trimws(as.character(FullLegalN)),
                             type_new = trimws(as.character(OwnerTypeT)))
    ix <- suppressWarnings(sf::st_intersection(p20[miss, ], p26n))
    ix$a <- as.numeric(sf::st_area(ix))
    area20 <- setNames(as.numeric(sf::st_area(p20[miss, ])), p20$parid[miss])
    best <- sf::st_drop_geometry(ix) |>
      dplyr::group_by(parid) |>
      dplyr::summarise(cover = sum(a), owner_new = owner_new[which.max(a)],
                       type_new = type_new[which.max(a)], .groups = "drop") |>
      dplyr::filter(cover / area20[parid] >= 0.5)
    j <- match(out$parid[miss], best$parid)
    out$name_2026[miss] <- best$owner_new[j]
    out$owner_type_2026[miss] <- best$type_new[j]
    out$matched_by[miss] <- dplyr::if_else(is.na(j), NA_character_, "overlap")
  }
  out
}

# One row per 2020 parcel: parid, share_in_cfp_2026 = share of the parcel's area covered
# by any 2026 Commercial Forest parcel (0-1). Shares of 0.98 or more are set to 1, since
# the two years' parcel lines differ slightly even where nothing changed.
cfp_2026_share <- function(p20, p26) {
  p20 <- p20[!duplicated(p20$parid), "parid"]
  u26 <- sf::st_union(sf::st_geometry(p26))
  ix <- suppressWarnings(sf::st_intersection(p20, u26))
  cover <- tapply(as.numeric(sf::st_area(ix)), ix$parid, sum)
  share <- unname(cover[p20$parid]) / as.numeric(sf::st_area(p20))
  share[is.na(share)] <- 0
  share <- pmin(share, 1)
  share[share >= 0.98] <- 1
  data.frame(parid = p20$parid, share_in_cfp_2026 = share)
}
