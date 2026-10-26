# Maps and charts for the corporate-owner presentation

Randy Swaty and Julia Petersen. Houghton and Keweenaw counties, western Upper Peninsula, Michigan.

## How to run

From the `kee_trees` folder:

```
Rscript maps_scripts_for_presentation/make_all.R
```

This runs every numbered script and writes the PNGs to `figures/` (16:9, ready to paste onto a
slide). Each script can also be run on its own.
Font size, titles on/off, figure size and scale bars (off by default) are set at the top of `00_setup.R`.

The locator map (01) downloads state, county, lake and Canada outlines the first time it runs
(needs internet) and saves them to `data/locator_layers.gpkg`. Figure 08 downloads township
outlines the same way, to `data/townships.gpkg`. Later runs use the saved copies.

## Figures, by question in the outline

| Outline question | Figures |
|---|---|
| Where? | 01 locator map |
| Who owns the land | 12 largest owners and owner groups, with bullets |
| Corporate owners as a whole | 02 map, 08 map of where canopy fell 20+ points |
| Corporate vs. other private owners | 03a–c by-year charts, 04 three-measure bars, 08 bar chart by land type |
| Corporate owners relative to each other | 05 map by destination, 06 heavily-cut bars, 07a–c by-year panels, 09a–b Heartlands and Songbird maps |
| What: disturbance | 03–09; 11 acres disturbed per year, historical vs. current (all land) |
| What: small parcel sales (first look) | 10a chart, 10b map; tables in `tables/` |

## Definitions

- **Corporate owners**: TRG/Verdant, Molpus (MWF) and Lyme, investment companies that own forest
  land for investors. Manulife owns no Commercial Forest land in these two counties.
- **Other private owners**: all other Commercial Forest owners: families, clubs and other timber
  companies. Lake Superior Timberlands alone holds 41% of this group's land.
- **Corporate owners relative to each other** are grouped by where the land went, 2020 → 2026:
  TRG → Verdant; TRG → Verdant → The Nature Conservancy (Heartlands); TRG → Verdant → American
  Songbird (2026); TRG → other owners (still enrolled); TRG → left the program; Molpus (mostly kept); Lyme (kept).
  TRG → Verdant counts as a sale, so Molpus and Lyme are the only corporate "keepers"
  (about 6,300 acres). That is why the figures group by destination rather than sellers vs. keepers.
- **Heavily cut**: parcel-average canopy fell more than 20 points, 2020–2025 (parcels of 20+ acres).
- **USFS FIA Tree Canopy Cover loss year**: first year a 30 m cell's canopy fell 15+ points below its
  highest value in the previous 3 years.
- **Hansen forest cleared**: yes/no stand-replacing loss by year, through 2024.
- **Small parcel sale**: a transaction under 160 acres. A transaction is all parcels one 2020 owner
  passed to one 2026 owner, a connected block that left the program, or part of a parcel removed.
  The bulk TRG → Verdant transfer is therefore one large transaction, not hundreds of small ones.

## New: sale dates

The 2026 state list has the date each current owner acquired each parcel (`OwnerParce`). These dates
appear in the side notes of figures 05 and 09 (the by-year charts, 03 and 07, no longer mark them):

- TRG → Verdant: Jul–Aug 2022
- Verdant → The Nature Conservancy (Heartlands): Nov 2022 – Jan 2023
- Lyme: Mar–Jul 2020
- Molpus: Jan 2020 (rest in 2014)
- Verdant → American Songbird: 2026 (exact date not in the list)

So the 2020–21 clearing on Heartlands happened under TRG, and the 2024–25 cutting on Songbird
happened under Verdant.

## Caveats

- **Owner name matching (figure 10 only).** 2020 and 2026 owners are compared by name words, with
  generic words (LLC, trust, timber, Michigan, land, forest, ...) ignored. Two names count as the
  same owner if they share at least two-thirds of the remaining words. For individuals, a 2020 last
  name that appears in the 2026 name counts as kept, so transfers within a family (to a trust,
  a spouse or children) are not counted as sales. Check the results in
  `tables/small_parcel_status_2020_parcels.csv` before presenting.
- **Land that left the program** may have been sold or simply withdrawn; the state lists don't say.
- **Pixel vs. parcel measures.** Figure 08 counts 30 m cells; figures 04 and 06 ("heavily cut") use
  parcel averages. They answer slightly different questions and the percentages differ.
- **Acreage.** Figures 01 and 02 include parcels under 20 acres (corporate 172,349 acres). The
  disturbance figures use parcels of 20+ acres (corporate 171,890 acres).
- **Isle Royale** is left off the county maps (it is a national park, and including it shrinks the maps and inflates the "not enrolled" share).
  It is shown on the locator map.
- The "uneven horizontal intervals" warnings when drawing rasters are harmless.
