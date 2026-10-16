# Q&A study guide: methods, input data and results

For the "Who cut, who sold?" deck (`docs/cfp_sale_story_slides.qmd`). Numbers match the rendered deck
and the CSVs in `output_csvs/`.

## Watch-outs

1. **Molpus, not Songbird, has the highest heavy-cut share on the bar chart.** On the land Molpus
   kept, 25.3% of acres were heavily cut, against 20.2% for Songbird. It's only 3 parcels (4,579
   acres), but the short-answer slide calls Songbird "the heaviest cutting." Say "the heaviest
   cutting on TRG's former land."
2. **"About 11 times" is the high end of a range.** At the 20-point cutoff it's 11×. At other
   cutoffs it's 6–8×, and about 5× at 30 points. Counting parcels instead of acres gives 7.7×.
   Only 11 other-private parcels were heavily cut, so the bottom of the ratio is small. Safer:
   "roughly 5 to 11 times, depending on the cutoff."
3. **Lake Superior Timberlands.** It's counted as "other private." Counting it as institutional
   drops the ratio to about 6×; on its own 65,308 acres it cut almost nothing. Owner-name spelling
   variants (Penegor, Gorski and so on) do not affect the ratio, because it pools sold and kept
   land.

| Cutoff (points of canopy lost) | Institutional | Other private | Ratio |
|---:|---:|---:|---:|
| 10 | 14.7% | 2.5% | 5.9× |
| 15 | 10.2% | 1.6% | 6.4× |
| 20 (used) | 6.6% | 0.6% | 11.3× |
| 25 | 4.2% | 0.5% | 7.9× |
| 30 | 2.2% | 0.5% | 4.7× |

## Input data

**Q: What data did you use?**
- **Owners:** the Michigan DNR's Commercial Forest parcel lists for 2020 and 2026.
- **Canopy:** USFS Tree Canopy Cover for every year 2010–2025, on 30 m cells.
- **Clearing:** Hansen Global Forest Change, with loss years through 2024.
- **Disturbance:** LANDFIRE Annual Disturbance 2010–2025, plus LANDFIRE 2020 Biophysical Settings
  (BpS), 2024 Existing Vegetation Type (EVT), and the BpS model tables.
- **Sale boundaries:** maps of the Songbird and Keweenaw Heartlands sales.

**Q: What is Tree Canopy Cover?**
It's a satellite-based estimate of the percentage of each 30 × 30 m cell (about ¼ acre) covered by
tree crowns. It's a model, so values jump around a few points from year to year, and single-year
spikes are partly noise.

**Q: What is Hansen?**
It's a global map of the year a 30 m cell lost its trees outright. It's yes/no, records only
stand-replacing loss such as a clearcut, and misses thinning and partial cuts.

**Q: What is LANDFIRE Annual Disturbance?**
It's a federal map of where disturbance happened each year, with a cause label where one is known.
On this land, 94% of it is labelled "unknown," so it gives us timing but not cause.

**Q: Why use three sources?**
They measure different things. Canopy cover shows any canopy loss, Hansen shows only complete
clearing, and LANDFIRE gives a third view of timing. When they agree on place and year, the result
isn't an artifact of one product.

**Q: Why 2020 and 2026?**
Those are the two parcel lists we have.

**Q: When did the sales happen?**
The 2026 list records the date each current owner acquired each parcel (field `OwnerParce`):
- TRG sold to Verdant in Jul–Aug 2022.
- Verdant sold the Heartlands land to The Nature Conservancy in Nov 2022–Jan 2023.
- Verdant sold to American Songbird in 2026, after the list was compiled.
- Molpus bought most of its land in Jan 2020, and Lyme in Mar–Jul 2020.

The main slide deck still lists exact sale dates under "What we can't say"; the presentation
figures in `maps_scripts_for_presentation/` use these dates.

**Q: Why does the 2026 list show the Songbird land as Verdant's?**
The Songbird sale happened after the 2026 list was compiled, so we assigned that land using the
sale boundary map.

## Methods

**Q: Which parcels are included?**
All 2020 enrolled parcels of 20 acres or more: 2,128 parcels covering 330,094 acres (the median is
80 acres). Smaller parcels have too few cells for a reliable average.

**Q: How did you follow a parcel from 2020 to 2026?**
By its state parcel ID. A parcel that's missing from the 2026 list counts as "left the program"
(168 parcels).

**Q: How did you decide who sold?**
- We compared the owner names after cleaning out punctuation and suffixes like LLC and Inc.
- Same name means kept. A different name, or a parcel that left the program, means sold.
- TRG to Verdant counts as a sale.
- Land is assigned to Songbird or the Heartlands when at least 50% of the parcel lies inside that
  sale boundary.

**Q: Who counts as "institutional"?**
- TRG/Verdant, Molpus (listed as MWF, for Molpus Woodlands Fund) and Lyme.
- Manulife was also on our list of investment companies, but it has no enrolled land in these two
  counties.
- Everyone else is "other private," including timber companies like Lake Superior Timberlands (69%
  of the "kept" other-private acres).

**Q: What does "heavily cut" mean?**
A parcel's average canopy fell by more than 20 points between 2020 and 2025. It's our own
yardstick, not an official standard.

**Q: Does "acres heavily cut" mean acres that were actually cut?**
No, and this question is likely. It's the total acreage of parcels that crossed the 20-point line.
Part of a heavily cut parcel may be untouched, and a parcel that's partly clearcut may never cross
the line. Treat it as an index, not a harvest acreage.

**Q: How do the year-by-year charts work?**
- **Canopy:** we average every 30 m cell in each group's land.
- **Hansen and LANDFIRE:** we report the percentage of the group's area flagged each year.
- **Overlaps:** the Songbird and Heartlands boundaries are drawn on top of the parcel groups, so
  those cells count only under the sale they belong to.

**Q: How does a cell get its colour on the maps?**
- **Left map (canopy):** the first year its canopy fell 15 or more points below its highest value
  in the previous three years.
- **Right map (Hansen):** the year Hansen recorded stand-replacing loss.

**Q: Why compare against the previous three years and not just last year?**
It stops a single noisy dip from being flagged and catches cuts whose effect shows up over two
years.

**Q: How did you estimate historical disturbance?**
- For each BpS, we take the reference share of each succession class and multiply it by the annual
  probability of each disturbance transition in the LANDFIRE BpS model.
- **"Structure-changing"** means a transition that moves the stand into a different succession
  class.
- These are model estimates, not measurements.

## Results

**Q: Where did TRG's land go?**
TRG had 164,825 acres in 2020:
- 101,301 acres went to Verdant.
- 30,813 acres went to The Nature Conservancy (the Heartlands).
- 15,350 acres went to American Songbird.
- 17,361 acres went to other owners or left the program.

**Q: The headline result?**
What happened depends on who ended up with the land:
- **Songbird:** 20.2% heavily cut, with canopy falling from 75.0% in 2023 to 65.8% in 2025.
- **Verdant:** 6.7% heavily cut, with canopy going from 75.0% to 73.4%.
- **Heartlands:** 0% heavily cut, with canopy recovering from 70.3% in 2021 to 75.3%.

**Q: Why does the Heartlands show 0% heavily cut when clearing peaked in 2020–21?**
The heavy-cut measure is the net change from 2020 to 2025. Much of the cutting happened in
2018–2020, so the 2020 starting canopy was already low, and the land regrew after that. The
2020–21 clearing happened while TRG still owned the land (TRG sold to Verdant in mid-2022).

**Q: How much of all heavy cutting was on TRG's former land?**
82% of 12,219 acres, even though TRG held about half the enrolled acres.

**Q: Do the satellite records agree?**
- **Where Hansen shows clearing:** canopy cover also shows a drop on 83% of those acres
  (Heartlands) and 88% (Songbird), within a year of each other 90–94% of the time.
- **Overall:** canopy cover flags about 3 times as many acres, which is mostly lighter cutting that
  Hansen can't see, plus some noise.

**Q: What about Molpus and Lyme?**
- **Molpus:** canopy went from 80.3% in 2020 to 74.8% in 2024, then 67.4% in 2025. It's
  concentrated in the Vulcan parcels (average drop of 14.4 points). The share heavily cut is high,
  but the area is small at 5,380 acres.
- **Lyme:** little cutting, with 1 of 28 parcels heavily cut.

**Q: Big companies vs. other private owners?**
- **Institutional owners:** 6.6% of 171,890 acres heavily cut.
- **Other private owners:** 0.6% of 158,203 acres.
- That's about 11× at our cutoff and 5–8× at others. The direction holds at every cutoff we tried.

**Q: What about the counties as a whole?**
- **Area:** 860,741 acres of mainland, not counting open water. Isle Royale isn't in the LANDFIRE
  data.
- **Vegetation:** northern hardwoods with hemlock make up 54.6% of the BpS and 52.3% of current
  vegetation.
- **Disturbance:** since 2020, about 14,128 acres a year have been disturbed (1.6% of the land), up
  from 8,699 a year in 2010–19. That's about 6× the historical structure-changing rate of 2,495
  acres a year (0.3%), but close to the historical rate when light disturbances are included
  (13,238 acres a year).

## Skeptical questions

**Q: Couldn't the drops be insects, wind or disease?**
- LANDFIRE can't say: on TRG's former land it labels 94% of disturbance "unknown," with no harvest
  labels at all, and the rest insects or disease.
- The pattern fits harvest, though: sharp, block-shaped losses that Hansen also records as
  stand-replacing.
- We don't claim a cause.

**Q: Did Songbird's buyers do the cutting?**
- No. The cutting happened in 2024–25, before the 2026 sale, while Verdant owned the land (Verdant
  bought it from TRG in Jul–Aug 2022).
- We can't say whether it was planned as part of the deal.

**Q: Isn't 2025 unreliable?**
- It's the newest and least settled year. Every group dipped slightly (other private: 79.6% to
  79.0%).
- Songbird and Molpus fell far more, and Hansen and LANDFIRE show the activity starting before 2025.

**Q: Did the sale boundaries match the parcels exactly?**
- Not quite. The Songbird boundary covers 17,236 acres, while parcels at least 50% inside total
  15,350 acres. For the Heartlands it's 32,579 versus 30,813.
- The parcel table uses parcels; the maps and year-by-year charts use the boundaries.
- The presentation map of corporate land by destination (figure 05) includes parcels under 20
  acres, so its acreages differ slightly again (Songbird 16,863 acres, Heartlands 30,828).

**Q: Would different cutoffs change the story?**
- The counts change, but the ranking doesn't.
- Big companies show more heavy cutting at every cutoff from 10 to 30 points (about 5 to 11 times
  as often).
