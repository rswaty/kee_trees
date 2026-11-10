"""Clip NLCD tree canopy cover for Baraga County, 2020 and 2025.

The Houghton-Keweenaw canopy rasters stop at the county line, so Baraga
needs its own clip of the same national product. Tiles come from the
Forest Service image service and are snapped to the same 30 m Albers grid
as inputs/tcc/HK_TCC_2020.tif.

Source: USFS NLCD Tree Canopy Cover v2025.6
https://data.fs.usda.gov/geodata/rastergateway/treecanopycover/
"""

import json
import urllib.parse
import urllib.request
from pathlib import Path

from osgeo import gdal, ogr, osr

gdal.UseExceptions()

ROOT = Path(__file__).resolve().parents[1]
SHP = ROOT / "data/cfp_data/cfp_2020_mi.shp"
OUT = ROOT / "baraga_county/tcc"
SERVICE = (
    "https://imagery.geoplatform.gov/iipp/rest/services/"
    "Vegetation/USFS_EDW_NLCD_TCC_CONUS/ImageServer/exportImage"
)
YEARS = {2020: 75, 2025: 80}
# Same origin and cell size as the Houghton-Keweenaw canopy grid.
ORIGIN_X = 507525.0
ORIGIN_Y = 2827635.0
CELL = 30.0
TILE = 1000
BUFFER_M = 1500

ALBERS_WKT = (
    'PROJCS["Albers",GEOGCS["GCS_WGS_1984",DATUM["D_WGS_1984",'
    'SPHEROID["WGS_1984",6378137.0,298.257223563]],PRIMEM["Greenwich",0.0],'
    'UNIT["Degree",0.0174532925199433]],PROJECTION["Albers"],'
    'PARAMETER["False_Easting",0.0],PARAMETER["False_Northing",0.0],'
    'PARAMETER["Central_Meridian",-96.0],PARAMETER["Standard_Parallel_1",29.5],'
    'PARAMETER["Standard_Parallel_2",45.5],PARAMETER["Latitude_Of_Origin",23.0],'
    'UNIT["Meter",1.0]]'
)


def snap_down(value, origin):
    return origin + ((value - origin) // CELL) * CELL


def snap_up(value, origin):
    return origin + (-(-(value - origin) // CELL)) * CELL


def baraga_window():
    src = ogr.Open(str(SHP))
    layer = src.GetLayer(0)
    layer.SetAttributeFilter("County_Nam = 'Baraga'")
    target = osr.SpatialReference()
    target.ImportFromWkt(ALBERS_WKT)
    target.SetAxisMappingStrategy(osr.OAMS_TRADITIONAL_GIS_ORDER)
    transform = osr.CoordinateTransformation(layer.GetSpatialRef(), target)
    xmin = ymin = 1e18
    xmax = ymax = -1e18
    for feat in layer:
        geom = feat.GetGeometryRef().Clone()
        geom.Transform(transform)
        e = geom.GetEnvelope()
        xmin, xmax = min(xmin, e[0]), max(xmax, e[1])
        ymin, ymax = min(ymin, e[2]), max(ymax, e[3])
    xmin = snap_down(xmin - BUFFER_M, ORIGIN_X)
    ymin = snap_down(ymin - BUFFER_M, ORIGIN_Y)
    xmax = snap_up(xmax + BUFFER_M, ORIGIN_X)
    ymax = snap_up(ymax + BUFFER_M, ORIGIN_Y)
    return xmin, ymin, xmax, ymax


def export_tile(raster_id, xmin, ymin, xmax, ymax, dest):
    width = int(round((xmax - xmin) / CELL))
    height = int(round((ymax - ymin) / CELL))
    sr = {"wkt": ALBERS_WKT}
    params = {
        "bbox": f"{xmin},{ymin},{xmax},{ymax}",
        "bboxSR": json.dumps(sr),
        "imageSR": json.dumps(sr),
        "size": f"{width},{height}",
        "format": "tiff",
        "pixelType": "U8",
        "interpolation": "RSP_NearestNeighbor",
        "mosaicRule": json.dumps(
            {"mosaicMethod": "esriMosaicLockRaster", "lockRasterIds": [raster_id]}
        ),
        "f": "json",
    }
    url = SERVICE + "?" + urllib.parse.urlencode(params)
    req = urllib.request.Request(url, headers={"User-Agent": "Mozilla/5.0"})
    with urllib.request.urlopen(req, timeout=180) as response:
        meta = json.loads(response.read().decode())
    if "href" not in meta:
        raise RuntimeError(meta)
    urllib.request.urlretrieve(meta["href"], dest)
    ds = gdal.Open(str(dest))
    if ds.RasterXSize != width or ds.RasterYSize != height:
        raise RuntimeError(f"{dest.name} came back {ds.RasterXSize}x{ds.RasterYSize}, expected {width}x{height}")


def build_year(year, raster_id, window):
    xmin, ymin, xmax, ymax = window
    tiles = []
    tile_dir = OUT / f"tiles_{year}"
    tile_dir.mkdir(parents=True, exist_ok=True)
    y = ymax
    row = 0
    while y > ymin + 0.1:
        y2 = max(ymin, y - TILE * CELL)
        x = xmin
        col = 0
        while x < xmax - 0.1:
            x2 = min(xmax, x + TILE * CELL)
            dest = tile_dir / f"r{row}_c{col}.tif"
            print(f"  {year} tile {row},{col}", flush=True)
            export_tile(raster_id, x, y2, x2, y, dest)
            tiles.append(str(dest))
            x = x2
            col += 1
        y = y2
        row += 1
    out = OUT / f"Baraga_TCC_{year}.tif"
    vrt = OUT / f"Baraga_TCC_{year}.vrt"
    gdal.BuildVRT(str(vrt), tiles)
    gdal.Translate(
        str(out),
        str(vrt),
        creationOptions=["COMPRESS=LZW", "TILED=YES"],
        noData=255,
    )
    vrt.unlink()
    print(f"wrote {out}", flush=True)


def main():
    OUT.mkdir(parents=True, exist_ok=True)
    window = baraga_window()
    print("window", window, flush=True)
    for year, raster_id in YEARS.items():
        build_year(year, raster_id, window)


if __name__ == "__main__":
    main()
