---
layout: post
title:  "Geo-Formats"
date:   2014-12-31 16:26:00
categories: gis
---

## Geospatial data formats

|||
|----------:|:--------------|
| **Shapefile**
| 1992 | ? (ArcView 2) published 1997
| | *ESRI binary vector data format*
| | [http://www.esri.com/library/whitepapers/pdfs/shapefile.pdf]() [http://shapelib.maptools.org/]()
| | Shapefile C Library - C - <http://shapelib.maptools.org/> - read/write
| | pyshp - Python - <https://github.com/GeospatialPython/pyshp> - read/write
| | rgeo-shapefile - Ruby [gem: rgeo-shapefile] - <https://github.com/rgeo/rgeo-shapefile> - read
| | GeoRuby - Ruby [gem: georuby] - <https://github.com/nofxx/georuby> - read/write
| **GeoPackage**
| 2013 | *OGC binary (SQLite-based) vector & image data format*
| | <http://www.geopackage.org/>
| **SDTS** |
| 1992 | **Spatial Data Transfer Standard**
| | *USGS format*
| | http://mcmcweb.er.usgs.gov/sdts/
| **GeoJSON**
| 2008 | *JSON-based vector format*
| | <http://geojson.org/>
| | rgeo-geojson - Ruby gem - <https://github.com/rgeo/rgeo-geojson> - read/write
| **TopoJSON**
| 2012 | *Extension of GeoJSON that encodes topology*
| | <https://github.com/topojson/topojson-specification>
| | <https://github.com/mbostock/topojson/wiki>
| **GML**
| 2000 | **Geography Markup Language**
| | *OGC XML-based vector & coverage data*
| | <http://www.opengeospatial.org/standards/gml>
| | <http://www.iso.org/iso/iso_catalogue/catalogue_tc/catalogue_detail.htm?csnumber=32554>
| **KML**
| 2004 | **Keyhole Markup Language** (2.0)
| 2008 | (OGC standard)
| | *Google Earth XML format for geographic annotation and visualization*
| |  <http://www.opengeospatial.org/standards/kml>
| |  <https://developers.google.com/kml/>
| | GeoRuby - Ruby [gem: georuby] - <https://github.com/nofxx/georuby> - read/write
| **GPX**
| 2002 | **GPS Exchange Format**
| | *XML format for GPS waypoints, tracks and routes*
| | <http://www.topografix.com/gpx.asp>
| |  gpx - Ruby gem - http://dougfales.github.io/gpx/ https://github.com/dougfales/gpx - read/write
| **WKT**
| 1997 | **Well-known text**
| | *OGC text format for vector geometry (Simple Features) DB-transfer*
| | **EWKB** (Extended WKB) - PostGIS-specific extension including SRID and Z, M ordinate values
| | <http://www.opengeospatial.org/standards/sfa>
| | GeoRuby - Ruby [gem: georuby] - <https://github.com/nofxx/georuby> - read/write
| | RGeo - Ruby [gem: rgeo] - <https://github.com/rgeo/rgeo> - read/write
| **WKB**
| 1997 | **Well-known binary**
| | *OGC text format for vector geometry (Simple Features) DB-storage/transfer*
| | EWKT (Extended WKT) - PostGIS-specific extension including SRID and Z, M ordinate values
| | <http://www.opengeospatial.org/standards/sfa>
| | GeoRuby - Ruby [gem: georuby] - <https://github.com/nofxx/georuby> - read/write
| | RGeo - Ruby [gem: rgeo] - <https://github.com/rgeo/rgeo> - read/write
| **SDF**
| <2006 | **Spatial Data File**
| | *Autodesk storage format* (personal geodatabase); currently based on SQLite3
| | Supported by OSGeo FDO API <http://fdo.osgeo.org/>
| | fdotoolbox - Windows tools written in C# - <https://github.com/jumpinjackie/fdotoolbox>
| | Fdo2Fdo - Windows tools - <http://www.sl-king.com/Fdo2Fdo/fdo2fdo.html>
| **TAB**
| 1991 | **MapInfo Table**
| | *MapInfo vector data format*
| | MITAB - C++ - <http://mitab.maptools.org/> - read/write
| **DLG**
| 1988 | **Digital Line Graph**
| | *USGS vector format*
| | <http://egsc.usgs.gov/isb/pubs/factsheets/fs07896t.pdf>
| **GeoTiff**
| | <http://trac.osgeo.org/geotiff/>
| **JPEG 2000**
| (JP2) | <http://www.jpeg.org/jpeg2000/index.html>
| **World Files**
| | *(image registration)* .tfw/.tifw .jgw/.jpgw …  
| | <http://www.kralidis.ca/gis/worldfile.htm>
| | <http://en.wikipedia.org/wiki/World_file>
| | <http://resources.arcgis.com/en/help/main/10.1/index.html#//009t000000mn000000>
| | <http://help.arcgis.com/en/arcgisdesktop/10.0/help/index.html#//009t00000028000000>
| | <http://gis.stackexchange.com/questions/64599/geotiff-explanation-of-aux-xml-and-tfw-world-file>  

## OpenStreetMap formats

<https://www.openstreetmap.org/>

|||
|----------:|:--------------|
| **OSM XML**
| | (.osm) *main OpenStreetMap data format*
| **PBF**
| | **Protocolbuffer Binary Format**
| | (.osm.pbf) *binary format*
| **O5m**
| | (.o5m) *newer compact format*

### OSM Tools

| **Osmosis** | <http://wiki.openstreetmap.org/wiki/Osmosis>
|| command line Java application for processing OSM data (OSM XML & PBF)
| **osmconvert** | <http://wiki.openstreetmap.org/wiki/Osmconvert>
|| tool (written in C) to convert and process OSM files (OSM XML, PBF, O5m)
| **Osmium** | <http://wiki.openstreetmap.org/wiki/Osmium>
|| C++ & JavaScript toolkit for working with OSM data (OSM XML & PBF)
| **osm2pgsql** | <http://wiki.openstreetmap.org/wiki/Osm2pgsql>
|| command line program to convert OSM data to PostGIS SQL (OSM XML & PBF)
|| patch to read O5m: <http://wiki.openstreetmap.org/wiki/Osm2pgsql/o5m>
| **lmposm** | <http://imposm.org/>
|| Tool to import OSM XML/PBF into PostGIS (Version 1, 2 written in Python; Version 3 in Go)
| **osm2geo.js** | <https://gist.github.com/tecoholic/1396990>
|| Converter from OSM XML to GeoJSON
| **OpenStreetMap.jl** | <https://github.com/tedsteiner/OpenStreetMap.jl>
|| Julia language package to read OSM XML

Note: GDAL/OGR supports OSM XML and PBF

Conversions information: <http://wiki.openstreetmap.org/wiki/Converting_map_data_between_formats>

### OSM Editors:

* **iD** (newest HTML5/JS online editor) <http://ideditor.com/  https://github.com/openstreetmap/iD>
* **Potlatch** (older Flash online editor) <http://wiki.openstreetmap.org/wiki/Potlatch/Primer>
* **JOSM** Java OpenStreetMap Editor (Java based offline editor) <https://josm.openstreetmap.de/>
* **Merkaartor** (offline editor C++)  <http://merkaartor.be/>


## Generic conversion tools

| **GDAL** | <http://www.gdal.org/>
|| C/C++ library & tools for geospatial data formats
|| OGR - simple features vector data
|| supported formats: <http://www.gdal.org/formats_list.html http://www.gdal.org/ogr_formats.html>
| **MapPLZ** | <http://www.mapplz.com/>
|| Simple mapping framework to store query and visualize map data.
|| packages for Ruby, Go, Node
|| Supports input from GeoJSON, CSV, ogr2gor, etc.
|| Supports PostGIS, MongoDB, SQLite/Spatialite (untested)
|| Supports output in GeoJSON, WKT, Leaflet.js (HTML)
|| MapPLZ language to define maps
|| Ruby gem: <https://github.com/mapmeld/mapplz-ruby>
| **GeoConverter** | <http://giswiki.hsr.ch/Geoconverter>
|| online format conversion
