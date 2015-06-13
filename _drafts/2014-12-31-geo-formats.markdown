---
layout: post
title:  "Geo-Formats"
date:   2013-12-31 16:26:00
categories: gis
---

<pre>
Shapefile
  ESRI binary vector data format
  1992? (ArcView 2) published 1997
  http://www.esri.com/library/whitepapers/pdfs/shapefile.pdf http://shapelib.maptools.org/
  Shapefile C Library - C - http://shapelib.maptools.org/ - read/write
  pyshp - Python - https://github.com/GeospatialPython/pyshp - read/write
  rgeo-shapefile - Ruby [gem: rgeo-shapefile] - https://github.com/rgeo/rgeo-shapefile - read
  GeoRuby - Ruby [gem: georuby] - https://github.com/nofxx/georuby - read/write

GeoPackage
  OGC binary (SQLite-based) vector & image data format
  http://www.geopackage.org/
  2013

SDTS - Spatial Data Transfer Standard
  USGS format
  1992
  http://mcmcweb.er.usgs.gov/sdts/

GeoJSON
  JSON-based vector format
  2008
  http://geojson.org/
  rgeo-geojson - Ruby gem - https://github.com/rgeo/rgeo-geojson - read/write

TopoJSON
  Extension of GeoJSON that encodes topology.
  2012
  https://github.com/topojson/topojson-specification
  https://github.com/mbostock/topojson/wiki  

GML - Geography Markup Language
  OGC XML-based vector & coverage data
  http://www.opengeospatial.org/standards/gml
  http://www.iso.org/iso/iso_catalogue/catalogue_tc/catalogue_detail.htm?csnumber=32554
  2000

KML - Keyhole Markup Language
  Google Earth XML format for geographic annotation and visualization
  2004: 2.0
  2008: OGC standard
  http://www.opengeospatial.org/standards/kml
  https://developers.google.com/kml/
  GeoRuby - Ruby [gem: georuby] - https://github.com/nofxx/georuby - read/write

GPX - GPS Exchange Format
  XML format for GPS waypoints, tracks and routes
  2002
  http://www.topografix.com/gpx.asp
  gpx - Ruby gem - http://dougfales.github.io/gpx/ https://github.com/dougfales/gpx - read/write

WKT - Well-known text
  OGC text format for vector geometry (Simple Features) DB-transfer
  1997
  EWKB (Extended WKB) - PostGIS-specific extension including SRID and Z, M ordinate values
  http://www.opengeospatial.org/standards/sfa
  GeoRuby - Ruby [gem: georuby] - https://github.com/nofxx/georuby - read/write
  RGeo - Ruby [gem: rgeo] - https://github.com/rgeo/rgeo - read/write

WKB - Well-known binary
  OGC text format for vector geometry (Simple Features) DB-storage/transfer
  1997
  EWKT (Extended WKT) - PostGIS-specific extension including SRID and Z, M ordinate values
  http://www.opengeospatial.org/standards/sfa
  GeoRuby - Ruby [gem: georuby] - https://github.com/nofxx/georuby - read/write
  RGeo - Ruby [gem: rgeo] - https://github.com/rgeo/rgeo - read/write
SDF - Spatial Data File
  Autodesk storage format (personal geodatabase); currently based on SQLite3
  < 2006
  Supported by OSGeo FDO API http://fdo.osgeo.org/
  fdotoolbox - Windows tools written in C# - https://github.com/jumpinjackie/fdotoolbox
  Fdo2Fdo - Windows tools - http://www.sl-king.com/Fdo2Fdo/fdo2fdo.html

MapInfo TAB - MapInfo Table
  MapInfo vector data format
  1991
  MITAB - C++ - http://mitab.maptools.org/ - read/write

DLG - Digital Line Graph
  USGS vector format
  1988
  http://egsc.usgs.gov/isb/pubs/factsheets/fs07896t.pdf

GeoTiff
  http://trac.osgeo.org/geotiff/

JPEG 2000 (JP2)
  http://www.jpeg.org/jpeg2000/index.html

World Files (image registration)
  http://www.kralidis.ca/gis/worldfile.htm

  http://en.wikipedia.org/wiki/World_file
  http://resources.arcgis.com/en/help/main/10.1/index.html#//009t000000mn000000
  http://help.arcgis.com/en/arcgisdesktop/10.0/help/index.html#//009t00000028000000
  http://gis.stackexchange.com/questions/64599/geotiff-explanation-of-aux-xml-and-tfw-world-file
  .tfw/.tifw .jgw/.jpgw …  

OpenStreetMap - https://www.openstreetmap.org/  
  OSM XML (.osm) main OpenStreetMap data format
  PBF (Protocolbuffer Binary Format) (.osm.pbf) binary format
  O5m (.o5m) newer compact format
  Tools:
  Osmosis - http://wiki.openstreetmap.org/wiki/Osmosis
    command line Java application for processing OSM data (OSM XML & PBF)
  osmconvert - http://wiki.openstreetmap.org/wiki/Osmconvert
    tool (written in C) to convert and process OSM files (OSM XML, PBF, O5m)
  Osmium - http://wiki.openstreetmap.org/wiki/Osmium
    C++ & JavaScript toolkit for working with OSM data (OSM XML & PBF)
  osm2pgsql - http://wiki.openstreetmap.org/wiki/Osm2pgsql
    command line program to convert OSM data to PostGIS SQL (OSM XML & PBF)
    patch to read O5m: http://wiki.openstreetmap.org/wiki/Osm2pgsql/o5m
  lmposm - http://imposm.org/
    Tool to import OSM XML/PBF into PostGIS (Version 1, 2 written in Python; Version 3 in Go)
  osm2geo.js - https://gist.github.com/tecoholic/1396990
    Converter from OSM XML to GeoJSON
  OpenStreetMap.jl - https://github.com/tedsteiner/OpenStreetMap.jl
   Julia language package to read OSM XML
  GDAL/OGR supports OSM XML and PBF
  Conversions information: http://wiki.openstreetmap.org/wiki/Converting_map_data_between_formats
  Editors:
  iD (newest HTML5/JS online editor) http://ideditor.com/  https://github.com/openstreetmap/iD
  Potlatch (older Flash online editor) http://wiki.openstreetmap.org/wiki/Potlatch/Primer
  JOSM Java OpenStreetMap Editor (Java based offline editor) https://josm.openstreetmap.de/
  Merkaartor (offline editor C++)  http://merkaartor.be/



GENERIC TOOLS

GDAL - http://www.gdal.org/
  C/C++ library & tools for geospatial data formats
  OGR - simple features vector data
  supported formats: http://www.gdal.org/formats_list.html http://www.gdal.org/ogr_formats.html

MapPLZ http://www.mapplz.com/
Simple mapping framework to store query and visualize map data.
  packages for Ruby, Go, Node
  Supports input from GeoJSON, CSV, ogr2gor, etc.
  Supports PostGIS, MongoDB, SQLite/Spatialite (untested)
  Supports output in GeoJSON, WKT, Leaflet.js (HTML)
  MapPLZ language to define maps
Ruby gem: https://github.com/mapmeld/mapplz-ruby

GeoConverter http://giswiki.hsr.ch/Geoconverter
  online format conversion
</pre>
