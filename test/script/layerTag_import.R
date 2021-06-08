
layerTag("GB.Wm.5th", "Westminster constituencies, 5th review", paste(sep = "\n", 
    "#'@references", "#' Boundary Line Dataset, Ordnance Survey", "#'\\cr https://www.ordnancesurvey.co.uk/business-and-government/products/boundary-line.html"), 
    path.expand("~/git/boundary-line/Data/GB/"), "westminster_const_region", c("Na h-Eileanan an Iar Co Const", 
        "Ross, Skye and Lochaber Co Const", "Caithness, Sutherland and Easter Ross Co Const", 
        "Orkney and Shetland Co Const", "Aberdeen North Burgh Const", "Orkney and Shetland Co Const", 
        "Dundee East Burgh Const", "North East Fife Co Const", "Dundee West Burgh Const", 
        "North East Fife Co Const", "Dunfermline and West Fife Co Const", "Edinburgh West Burgh Const", 
        "Arfon Co Const", "Ynys Mon Co Const", "Southampton, Itchen Boro Const", 
        "Isle of Wight Co Const", "Dunfermline and West Fife Co Const", "Falkirk Co Const"), 
    "NAME")

layerTag("NI.Wm.5th", "Westminster constituencies, 5th review", "OSNI", path.expand("~/git/OSNI/"), 
    ls.layers("NI.Wm.5th"), "", "PC_NAME")

layerTag("UK.Unitary.Regions", "Scottish Independence Referendum 2014", paste(sep = "\n", 
    "#'@references", "#' Boundary Line Dataset, Ordnance Survey", "#'\\cr https://www.ordnancesurvey.co.uk/business-and-government/products/boundary-line.html"), 
    path.expand("~/git/boundary-line/Data/GB/"), "district_borough_unitary_region", 
    c("Na h-Eileanan an Iar", "Highland", "Highland", "Orkney Islands", "Orkney Islands", 
        "Shetland Islands", "Aberdeen City", "Orkney Islands", "Aberdeen City", "Shetland Islands", 
        "Dundee City", "Fife", "City of Edinburgh", "Fife", "Falkirk", "Fife"), "NAME")

layerTag("TIGER2010.KS", "Kansas counties from 2010", paste(sep = "\n", "#'@references", 
    "#' TIGER Dataset, US Census", "#'\\cr https://www.census.gov/geo/maps-data/data/tiger.html", 
    "#'\\cr via kansasgis.org", "#'\\cr https://www.kansasgis.org/catalog/index.cfm"), 
    path.expand("~/git/TIGER2010.KS/"), "Tiger2010_Census_County", "", "NAME10")

