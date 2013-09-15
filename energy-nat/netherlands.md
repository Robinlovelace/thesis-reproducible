Loading and plotting Dutch commuting data
=====
As with the Enlgish [commuting data](http://rpubs.com/RobinLovelace/7178), we begin by loading and inspecting the raw commuting data. As we shall see, it is in a different format than the English data, so needs to be treated differently. In addition, we need to translate some of the Dutch words into English, using the command `gsub`. As with the English data, the information was taken from an official data dissemination portal, in this case the CBS. The data is [publicly available on the internet](http://statline.cbs.nl/StatWeb/publication/?DM=SLNL&PA=81129ned&D1=0-1,3&D2=0&D3=a&D4=1&D5=0-12&D6=a&HDR=T&STB=G1,G2,G3,G4,G5&VW=T).

Loading and viewing the Dutch commuting data
-----------


```r
nm <- read.csv("NL-Data/2011netherlands.csv", sep = ";")  # This is the Dutch commuting data, downloaded from the CBS
head(nm)
```

```
##   Vervoerwijzen Totaal.vervoerwijzen Auto..bestuurder. Auto..passagier.
## 1           Reg               aantal            aantal           aantal
## 2     Nederland                 0.49              0.26             0.02
## 3     Groningen                 0.46              0.23             0.02
## 4     Friesland                 0.47              0.27             0.03
## 5       Drenthe                 0.52              0.31             0.03
## 6    Overijssel                 0.51              0.27             0.02
##    Trein Bus.tram.metro Brom..snorfiets  Fiets  Lopen
## 1 aantal         aantal          aantal aantal aantal
## 2   0.02           0.02            0.01   0.12   0.03
## 3      0              0               0   0.15   0.02
## 4      0              0               0   0.12   0.02
## 5      0              0               0   0.12   0.02
## 6      0              0               0   0.15   0.02
##   Overige.vervoerwijzen Totaal.vervoerwijzen.1 Auto..bestuurder..1
## 1                aantal                     km                  km
## 2                  0.01                  17.75               23.86
## 3                     0                  18.82               27.77
## 4                     0                  17.94               24.56
## 5                     0                  19.81               23.48
## 6                     0                  18.74               25.23
##   Auto..passagier..1 Trein.1 Bus.tram.metro.1 Brom..snorfiets.1 Fiets.1
## 1                 km      km               km                km      km
## 2              24.97   38.35            13.37              8.75     4.5
## 3               33.8       0                0                 0    4.05
## 4              35.61       0                0                 0    3.42
## 5              39.54       0                0                 0    4.41
## 6              19.49       0                0                 0    4.28
##   Lopen.1 Overige.vervoerwijzen.1 Totaal.vervoerwijzen.2
## 1      km                      km                minuten
## 2    2.68                   22.59                  27.39
## 3    3.13                       0                  26.53
## 4    1.94                       0                  23.35
## 5    2.32                       0                  23.82
## 6       2                       0                   25.3
##   Auto..bestuurder..2 Auto..passagier..2 Trein.2 Bus.tram.metro.2
## 1             minuten            minuten minuten          minuten
## 2               28.89              31.78   64.48             42.4
## 3               29.14              31.81       0                0
## 4               26.33              32.94       0                0
## 5               24.29              35.93       0                0
## 6               28.05               26.6       0                0
##   Brom..snorfiets.2 Fiets.2 Lopen.2 Overige.vervoerwijzen.2
## 1           minuten minuten minuten                 minuten
## 2              19.8   18.46   12.38                   30.56
## 3                 0   19.29   17.65                       0
## 4                 0    13.9   23.55                       0
## 5                 0   15.82   11.58                       0
## 6                 0   15.22    7.74                       0
```

```r
agrep(pattern = " (PV)", x = nm[, 1], )
```

```
## integer(0)
```

```r
sub(pattern = " (PV)", replacement = "", x = as.character(nm[, 1]))  # also don't work... calc
```

```
##  [1] "Reg"           "Nederland"     "Groningen"     "Friesland"    
##  [5] "Drenthe"       "Overijssel"    "Flevoland"     "Gelderland"   
##  [9] "Utrecht"       "Noord-Holland" "Zuid-Holland"  "Zeeland"      
## [13] "Noord-Brabant" "Limburg"
```

```r
class(nm[, 1])
```

```
## [1] "factor"
```

```r

head(nm)
```

```
##   Vervoerwijzen Totaal.vervoerwijzen Auto..bestuurder. Auto..passagier.
## 1           Reg               aantal            aantal           aantal
## 2     Nederland                 0.49              0.26             0.02
## 3     Groningen                 0.46              0.23             0.02
## 4     Friesland                 0.47              0.27             0.03
## 5       Drenthe                 0.52              0.31             0.03
## 6    Overijssel                 0.51              0.27             0.02
##    Trein Bus.tram.metro Brom..snorfiets  Fiets  Lopen
## 1 aantal         aantal          aantal aantal aantal
## 2   0.02           0.02            0.01   0.12   0.03
## 3      0              0               0   0.15   0.02
## 4      0              0               0   0.12   0.02
## 5      0              0               0   0.12   0.02
## 6      0              0               0   0.15   0.02
##   Overige.vervoerwijzen Totaal.vervoerwijzen.1 Auto..bestuurder..1
## 1                aantal                     km                  km
## 2                  0.01                  17.75               23.86
## 3                     0                  18.82               27.77
## 4                     0                  17.94               24.56
## 5                     0                  19.81               23.48
## 6                     0                  18.74               25.23
##   Auto..passagier..1 Trein.1 Bus.tram.metro.1 Brom..snorfiets.1 Fiets.1
## 1                 km      km               km                km      km
## 2              24.97   38.35            13.37              8.75     4.5
## 3               33.8       0                0                 0    4.05
## 4              35.61       0                0                 0    3.42
## 5              39.54       0                0                 0    4.41
## 6              19.49       0                0                 0    4.28
##   Lopen.1 Overige.vervoerwijzen.1 Totaal.vervoerwijzen.2
## 1      km                      km                minuten
## 2    2.68                   22.59                  27.39
## 3    3.13                       0                  26.53
## 4    1.94                       0                  23.35
## 5    2.32                       0                  23.82
## 6       2                       0                   25.3
##   Auto..bestuurder..2 Auto..passagier..2 Trein.2 Bus.tram.metro.2
## 1             minuten            minuten minuten          minuten
## 2               28.89              31.78   64.48             42.4
## 3               29.14              31.81       0                0
## 4               26.33              32.94       0                0
## 5               24.29              35.93       0                0
## 6               28.05               26.6       0                0
##   Brom..snorfiets.2 Fiets.2 Lopen.2 Overige.vervoerwijzen.2
## 1           minuten minuten minuten                 minuten
## 2              19.8   18.46   12.38                   30.56
## 3                 0   19.29   17.65                       0
## 4                 0    13.9   23.55                       0
## 5                 0   15.82   11.58                       0
## 6                 0   15.22    7.74                       0
```

```r
names(nm) <- gsub(pattern = "Auto..passagier.", replacement = "Car.p", x = names(nm))
names(nm) <- gsub(pattern = "Auto..bestuurder.", replacement = "Car.d", x = names(nm))
names(nm) <- gsub(pattern = "Trein", replacement = "Train", x = names(nm))
names(nm) <- gsub("Brom..snorfiets", "Moto", x = names(nm))
names(nm) <- gsub(names(nm[8]), "Bicycle", x = names(nm))
names(nm) <- gsub(names(nm[9]), "Walk", x = names(nm))
names(nm) <- gsub(names(nm[10]), "Other", x = names(nm))
names(nm)
```

```
##  [1] "Vervoerwijzen"          "Totaal.vervoerwijzen"  
##  [3] "Car.d"                  "Car.p"                 
##  [5] "Train"                  "Bus.tram.metro"        
##  [7] "Moto"                   "Bicycle"               
##  [9] "Walk"                   "Other"                 
## [11] "Totaal.vervoerwijzen.1" "Car.d.1"               
## [13] "Car.p.1"                "Train.1"               
## [15] "Bus.tram.metro.1"       "Moto.1"                
## [17] "Bicycle.1"              "Walk.1"                
## [19] "Other.1"                "Totaal.vervoerwijzen.2"
## [21] "Car.d.2"                "Car.p.2"               
## [23] "Train.2"                "Bus.tram.metro.2"      
## [25] "Moto.2"                 "Bicycle.2"             
## [27] "Walk.2"                 "Other.2"
```


Adding the geographic data for the Nethlands 
-----
Now we take the processed Dutch data, and merge them with geographic data, saved as a shapefile:

```r
library(rgeos)  # ensure the right packages are installed
```

```
## Loading required package: sp
```

```
## rgeos version: 0.2-17, (SVN revision 392) GEOS runtime version:
## 3.3.8-CAPI-1.7.8 Polygon checking: TRUE
```

```r
library(rgdal)
```

```
## rgdal: version: 0.8-9, (SVN revision 470) Geospatial Data Abstraction
## Library extensions to R successfully loaded Loaded GDAL runtime: GDAL
## 1.10.0, released 2013/04/24 but rgdal build and GDAL runtime not in sync:
## ... consider re-installing rgdal!! Path to GDAL shared files:
## /usr/share/gdal/1.10 Loaded PROJ.4 runtime: Rel. 4.8.0, 6 March 2012,
## [PJ_VERSION: 480] Path to PROJ.4 shared files: (autodetected)
```

```r
setwd("NL-Data/")
regs <- readOGR(dsn = ".", layer = "Provinces")
```

```
## OGR data source with driver: ESRI Shapefile 
## Source: ".", layer: "Provinces"
## with 13 features and 40 fields
## Feature type: wkbPolygon with 2 dimensions
```

```r
setwd("../")
plot(regs)  # distant islands present
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-21.png) 

```r
r2 <- regs[2:nrow(regs@data), ]  # remove 1st row, so only mainland regions present
plot(r2)
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-22.png) 

```r
head(r2@data)  # remove excess cols
```

```
##   adm1_code Shape_Leng Shape_Area diss_me adm1_code_ iso_3166_2 wikipedia
## 1  NLD-3483      5.374     0.6741    3483   NLD-3483        NL-      <NA>
## 2  NLD-3484      2.896     0.1830    3484   NLD-3484        NL-      <NA>
## 3  NLD-3485      4.157     0.4175    3485   NLD-3485        NL-      <NA>
## 4  NLD-3486      4.488     0.4624    3486   NLD-3486        NL-      <NA>
## 5   NLD-894      2.964     0.3518     894    NLD-894        NL-      <NA>
## 6   NLD-895      5.914     0.5265     895    NLD-895        NL-      <NA>
##   sr_sov_a3 sr_adm0_a3 iso_a2 adm0_sr admin0_lab          name
## 1       NL1        NLD     NL       1          6 Noord-Brabant
## 2       NL1        NLD     NL       1          6       Utrecht
## 3       NL1        NLD     NL       1          6  Zuid-Holland
## 4       NL1        NLD     NL       5          6 Noord-Holland
## 5       NL1        NLD     NL       1          6       Drenthe
## 6       NL1        NLD     NL       5          6     Friesland
##                                                                                               name_alt
## 1 Brabante del Norte|Brabante do Norte|Brabante settentrionale|Brabant-septentrional|Nord-Brabant|Nort
## 2                                                                                                 <NA>
## 3                                                                    Hollande-mridionale|South Holland
## 4                                               Holanda do Norte|Hollande-septentrionale|North Holland
## 5                                                                                                 <NA>
## 6                                                                                   Frise|Frisia|Frsia
##   name_local      type  type_en code_local code_hasc note hasc_maybe
## 1       <NA> Provincie Province       <NA>     NL.NB <NA>       <NA>
## 2       <NA> Provincie Province       <NA>     NL.UT <NA>       <NA>
## 3       <NA> Provincie Province       <NA>     NL.ZH <NA>       <NA>
## 4       <NA> Provincie Province       <NA>     NL.NH <NA>       <NA>
## 5       <NA> Provincie Province       <NA>     NL.DR <NA>       <NA>
## 6       <NA> Provincie Province       <NA>     NL.FR <NA>       <NA>
##   region region_cod region_big big_code provnum_ne gadm_level check_me
## 1   <NA>       <NA>       <NA>     <NA>          0          1        0
## 2   <NA>       <NA>       <NA>     <NA>          0          1        0
## 3   <NA>       <NA>       <NA>     <NA>          0          1        0
## 4   <NA>       <NA>       <NA>     <NA>          0          1        0
## 5   <NA>       <NA>       <NA>     <NA>          7          1        0
## 6   <NA>       <NA>       <NA>     <NA>          2          1        0
##   scalerank datarank abbrev postal area_sqkm sameascity labelrank
## 1         8        8   <NA>   <NA>         0        -99         8
## 2         8        8   <NA>   <NA>         0          9         9
## 3         8        8   <NA>   <NA>         0        -99         8
## 4         8        8   <NA>   <NA>         0        -99         8
## 5         8        8   <NA>     DR         0        -99         8
## 6         8        8   <NA>     FR         0        -99         8
##           featurecla       admin name_len mapcolor9 mapcolor13
## 1 Admin-1 scale rank Netherlands       13         2          9
## 2 Admin-1 scale rank Netherlands        7         2          9
## 3 Admin-1 scale rank Netherlands       12         2          9
## 4 Admin-1 scale rank Netherlands       13         2          9
## 5 Admin-1 scale rank Netherlands        7         2          9
## 6 Admin-1 scale rank Netherlands        9         2          9
```

```r
r2@data <- r2@data[, -c(1, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 14, 15:18, 20:ncol(r2@data))]

# match-up names
r2@data
```

```
##    Shape_Leng          name code_hasc
## 1       5.374 Noord-Brabant     NL.NB
## 2       2.896       Utrecht     NL.UT
## 3       4.157  Zuid-Holland     NL.ZH
## 4       4.488 Noord-Holland     NL.NH
## 5       2.964       Drenthe     NL.DR
## 6       5.914     Friesland     NL.FR
## 7       5.820    Gelderland     NL.GE
## 8       3.325     Groningen     NL.GR
## 9       4.515       Limburg     NL.LI
## 10      4.323    Overijssel     NL.OV
## 11      2.675     Flevoland     NL.FL
## 12      5.779       Zeeland     NL.ZE
```

```r
nm[, 1]
```

```
##  [1] Reg           Nederland     Groningen     Friesland     Drenthe      
##  [6] Overijssel    Flevoland     Gelderland    Utrecht       Noord-Holland
## [11] Zuid-Holland  Zeeland       Noord-Brabant Limburg      
## 14 Levels: Drenthe Flevoland Friesland Gelderland Groningen ... Zuid-Holland
```

```r
nm <- nm[-2, ]
names(nm)
```

```
##  [1] "Vervoerwijzen"          "Totaal.vervoerwijzen"  
##  [3] "Car.d"                  "Car.p"                 
##  [5] "Train"                  "Bus.tram.metro"        
##  [7] "Moto"                   "Bicycle"               
##  [9] "Walk"                   "Other"                 
## [11] "Totaal.vervoerwijzen.1" "Car.d.1"               
## [13] "Car.p.1"                "Train.1"               
## [15] "Bus.tram.metro.1"       "Moto.1"                
## [17] "Bicycle.1"              "Walk.1"                
## [19] "Other.1"                "Totaal.vervoerwijzen.2"
## [21] "Car.d.2"                "Car.p.2"               
## [23] "Train.2"                "Bus.tram.metro.2"      
## [25] "Moto.2"                 "Bicycle.2"             
## [27] "Walk.2"                 "Other.2"
```

```r
code2 <- strtrim(nm[, 1], width = 2)
nm$c2 <- toupper(code2)
c2 <- strsplit(as.character(r2$code_hasc), split = "[.]")
r2@data
```

```
##    Shape_Leng          name code_hasc
## 1       5.374 Noord-Brabant     NL.NB
## 2       2.896       Utrecht     NL.UT
## 3       4.157  Zuid-Holland     NL.ZH
## 4       4.488 Noord-Holland     NL.NH
## 5       2.964       Drenthe     NL.DR
## 6       5.914     Friesland     NL.FR
## 7       5.820    Gelderland     NL.GE
## 8       3.325     Groningen     NL.GR
## 9       4.515       Limburg     NL.LI
## 10      4.323    Overijssel     NL.OV
## 11      2.675     Flevoland     NL.FL
## 12      5.779       Zeeland     NL.ZE
```

```r
r2@data$c2 <- unlist(c2)[seq(2, 24, 2)]
nm$Vervoerwijzen
```

```
##  [1] Reg           Groningen     Friesland     Drenthe       Overijssel   
##  [6] Flevoland     Gelderland    Utrecht       Noord-Holland Zuid-Holland 
## [11] Zeeland       Noord-Brabant Limburg      
## 14 Levels: Drenthe Flevoland Friesland Gelderland Groningen ... Zuid-Holland
```

```r
nm$c2
```

```
##  [1] "RE" "GR" "FR" "DR" "OV" "FL" "GE" "UT" "NO" "ZU" "ZE" "NO" "LI"
```

```r
r2$c2
```

```
##  [1] "NB" "UT" "ZH" "NH" "DR" "FR" "GE" "GR" "LI" "OV" "FL" "ZE"
```

```r
r2$name
```

```
##  [1] Noord-Brabant Utrecht       Zuid-Holland  Noord-Holland Drenthe      
##  [6] Friesland     Gelderland    Groningen     Limburg       Overijssel   
## [11] Flevoland     Zeeland      
## 13 Levels: Drenthe Flevoland Friesland Gelderland Groningen ... Zuid-Holland
```


Matching the regions with merge
------

```r
merge(y = nm, x = r2@data, by = "c2")  # test of the merge process...
```

```
##   c2 Shape_Leng       name code_hasc Vervoerwijzen Totaal.vervoerwijzen
## 1 DR      2.964    Drenthe     NL.DR       Drenthe                 0.52
## 2 FL      2.675  Flevoland     NL.FL     Flevoland                 0.52
## 3 FR      5.914  Friesland     NL.FR     Friesland                 0.47
## 4 GE      5.820 Gelderland     NL.GE    Gelderland                 0.51
## 5 GR      3.325  Groningen     NL.GR     Groningen                 0.46
## 6 LI      4.515    Limburg     NL.LI       Limburg                 0.48
## 7 OV      4.323 Overijssel     NL.OV    Overijssel                 0.51
## 8 UT      2.896    Utrecht     NL.UT       Utrecht                  0.5
## 9 ZE      5.779    Zeeland     NL.ZE       Zeeland                 0.47
##   Car.d Car.p Train Bus.tram.metro Moto Bicycle Walk Other
## 1  0.31  0.03     0              0    0    0.12 0.02     0
## 2   0.3  0.02     0              0    0    0.09 0.02     0
## 3  0.27  0.03     0              0    0    0.12 0.02     0
## 4  0.28  0.02     0              0    0    0.13 0.03     0
## 5  0.23  0.02     0              0    0    0.15 0.02     0
## 6   0.3  0.02     0              0    0    0.09 0.03     0
## 7  0.27  0.02     0              0    0    0.15 0.02     0
## 8  0.26     0     0              0    0    0.13 0.02     0
## 9  0.28  0.02     0              0    0    0.12 0.02     0
##   Totaal.vervoerwijzen.1 Car.d.1 Car.p.1 Train.1 Bus.tram.metro.1 Moto.1
## 1                  19.81   23.48   39.54       0                0      0
## 2                  24.71   30.39   39.76       0                0      0
## 3                  17.94   24.56   35.61       0                0      0
## 4                  18.63   25.24   25.12       0                0      0
## 5                  18.82   27.77    33.8       0                0      0
## 6                  16.66   20.69   14.34       0                0      0
## 7                  18.74   25.23   19.49       0                0      0
## 8                  17.57   24.17       0       0                0      0
## 9                  17.17   23.86   19.28       0                0      0
##   Bicycle.1 Walk.1 Other.1 Totaal.vervoerwijzen.2 Car.d.2 Car.p.2 Train.2
## 1      4.41   2.32       0                  23.82   24.29   35.93       0
## 2      3.67   3.64       0                  33.09   33.25   49.47       0
## 3      3.42   1.94       0                  23.35   26.33   32.94       0
## 4      4.57   1.74       0                  26.92   29.33   30.41       0
## 5      4.05   3.13       0                  26.53   29.14   31.81       0
## 6      4.52   2.29       0                  24.63   25.75   20.32       0
## 7      4.28      2       0                   25.3   28.05    26.6       0
## 8      5.11   2.32       0                  28.91   31.02       0       0
## 9      3.32   1.46       0                  22.92   26.05   39.79       0
##   Bus.tram.metro.2 Moto.2 Bicycle.2 Walk.2 Other.2
## 1                0      0     15.82  11.58       0
## 2                0      0     13.69   9.38       0
## 3                0      0      13.9  23.55       0
## 4                0      0     18.33   9.14       0
## 5                0      0     19.29  17.65       0
## 6                0      0     17.13  12.22       0
## 7                0      0     15.22   7.74       0
## 8                0      0     18.96   9.79       0
## 9                0      0     12.87  10.04       0
```

```r
unmatched <- merge(y = nm, x = r2@data, by = "c2")[, 1]
unmatched
```

```
## [1] "DR" "FL" "FR" "GE" "GR" "LI" "OV" "UT" "ZE"
```

```r
nm[which(nm$c2 %in% unmatched == F), 1:3]  # regions that are not matched 1st time
```

```
##    Vervoerwijzen Totaal.vervoerwijzen  Car.d
## 1            Reg               aantal aantal
## 10 Noord-Holland                  0.5   0.23
## 11  Zuid-Holland                 0.47   0.22
## 13 Noord-Brabant                 0.49   0.29
```

```r
duplicated(regs@data$c2)  # no duplicates
```

```
## logical(0)
```

```r
nm[, c(1, 29)]
```

```
##    Vervoerwijzen c2
## 1            Reg RE
## 3      Groningen GR
## 4      Friesland FR
## 5        Drenthe DR
## 6     Overijssel OV
## 7      Flevoland FL
## 8     Gelderland GE
## 9        Utrecht UT
## 10 Noord-Holland NO
## 11  Zuid-Holland ZU
## 12       Zeeland ZE
## 13 Noord-Brabant NO
## 14       Limburg LI
```

```r
nm[10, 29]  # Says ZU, but should be ZH
```

```
## [1] "ZU"
```

```r
nm[10, 29] <- "ZH"
nm[9, 29]  # NO but should be NH, North Holland
```

```
## [1] "NO"
```

```r
nm[9, 29] <- "NH"
nm$c2 <- sub("NO", "NB", nm$c2)
nm[, c(1, 29)]
```

```
##    Vervoerwijzen c2
## 1            Reg RE
## 3      Groningen GR
## 4      Friesland FR
## 5        Drenthe DR
## 6     Overijssel OV
## 7      Flevoland FL
## 8     Gelderland GE
## 9        Utrecht UT
## 10 Noord-Holland NH
## 11  Zuid-Holland ZH
## 12       Zeeland ZE
## 13 Noord-Brabant NB
## 14       Limburg LI
```

```r
head(nm)  # still contains superfluous units
```

```
##   Vervoerwijzen Totaal.vervoerwijzen  Car.d  Car.p  Train Bus.tram.metro
## 1           Reg               aantal aantal aantal aantal         aantal
## 3     Groningen                 0.46   0.23   0.02      0              0
## 4     Friesland                 0.47   0.27   0.03      0              0
## 5       Drenthe                 0.52   0.31   0.03      0              0
## 6    Overijssel                 0.51   0.27   0.02      0              0
## 7     Flevoland                 0.52    0.3   0.02      0              0
##     Moto Bicycle   Walk  Other Totaal.vervoerwijzen.1 Car.d.1 Car.p.1
## 1 aantal  aantal aantal aantal                     km      km      km
## 3      0    0.15   0.02      0                  18.82   27.77    33.8
## 4      0    0.12   0.02      0                  17.94   24.56   35.61
## 5      0    0.12   0.02      0                  19.81   23.48   39.54
## 6      0    0.15   0.02      0                  18.74   25.23   19.49
## 7      0    0.09   0.02      0                  24.71   30.39   39.76
##   Train.1 Bus.tram.metro.1 Moto.1 Bicycle.1 Walk.1 Other.1
## 1      km               km     km        km     km      km
## 3       0                0      0      4.05   3.13       0
## 4       0                0      0      3.42   1.94       0
## 5       0                0      0      4.41   2.32       0
## 6       0                0      0      4.28      2       0
## 7       0                0      0      3.67   3.64       0
##   Totaal.vervoerwijzen.2 Car.d.2 Car.p.2 Train.2 Bus.tram.metro.2  Moto.2
## 1                minuten minuten minuten minuten          minuten minuten
## 3                  26.53   29.14   31.81       0                0       0
## 4                  23.35   26.33   32.94       0                0       0
## 5                  23.82   24.29   35.93       0                0       0
## 6                   25.3   28.05    26.6       0                0       0
## 7                  33.09   33.25   49.47       0                0       0
##   Bicycle.2  Walk.2 Other.2 c2
## 1   minuten minuten minuten RE
## 3     19.29   17.65       0 GR
## 4      13.9   23.55       0 FR
## 5     15.82   11.58       0 DR
## 6     15.22    7.74       0 OV
## 7     13.69    9.38       0 FL
```

```r
nm <- nm[-1, ]  # removes superfluous units
head(nm)  # missing times + distances for key modes
```

```
##   Vervoerwijzen Totaal.vervoerwijzen Car.d Car.p Train Bus.tram.metro Moto
## 3     Groningen                 0.46  0.23  0.02     0              0    0
## 4     Friesland                 0.47  0.27  0.03     0              0    0
## 5       Drenthe                 0.52  0.31  0.03     0              0    0
## 6    Overijssel                 0.51  0.27  0.02     0              0    0
## 7     Flevoland                 0.52   0.3  0.02     0              0    0
## 8    Gelderland                 0.51  0.28  0.02     0              0    0
##   Bicycle Walk Other Totaal.vervoerwijzen.1 Car.d.1 Car.p.1 Train.1
## 3    0.15 0.02     0                  18.82   27.77    33.8       0
## 4    0.12 0.02     0                  17.94   24.56   35.61       0
## 5    0.12 0.02     0                  19.81   23.48   39.54       0
## 6    0.15 0.02     0                  18.74   25.23   19.49       0
## 7    0.09 0.02     0                  24.71   30.39   39.76       0
## 8    0.13 0.03     0                  18.63   25.24   25.12       0
##   Bus.tram.metro.1 Moto.1 Bicycle.1 Walk.1 Other.1 Totaal.vervoerwijzen.2
## 3                0      0      4.05   3.13       0                  26.53
## 4                0      0      3.42   1.94       0                  23.35
## 5                0      0      4.41   2.32       0                  23.82
## 6                0      0      4.28      2       0                   25.3
## 7                0      0      3.67   3.64       0                  33.09
## 8                0      0      4.57   1.74       0                  26.92
##   Car.d.2 Car.p.2 Train.2 Bus.tram.metro.2 Moto.2 Bicycle.2 Walk.2 Other.2
## 3   29.14   31.81       0                0      0     19.29  17.65       0
## 4   26.33   32.94       0                0      0      13.9  23.55       0
## 5   24.29   35.93       0                0      0     15.82  11.58       0
## 6   28.05    26.6       0                0      0     15.22   7.74       0
## 7   33.25   49.47       0                0      0     13.69   9.38       0
## 8   29.33   30.41       0                0      0     18.33   9.14       0
##   c2
## 3 GR
## 4 FR
## 5 DR
## 6 OV
## 7 FL
## 8 GE
```

```r
class(nm$Car.d)  # but still factors
```

```
## [1] "factor"
```

```r
for (i in 3:(ncol(nm) - 1)) {
    nm[, i] <- as.numeric(as.character(nm[, i]))
}
class(nm$Car.d)  # numeric - previous for loop worked
```

```
## [1] "numeric"
```

```r
natdis <- as.matrix(read.csv("NL-Data/2011netherlands.csv", sep = ";")[2, 12:19])
natdis <- as.numeric(natdis)  # converts data to numeric
(nm)[, 12:19]  # zeros must be replaced by nat. averages
```

```
##    Car.d.1 Car.p.1 Train.1 Bus.tram.metro.1 Moto.1 Bicycle.1 Walk.1
## 3    27.77   33.80       0             0.00   0.00      4.05   3.13
## 4    24.56   35.61       0             0.00   0.00      3.42   1.94
## 5    23.48   39.54       0             0.00   0.00      4.41   2.32
## 6    25.23   19.49       0             0.00   0.00      4.28   2.00
## 7    30.39   39.76       0             0.00   0.00      3.67   3.64
## 8    25.24   25.12       0             0.00   0.00      4.57   1.74
## 9    24.17    0.00       0             0.00   0.00      5.11   2.32
## 10   24.56   20.91       0            12.10  10.34      4.68   3.10
## 11   22.30   20.64       0            12.69   8.46      4.62   3.07
## 12   23.86   19.28       0             0.00   0.00      3.32   1.46
## 13   22.73   31.70       0             0.00   0.00      4.53   3.52
## 14   20.69   14.34       0             0.00   0.00      4.52   2.29
##    Other.1
## 3     0.00
## 4     0.00
## 5     0.00
## 6     0.00
## 7     0.00
## 8     0.00
## 9     0.00
## 10    0.00
## 11   19.09
## 12    0.00
## 13   19.14
## 14    0.00
```

```r
for (i in 1:8) {
    nm[which(nm[, i + 11] == 0), i + 11] <- natdis[i]
}
```

```
## Warning: no non-missing arguments to max; returning -Inf
```

```
## Warning: no non-missing arguments to max; returning -Inf
```

```
## Warning: no non-missing arguments to max; returning -Inf
```

```r
head(nm)
```

```
##   Vervoerwijzen Totaal.vervoerwijzen Car.d Car.p Train Bus.tram.metro Moto
## 3     Groningen                 0.46  0.23  0.02     0              0    0
## 4     Friesland                 0.47  0.27  0.03     0              0    0
## 5       Drenthe                 0.52  0.31  0.03     0              0    0
## 6    Overijssel                 0.51  0.27  0.02     0              0    0
## 7     Flevoland                 0.52  0.30  0.02     0              0    0
## 8    Gelderland                 0.51  0.28  0.02     0              0    0
##   Bicycle Walk Other Totaal.vervoerwijzen.1 Car.d.1 Car.p.1 Train.1
## 3    0.15 0.02     0                  18.82   27.77   33.80   38.35
## 4    0.12 0.02     0                  17.94   24.56   35.61   38.35
## 5    0.12 0.02     0                  19.81   23.48   39.54   38.35
## 6    0.15 0.02     0                  18.74   25.23   19.49   38.35
## 7    0.09 0.02     0                  24.71   30.39   39.76   38.35
## 8    0.13 0.03     0                  18.63   25.24   25.12   38.35
##   Bus.tram.metro.1 Moto.1 Bicycle.1 Walk.1 Other.1 Totaal.vervoerwijzen.2
## 3            13.37   8.75      4.05   3.13   22.59                  26.53
## 4            13.37   8.75      3.42   1.94   22.59                  23.35
## 5            13.37   8.75      4.41   2.32   22.59                  23.82
## 6            13.37   8.75      4.28   2.00   22.59                  25.30
## 7            13.37   8.75      3.67   3.64   22.59                  33.09
## 8            13.37   8.75      4.57   1.74   22.59                  26.92
##   Car.d.2 Car.p.2 Train.2 Bus.tram.metro.2 Moto.2 Bicycle.2 Walk.2 Other.2
## 3   29.14   31.81       0                0      0     19.29  17.65       0
## 4   26.33   32.94       0                0      0     13.90  23.55       0
## 5   24.29   35.93       0                0      0     15.82  11.58       0
## 6   28.05   26.60       0                0      0     15.22   7.74       0
## 7   33.25   49.47       0                0      0     13.69   9.38       0
## 8   29.33   30.41       0                0      0     18.33   9.14       0
##   c2
## 3 GR
## 4 FR
## 5 DR
## 6 OV
## 7 FL
## 8 GE
```

```r
# now re-match
`?`(merge  # sort = T important to see this default, will result in wrong join if not overridden
)
r2@data <- merge(x = r2@data, y = nm, by = "c2", sort = F)  # worked a treat
```


Plotting the data
-------


```r
fr3 <- fortify(r2, region = "c2")
head(fr3)
```

```
##    long   lat order  hole piece group id
## 1 6.520 53.19     1 FALSE     1  DR.1 DR
## 2 6.531 53.19     2 FALSE     1  DR.1 DR
## 3 6.548 53.18     3 FALSE     1  DR.1 DR
## 4 6.564 53.16     4 FALSE     1  DR.1 DR
## 5 6.567 53.16     5 FALSE     1  DR.1 DR
## 6 6.572 53.16     6 FALSE     1  DR.1 DR
```

```r
library(plyr)
names(fr3)[names(fr3) == "id"] <- "c2"
fr3 <- join(x = fr3, y = r2@data)
```

```
## Joining by: c2
```

```r
head(fr3)
```

```
##    long   lat order  hole piece group c2 Shape_Leng    name code_hasc
## 1 6.520 53.19     1 FALSE     1  DR.1 DR      2.964 Drenthe     NL.DR
## 2 6.531 53.19     2 FALSE     1  DR.1 DR      2.964 Drenthe     NL.DR
## 3 6.548 53.18     3 FALSE     1  DR.1 DR      2.964 Drenthe     NL.DR
## 4 6.564 53.16     4 FALSE     1  DR.1 DR      2.964 Drenthe     NL.DR
## 5 6.567 53.16     5 FALSE     1  DR.1 DR      2.964 Drenthe     NL.DR
## 6 6.572 53.16     6 FALSE     1  DR.1 DR      2.964 Drenthe     NL.DR
##   Vervoerwijzen Totaal.vervoerwijzen Car.d Car.p Train Bus.tram.metro Moto
## 1       Drenthe                 0.52  0.31  0.03     0              0    0
## 2       Drenthe                 0.52  0.31  0.03     0              0    0
## 3       Drenthe                 0.52  0.31  0.03     0              0    0
## 4       Drenthe                 0.52  0.31  0.03     0              0    0
## 5       Drenthe                 0.52  0.31  0.03     0              0    0
## 6       Drenthe                 0.52  0.31  0.03     0              0    0
##   Bicycle Walk Other Totaal.vervoerwijzen.1 Car.d.1 Car.p.1 Train.1
## 1    0.12 0.02     0                  19.81   23.48   39.54   38.35
## 2    0.12 0.02     0                  19.81   23.48   39.54   38.35
## 3    0.12 0.02     0                  19.81   23.48   39.54   38.35
## 4    0.12 0.02     0                  19.81   23.48   39.54   38.35
## 5    0.12 0.02     0                  19.81   23.48   39.54   38.35
## 6    0.12 0.02     0                  19.81   23.48   39.54   38.35
##   Bus.tram.metro.1 Moto.1 Bicycle.1 Walk.1 Other.1 Totaal.vervoerwijzen.2
## 1            13.37   8.75      4.41   2.32   22.59                  23.82
## 2            13.37   8.75      4.41   2.32   22.59                  23.82
## 3            13.37   8.75      4.41   2.32   22.59                  23.82
## 4            13.37   8.75      4.41   2.32   22.59                  23.82
## 5            13.37   8.75      4.41   2.32   22.59                  23.82
## 6            13.37   8.75      4.41   2.32   22.59                  23.82
##   Car.d.2 Car.p.2 Train.2 Bus.tram.metro.2 Moto.2 Bicycle.2 Walk.2 Other.2
## 1   24.29   35.93       0                0      0     15.82  11.58       0
## 2   24.29   35.93       0                0      0     15.82  11.58       0
## 3   24.29   35.93       0                0      0     15.82  11.58       0
## 4   24.29   35.93       0                0      0     15.82  11.58       0
## 5   24.29   35.93       0                0      0     15.82  11.58       0
## 6   24.29   35.93       0                0      0     15.82  11.58       0
```

```r
p <- ggplot(data = fr3, aes(x = long, y = lat, fill = Car.d))
p + geom_polygon(aes(group = group)) + geom_path(aes(group = group)) + geom_text(data = r2@data, 
    aes(x = coordinates(r2)[, 1], y = coordinates(r2)[, 2], label = r2$c2))
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 

```r
r2@data
```

```
##    c2 Shape_Leng          name code_hasc Vervoerwijzen
## 1  NB      5.374 Noord-Brabant     NL.NB Noord-Brabant
## 2  UT      2.896       Utrecht     NL.UT       Utrecht
## 3  ZH      4.157  Zuid-Holland     NL.ZH  Zuid-Holland
## 4  NH      4.488 Noord-Holland     NL.NH Noord-Holland
## 5  DR      2.964       Drenthe     NL.DR       Drenthe
## 6  FR      5.914     Friesland     NL.FR     Friesland
## 7  GE      5.820    Gelderland     NL.GE    Gelderland
## 8  GR      3.325     Groningen     NL.GR     Groningen
## 9  LI      4.515       Limburg     NL.LI       Limburg
## 10 OV      4.323    Overijssel     NL.OV    Overijssel
## 11 FL      2.675     Flevoland     NL.FL     Flevoland
## 12 ZE      5.779       Zeeland     NL.ZE       Zeeland
##    Totaal.vervoerwijzen Car.d Car.p Train Bus.tram.metro Moto Bicycle Walk
## 1                  0.49  0.29  0.02     0           0.00 0.00    0.12 0.02
## 2                   0.5  0.26  0.00     0           0.00 0.00    0.13 0.02
## 3                  0.47  0.22  0.02     0           0.03 0.01    0.12 0.03
## 4                   0.5  0.23  0.02     0           0.03 0.01    0.14 0.02
## 5                  0.52  0.31  0.03     0           0.00 0.00    0.12 0.02
## 6                  0.47  0.27  0.03     0           0.00 0.00    0.12 0.02
## 7                  0.51  0.28  0.02     0           0.00 0.00    0.13 0.03
## 8                  0.46  0.23  0.02     0           0.00 0.00    0.15 0.02
## 9                  0.48  0.30  0.02     0           0.00 0.00    0.09 0.03
## 10                 0.51  0.27  0.02     0           0.00 0.00    0.15 0.02
## 11                 0.52  0.30  0.02     0           0.00 0.00    0.09 0.02
## 12                 0.47  0.28  0.02     0           0.00 0.00    0.12 0.02
##    Other Totaal.vervoerwijzen.1 Car.d.1 Car.p.1 Train.1 Bus.tram.metro.1
## 1   0.01                  18.43   22.73   31.70   38.35            13.37
## 2   0.00                  17.57   24.17   24.97   38.35            13.37
## 3   0.01                  16.27   22.30   20.64   38.35            12.69
## 4   0.00                  16.77   24.56   20.91   38.35            12.10
## 5   0.00                  19.81   23.48   39.54   38.35            13.37
## 6   0.00                  17.94   24.56   35.61   38.35            13.37
## 7   0.00                  18.63   25.24   25.12   38.35            13.37
## 8   0.00                  18.82   27.77   33.80   38.35            13.37
## 9   0.00                  16.66   20.69   14.34   38.35            13.37
## 10  0.00                  18.74   25.23   19.49   38.35            13.37
## 11  0.00                  24.71   30.39   39.76   38.35            13.37
## 12  0.00                  17.17   23.86   19.28   38.35            13.37
##    Moto.1 Bicycle.1 Walk.1 Other.1 Totaal.vervoerwijzen.2 Car.d.2 Car.p.2
## 1    8.75      4.53   3.52   19.14                  25.19   26.34   31.63
## 2    8.75      5.11   2.32   22.59                  28.91   31.02    0.00
## 3    8.46      4.62   3.07   19.09                  29.10   30.48   29.89
## 4   10.34      4.68   3.10   22.59                  30.37   31.88   33.11
## 5    8.75      4.41   2.32   22.59                  23.82   24.29   35.93
## 6    8.75      3.42   1.94   22.59                  23.35   26.33   32.94
## 7    8.75      4.57   1.74   22.59                  26.92   29.33   30.41
## 8    8.75      4.05   3.13   22.59                  26.53   29.14   31.81
## 9    8.75      4.52   2.29   22.59                  24.63   25.75   20.32
## 10   8.75      4.28   2.00   22.59                  25.30   28.05   26.60
## 11   8.75      3.67   3.64   22.59                  33.09   33.25   49.47
## 12   8.75      3.32   1.46   22.59                  22.92   26.05   39.79
##    Train.2 Bus.tram.metro.2 Moto.2 Bicycle.2 Walk.2 Other.2
## 1        0             0.00   0.00     16.57  12.66   23.53
## 2        0             0.00   0.00     18.96   9.79    0.00
## 3        0            43.59  20.94     18.71  15.56   29.11
## 4        0            41.20  22.07     23.69   9.91    0.00
## 5        0             0.00   0.00     15.82  11.58    0.00
## 6        0             0.00   0.00     13.90  23.55    0.00
## 7        0             0.00   0.00     18.33   9.14    0.00
## 8        0             0.00   0.00     19.29  17.65    0.00
## 9        0             0.00   0.00     17.13  12.22    0.00
## 10       0             0.00   0.00     15.22   7.74    0.00
## 11       0             0.00   0.00     13.69   9.38    0.00
## 12       0             0.00   0.00     12.87  10.04    0.00
```


Calculating the energy costs
-----

```r
names(fr3)
```

```
##  [1] "long"                   "lat"                   
##  [3] "order"                  "hole"                  
##  [5] "piece"                  "group"                 
##  [7] "c2"                     "Shape_Leng"            
##  [9] "name"                   "code_hasc"             
## [11] "Vervoerwijzen"          "Totaal.vervoerwijzen"  
## [13] "Car.d"                  "Car.p"                 
## [15] "Train"                  "Bus.tram.metro"        
## [17] "Moto"                   "Bicycle"               
## [19] "Walk"                   "Other"                 
## [21] "Totaal.vervoerwijzen.1" "Car.d.1"               
## [23] "Car.p.1"                "Train.1"               
## [25] "Bus.tram.metro.1"       "Moto.1"                
## [27] "Bicycle.1"              "Walk.1"                
## [29] "Other.1"                "Totaal.vervoerwijzen.2"
## [31] "Car.d.2"                "Car.p.2"               
## [33] "Train.2"                "Bus.tram.metro.2"      
## [35] "Moto.2"                 "Bicycle.2"             
## [37] "Walk.2"                 "Other.2"
```

```r
ec <- c(3, 0, 0.77, 0.57, 1.7, 0.09, 0.13, 0)  # Energy costs of transport modes, in order
names(ec) <- names(fr3[, 14:21])
ec
```

```
##                  Car.p                  Train         Bus.tram.metro 
##                   3.00                   0.00                   0.77 
##                   Moto                Bicycle                   Walk 
##                   0.57                   1.70                   0.09 
##                  Other Totaal.vervoerwijzen.1 
##                   0.13                   0.00
```

```r

r3 <- r2  # new object, to add energy costs to (keep original r2, as backup)
names(r3@data)
```

```
##  [1] "c2"                     "Shape_Leng"            
##  [3] "name"                   "code_hasc"             
##  [5] "Vervoerwijzen"          "Totaal.vervoerwijzen"  
##  [7] "Car.d"                  "Car.p"                 
##  [9] "Train"                  "Bus.tram.metro"        
## [11] "Moto"                   "Bicycle"               
## [13] "Walk"                   "Other"                 
## [15] "Totaal.vervoerwijzen.1" "Car.d.1"               
## [17] "Car.p.1"                "Train.1"               
## [19] "Bus.tram.metro.1"       "Moto.1"                
## [21] "Bicycle.1"              "Walk.1"                
## [23] "Other.1"                "Totaal.vervoerwijzen.2"
## [25] "Car.d.2"                "Car.p.2"               
## [27] "Train.2"                "Bus.tram.metro.2"      
## [29] "Moto.2"                 "Bicycle.2"             
## [31] "Walk.2"                 "Other.2"
```

```r
# r3@data[,7:14] <- (r3@data[,7:14]) * 2 Originally used this as an
# approximation - multiply by 2
`?`(sweep  # check how this function works
)
r3$Totaal.vervoerwijzen <- as.numeric(as.character(r3$Totaal.vervoerwijzen))
summary(1/r3$Totaal.vervoerwijzen)  # Shows that 2 is a good estimate, but out by an average of 3.7%
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    1.92    1.96    2.02    2.04    2.13    2.17
```

```r
r3@data[, 7:14] <- sweep(r3@data[, 7:14], MARGIN = 1, STATS = r3$Totaal.vervoerwijzen, 
    FUN = "/")  # true proportions

ecosts <- r3@data[, 7:14]  # set-up correctly dimensioned df
for (i in 1:nrow(r3@data)) {
    ecosts[i, ] <- r3@data[i, 7:14] * r3@data[i, 16:23] * ec
}
head(r3@data)
```

```
##   c2 Shape_Leng          name code_hasc Vervoerwijzen Totaal.vervoerwijzen
## 1 NB      5.374 Noord-Brabant     NL.NB Noord-Brabant                 0.49
## 2 UT      2.896       Utrecht     NL.UT       Utrecht                 0.50
## 3 ZH      4.157  Zuid-Holland     NL.ZH  Zuid-Holland                 0.47
## 4 NH      4.488 Noord-Holland     NL.NH Noord-Holland                 0.50
## 5 DR      2.964       Drenthe     NL.DR       Drenthe                 0.52
## 6 FR      5.914     Friesland     NL.FR     Friesland                 0.47
##    Car.d   Car.p Train Bus.tram.metro    Moto Bicycle    Walk   Other
## 1 0.5918 0.04082     0        0.00000 0.00000  0.2449 0.04082 0.02041
## 2 0.5200 0.00000     0        0.00000 0.00000  0.2600 0.04000 0.00000
## 3 0.4681 0.04255     0        0.06383 0.02128  0.2553 0.06383 0.02128
## 4 0.4600 0.04000     0        0.06000 0.02000  0.2800 0.04000 0.00000
## 5 0.5962 0.05769     0        0.00000 0.00000  0.2308 0.03846 0.00000
## 6 0.5745 0.06383     0        0.00000 0.00000  0.2553 0.04255 0.00000
##   Totaal.vervoerwijzen.1 Car.d.1 Car.p.1 Train.1 Bus.tram.metro.1 Moto.1
## 1                  18.43   22.73   31.70   38.35            13.37   8.75
## 2                  17.57   24.17   24.97   38.35            13.37   8.75
## 3                  16.27   22.30   20.64   38.35            12.69   8.46
## 4                  16.77   24.56   20.91   38.35            12.10  10.34
## 5                  19.81   23.48   39.54   38.35            13.37   8.75
## 6                  17.94   24.56   35.61   38.35            13.37   8.75
##   Bicycle.1 Walk.1 Other.1 Totaal.vervoerwijzen.2 Car.d.2 Car.p.2 Train.2
## 1      4.53   3.52   19.14                  25.19   26.34   31.63       0
## 2      5.11   2.32   22.59                  28.91   31.02    0.00       0
## 3      4.62   3.07   19.09                  29.10   30.48   29.89       0
## 4      4.68   3.10   22.59                  30.37   31.88   33.11       0
## 5      4.41   2.32   22.59                  23.82   24.29   35.93       0
## 6      3.42   1.94   22.59                  23.35   26.33   32.94       0
##   Bus.tram.metro.2 Moto.2 Bicycle.2 Walk.2 Other.2
## 1             0.00   0.00     16.57  12.66   23.53
## 2             0.00   0.00     18.96   9.79    0.00
## 3            43.59  20.94     18.71  15.56   29.11
## 4            41.20  22.07     23.69   9.91    0.00
## 5             0.00   0.00     15.82  11.58    0.00
## 6             0.00   0.00     13.90  23.55    0.00
```

```r
r3@data$etot <- rowSums(ecosts)
```


Re-plot with total energy costs/trip estimated (MJ)
-------

```r
r3 <- spTransform(r3, CRS("+init=epsg:27700"))
r3$id <- 1:length(r3$name)
fr3 <- fortify(r3, region = "c2")
head(fr3)
```

```
##     long    lat order  hole piece group id
## 1 968762 399879     1 FALSE     1  DR.1 DR
## 2 969477 400058     2 FALSE     1  DR.1 DR
## 3 970799 398641     3 FALSE     1  DR.1 DR
## 4 972115 396858     4 FALSE     1  DR.1 DR
## 5 972343 396705     5 FALSE     1  DR.1 DR
## 6 972720 396617     6 FALSE     1  DR.1 DR
```

```r
names(fr3)[7] <- "c2"
fr3 <- join(x = fr3, y = r3@data, by = "c2")
head(fr3)
```

```
##     long    lat order  hole piece group c2 Shape_Leng    name code_hasc
## 1 968762 399879     1 FALSE     1  DR.1 DR      2.964 Drenthe     NL.DR
## 2 969477 400058     2 FALSE     1  DR.1 DR      2.964 Drenthe     NL.DR
## 3 970799 398641     3 FALSE     1  DR.1 DR      2.964 Drenthe     NL.DR
## 4 972115 396858     4 FALSE     1  DR.1 DR      2.964 Drenthe     NL.DR
## 5 972343 396705     5 FALSE     1  DR.1 DR      2.964 Drenthe     NL.DR
## 6 972720 396617     6 FALSE     1  DR.1 DR      2.964 Drenthe     NL.DR
##   Vervoerwijzen Totaal.vervoerwijzen  Car.d   Car.p Train Bus.tram.metro
## 1       Drenthe                 0.52 0.5962 0.05769     0              0
## 2       Drenthe                 0.52 0.5962 0.05769     0              0
## 3       Drenthe                 0.52 0.5962 0.05769     0              0
## 4       Drenthe                 0.52 0.5962 0.05769     0              0
## 5       Drenthe                 0.52 0.5962 0.05769     0              0
## 6       Drenthe                 0.52 0.5962 0.05769     0              0
##   Moto Bicycle    Walk Other Totaal.vervoerwijzen.1 Car.d.1 Car.p.1
## 1    0  0.2308 0.03846     0                  19.81   23.48   39.54
## 2    0  0.2308 0.03846     0                  19.81   23.48   39.54
## 3    0  0.2308 0.03846     0                  19.81   23.48   39.54
## 4    0  0.2308 0.03846     0                  19.81   23.48   39.54
## 5    0  0.2308 0.03846     0                  19.81   23.48   39.54
## 6    0  0.2308 0.03846     0                  19.81   23.48   39.54
##   Train.1 Bus.tram.metro.1 Moto.1 Bicycle.1 Walk.1 Other.1
## 1   38.35            13.37   8.75      4.41   2.32   22.59
## 2   38.35            13.37   8.75      4.41   2.32   22.59
## 3   38.35            13.37   8.75      4.41   2.32   22.59
## 4   38.35            13.37   8.75      4.41   2.32   22.59
## 5   38.35            13.37   8.75      4.41   2.32   22.59
## 6   38.35            13.37   8.75      4.41   2.32   22.59
##   Totaal.vervoerwijzen.2 Car.d.2 Car.p.2 Train.2 Bus.tram.metro.2 Moto.2
## 1                  23.82   24.29   35.93       0                0      0
## 2                  23.82   24.29   35.93       0                0      0
## 3                  23.82   24.29   35.93       0                0      0
## 4                  23.82   24.29   35.93       0                0      0
## 5                  23.82   24.29   35.93       0                0      0
## 6                  23.82   24.29   35.93       0                0      0
##   Bicycle.2 Walk.2 Other.2 etot id
## 1     15.82  11.58       0 42.1  5
## 2     15.82  11.58       0 42.1  5
## 3     15.82  11.58       0 42.1  5
## 4     15.82  11.58       0 42.1  5
## 5     15.82  11.58       0 42.1  5
## 6     15.82  11.58       0 42.1  5
```

```r
fr3$lat <- fr3$lat/1000
fr3$long <- fr3$long/1000
head(fr3)
```

```
##    long   lat order  hole piece group c2 Shape_Leng    name code_hasc
## 1 968.8 399.9     1 FALSE     1  DR.1 DR      2.964 Drenthe     NL.DR
## 2 969.5 400.1     2 FALSE     1  DR.1 DR      2.964 Drenthe     NL.DR
## 3 970.8 398.6     3 FALSE     1  DR.1 DR      2.964 Drenthe     NL.DR
## 4 972.1 396.9     4 FALSE     1  DR.1 DR      2.964 Drenthe     NL.DR
## 5 972.3 396.7     5 FALSE     1  DR.1 DR      2.964 Drenthe     NL.DR
## 6 972.7 396.6     6 FALSE     1  DR.1 DR      2.964 Drenthe     NL.DR
##   Vervoerwijzen Totaal.vervoerwijzen  Car.d   Car.p Train Bus.tram.metro
## 1       Drenthe                 0.52 0.5962 0.05769     0              0
## 2       Drenthe                 0.52 0.5962 0.05769     0              0
## 3       Drenthe                 0.52 0.5962 0.05769     0              0
## 4       Drenthe                 0.52 0.5962 0.05769     0              0
## 5       Drenthe                 0.52 0.5962 0.05769     0              0
## 6       Drenthe                 0.52 0.5962 0.05769     0              0
##   Moto Bicycle    Walk Other Totaal.vervoerwijzen.1 Car.d.1 Car.p.1
## 1    0  0.2308 0.03846     0                  19.81   23.48   39.54
## 2    0  0.2308 0.03846     0                  19.81   23.48   39.54
## 3    0  0.2308 0.03846     0                  19.81   23.48   39.54
## 4    0  0.2308 0.03846     0                  19.81   23.48   39.54
## 5    0  0.2308 0.03846     0                  19.81   23.48   39.54
## 6    0  0.2308 0.03846     0                  19.81   23.48   39.54
##   Train.1 Bus.tram.metro.1 Moto.1 Bicycle.1 Walk.1 Other.1
## 1   38.35            13.37   8.75      4.41   2.32   22.59
## 2   38.35            13.37   8.75      4.41   2.32   22.59
## 3   38.35            13.37   8.75      4.41   2.32   22.59
## 4   38.35            13.37   8.75      4.41   2.32   22.59
## 5   38.35            13.37   8.75      4.41   2.32   22.59
## 6   38.35            13.37   8.75      4.41   2.32   22.59
##   Totaal.vervoerwijzen.2 Car.d.2 Car.p.2 Train.2 Bus.tram.metro.2 Moto.2
## 1                  23.82   24.29   35.93       0                0      0
## 2                  23.82   24.29   35.93       0                0      0
## 3                  23.82   24.29   35.93       0                0      0
## 4                  23.82   24.29   35.93       0                0      0
## 5                  23.82   24.29   35.93       0                0      0
## 6                  23.82   24.29   35.93       0                0      0
##   Bicycle.2 Walk.2 Other.2 etot id
## 1     15.82  11.58       0 42.1  5
## 2     15.82  11.58       0 42.1  5
## 3     15.82  11.58       0 42.1  5
## 4     15.82  11.58       0 42.1  5
## 5     15.82  11.58       0 42.1  5
## 6     15.82  11.58       0 42.1  5
```

```r
# if analysis alread done... load('.RData')
p <- ggplot(data = fr3, aes(x = long, y = lat))
p + geom_polygon(aes(group = group, fill = etot)) + geom_path(aes(group = group)) + 
    geom_text(data = r3@data, aes(x = coordinates(r3)[, 1]/1000, y = coordinates(r3)[, 
        2]/1000, label = r3$c2), color = "blue") + scale_fill_continuous(low = "green", 
    high = "red", name = "Etrp (MJ)", limits = c(20, 55)) + coord_fixed() + 
    xlab("Easting (km)") + ylab("Northing (km)")  #+ theme_minimal()
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6.png) 


