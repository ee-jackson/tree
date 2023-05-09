File README-2021.txt (UTF-8)				  12.4.2023

MULTI-SOURCE NATIONAL FOREST INVENTORY (MS-NFI) RASTER MAPS OF 2021
===================================================================

1. Terms of Use
---------------

The Natural Resources Institute Finland (Luke) owns the copyright, data
protection, and other immaterial rights to this product. The Topographic
Database from the National Land Survey has been utilized when making the
product. This work is licensed under the Creative Commons Attribution 4.0
International License. To view a copy of this license, visit
https://creativecommons.org/licenses/by/4.0/ or send a letter to Creative
Commons, PO Box 1866, Mountain View, CA 94042, USA.

When using the material, the owner of the rights to the material must be given
as "©Natural Resources Institute Finland, 2021" and the name of the material
must be given as "The Multi-source National Forest Inventory Raster Maps of
2021". The description of the method is in the references at the end of this
file. A scientific citation practice shall be used in research use.


2. The Files
------------

The raster maps include 45 themes (described later). For delivery, each theme
is divided into subsets according to the Finnish TM35 division of maps from
the National Land Survey (URL https://www.nls.fi/). For simplicity, the sheets
K2 and L2 (Ahvenanmaa) are combined as well as the sheets V3 and W3 (Lapland).

The raster files are in GEOTIFF format. The map coordinate system is
ETRS-TM35FIN. The pixel size in the map coordinate system is 16 m by 16 m.

The results are in 16-bit numbers (the exception is the index map, mista_*,
that is in 8-bit numbers). The unit of each theme is given later. The numbers
32766 and 32767 have been reserved for the following purposes:
32766  pixel where result should have been computed but it was not possible
       because of, e.g., clouds
32767  pixel that is outside forest land, poorly productive land, or
       unproductive land, or is not in Finland


3. Contact and more information
-------------------------------

More information about the Finnish multi-source national forest inventory
is given at http://urn.fi/URN:ISBN:978-952-380-538-5

The queries about the products are asked to be sent to the email address
mvmi@luke.fi.


4. The Material
---------------

4.1 General information

The Finnish Forest Research Institute (Metla) developed a method called
multi-source national forest inventory (MS-NFI). The first operative results
were calculated in 1990. Small area forest resource estimates, in here
municipality level estimates, and estimates of variables in map form are
calculated using field data from the Finnish national forest inventory,
satellite images and other digital georeferenced data, such as topographic
database of the National Land Survey of Finland. Eleven sets of estimates have
been produced for the most part of the country until now and ten sets for
Lapland. The number of the map form themes in the most recent version, from
year 2021, is 45. In addition to the volumes by tree species and timber
assortments, the biomass by tree species groups and tree compartments have
been estimated.

The first country level estimates correspond to years 1990-1994. The most
recent versions are from years 2005, 2007, 2009, 2011, 2013, 2015, 2017, 2019 and 2021.
The maps from 2021 is the seventh set of products freely available. The MTT
Agrifood Research Finland, the Finnish Forest Research Institute, the Finnish
Game and Fisheries Research Institute (RKTL) and the Information Centre of the
Ministry of Agriculture and Forestry Tike merged in 2015 to create the Natural
Resources Institute of Finland (Luke). The MS-NFI results from 2013 were the
first published by Luke. A new set of the products will be produced annually
or biannually in the future. The maps are in raster format in ETRS-TM35FIN
coordinate system. Until 2011, the pixel size was 20mx20m. In 2013 the pixel
size was changed to 16mx16m.

The products cover the combined land categories forest land, poorly productive
forest land and unproductive land. The other land categories, as well as water
bodies, have been delineated out using the elements of topographic database of
the Land Survey of Finland. This classification is independent of the
estimated themes.

4.2 Construction of the theme maps

For the 2021 products, in total 51 833 NFI field plots were used, locating
either on forest land, poorly productive forest land or unproductive land.
The satellite images used included 7 Sentinel-2A MSI images, 6 Sentinel-2B MSI
images and 6 (3 orbits) Landsat 8 OLI images. The images were from 2021.

The field data in the 2021 products were up-dated to correspond the situation
on 31 July, 2021. The length of the up-dating period was calculated for each
field plot from the date of the field measurement to the up-dating date 31
July, 2021. The start of the tree growth was supposed to be on May 1.

The relative increment of the volume of the growing stock in a forest stand
was calculated using the models by Nyyssönen and Mielikäinen (1978) for pine
(Pinus silvestris) and spruce (Picea abies). The models for pine were used for
broad-leaved trees. The volume increments were calculated by stand layers in
case of multi-layer stands. The proportions of volumes by layers were estimated 
proportionally to the quantity the basal area of the layer multiplied by the
mean height of the layer. 

Regeneration cuttings on the field plots were assessed using satellite images
and, in some cases, with aerial photographs. The stand data of the plots cut
were changed to stand data for open area plots. The final volume increments
were calibrated in such a way that the volumes by tree species on July 31,
2021, was the same as that given by the regression line estimated from field
data alone when the results seemed reasonable. Otherwise the target was
determined heuristically.

For the relative height increment, diameter increment and basal area
increment, simple fixed parameter regression models were estimated using data
from the permanent sample plots of NFI10. The models were used in a similar
way as the volume models. The biomass estimates by field plots and biomass
compartments were up-dated proportionally to the volume changes.

For a cover as complete as possible from the entire country, the 2021 product has
been completed by the data estimates from the recent years. The product thus
consists of the following sub-products:

1. The estimates from 2021, based on the NFI field data from 2017-2021 updated
   to 31.7.2021 and the satellite images from 2021 (99.45 % of
   forest pixels),
2. The estimates from 2019, based on the NFI field data from 2015-2019 updated
   to 31.7.2019 and the satellite images from 2018-2019 (0.52 % of
   forest pixels),
3. The estimates from 2019, based on the NFI field data from 2013-2017 updated
   to 31.7.2017 and the satellite images from 2017-2018 (0.02 % of
   forest pixels),
4. The estimates from 2015, based on the NFI field data from 2012-2016 updated
   to 31.7.2015 and the satellite images from 2015-2016 (0.002 % of
   forest pixels),
5. The estimates from 2013, based on the NFI field data from 2009-2013 updated
   to 31.7.2013 and the satellite images from 2012-2014 (0.0005 % of
   forest pixels).
Data source index, MS-NFI-2021, has been added to the product to indicate
the source of the estimates. The pixel values in the index image correspond to
the numbering of the previous list. The value is zero for those pixels where
no applicable estimate was found.

The map form estimates were made using the improved k-Nearest Neighbour method 
(ik-NN method). The value of five for k was used most frequently. The weights
of the features in the ik-NN method are sought using an optimization method
based on genetic algorithm. Coarse scale estimates of forest variables were
used as the supplementary data. The volumes by tree species groups were
selected as the variables. The purpose is to direct the selection of the
neighbours, on the average, to forests similar to the target pixel (see the
references below). The estimation was made separately for mineral soils, mires
and open bogs and fens. The stratification of both the satellite image and the
field plots were made using the topographic map data of Land Survey Finland.

4.3 The themes

The product consists of 44 theme maps in raster format plus data source index.
The themes can be grouped as follows:

The volume of growing stock is available as a total for all tree species 
and broken down into tree species groups (Scots pine, Norway spruce, Birch,
Other broad-leaved trees) and into timber assortments (saw timber, pulpwood).
The volume of a tree is defined as the volume of the stem wood above stump until 
the top of the tree. The volume of a tree in the field data is estimated using
the parameters measured in the field and the volume models. The unit and class
interval of the volume is 1 m3/ha in the products available for downloading.

The biomass of the growing stock has been estimated and is available by tree
species groups and by seven tree compartments. The biomass of stem and bark of
a tree is defined as the biomass of the stem above bark and above stump until
the top of the tree. The biomass of the living branches includes the biomass
of the living branches without needles or leaves. The biomass of the dead
branches includes the biomass of the dead branches possibly left in a living
tree. The foliage biomass includes the biomass of the living needles or
leaves. The biomass of stumps includes the biomass of the above and below
ground stump parts without roots. The root biomass includes the biomass of the
living roots with a diameter of at least 1 cm. The biomass of stem residual is
defined as that part of the stem biomass that can not be used as timber or
pulpwood due it size or quality. 

The biomasses of the sample trees on a NFI field plot are calculated from the
living sample trees belonging to a plot using the wood density models (Repola
et al. 2007) and biomass models (Repola 2008, 2009). The biomasses of the
trees called tally trees are estimated using the estimates of the sample trees
(with more parameters measured) and the parameters of tally trees and
stands. The unit of the biomass in the maps available for downloading is 10 kg/ha. 

The basal area of the growing stock on a forest stand is the cross section area
of the tree stems of a stand per hectare and measured at a height of 1.3 m.
The basal area is measured in the field for the field plot stands on forest land
and poorly productive forest land in the classes of 1 m2/ha.

The age of the growing stock on a forest stand is the weighted average of the
trees, the basal area of the tree as the weight. The age is assessed in the field
for the field plot stands on forest land and poorly productive forest land in
the classes of one year. 

The mean height of the trees on a forest stand is the height of the basal
area median tree for the development classes young thinning stand or more
mature stands. It is about the same as the basal area weighted average
height. For seedling stands, the mean height is the average height of the
dominant and co-dominant seedlings. The mean height is assessed in the field
in the classes of 1 dm.

The mean diameter of trees is assessed at a height of 1.3 metres and is the
the diameter of the basal area median tree. It is about the same as the
weighted average diameter, the basal area of a tree as the weight. It is
assessed for the field plot stands on forest land and poorly productive forest
land in the classes of 1 cm.

The canopy cover of trees is the vertical projection area on the horizontal
plane of the canopies of the individual trees on a field plot (without double
counting the overlapping canopies). In NFI10, it was assessed in the field as
a shares (0-99%) on a fixed radius plot. For the NFI11 plots, it was estimated
using k-NN method and the NFI10 plot data. In North Lapland in NFI9, the
canopy cover was assessed in three categories if the plot was either on forest
land, poorly productive forest land or unproductive land. A regression model
was constructed to estimate the cover in the classes of one percent. 

The canopy cover proportion of broad-leaved trees is derived from the total
cover using the basal area. However, in the seedling stands, the canopy cover
of broad-leaved trees is assessed using the shares of the stem numbers.

The theme "Land class" divides forestry land into three sub-categories, forest
land (pixel value 1), poorly productive forest land (2), unproductive land (3).

The theme "Land Class based on FAO FRA" divides forest into four sub-categories
forest (pixel value 1), other wooded land (2), other land (3) and other land
with tree cover (4), based on the definition of the United Nations FAO Global
Forest Resource Assessment (FRA).

The theme "Site main class" divides forest land, poorly productive forest land
and unproductive land into mineral soil (pixel value 1) and peatland site
classes. The peatland site class is sub-divided into three categories spruce
mires (2), pine mires (3) and treeless peatland (4, open bogs and fens). A
site is classified as peatland if the organic layer is peat, or if more than
75% of the ground vegetation is peatland vegetation. Otherwise, the site is
mineral soil. Both the field plots and satellite images are stratified prior
the analyses into three strata, 1) mineral soil, 2) pine and spruce mires, 3)
treeless peatland using the topographic map data. Thus in the products,
each NFI field data based category can occur within each map based stratum.

The site fertility classes are used for grouping the forest by vegetation
zones into uniform classes according to their site fertility and wood
production capacity. In national land-use classification, all stands on
mineral soil with site fertility class in 1 - 6 were classified as forest land
(1 is herb rich sites, 2 is herb rich heath forests, 3 is mesic forests, 4 is
sub-xeric forests, 5 is xeric forests, 6 is barren forests). Class 7 (rocky
and sandy soils and alluvial lands) can be forest land, poorly productive
forest land, or unproductive land, and class 8 (summit and fjeld land with
single coniferous trees) either poorly productive forest land or unproductive
land. Classes 9 (mountain birch dominated fjelds) and 10 (Open fjelds) are
poorly productive forest land or unproductive land. Both natural and drained
peatlands are classified into six site fertility classes independently of the
land class. Class 1 includes euthropic mines and fens, 2 mesothropic mires and
fens, 3 meso-oligothropic mires, 4 oligothropic mires, 5 oligo-ombothropic
mires and 6 Sphagnum fuscum dominated mires. Both the field plots and
satellite images are stratified prior the analyses into three strata, mineral
soil, pine mires and spruce mires, treeless peatland . The site fertility
class is estimated for each pixel as the most likely site fertility
class. Thus in the products, each NFI field data based category can occur
within each map based stratum.

4.4 The accuracy of the estimates

The estimation errors at pixel level are rather high but decrease when the
area in question increases, i.e., when the area of interest consists of
several pixels. The errors vary by the themes and depend also on the actual
value in the field, for example on the volume of growing stock and the site
fertility class.

The following error estimates are based on the MS-NFI 2019 product.

The magnitude of the average errors of the volume estimates at pixel level
are presented below (SF = South Finland, NF = North Finland, min = mineral
soil, peat = peatland, m3/ha):

species group	assort. SF/min	SF/peat	NF/min	NF/peat
all 		all	  87      67      50      39
pine		all	  67      51      44      30
pine		saw t. 	  44      31      24      12
pine		pulpw. 	  39      32      32      24
spruce		all	  64      43      28      19
spruce		saw t. 	  47      30      16      10
spruce		pulpw. 	  30      22      17      13
birch		all	  35      32      18      19
birch		saw t. 	  11       7       2       2
birch		pulpw. 	  27      26      15      17
other br. l.   	all	  25      16       9       6
other br. l.    saw t. 	   8       5       1       1
other br. l.    pulpw. 	  18      11       7       5

The magnitude of the average error of the biomass estimates at pixel level are
presented below (SF = South Finland, NF = North Finland, min = mineral
soil, peat = peatland, 10 kg/ha):

tree species	compartment	SF/min	SF/peat	NF/min	NF/peat
pine		stem and bark	 2624    1982    1711    1179
pine		living branches	  355     280     315     215
pine		dead branches	   91      74      70      51
pine		foliage 	  118      98     108      81
pine		stump		  186     150     141     100
pine		roots		  609     470     444     295
pine		stem residual	  153     146     176     135
spruce		stem and bark	 2342    1596    1074     744
spruce		living branches	  511     367     319     211
spruce		dead branches	   95      68      46      34
spruce		foliage 	  324     240     195     142
spruce		stump		  201     142     115      75
spruce		roots		  722     529     424     292
spruce		stem residual	  164     158     103      99
broad-leaved	stem and bark	 2127    1755     974     994
broad-leaved	living branches	  350     272     189     181
broad-leaved	dead branches	   19      16      11      11
broad-leaved	foliage 	   75      70      55      54
broad-leaved	stump		  190     152     116     111
broad-leaved	roots		  585     505     340     342
broad-leaved	stem residual	  386     339     285     274

The magnitude of the average error of the estimates of the other continuous
variables at pixel level are presented below (SF = South Finland, NF = North
Finland, min = mineral soil, peat = peatland):

theme			SF/min	SF/peat	NF/min	NF/peat	unit
age			  26      30      47      36	a
basal area		   7       7       5       5	m2/ha
mean height 		  52      43      38      31	dm
mean diameter		   7       5       6       4	cm
canopy cover		  17      17      16      15	%
canopy cover of br. l.	  15      14      10      11	%

The overall accuracy (OA) at pixel level of the land class is on the average
94% when the classification is compared to that based on the NFI field data.
The user accuracy (UA) of the category forest land is on average 96% while the
producers accuracy (PA) is on average 99%. The corresponding figures on poorly
productive forest land are 61% and 52% and on unproductive land 89% and 78%.

The pixel level OA of main site class (mineral soil, spruce mire, pine mire,
treeless mire) is 85%. For the category mineral soil, UA is 89% and PA 96%.
The corresponding figures for spruce mires are 47% and 22%, for pine mires 79%
and 78% and for treeless mires 96% and 84%.

The assessment of site fertility is very challenging even in the field and the
results vary by the assessors (field crew leaders). For site fertility class,
OA is 55% as compared to the NFI field data. In most cases, the difference was
not more than one class. The differences were most frequent on one hand for
the categories herb rich sites and herb rich heath forests and for the
corresponding peatland sites, and on the other for poor mineral soil sites and
for ombrotrophic peatlands. The accuracies are highest for mesic forest and
for the corresponding peatlands (meso-oligotrophic peatlands). In this
category, UA is 61% and PA 68% when compared to the result based on the NFI
field data. 

The errors of estimates at areal level are lower than the errors presented
above.


4.5 References

More information about the methods and the accuracies are given in the
publications, e.g.:

Tomppo, E., Haakana, M., Katila, M. & Peräsaari, J. 2008. Multi-source
national forest inventory - Methods and applications. Managing Forest
Ecosystems 18. Springer. 374 p. ISBN 978-1-4020-8712-7.

Mäkisara, K., Katila, M. & Peräsaari, J. 2022. The Multi-Source
National Forest Inventory of Finland – methods and results 2017 and 2019. Natural
resources and bioeconomy studies 90/2022, Natural Resources Institute
Finland. 73 s. http://urn.fi/URN:ISBN:978-952-380-538-5

Mäkisara, K., Katila, M. & Peräsaari, J. 2019. The Multi-Source
National Forest Inventory of Finland – methods and results 2015. Natural
resources and bioeconomy studies 8/2019, Natural Resources Institute
Finland. 57 p. https://urn.fi/URN:ISBN:978-952-326-711-4

Tomppo, E. & Halme, M. 2004. Using coarse scale forest variables as
ancillary information and weighting of variables in k-NN estimation: a genetic
algorithm approach. Remote Sensing of Environment 92: 1-20.


5. The list of the themes and the file names

File name		 Theme
maaluokka_		 Land class 2021 (1-3)
fra_luokka_		 Land class based on FAO FRA 2021 (1-4)
paatyyppi_		 Site main class 2021 (1-4)
kasvupaikka_		 Site fertility class 2021 (1-10)
keskilapimitta_		 Stand mean diameter 2021 (cm)
keskipituus_		 Stand mean height 2021 (dm)
ika_			 Stand age 2021 (year) 
ppa_			 Stand basal area 2021 (m2/ha) 
latvuspeitto_		 Canopy cover 2021 (%)
lehtip_latvuspeitto_  	 Canopy cover of broad-leaved trees 2021 (%)
tilavuus_		 Volume, the growing stock 2021 (m3/ha) 
manty_			 Volume, pine 2021 (m3/ha)
mantytukki_		 Volume, pine saw timber 2021 (m3/ha)
mantykuitu_		 Volume, pine pulpwood 2021 (m3/ha)
kuusi_			 Volume, spruce 2021 (m3/ha)
kuusitukki_		 Volume, spruce saw timber 2021 (m3/ha)
kuusikuitu_		 Volume, spruce pulpwood 2021 (m3/ha)
koivu_			 Volume, birch 2021 (m3/ha)
koivutukki_		 Volume, birch saw timber 2021 (m3/ha)
koivukuitu_		 Volume, birch pulpwood 2021 (m3/ha)
muulp_			 Volume, other broad-leaved trees 2021 (m3/ha)
muulptukki_		 Volume, other broad-leaved trees saw timber 2021 (m3/ha)
muulpkuitu_		 Volume, other broad-leaved trees pulpwood 2021 (m3/ha)
bm_manty_runkokuori_	 Biomass, pine, stem and bark 2021 (10 kg/ha)
bm_manty_neulaset_	 Biomass, pine, foliage 2021 (10 kg/ha)
bm_manty_elavatoksat_	 Biomass, pine, living branches 2021 (10 kg/ha)
bm_manty_kanto_		 Biomass, pine, stump 2021 (10 kg/ha)
bm_manty_juuret_	 Biomass, pine, roots, d > 1 cm 2021 (10 kg/ha)
bm_manty_kuolleetoksat_	 Biomass, pine, dead branches 2021 (10 kg/ha)
bm_manty_latva_		 Biomass, pine, stem residual 2021 (10 kg/ha)
bm_kuusi_runkokuori_	 Biomass, spruce, stem and bark 2021 (10 kg/ha)
bm_kuusi_neulaset_	 Biomass, spruce, foliage 2021 (10 kg/ha)
bm_kuusi_elavatoksat_	 Biomass, spruce, living branches 2021 (10 kg/ha)
bm_kuusi_kanto_		 Biomass, spruce, stump 2021 (10 kg/ha)
bm_kuusi_juuret_	 Biomass, spruce, roots, d > 1 cm 2021 (10 kg/ha)
bm_kuusi_kuolleetoksat_	 Biomass, spruce, dead branches 2021 (10 kg/ha)
bm_kuusi_latva_		 Biomass, spruce, stem residual 2021 (10 kg/ha)
bm_lehtip_runkokuori_	 Biomass, broad-leaved trees, stem and bark 2021 (10 kg/ha)
bm_lehtip_neulaset_	 Biomass, broad-leaved trees, foliage 2021 (10 kg/ha)
bm_lehtip_elavatoksat_	 Biomass, broad-leaved trees, living branches 2021 (10 kg/ha)
bm_lehtip_kanto_	 Biomass, broad-leaved trees, stump 2021 (10 kg/ha)
bm_lehtip_juuret_	 Biomass, broad-leaved trees, roots, d > 1 cm 2021 (10 kg/ha)
bm_lehtip_kuolleetoksat_ Biomass, broad-leaved trees, dead branches 2021 (10 kg/ha)
bm_lehtip_latva_	 Biomass, broad-leaved trees, stem residual 2021 (10 kg/ha)
mista_			 Data source index, MSNFI 2021
