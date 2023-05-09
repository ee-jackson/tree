# README.md for trivino_data.csv
data: https://doi.org/10.5061/dryad.4j0zpc8g4 
paper: https://doi.org/10.1111/gcb.16566

Title of Dataset: Future supply of boreal forest ecosystem services is driven by management rather than by climate change.	

We estimated the potential of Finnish boreal forests to provide eight key forest ecosystem services which include a wide range of goods and services (provisioning, regulating and cultural ones) that are relevant in Finland (Saastamoinen et al., 2014): (i) timber; (ii) bilberry; (iii) cowberry; (iv) mushrooms; (v) carbon storage; (vi) scenic beauty; (vii) habitat availability for key vertebrate species; (viii) deadwood (check the main manuscript Table 2 and Supporting Information Appendix S2). We used already published models (see Table 2; Supporting Information Appendix S2) to link the potential supply of forest services to the forest’s structural characteristics and environmental factors, as projected by SIMO under the 28 scenarios resulting from the combination of forest management regimes and climate change scenarios.

The "trivino_data.csv" dataset consists of 132604 observations and 12 variables. This data was used to run the generalized linear mixed effect models (GLMMs) used to assess the contribution of management and climate on the supply of eight forest ecosystem services. It shows the aggregated values across the 100 year time period

# Description of the variables:
ID: Forest plot ID
Zone: Indicates the biogeographical boreal zone where the forest plot is located
Climate_scenario: Indicates which one of the 4 climate change scenarios was applied
Management_regime: Indicates which one of the 7 forest management regimes was applied
Timber: Indicates the volume of harvested timber (m3/ha)
Bilberry: Indicates the bilberry yield (kg/ha)
Cowberry: Indicates the cowberry yield (kg/ha)
Mushrooms: Indicates the marketed mushrooms yield (kg/ha)
Carbon: Indicates the carbon storage in biomass and soil (kg/ha)
Scenic beauty: Indicates the value of the scenic beauty index (no unit)
Habitat availability: Indicates the combined habitat availability index of six key vertebrate species which ranges from 0 to 1 (no unit)
Deadwood: Indicates the volume of deadwood in the forest plot (m3/ha)


We used the Finnish Multi-Source National Forest Inventory (MS-NFI) as input data for the open-source forest simulator SIMO (SIMulation and Optimization for forest management planning; https://www.simo-project.org). SIMO is released under the open-source GPL 2.0 license. We used SIMO 1.0.0 which was then modified by JYU BERG team (https://www.jyu.fi/berg). The MS-NFI data is openly available from the National Resources Institute Finland (Luke) (http://kartta.luke.fi/index-en.html). 
