# Frontiers in Ecology & Evolution Special Issue

The R codes and scripts here implement the analyses and figures
reported in Bennion, Simpson, and Goldsmith (in review) *Frontiers in
Ecology and Evolution*, submitted to the special issue *Using
paleolimnology for management and restoration of lakes*. The paper will
be available via Open Access as per Frontiers model following acceptance
of the manuscript.

The files are licenced under the MIT licence; see the `LICENCE` file
details, but basically as long as the source of the files and the
specified information in `LICENCE` is included in any subsequent
re-distribution of the code, you can do what you want with it.

## Manuscript abstract

Efforts to restore enriched lakes have increased yet there remains 
uncertainty about whether restoration targets can be achieved and over 
what timescale. Paleoecological techniques, principally diatom 
analyses, were used to examine the degree of impact and recovery in 12 
European lakes subject to eutrophication and subsequent reduction in 
nutrient loading. Dissimilarity scores showed that all sites 
experienced progressive deviation from the reference sample (core 
bottom) prior to nutrient reduction, and principal curves indicated 
gradual compositional change with enrichment. When additive models were 
applied to the latter, the changes were statistically significant in 9 
of the 12 sites.

Shifts in diatom composition following reduction in nutrient loading 
were more equivocal, with a reversal towards the reference flora seen 
only in four of the deep lakes and one of the shallow lakes. Of these, 
only two were significant (Lake Bled and Mjøsa). Alternative nutrient 
sources seem to explain the lack of apparent recovery in the other deep 
lakes. In three shallow lakes diatom assemblages were replaced by a 
community associated with lower productivity but not the one seen prior 
to enrichment. Internal loading and top down control may influence 
recovery in shallow lakes and climate change may have confounded 
recovery in several of the study sites. Hence, ecosystem recovery is 
not simply a reversal of the degradation pathway and may take several 
decades to complete or, for some lakes, may not take place at all. By 
assessing ecological change over a decadal to centennial timescale, the 
study highlights the important role that paleolimnology can play in 
establishing a benchmark against which managers can evaluate the degree 
to which their restoration efforts are successful.

## Description of the `.R` files

There are five `.R` files included here which do the following:

 1. `functions.R` contains a `ttplot()` function that acts as a wrapper
 to the `plot()` method for `timetrack()` in the **analogue** package.
 This is used to provide a consistent base-graphics plot for the timetrack
 plots included in the paper.
 2. `core-data-processing.R` contains code to read and process the sediment
 core data presented in the paper.
 3. `wiser-analysis.R` contains the code to do the passive overlay of the
 core samples on to deep and shallow-lake training sets, aka timetracks.
 This work was done as part of the EU Seventh Framework Programme project
 [WISER](http://www.wiser.eu), hence the file name.
 4. `principal-curves.R` fits principal curves to each of the sediment
 core records.
 5. `additive-modelling-prcurve-data-script.R` contains the time series
 via additive modelling code and code to produced the time series figures
 showing the fitted trend in principal curve scores and the derivatives
 of the fitted trend which we used to identify periods of change in the
 diatom community composition.
