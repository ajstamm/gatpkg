# Archive for historical information on GAT's development

This folder contains prior versions of GAT prior to converting GAT to an R package. These files are provided for historical use only and are no longer being maintained or supported. The only version of GAT that we support currently is the R package. If you have questions about GAT's history, please email epht@health.ny.gov.

## GAT for R

This script was written by Gwen Babcock with documentation by Gwen Babcock and Tom Talbot. Version 1.34 was released on August 28, 2015. The original release also included zipped versions of R and all required packages. The download here contains only the script and documentation. This version of GAT contained the following features: 

* aggregate by geographic centroid
* aggregate by similar areas (ratio of two values)
* calculate a rate/ratio
* enter minimum value for up to two variables
* save results as shapefile and KML file

The script was designed to run in R versions 2.9-3.0 and may run in later versions, but that has not been tested. It required the following packages: tcltk, stats, graphics, grDevices, utils, datasets, methods, base, Matrix_1.0-12, lattice_0.20-15, rgdal_0.8-10, foreign_0.8-53, svDialogs_0.9-54, svGUI_0.9-54, classInt_0.1-20, e1071_1.6-1, class_7.3-8, RColorBrewer_1.0-5, maptools_0.8-26, rgeos_0.2-19, sp_1.0-11, boot_1.3-9, coda_0.16-1, deldir_0.0-22, grid_3.0.1, LearnBayes_2.12, MASS_7.3-26, nlme_3.1-109, spdep_0.5-62, splines_3.0.1  

Download [GAT for R v1.34](NYS_GAT_R-1-34.zip)


## GAT for SAS

This set of scripts were written by Gwen Babcock and Sanjaya Kumar with documentation by Gwen Babcock and Tom Talbot. The download here contains only the script and documentation. The original release also included zipped sample shapefiles. Version 4.1 was released on April 1, 2015. This version of GAT contained the following features: 

* aggregate by geographic centroid
* aggregate by population centroid
* aggregate by similar areas (ratio of two values)
* calculate a rate/ratio
* enter minimum value for up to two variables
* save results as shapefile and KML file

The script was designed to run in SAS versions 9.1-9.4 and may run in later versions, but that has not been tested.

Download [GAT for SAS v4.1](NYS_GAT_SAS-4-1.zip)


## Publications that have used or cited prior iterations of GAT

* Sherman RL, Henry KA, Tannenbaum SL, Feaster DJ, Kobetz E, Lee DJ. Prev Chronic Dis.   
    DOI: http://dx.doi.org/10.5888/pcd11.130264   
    Referenced R v1.2
* Werner AK, Strosnider HM. Spatial and Spatio-temporal Epidemiology 2020;33.   
    DOI: https://doi.org/10.1016/i.sste.2020.100339   
    Used SAS v1.31
* Werner AK, Strosnider H, Kassinger C, Shin M. J Public Health Manag Pract.   
    DOI: https://doi.org/10.1097/PHH.0000000000000686   
    Used SAS v1.31
* Boscoe FP, Talbot TO, Kulldorff M. Geospat Health. Published 2016 Apr 18.   
    DOI: https://doi.org/10.4081/gh.2016.304   
    Used SAS v1.31
* Boothe VL, Fierro LA, Laurent A, Shih M. Global Diaspora News. Published 3/28/2020.   
    https://www.globaldiasporanews.com/a-tool-to-improve-communitv-health-and-advance-health-equity/   
    Used R v1.33 
* California Department off Public Health & Mariposa County Health Department. Community Assessment for Public Health Emergency Response (CASPER) addressing the California drought—Mariposa County, California, November, 2015.
    https://www.cdph.ca.gov/Programs/CCDPHP/DEODC/CDPH%20Document%20Library/Mariposa%202015%20CASPER%20report.pdf  
    Uses SAS v1.31
  
## Presentations about legacy versions of GAT

* Talbot, T & LaSelva, G. "GIS Tools for Sharing Health Data and Protecting Patient Confidentiality." NYGeoCon, Saratoga Springs, NY, November 12-13, 2013.  
    https://www.nysgis.net/Docs/NYGeoCon2013/Slides/Wed_0245_0415/Sharing4_HealthData.pdf   
    Used R v1.2



## Previous versions of the GAT package for R

In 2019, Abigail Stamm began transitioning GAT to an R package. For a full list of authors and acknowledgements, please see the package documentation. The first public version was 1.61.0, released in August 2020 when this repository went live. Versioning for the package began with 1.35, to remain consistent with the original R script. To see how the package differs from the previous versions of GAT listed above, please see the package [change log](../compiles/Readme.md).

All publicly released versions of the R package can be found in the [compiles](../compiles/) folder.

