# Geographic Aggregation Tool (GAT)

The geographic aggregation tool (GAT) was developed by the New York State Department of Health (NYSDOH) Environmental Public Health Tracking (EPHT) Program with funding from the CDC administered through Health Research Incorporated. GAT is maintained by Abigail Stamm at the NYSDOH. 

GAT aggregates, or dissolves, geographic areas based on numeric values for each area, such as case or population numbers.

To download the most recently compiled version of GAT, click [here](compiles/gatpkg_1.61.0.tar.gz?raw=TRUE). To download prior versions of GAT, check the [compiles](compiles) folder.

Click for [instructions on how to install GAT](presentations/gat_install_instructions.pdf), including a list of required R packages.

### Why create GAT

Health outcome maps with fine geographic resolution can be misleading due to random fluctuations in disease rates caused by small numbers. In some cases these maps can also inadvertently disclose confidential data. To overcome these limitations we developed GAT to join neighboring geographic areas together until a user defined population and/or number of cases is reached. GAT can be used to produce maps for the public at the finest geographic resolution practicable.

### How GAT works

GAT reads in a polygon shapefile. The shapefile must contain at minimum a character variable that uniquely identifies areas and a numeric variable to sum for aggregation. A series of dialog boxes allows the user to select: 

1. a variable to uniquely identify areas
2. one or two aggregation variables
3. optionally, a variable of areas within which merging will be preferred (ex. county)
4. the value (sum) to which the selected aggregation variable(s) should be aggregated
5. the preferred aggregation method: closest geographic or population-weighted centroid, least value, or ratio of two values

GAT outputs two shapefiles, one containing aggregated areas and one containing a crosswalk between the original areas and the aggregated areas. GAT also outputs a log, several maps to help you identify potential issues in the aggregation process, and, optionally, a KML file.

The package for GAT includes a tutorial to learn the tool, an embedded map file to use when testing several of the package functions, and several other documents describing how GAT works and how to evaluate the resulting aggregated shapefile.

### Learn more about GAT

View an [introduction to GAT](https://youtu.be/BRxhay4FhGI?t=1096) presented at the 2020 NAHDO conference. Download the [NAHDO conference slides](presentations/NYS_GAT_NAHDO_2020.pdf).

View an elevator pitch on the [development of GAT](https://www.youtube.com/watch?v=r_wRMpqZcE8&list=PL4IzsxWztPdnviiir8c5GaarXd9vZ2TIP&index=24) presented at the 2021 useR! conference. Download the [slides in PDF](presentations/NYS_GAT_useR_2021.pdf) or [slides in PPT](presentations/NYS_GAT_useR_2021.ppt). 

### Documentation available within gatpkg

**Technical Notes**: How each merge type works, compactness ratio, thinning  
**Setting up GAT**: How to install R and GAT (variant of [GAT installation instructions](presentations/gat_install_instructions.pdf))  
**Shapefile specifications**: Requirements for a shapefile you plan to process with GAT  
**Tutorial**: A basic run-through of GAT  
**Evaluating results**: Ways to identify and address issues with your aggregated areas  
**Troubleshooting**: How to handle issues that arise in GAT  
**Change Log**: Changes across GAT versions, notably from the 2015 script to the 2020 package 

### Disclaimer

GAT is provided as is. 

**GAT is still in beta testing.** We welcome feedback on what worked well, suggestions for improvement, and bugs you encounter. Report all issues via the "Issues" tab or by emailing [NYSDOH EPHT](mailto:epht@health.ny.gov?subject=[GAT in R]).

GAT was written in R-2.9.2 under Windows XP and was revised and converted to a package in R-3.4.3 under Windows 10 using RStudio-1.4.1103 and devtools-2.3.2. The latest version of GAT was compiled in R-3.6.1 and runs in R-3.5.3 through R-4.0.3. 

If you are interested in GAT's history, including the original SAS and R scripts, visit the [archive](archive/). 


