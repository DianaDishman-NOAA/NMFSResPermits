# NMFSResPermits
Package for tidying and analyzing NMFS WCR research permit data from the APPS database (https://apps.nmfs.noaa.gov/)

## Documentation

Visit the pkgdown site (https://dianadishman-noaa.github.io/NMFSResPermits/) to learn more about what this package does.

## Setup
To install this package from GitHub:

```
install.packages("devtools") #if needed
devtools::install_github("DianaDishman-NOAA/NMFSResPermits")
library(NMFSResPermits)
```

## Usage

This package is designed specifically to work with data exported from the APPS database (https://apps.nmfs.noaa.gov/) containing data on permits issued by NMFS for scientific research, monitoring, rescue, salvage, collection, harassment, enhancement, propagation, and other activities resulting in take of ESA-listed species within our jurisdiction. Data that comes out of APPS has a very specific structure, and highly specialized utilization. If you want to know more about the data contained in the database please visit the link above, and further resources linked there with information on Federally protected species, permit types, and application instructions. 

You should only be using this package if you first know what APPS data are all about. This package's purpose is to make it easier to do the things you'll commonly want to do to tidy and synthesize APPS data for interpretation.

### Contributors

This package is derived from a set of functioned originally developed in collaboration with Ericka Howard
(https://github.com/ericka-howard;
https://ericka-howard.github.io)

## NOAA README
This repository is a scientific product and is not official communication of the National Oceanic and Atmospheric Administration, or the United States Department of Commerce. All NOAA GitHub project code is provided on an ‘as is’ basis and the user assumes responsibility for its use. Any claims against the Department of Commerce or Department of Commerce bureaus stemming from the use of this GitHub project will be governed by all applicable Federal law. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by the Department of Commerce. The Department of Commerce seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by DOC or the United States Government.

## NOAA License
Software code created by U.S. Government employees is not subject to copyright in the United States (17 U.S.C. §105). The United States/Department of Commerce reserve all rights to seek and obtain copyright protection in countries other than the United States for Software authored in its entirety by the Department of Commerce. To this end, the Department of Commerce hereby grants to Recipient a royalty-free, nonexclusive license to use, copy, and create derivative works of the Software outside of the United States.

NOAA Fisheries

U.S. Department of Commerce | National Oceanographic and Atmospheric Administration | NOAA Fisheries
