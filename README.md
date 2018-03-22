<div id="devex-badge"><a rel="Exploration" href="https://github.com/BCDevExchange/assets/blob/master/README.md"><img alt="Being designed and built, but in the lab. May change, disappear, or be buggy." style="border-width:0" src="https://assets.bcdevexchange.org/images/badges/exploration.svg" title="Being designed and built, but in the lab. May change, disappear, or be buggy." /></a></div>

# Roads & Roadless Areas in B.C.

This repository contains R code that summarizes the length of roads and amount of roadless area in B.C. It supports the 'Roads & Roadless Areas' indicator published on [Environmental Reporting BC](http://www2.gov.bc.ca/gov/content?id=FF80E0B985F245CEA62808414D78C41B).

### Data

This analysis uses the British Columbia [Digital Road Atlas available from the B.C. Data Catalogue]((https://catalogue.data.gov.bc.ca/dataset/bb060417-b6e6-4548-b837-f9060d94743e)) and distributed under the [Access Only - B.C. Crown Copyright](https://www2.gov.bc.ca/gov/content?id=1AAACC9C65754E4D89A118B875E0FBDA) licence. The Digital Road Atlas is a [single, authoritative source of road data for the Province of B.C.](https://www2.gov.bc.ca/gov/content?id=21FFEC94B0AD40818D2D2AF06D522714) Metadata details for the Digital Road Atlas (DRA) are available in PDF format from the [B.C. Data Catalogue](https://catalogue.data.gov.bc.ca/dataset/bb060417-b6e6-4548-b837-f9060d94743e).

The analyses exclude some surface and road types in the [Digital Road Atlas](https://catalogue.data.gov.bc.ca/dataset/bb060417-b6e6-4548-b837-f9060d94743e). Boat (B), overgrown (O) & decomissioned (D) roads are excluded from `TRANSPORT_LINE_SURFACE_CODE` and ferry routes (F, FP, FR, RWA), non-motorized trails (T, TD), road proposed (RP), and road pedestrian mall (RPM) are excluded from `TRANSPORT_LINE_TYPE_CODE`.

The road length analysis sources the [Digital Road Atlas](https://catalogue.data.gov.bc.ca/dataset/bb060417-b6e6-4548-b837-f9060d94743e) from the B.C. Data Catalogue. The roadless area analysis is based on [rasterized input data](https://en.wikipedia.org/wiki/Raster_data), generated with [R](https://www.r-project.org/) code that is also available in [GitHub](https://github.com/bcgov/bc-raster-roads).

### Usage

#### Road Length Analysis

One script is required for the road length and road lnegth by road type analysis:

- road_summary.R

#### Roadless Area Analysis

There are four core scripts that are required for the roadless area analysis, they need to be run in order:

-   01\_load.R
-   02\_clean.R
-   03\_analysis.R
-   04\_output.R

Most packages used in the analysis can be installed from CRAN using `install.packages()`, but you will need to install [`envreportutils`](https://github.com/bcgov/envreportutils) and [`patchwork`](https://github.com/thomasp85/patchwork) using devtools:

```r
install.packages("devtools") # if you don't already have it installed

library(devtools)
install_github("bcgov/envreportutils")
install_github("thomasp85/patchwork")
```

## Getting Help or Reporting an Issue

To report bugs/issues/feature requests, please file an [issue](https://github.com/bcgov-c/roadless-areas-indicator/issues/).

## How to Contribute

If you would like to contribute, please see our [CONTRIBUTING](CONTRIBUTING.md) guidelines.

Please note that this project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md).
By participating in this project you agree to abide by its terms.

## Licence

    Copyright 2017 Province of British Columbia

    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and
    limitations under the License.


This repository is maintained by [Environmental Reporting BC](http://www2.gov.bc.ca/gov/content?id=FF80E0B985F245CEA62808414D78C41B). Click [here](https://github.com/bcgov/EnvReportBC-RepoList) for a complete list of our repositories on GitHub.
