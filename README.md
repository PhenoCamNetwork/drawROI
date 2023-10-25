## PhenoCam ROI: An interactive Region of Interest (ROI) delineator for the PhenoCam database

In order to extract time series data from a series of images, one needs to 1) delineate a region of interest (ROI); 2) create a mask file identifying pixles of interest; and 3) calculate averaged values of particular bands (e.g. Green Chromatic Coordinate or GCC) over a time period. However, these steps are painstaking and need special accuracy. The PhenoCam ROI tool provides an interactive web interface to facilitate these process of large imagery datasets. This document is a simple guide to explain different elements of the PhenoCam ROI and their functionality.

### Using PhoenoCam ROI
The easiest way to use PhoenoCam ROI is to access it via the PhoenoCam website
https://phenocam.nau.edu/drawROI/

### Notes for Development
The PhoenoCam ROI code can be used on a server or a local machine. In order to run the code on a local machine the version of Shiny must match the version on the PhoenoCam server. As of this writting that was version 1.4.0.2. The R package can be downloaded as a zip file and unpacked and the location can be manually specified in the code. 

```R
library(shiny, lib="/pathToLibrary/shiny_1_4_0_2")
```
if R studio is running in the drawROI directory run the code using.
```R
library(shiny, lib="/pathToLibrary/shiny_1_4_0_2")
runApp()
```

The code should run just using these commands. If the goal is to update the code it is necessary to stop the code and run the `runApp()` function again to see the effect of any changes made to the code. 


### Maintanance Information
The R package is currently maintained by the PhoenoCam project (https://phenocam.nau.edu/webcam/). 
The R package was developed and maintained by [Bijan Seyednarollah](https://github.com/bnasr) April, 2017.

Most recent release is available from: https://github.com/bnasr/drawROI
