## PhenoCam ROI (drawROI): An interactive Region of Interest (ROI) delineator for the PhenoCam database

In order to extract time series data from a series of images, one needs to 1) delineate a region of interest (ROI); 2) create a mask file identifying pixles of interest; and 3) calculate averaged values of particular bands (e.g. Green Chromatic Coordinate or GCC) over a time period. However, these steps are painstaking and need special accuracy. The PhenoCam ROI tool provides an interactive web interface to facilitate these process of large imagery datasets. This document is a simple guide to explain different elements of the PhenoCam ROI and their functionality.

### Using drawROI
The easiest way to use drawROI is to access it via the PhenoCam website
https://phenocam.nau.edu/drawROI/

### Setting up the Environment
In order to work on drawROI clone the GitHub repository. 

```bash
git clone https://github.com/PhenoCamNetwork/drawROI.git
```
Once the repository is cloned navigate into the directory using `cd` or a file manager/browser. To use drawROI with RStudio open `drawROI.Rproj`. For this to work RStudio must be installed locally. For drawROI to work internet access is necessary because the Shiny code references several drawROI APIs. 

### Notes for Development
The drawROI code can be used on a server or a local machine. If trying to debug issues in the production version of drawROI the version of Shiny should match the version on the PhenoCam server. As of this writting version 1.8.0 was used for development. The R package can be downloaded as a zip file, unpacked, and the location can be manually specified in the code. In the code below replace <version> with the correct value being used for development.

```R
# library(shiny, lib="/pathToLibrary/shiny<version>")
```
if R studio is running in the drawROI directory run the code using.
```R
# library(shiny, lib="/pathToLibrary/shiny<version>")
runApp()
```

The code should run just using these commands. If the goal is to update the code it is necessary to stop the code and run the `runApp()` function again to see the effect of any changes made to the code. 


### Maintanance Information
The R package is currently maintained by the PhenoCam project (https://phenocam.nau.edu/webcam/). 
The R package was developed and maintained by [Bijan Seyednarollah](https://github.com/bnasr) April, 2017.

Most recent release is available from: https://github.com/PhenoCamNetwork/drawROI
