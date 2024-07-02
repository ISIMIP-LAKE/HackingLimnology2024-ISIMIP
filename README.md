# Hacking Limnology 2024 workshop: Modeling the impacts of Climate Change on Lake Ecosystems (ISIMIP)

To adapt management for climate change impacts on lakes, there is an increased need to project future changes in lake thermal structure and resulting changes in lake biogeochemistry. Consistent ensemble modeling studies to project global water quality changes are needed for policy-relevant documents, such as the Assessment Reports of the Intergovernmental Panel on Climate Change (IPCC). The international Lake Sector of the Inter-Sectoral Impact Model Intercomparison Project (ISIMIP) aims to fulfill this need for simulating climate change impacts on lakes using an ensemble of lake models and climate change scenarios. ISIMP prescribes lake simulations driven by climate forcing from gridded observations and different Earth system models under various representative greenhouse gas concentration pathways (RCPs), all consistently bias-corrected on a global grid. The ISIMIP Lake Sector is the largest international effort to project future water temperature, thermal structure, and ice phenology of lakes at local and global scales and paves the way for future simulations of the impacts of climate change on water quality and biogeochemistry in lakes.

-----

:email: [Contact](mailto:rladwig@ecos.au.dk)
:teacher: Daniel Mercado (Catalan Institute for Water Resources Research), Ana Isabel Ayala-Zamora (Uppsala University), Robert Ladwig (Aarhus University)
-----

## Description

This workshop material applies the lake model GLM to a real-world case, e.g. model calibration, output post-processing and interpreting water quality results. This workshop is intended for all skill levels, although prior knowledge of R is helpful.

## What will this workshop material cover?

  - Running GLM in R
  - Manipulating the configuration files
  - Visualising the results
  - Calibrating water temp. and oxygen parameters
  - Evaluating effects of external drivers on in-lake processes
  - Checking your phytoplankton

## Prerequisites

### Word of caution
  This workshop example was tested on General Lake Model (GLM) Version 3.1.1. The setup may not work using older and more recent versions of GLM.

### Use Github and your local R setup
   Alternatively, you can clone or download files from this [Github repository](https://github.com/robertladwig/GLM_workshop) (click the green "Code" button and select the "Clone" or "Download ZIP" option).
  You’ll need R (version >= 3.5), preferably a GUI of your choice (e.g., Rstudio) and these packages:
  ```
  require(devtools)
  devtools::install_github("robertladwig/GLM3r", ref = "v3.1.1")
  devtools::install_github("USGS-R/glmtools")
  install.packages("rLakeAnalyzer")
  install.packages("tidyverse")
  ```
Update: If the GLM3r installation does not work for you and you're experiencing problems when running ```run_glm()```, then you can try installing:

  ```
  # macOS/Linux
  devtools::install_github("GLEON/GLM3r", ref = "GLMv.3.1.0a3")
  # Windows:
  devtools::install_github("GLEON/GLM3r")
  ```

Windows users will then run v3.1.0a4 whereas Unix users use v3.1.0b1. Unfortunately, some differences between these versions can occur in the model outputs. We are still working on the GLM3r and glmtools packages to keep them updated with new GLM-AED2 releases and to implement new features for model evaluation. This Windows binary sometimes freezes, which can stop the calibration routine. If this happens, please 'stop' the command and re-run it. If you experience problems on macOS (we tested the package only for macOS Catalina) with error messages like 'dyld: Library not loaded', you can also try the following approaches:

   - use and try ``` devtools::install_github("robertladwig/GLM3r", ref = "v3.1.0a3") ``` to install GLM3r
   - or install the missing libraries, e.g. by using ['brew'](https://brew.sh): ``` brew install gcc ```, ``` brew install netcdf```, ``` brew install gc```; afterwards you should install this GLM3r version: ```devtools::install_github("robertladwig/GLM3r", ref = "v3.1.0a3-2")``` (we are working on fixing all these macOS-specific problems)

-----
