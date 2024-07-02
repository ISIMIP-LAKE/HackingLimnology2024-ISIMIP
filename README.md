# Hacking Limnology 2024 Workshop: Modeling the Impacts of Climate Change on Lake Ecosystems (ISIMIP)

To adapt management for climate change impacts on lakes, there is an increased need to project future changes in lake thermal structure and resulting changes in lake biogeochemistry. Consistent ensemble modeling studies to project global water quality changes are needed for policy-relevant documents, such as the Assessment Reports of the Intergovernmental Panel on Climate Change (IPCC). The international Lake Sector of the Inter-Sectoral Impact Model Intercomparison Project (ISIMIP) aims to fulfill this need for simulating climate change impacts on lakes using an ensemble of lake models and climate change scenarios. ISIMP prescribes lake simulations driven by climate forcing from gridded observations and different Earth system models under various representative greenhouse gas concentration pathways (RCPs), all consistently bias-corrected on a global grid. The ISIMIP Lake Sector is the largest international effort to project future water temperature, thermal structure, and ice phenology of lakes at local and global scales and paves the way for future simulations of the impacts of climate change on water quality and biogeochemistry in lakes.

-----

:computer: [Workshop information](https://aquaticdatasciopensci.github.io/day1-climate-change/)

:email: [Contact](mailto:rladwig@ecos.au.dk)

:teacher: **Organisers:** Daniel Mercado (Catalan Institute for Water Resources Research), Ana Isabel Ayala-Zamora (Uppsala University), Robert Ladwig (Aarhus University)

-----

## Description

This workshop material highlights how to access and use modeled ISIMIP data for climate change assessment research for lakes. The focus will be on changes in a lake's thermal structure and hydrodynamics. The scripting will include accessing data, manipulating data, visualizing data, numerical modeling, coupling multiple data sources, and extracting information from data.

## What will this workshop material cover?

  - Accessing and visualising global simulated lake data
  - Accessing local lake data
  - Analysing local lake trends for physical variables
  - Coupling local lake model output with custom code for water quality modeling
  - Coupling lake model output with watershed simulations for land use analysis

## Prerequisites

### Word of caution
  This workshop material was tested in R v.4.3.2.

### Use Github and your local R setup
   You can clone or download files from this [Github repository](https://github.com/ISIMIP-LAKE/HackingLimnology2024-ISIMIP) (click the green "Code" button and select the "Clone" or "Download ZIP" option).
  You’ll need R, preferably a GUI of your choice (e.g., Rstudio) and these packages:
  ```
  install.packages("ncdf4")
  install.packages("rLakeAnalyzer")
  install.packages("tidyverse")
  install.packages("reshape2")
  install.packages("patchwork")
  ```
-----
