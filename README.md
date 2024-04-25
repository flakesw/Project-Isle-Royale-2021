# Models and code for "A trophic cascade mediates rates of carbon sequestration in a warming climate"
 
Project maintained by Sam Flake (swflake@ncsu.edu) with collaborators Nathan De Jager and Robert Scheller.

Original code and data are released under a CC-BY license (free to use and modify, but please cite the forthcoming manuscript and appropriate LANDIS-II extensions). Some data from other sources are included in this repository in order to run some scripts; they are public domain and are shared under their relevant copyrights. Some data with unclear licenses with respect to sharing modified data have been removed from this repository. Please contact Sam Flake for advice on running the scripts in this repository and obtaining appropriate data.

Four folders are included here:
1. Models -- this folder contains the input files needed to run the LANDIS-II model. 
  - LANDIS inputs -- input files needed to run the model
    - browse -- this folder contains text input files that specify forage preferences, moose population parameters, and predation levels
    - input rasters -- this folder contains the initial input raster files needed by LANDIS-II and its extensions
    - Installers -- this folder contains two special installers for modified branch versions of the Net Ecosystem and Carbon (NECN) v6.10 and Biomass Browse v2.0 extensions. For future use of this model, we recommend upgrading to NECN v7. Contact Sam Flake for details. All other necessary extensions may be obtained from landis-ii.org
    - NECN files -- this folder contains text and tabular inputs needed by NECN; primarily these files are climate data and text files needed to parse the climate data.
    - other inputs -- this folder contains some text input files needed for LANDIS-II extensions, including basic species information and text files for the Base Wind extension and output extensions
    - Spunup inputs -- this folder contains ecosystem and vegetation state data after the 20-year spinup process. These are used to start the  model in its 2020 state rather than the original 1999 vegetation survey state.
  -  Model templates -- folders containing scenario files and executables needed to run the models. These folders are intended to be copied to a folder within the "Models" folder, e.g. "./Models/Model_runs/". The location of the model .bat file is important, because the location of the input files is relative to the executable. 
     - Model template folders -- each folder contains a model with a given climate scenario and predation level. It contains the following files:
       - NECN_Succession_climate_.txt -- This input file provides the parameters for the NECN succession extension
       - Scenario_climate__predation_.txt -- This file specifies which input files are used for the LANDIS-II extensions, including defining with succession extension and browse extension versions are being used
       - Scenario_climate_predation_.bat -- This executable runs the model
2. Parameterization -- this folder contains R scripts and data used for parameterizing the model, including deriving the soil and hydrologic inputs, the initial communities, and climate scenarios
3. Calibration -- this folder contains R scripts used to calibrate and validate the model
4. Analysis -- this folder contains R scripts used for analysis of model outputs as well as some postprocessed model output products, figures, and maps. Most of the scripts are designed to be run on the raw model outputs (rather than intermediate products), which are too large to host easily. 
	









