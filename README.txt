This repository contains code and folder structure to recreate the analysis in the manuscript: 

Efficient effort allocation in line transect distance sampling: when to walk further, measure less-often and gain precision
Authors: KA Knights, MA McCarthy, J Camac, G Guillera-Arroita
Accepted for publication, no date (as of Feb 2021).

The manuscript covers the derivation of expressions to optimise the effort allocation between walking (and counting) and measuring distances.  
The expressions calculate the optimal proportion of distances to measure, such that the variance of the density estimate is minimised,
and the predicted benefit from optimising.  
The expressions are tested using simulated data, which is generated and analysed with the scripts and data outlined below.

To recreate the simulations, tables and figures used in the manuscript, conserve the folder structure as:
Ch1MsCodeArchive (root folder, set as working directory)/
	analysis/ (empty, dataframes and tables for analysis are stored here when generated)
	caseStudyData/ (raw data on Grevillea and Brassica distance sampling surveys)
	graphs/ (empty, figures will be written here)
	scripts/ (contains simulation script, functions, analysis scripts
	SimOutput/ (empty, fragmented simulation output is written and compiled here)

Packages used:
Distance
future.apply
tidyverse

Scripts do the following:

simDistArchive.R: 
	generates simulated distance data for conventional line transect sampling and optimised sampling protocols *
	analyses using Distance package
	compiles and saves master dataframe (all simulation results) and summary dataframe (scenario level means and variance)

simDistArchiveFunctions.R:
	contains function definitions for custom functions used in other scripts 
	this script is sourced at the beginning of the simDistArchive.R script

caseStudiesArchive.R:
	loads and cleans case study data
	applies optimisation calculations to calculate variables presented in the paper (case study tables)
	creates and stores the table from which tables in the manuscript are created

Fig1Archive.R:
	creates figure 1 using summary data

Fig2Archive.R: 
	creates figure 2 using master data	

*NOTE: no random seed was implemented - recreated analyses will be similar but not identical to those given in the paper.