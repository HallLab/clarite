# clarite
CLeaning to Anlaysis: Reproducibility-based Interface for Traits and Exposures

## Overview

The goal of clarite is to guide a dataset from the “raw” data stage to EWAS analysis and 
subsequent visualization of results. The package is designed to lead a user through the 
stages of data cleaning: from generating descriptive statistics, to making QC decisions 
informed by the descriptive statistics, to running analyses on the filtered dataset and 
visualizing the results.

## Installation

A development version of the package can be installed using devtools.

```devtools::install_github('HallLab/clarite')```

There also exists a beta version of a GUI for clarite, ```clarite.py```, which is written in Python 2.7 
and will call R functions in the background.
