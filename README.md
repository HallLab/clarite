# clarite
CLeaning to Anlaysis: Reproducibility-based Interface for Traits and Exposures

## Overview

The goal of `clarite` is to guide a dataset from the “raw” data stage to EWAS analysis and 
subsequent visualization of results. The package is designed to lead a user through the 
stages of data cleaning: from generating descriptive statistics, to making QC decisions 
informed by the descriptive statistics, to running analyses on the filtered dataset and 
visualizing the results.

## Installation

A development version of the package can be installed using devtools.

`devtools::install_github('HallLab/clarite')`

## Example Workflow

The following image depicts a typical workflow for a project from raw data stage to analysis, in this case an
Environment-Wide Association Study, and results visualization, all of which can be performed using the `clarite`
package. The user starts with raw data and alternates filtering (dark boxes) or summary steps (light boxes) until it is sufficiently “cleaned” and in a stage where analyses can be run.

![Image](https://github.com/RitchieLab/utility/blob/master/personal/ana/images/clariteworkflow.svg)
