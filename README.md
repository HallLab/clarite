# clarite
CLeaning to Analysis: Reproducibility-based Interface for Traits and Exposures

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

## Citing CLARITE

1. Lucas AM, et al (2019)
[CLARITE facilitates the quality control and analysis process for EWAS of metabolic-related traits.](https://www.frontiersin.org/article/10.3389/fgene.2019.01240)
*Frontiers in Genetics*: 10, 1240

2. Passero K, et al (2020)
[Phenome-wide association studies on cardiovascular health and fatty acids considering phenotype quality control practices for epidemiological data.](https://www.worldscientific.com/doi/abs/10.1142/9789811215636_0058)
*Pacific Symposium on Biocomputing*: 25, 659