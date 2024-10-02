Strategus
=========

[![Build Status](https://github.com/OHDSI/Strategus/actions/workflows/R_CMD_check_Hades.yaml/badge.svg?branch=main)](https://github.com/OHDSI/Strategus/actions/workflows/R_CMD_check_Hades.yaml/badge.svg?branch=main)
[![codecov.io](https://codecov.io/github/OHDSI/Strategus/coverage.svg?branch=main)](https://codecov.io/github/OHDSI/Strategus?branch=main)

Introduction
============
Strategus is an R package for coordinating and executing analytics using [OHDSI HADES](https://ohdsi.github.io/Hades/) modules. Please see the [Introduction To Strategus](https://ohdsi.github.io/Strategus/articles/IntroductionToStrategus.html) for more details.

Features
========
- Design study analysis specifications
- Execute study analysis specifications against the OMOP CDM

Technology
============
Strategus is an R package.

System Requirements
============
Requires R (version 4.2.0 or higher). Installation on Windows requires [RTools](https://cran.r-project.org/bin/windows/Rtools/). Libraries used in Strategus require Java. Strategus requires you to setup your GitHub Personal Access Token as described [here](https://ohdsi.github.io/Hades/rSetup.html#GitHub_Personal_Access_Token)

Installation
=============
1. See the instructions [here](https://ohdsi.github.io/Hades/rSetup.html) for configuring your R environment, including RTools, Java and your GitHub Personal Access Token.

2. In R, use the following commands to download and install Strategus:

  ```r
  install.packages("remotes")
  remotes::install_github("ohdsi/Strategus")
  ```
User Documentation
==================
Documentation can be found on the [package website](https://ohdsi.github.io/Strategus).

PDF versions of the documentation are also available:
* Package manual: [Strategus.pdf](https://raw.githubusercontent.com/OHDSI/Strategus/main/extras/Strategus.pdf)
* Vignette: [Creating HADES Modules](https://github.com/OHDSI/Strategus/raw/main/inst/doc/CreatingModules.pdf)

Support
=======
* Developer questions/comments/feedback: <a href="http://forums.ohdsi.org/c/developers">OHDSI Forum</a>
* We use the <a href="https://github.com/OHDSI/Strategus/issues">GitHub issue tracker</a> for all bugs/issues/enhancements

Contributing
============
Read [here](https://ohdsi.github.io/Hades/contribute.html) how you can contribute to this package.

License
=======
Strategus is licensed under Apache License 2.0

Development
===========
Strategus is being developed in R Studio.

### Development status

Strategus is under active development.
