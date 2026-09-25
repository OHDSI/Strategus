# Strategus 1.6.0 Release Questions

This file tracks assumptions and questions raised while preparing the Strategus
1.6.0 release. Resolve or explicitly accept all open items before merging the
release branch into `main`.

## Open

1. The package-maintenance test data contains Celecoxib (cohort 1), Diclofenac
   (cohort 2), and GI bleed (cohort 3). For the representative PheValuator
   module specification, the proposed mapping is cohort 1 as xSpec, cohort 2 as
   xSens, and cohort 3 as both the phenotype under evaluation and the prevalence
   cohort. Is that acceptable for this synthetic fixture, or should additional
   purpose-built cohorts be added?

## Resolved During Preparation

- Release version: `1.6.0`, based on the release branch and existing
  `DESCRIPTION` version.
- Release date: `2026-09-25`.
- Documentation website generation is handled automatically and is excluded
  from the manual release-preparation work.
- No local or remote `v1.6.0` tag or GitHub release existed when preparation
  began.
