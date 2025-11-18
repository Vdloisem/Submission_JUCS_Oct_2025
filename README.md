[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.17314925.svg)](https://doi.org/10.5281/zenodo.17314925)

# A Fragmented Field: A Systematic Literature Review on Conceptual Compatibility in Multilingual Software Systems

This repository contains the primary data, analysis scripts, and figures associated with the systematic literature review (SLR) submitted to **J.UCS** on October, 2025.

> **Warning:** This work is currently under peer review. The materials are made available solely to ensure transparency and reproducibility for reviewers and editors. Public reuse, redistribution, or citation is not permitted until formal acceptance and publication of the associated article.

---

### 📁 Repository Structure

The repository is organized into three main modules corresponding to the methodology described in the article:

#### 1. `SLR_Conceptual_Compatibility_Data/`
**Content:** Raw Data, Screening & Extraction Materials.
* **Raw Search Exports:** Contains the unprocessed, raw export files (.CSV) directly obtained from the search tools/databases (e.g., `Elicit - gather-results...csv`). These represent the initial corpus (n=290 for RQ1, n=362 for RQ2) *before* any screening.
* **Screening Data:** Contains the processed spreadsheets used during the manual screening phase (e.g., `PhdSLR-...-SCREENING.csv`).
* **Extraction Data:** Contains the final data extraction spreadsheets with the categorized information from the 82 primary studies.

#### 2. `SLR_Selection_Procedure_Script_And_Outputs/`
**Content:** Selection Audit & Data Preparation (Paper Figure 1).
* **Selection Audit Scripts:** Contains the R scripts (`DecisionGapAudit...R`) used to generate the **quantitative consistency audit** of the screening process.
* **Preparation Scripts:** Includes data alignment scripts (`Alignment21.R`, `Alignment22.R`) used to format and prepare the data for analysis.
* **Outputs:** Generates the ROC and Calibration plots corresponding to **Figure 1** in the manuscript.

#### 3. `SLR_Descriptive_Overview_And_Structural_Analysis/`
**Content:** Descriptive Statistics (Paper Figures 3 & 4).
* Contains the dataset (`SLR_venue_temporal_data.csv`) and R scripts used to analyze the field's demographics.
* **Outputs:** Generates the visualizations for **Figure 3** (Temporal trends) and **Figure 4** (Venue distribution).
  
### 📁 Repository Contents

The repository is organized as follows:

#### Primary Data Files

* `PhdSLR-...-Sub-question2.1-SCREENING(2).csv`: Raw screening data for Sub-question 2.1 (corresponds to **RQ1** in the paper).
* `PhdSLR-...-Sub-question2.2-SCREENING(2).csv`: Raw screening data for Sub-question 2.2 (corresponds to **RQ2** in the paper).

#### R Scripts for Analysis

* `Alignment21.R` & `Alignment22.R`: Scripts used for data alignment and preparation for each sub-question.
* `DecisionGapAudit21.R` & `DecisionGapAudit22.R`: Scripts to perform and visualize the quantitative consistency audit of the screening process.
* `SLR_Conceptual_Compatibility.Rproj`: RStudio project file to ensure a consistent working environment for running the analysis scripts.

#### Generated Output Directories

* `/decision_gap_outputs_SQ2.1/`: Contains figures and other outputs related to the consistency audit for SQ2.1.
* `/decision_gap_outputs_SQ2.2/`: Contains figures and other outputs related to the consistency audit for SQ2.2.
* `/slr_outputs_sq21/`: Contains general analysis figures for SQ2.1.
* `/slr_outputs_sq22/`: Contains general analysis figures for SQ2.2.

#### Other Files

* `LICENSE.txt`: The license file for the repository's contents.
* `README.md`: This file.

---

### License and Terms of Use

This repository and its contents are released for review purposes only.

The data, figures, and derived materials may not be reused, redistributed, or cited until the official publication of the associated article.

#### License

This work is licensed under the [Creative Commons Attribution-NonCommercial-NoDerivatives 4.0 International License (CC BY-NC-ND 4.0)](http://creativecommons.org/licenses/by-nc-nd/4.0/).

You are free to:

* **Share** — copy and redistribute the material in any medium or format.

Under the following terms:

* **Attribution** — You must give appropriate credit.
* **NonCommercial** — You may not use the material for commercial purposes.
* **NoDerivatives** — You may not distribute modified versions of the material.

The license will be re-evaluated and possibly relaxed after the article's acceptance.

---

### Contact

For any questions or access requests, please contact:

**Mikel Vandeloise**
* University of Namur, Belgium
* [mikel.vandeloise@unamur.be]
