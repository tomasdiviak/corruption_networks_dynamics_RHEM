# **Understanding the Mechanisms that Drive Relational Events Dynamics and Structure in Corruption Networks** 
 Code used for data processing, description, and RHEM modelling in Diviák, T. & J. Lerner. Understanding the Mechanisms that Drive Relational Events Dynamics and Structure in Corruption Networks. *Journal of Quantitative Criminology* DETAILS TBD (2025). [DOI TBD](DOI TBD)

**SarcladDescriptives.R**, **AirbusDescriptives.R**, **ASLDescriptives.R**: descriptive analysis of a network focusing on its structure, visualisation, and analysing the frequency of events over time. Note that the core-periphery results reported in the study are based on the corresponding routine in UCINET rather than on the attached code due to its R version (xUCINET) not fully supporting it at the time of writing. The analysed datasets are based on the data coded from the corresponding Statement of Facts and they are stored in two sheets ('edgelist' and 'attributes') in each of *Sarclad_relEvents.xlsx*, *Airbus_relEvents.xlsx*, and *ASL_relEvents.xlsx*.

**SarcladProcessing.R**, **AirbusProcessing.R**, **ASLProcessing.R**: data processing of the originally coded data (see the data for descriptive analysis above) into the eventnet format for Relational Hyperevent Model (RHEM). This produces *SarcladRHEM.csv*, *AirbusRHEMAll.csv*, and *ASLRHEM.csv* respectively.

**SarcladModels2.R**, **AirbusModels2.R**, **ASLModels2.R**: fitting Cox Proportional Hazard to the data generated in event from the processed data files, specifically to *SarcladRHEM_EMAILS_RANDOM_SENDER.txt*, *AirbusRHEMAll_EMAILS_RANDOM_SENDER.txt*, and *ASLRHEM_EMAILS_RANDOM_SENDER.txt* respectively.

**SarcladModels_bootstrap.R**, **AirbusModels_bootstrap.R**, **ASLModels_bootstrap_new.R**: bootstrapping-based procedure for checking the robustness of the estimates reported in the appendix of the study.

**Sarclad_config2_JL2.txt**, **Airbus_config_JL_2.txt**, **ASL_config2.txt**: configuration files for specifying the output of eventnet (for details on eventnet and RHEM in general, see [Jürgen Lerner's GitHub page](https://github.com/juergenlerner/eventnet).
