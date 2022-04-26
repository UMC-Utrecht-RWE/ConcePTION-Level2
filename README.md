
<!-- PROJECT LOGO -->
<br />
<p align="left">
  <a href="https://github.com/vjolahoxhaj/Level-2-checks">
    <img src="images/conception_logo.png" alt="Logo" width="250" height="60">
  </a>
  </p>
  
 <h3 align="center">Level 2 checks</h3>
 <p align="center"> R scripts to check the logical relationships between the ConcePTION CDM tables. </p>
 
<!-- TABLE OF CONTENTS -->
<details open="open">
  <summary>Table of Contents</summary>
  <ol>
    <li>
      <a href="#about-the-project">About The Project</a>
    </li>
    <li>
      <a href="#level-2-checks">Level 2 checks</a>
    </li>
    <li>
      <a href="#getting-started">Getting Started</a>
      <ul>
        <li><a href="#prerequisites">Prerequisites</a></li>
        <li><a href="#installation">Installation</a></li>
        <li><a href="#Data characterization study links">Data characterization study links</a></li> 
        <li><a href="#installation">Current version</a></li>
      </ul>
    </li>
    <li><a href="#license">License</a></li>
    <li><a href="#contact">Contact</a></li>
  </ol>
</details>

<!-- ABOUT THE PROJECT -->
## About The Project

[ConcePTION](https://www.imi-conception.eu) aims to build an ecosystem that can use Real World Data (RWD) to generate Real World Evidence (RWE) that may be used for clinical and regulatory decision making. RWE is required to address the big information gap of medication safety in pregnancy.   

ConcePTION is designed to be a learning healthcare system (LHS). In the ConcePTION LHS, we have agreed upon a study-independent syntactically harmonized common data model and aim to assess the quality and fitness for purpose of data in this CDM in a study-independent way (for quality and completeness) and in study design and research question-specific ways (for fitness for purpose).

ConcePTION CDM tables:
<p align="left">
  <a href="https://github.com/vjolahoxhaj/Level-2-checks">
    <img src="images/conception_cdm_tables.png" alt="Logo" width="450" height="250">
  </a>
  </p>

<!-- Level 2 checks -->
## Level 2 checks

**Aims of Level 2 quality checks:**      
**1.**	To assess internal consistency both within and between tables of the [ConcePTION CDM]((https://docs.google.com/spreadsheets/d/1hc-TBOfEzRBthGP78ZWIa13C0RdhU7bK/edit#gid=413205035)) instance for each DAP.       

***Level 2 data checks assess the logical relationship and integrity of data values within a variable or between two or  more variables within and between tables.  Examples of this type of check include: observations occurring before birth date, observations occurring after a recorded death date, parents aged 12 years old or younger etc.*** 

**The level 1 checks are divided in 8 major steps:**   

1.	Event dates before birth date.   
2.	Event dates after date of death.   
3.	Event dates outside observation periods.   
4.	Subjects observed in a table of interest without a corresponding record in the `PERSONS` table.   
5.	Observations associated with a visit_occurrence_id which occur before the visit_start_date.    
6.	Observations associated with a visit_occurrence_id which occur after the visit_end_date.   
7.	Observations associated with a visit_occurrence_id for which the associated person_id differs from that in the `VISIT_OCCURRENCE` table.   
8.	Subjects indicated in `PERSON_RELATIONSHIPS` as the parent of a child with a birth_date less than 12 years prior to the recorded birth_date of the associated child.   

<!-- GETTING STARTED -->
## Getting Started

Follow the steps below to run Level 2 checks in your data.   

### Prerequisites

R version 4.1.0 (2021-05-18)   

### Installation

If you have applied the Level 1 on your data follow the shorten version of the installation steps.

1.	Click the green button **Code** on the top of the GitHub page, download the ZIP folder using the **Download ZIP** button and extract the contents. Remember where you stored the extracted ZIP folder on your computer, you will use it later.   
2.	Create a main folder with the name of your project (`Project_name`). We suggest to use a clear and short name, only using lower-case letters.
3.	Create the folder `Data characterisation` inside the main folder `Project_name`. 
4.	Copy the folder `Level_2_checks_to_be_deployed_v2.0` from the extracted folder zip file that we downloaded in step 1 inside the new folder `Data characterisation`. 
5.	Create a folder named `CDMInstances` inside the main folder `Project_name`, which will be used to store the .csv files representing the CDM tables.
6.	Create a folder with the name of your project inside the `CDMInstances` folder (use the same name as in Step 2).
7.	Copy all your .csv data files (that follow the Common Data Model) within the folder created in Step 7.
8.	Go to the script *99_path.R* in the folder `Level_2_checks_to_be_deployed_v2.0/Study Scripts`, and change the variable Studyname (line 6 in the code) to the name of your project. Make sure that the name of the folder created in the folder `CDMInstances` and the name of the variable match exactly. Save it.
9.	Open the *script_to_run_all.R*  script in **R-studio** and you are ready to run level 1 checks.


Short version:

1. Click the green button **Code** on the top of the GitHub page, download the ZIP folder using the **Download ZIP** button and extract the contents. Remember where you stored the extracted ZIP folder on your computer, you will use it later.        
2. Copy the folder `Level_2_checks_to_be_deployed_v2.0` from the extracted folder zip file that we downloaded in step 1 inside the folder `Data characterisation`. 
3. In the folder `Level_2_checks_to_be_deployed_v2.0/Study Scripts`, go to the script 99_path.R and change the variable Studyname(line 6) to the name of your project. Make sure that the name of the folder you have created in the folder `CDMInstances` and the name of the variable match exactly. Save it.         
4. Open the to_run.R script inside the folder `Level_2_checks_to_be_deployed_v2.0/Study Scripts` and you are ready to run the Level 2 checks.    


**Folder structure**

* [Project_name](./Project_name) (Main Folder)

    * [CDMInstances](./CDMInstances)
        * [Project_name](./CDMInstances/Project_name)
            * [files.csv](./CDMInstances/Project_name/files.csv) (here your files)    
     * [Data characterisation](./Data characterisation) 
       * [Level_1_checks_to_be_deployed_v5.2](./Data characterisation/Level_1_checks_to_be_deployed_v5.2)
       * [Level_2_checks_to_be_deployed2.0](./Data characterisation/Level_2_checks_to_be_deployed2.0)
       * [Level_3_to_be_deployed1.0](./Data characterisation/Level_3_to_be_deployed1.0)

**Folder structure example**

<p align="left">
  <a href="https://github.com/vjolahoxhaj/Level-1-checks">
    <img src="images/example_folderStructure.png" alt="example" width="284" height="304">
  </a>
  </p>
  

### Data characterization study links   

[Level 1 checks](https://github.com/vjolahoxhaj/Level-1-checks?organization=vjolahoxhaj&organization=vjolahoxhaj): Checking the integrity of the ETL procedure.     
[Level 2 checks](https://github.com/IMI-ConcePTION/Level-2-checks): Checking the logical relationship of the CDM tables.    
[Level 3 checks](https://github.com/IMI-ConcePTION/Level-3-checks): Benchamrking across DAPs and external sources.     

### Current version

The current version of the script is 2.0.

<!-- LICENSE -->
## License

Distributed under the BSD 2-Clause License License. See `LICENSE` for more information.

<!-- CONTACT -->
## Contact

Rutger van den Bor - R.M.vandenBor@umcutrecht.nl    
Vjola Hoxhaj - v.hoxhaj@umcutrecht.nl     

Project Link: [https://github.com/IMI-ConcePTION/Level-2-checks](https://github.com/IMI-ConcePTION/Level-2-checks)

