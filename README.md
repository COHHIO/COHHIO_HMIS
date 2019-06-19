# COHHIO HMIS

This repository would be helpful to any HUD-designated Continuum of Care looking for a way to get more out of their HMIS data. While there are some reports here, this repository's most important function is getting data from HMIS to a tidy form that can be used in other projects (and repositories). 

If you are interested in using this code, welcome! Also please be sure you have read the licensing and then follow the instructions here to get started. All the code in this repository is written using R and R Studio. Please consult the book [R for Data Science](https://r4ds.had.co.nz/) for help getting started with R and R Studio.

Once this is ready for production, I will be running the HUD CSV export and the RMisc file every few days and saving it to the data directory in my R Project. There will be no actual data backed up here, only the code that interacts with the data. 

Data I'm using:

1. The [HUD CSV export](https://hudhdx.info/Resources/Vendors/5_1_2/HMISCSVSpecifications6_12.pdf). Every HMIS should be able to export this regardless of what HMIS vendor you use.
2. Custom report I built, located in our system at: ART > Public > Ohio Balance of State > COHHIO Only > RMisc. While your reporting needs may be different, this report contains data we need for our reporting that is not included in the HUD CSVs:
   * SPDAT Scores
   * County data (Enrollment level)
   * Entry Exit Type (Specific to ServicePoint.)
   * User contact data
   * Provider level: full names (beyond the 50 character limit), Grant Type, Facility Type, Operational, Uses SP (All specific to ServicePoint.)
   * Veteran data for Coordinated Entry (Custom fields)
   * Housing Offers data (Custom fields)
3. A static "Regions" file that simply lists Counties with their Regions. As a Balance of State, we have 17 sub-regions we sometimes group our reporting on.

You may not need the extra data files, so whatever you use from this repository will need to be adjusted to leave it out.

The workflow I'm currently using (which I expect to become less tedious eventually) is the following:

1. Download the latest HUD CSV export, copy the files into the data directory of this project.
2. Run and save to .xlsx format the RMisc report from our system, save it to the data directory of this project.
3. Run the first script, beginning with 00_...R, then the other scripts that begin with a number, incrementally by the number, clearing the environment between each script.

At this point, the other scripts should work unless there are directions stating otherwise. Sometimes other data files are required for various scripts, and those will be described in the comments.

I hope you can find useful code here. Please share feedback in the issues or email me at genelledenzin at cohhio dot org! 
