# R Minor

This repository would be helpful to any HUD-designated Continuum of Care looking for a way to get more out of their HMIS data.

If you are interested in using this code, please be sure you have read the licensing and then follow the instructions here to get started. All the code in this repository is written using R and R Studio. Please consult the book [R for Data Science](https://r4ds.had.co.nz/) for help getting started with R and R Studio.

Once this is ready for production, I will be running the HUD CSV export and the RMisc file daily and saving it to the data directory in my R Project. There will be no actual data backed up here, only the code that interacts with the data. 

Data I'm using:

1. The [HUD CSV export](https://hudhdx.info/Resources/Vendors/5_1_2/HMISCSVSpecifications6_12.pdf). Every HMIS should be able to export this regardless of what HMIS vendor you use.
2. Custom report in our HMIS found in ART > Public > Ohio Balance of State > COHHIO Only > RMisc. While your reporting needs may be different, this report contains data we need for our reporting that is not included in the HUD CSVs:
   * SPDAT Scores
   * County data (Enrollment level)
   * Correct User Creating for Enrollments, Entry Exit Type
   * User contact data
   * Provider level: full names (beyond the 50 character limit), Grant Type, Facility Type, Operational, Uses SP (All specific to ServicePoint.)
   * Veteran data for Coordinated Entry (Custom fields)
   * Housing Offers data (Custom fields)
3. "Regions" file that simply lists Counties with their Regions. As a Balance of State, we have 17 sub-regions we sometimes group our reporting on.

I hope you can find useful code here. Please share feedback in the issues or email me at genelledenzin at cohhio dot org! 
