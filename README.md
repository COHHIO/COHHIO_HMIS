# COHHIO HMIS

An open-source Homeless Management Information System (HMIS) custom reporting project

This repository would be helpful to any HUD-designated Continuum of Care looking for a way to get more out of their HMIS data. While there are some reports here, this repository's most important function is getting data from HMIS to a tidy form that can be used in other projects (and repositories). Think of it like a staging area for other projects.

### About

This is an open source project released under the GNU AGPLv3 license. See LICENSE for more details or visit the official GNU page at http://www.gnu.org/licenses/agpl-3.0.html.

All the code in this repository is written using R and R Studio. Please consult the book [R for Data Science](https://r4ds.had.co.nz/) for help getting started with R and R Studio.

### Data Sources

1. The [HUD CSV export](https://hudhdx.info/Resources/Vendors/5_1_2/HMISCSVSpecifications6_12.pdf) - Every HMIS should be able to export this regardless of HMIS vendor.
2. I built a custom report in our system called "RMisc". While your reporting needs may be different, this report contains data we need for our reporting that is not included in the HUD CSVs:
   * SPDAT Scores
   * County (Enrollment level)
   * User Creating (because it is incorrect in our vendor's export)
   * Entry Exit Type (Specific to ServicePoint)
   * User Contact 
   * Full Provider Names (beyond the 50 character limit), Grant Type, Facility Type, Operational, Uses SP (All specific to ServicePoint)
   * Provider Addresses, Youth Beds (because they are incorrect in our vendor's export)
   * Veteran data for Coordinated Entry (Custom fields)
   * Housing Offers (Custom fields)

You may not need a miscellaneous (RMisc) file, so whatever you use from this repository will need to be adjusted to leave it out. Or you may need a different miscellaneous data file which will need to be incorporated to fit your needs.

### Workflow

The workflow I'm currently using (which I expect to become less tedious eventually) is the following:

0. Be sure you have the following directories in your R project:
   * data (to keep your HUD CSV files and whatever other data you will use)
   * images (see 4.)
1. Download and unzip the HUD CSV export into the data directory of the R Studio project.
2. Run and save the RMisc report in .xlsx format to the data directory of this project.
3. Run the first script, beginning with 00_...R, then the other scripts that begin with a number, incrementally by the number.
4. Doing 3. will create 1 .RData file per script that you run and drop it in the images folder.
5. Your other projects (like R minor or R minor elevated) each have data folders with symbolic links in them, each pointing to any relevant .RData files.

This will one day be written as a package, hopefully.

I hope you can find useful code here. Please share feedback in the issues or email me at genelledenzin at cohhio dot org! 

### Security

No HMIS data is ever included in this repository. To make this code work, you will need to supply your own HMIS data. You are responsible for securing your HUD CSV export on your computer and ensuring that it is not compromised using whatever security measures you use for that locally.



