# COHHIO HMIS

An open-source Homeless Management Information System (HMIS) custom reporting project

This repository would be helpful to any HUD-designated Continuum of Care looking to get more out of their HMIS data. While there are some reports here, this repository's most important function is getting data from HMIS to a tidy form that can be used in other projects (and repositories). Think of it like a staging area for other projects.

### About

This is an open source project released under the GNU AGPLv3 license. See LICENSE for more details or visit the official GNU page at http://www.gnu.org/licenses/agpl-3.0.html.

All the code in this repository is written using R in R Studio. Please consult the book [R for Data Science](https://r4ds.had.co.nz/) for help getting started with R and R Studio. Also feel free to do the [R for HMIS Admins](https://github.com/kiadso/rTraining#r-for-hmis-admins) training series for an easy landing.

### Data Sources

1. The [HUD CSV export](https://hudhdx.info/Resources/Vendors/HMIS%20CSV%20Specifications%20FY2020%20v1.6.pdf) - Every HMIS should be able to export this regardless of HMIS vendor.
2. Other data as necessary, to supplement what is available in the Export.  

While your reporting needs may be different, this report contains data we need for our reporting that is not included in the HUD CSVs. These include but are not limited to:

   * SPDAT Scores
   * County (Enrollment level)
   * Entry Exit Type (specific to ServicePoint)
   * Users' Default Providers 
   * Full Provider and Organization Names (beyond the 50 character limit)
   * Grant Type (specific to ServicePoint)
   * Veteran data for Coordinated Entry (Custom fields)
   * Housing Offers (Custom fields for Veteran Active List)
   * COVID-19 data

You may not need to supplement the data in your HUD CSV export, so whatever you use from this repository will need to be adjusted to leave it out. Or you may need a different set of miscellaneous data which will need to be incorporated to fit your needs. Other CoCs using this repository would need to work out what other data would be needed and how that data could be obtained. 

If you want to know exactly what data I'm using, see below, and email me for more specifics if you need it.

### Workflow

The workflow I'm currently using (which I expect to become less tedious eventually) is the following:

0. Be sure you have the following directories in your R project:
   * **data** (to keep your HUD CSV files and whatever other data you will use)
   * **images** (see #4)
   * **random_data**
1. Download and unzip your HUD CSV export into the **data** directory of the R Studio project. Permanently delete the .zip once it has been extracted so the PII doesn't remain on your hard drive.
2. You will probably want to build out your own data that's not available in the Export or you may find that the Export has everything you need for now. Either way, download any other necessary data into the **data** directory.
3. Run 00_first_run.R, which will check that you have all the main packages installed, then run 00_daily_update.R which will run all the other scripts that begin with a number. You can modify yours to fit your needs.
4. Doing 3. will create a .RData image file for each script that runs. Each image file has an extension of .RData and will be dropped into the **images** folder.
5. The 00_copy_images.R script will then copy the necessary files to R minor and R minor elevated (2 Shiny apps we use).

I hope you can find useful code here. Please share feedback in the issues or email me at genelledenzin at cohhio dot org! 

### Security

No HMIS data is ever included in this repository. To make this code work, you will need to supply your own HMIS data. You are responsible for securing your HUD CSV export on your computer and ensuring that it is not compromised using whatever security measures you use for that locally.



