# ESTS_Trump_Anti_Science
Code and supporting materials used to generate Figure 1 and conduct supporting analyses (produce 10 other related figures) discussed in Frickel and Rea (2020), "Drought, Hurricane, or Wildfire? Assessing the Trump Administration’s Anti-Science Disaster," Engaging Science, Technology, and Society(6), pp. 66-75.

See the paper here: https://estsjournal.org/index.php/ests/article/view/297

See the journal here: https://estsjournal.org/index.php/ests/index

All data is publicly available from the U.S. Office of Personnel Mangement (OPM) here: https://www.opm.gov/data/index.aspx

All visualization and analysis completed in R.

All scripts are built around the following files and file structure. Files included in this repository marked with * . Files created/written by script itself are not listed.

    /your_working_directory
      /a98_17_coded_update.csv *
      /FedScope
        /yyyy_Employment_Cube (where yyyy is a year ranging from 1998 to 2018)
          /DTagy.txt
          /FACTDATA_SEPyyyy.txt (same yyyy as enclosing folder above)
          /FACTDATA_MAR2015.txt (ONLY for yyyy==2015 enclosing folder)
      /Figures
        /Natureand_Resources_ESTS_1998_2017.png *
        /Natureand_Resources_1998_2017.png *
        /Health_and_Social_Welfare_1998_2017.png *
        /Research_Arts_Sciences_1998_2017.png *
        /Crime_and_Law_1998_2017.png *
        /Money_Finance_Banking_1998_2017.png *
        /Work_and_Labor_1998_2017.png *
        /Education_1998_2017.png *
        /Foreign_Affairs_1998_2017.png *
        /Commerce_and_Information_1998_2017.png *
        /Defense_1998_2017.png *
        /Management_and_Administration_1998_2017.png *
      /R_Analysis
        /US_Fed_Employ_98_17_index.R *

All the figures listed above are produced by the script itself; they are included here for reference and for visitors unfamilar/unable to use R but who are still curious about the claims in Frickel and Rea (2020). Note that the script is structure such that additional years of FedScope data from OPM can easily be added as they come available. 
