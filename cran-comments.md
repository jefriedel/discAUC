## Resubmission

-   Requested Changes for Manual Checks (part 2)

    -   Removed spaces from \<DOI:\> indicators

    -   Added DOI for Borges et al.

-   Requested Changes for Manual Checks

    -   Removed package name from title

    -   DOIs added, didn't even realize I could do this in DESCRIPTION. Thanks for the recommended fix.

-   Post Autochecks

    -   Windows and Debian failed autochecks because of the same two notes.

    -   DESCRIPTION FILE

        -   Mis-spelled words in description are all correct or proper nouns.
        -   Incorrect URL in vignette corrected to correct DOI link.
        -   Title field updated to follow title case.
        -   DOIs in examp_DD.RD and examp_PD.RD changed from links to DOIs.

-   News.MD file added to .Rbuildignore

## Environment

-   Tested and built in x86_64, mingw32
-   Tested on Linux with Travis CI.
-   R version 4.1.0 (2021-05-18)

## R CMD check results

-   This is the first submission for this package.
-   With R CMD check there were no ERRORs, WARNINGs, or NOTEs.
-   Travis CI default returned no ERRORs, WARNINGs, or NOTEs.
