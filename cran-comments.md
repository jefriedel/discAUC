## Version update

-   Increment to Version 1.0.0
    
    -   Squashed bug.
    
    -   Accounted for changes in tidy expression evaluations.

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

-   Initial build in x86_64, mingw32
-   Version 1.0.0 build in aarch64-apple-darwin20
-   R version 4.2.1 (2022-06-23)

## R CMD check results

-   This is an updated for this package.
-   There were no errors, warnings, or notes.
-   No errors, warnings, or notes from check on win-builder.r-project.org
