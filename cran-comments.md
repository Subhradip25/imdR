## R CMD check results

0 errors | 0 warnings | 1 note

* checking HTML version of manual: `tidy` not found on this system.
  This is a local environment issue. CRAN servers have `tidy` installed.

## Notes addressed since last submission

* ORCID removed from DESCRIPTION. Both bare format (0000-0003-2320-2979)
  and URL format (https://orcid.org/0000-0003-2320-2979) are rejected by
  R Under development (r90058). This appears to be a known bug in r-devel.
  The ORCID will be added back once the bug is resolved upstream.

* Spelling notes: IMD (India Meteorological Department) and Gridded are
  standard domain-specific meteorological terms, not misspellings. IMD is
  explicitly defined in the Description field. These terms are used
  consistently throughout the package documentation and are well established
  in the Indian meteorological literature.

* inst/WORDLIST added in previous submission for domain-specific terms
  (IMD, Gridded, NetCDF, GeoTIFF).

## Test environments

* Local: Pop!_OS 24.04 LTS, R 4.6.0
* win-builder: r-devel-windows-x86_64
* Debian: r-devel-linux-x86_64-debian-gcc

## Downstream dependencies

None — this is a new package.
