# coil20 0.0.0.9002

*July 2026*: Development version cleanup:

- better download handling: with use of HTTPS, binary download mode, and dedicated temporary
  directories.
- cleanup uses explicit ownership tracking so caller directories are never recursively removed.
- bug fix: zip entries are checked for absolute paths and parent-directory traversal before
  extraction. This avoids a scenario where the zip file was `coil20.zip` and the directory that was
  deleted would be `.` (oops). I don't think that could actually have happened, but the possibility
  has been removed.
- `download_coil20()` and `download_coil100()` can now return a matrix-list result with
  `as = "matrix"`.
- tests, lintr, air formatting, and (perhaps unnecessarily) GitHub Actions have been added.

# coil20 0.0.0.9001 and earlier

*March 2 2025*: The URLs for COIL-20 and COIL-100 changed; the package was updated to reflect this.
Data frame construction was also made much faster.

*December 7 2018*: Added support for the COIL-100 database and improved download extraction
cleanup.
