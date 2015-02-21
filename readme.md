## Loading Data

1. Take the STEP file from Maya do the following:
  1. Fill in missing student IDs.
  2. Check for duplicate student IDs.
  3. Clean numeric columns
    - 'n/a' or anything similar should be blank
    - Before is -1
    - Pre is 0
    - 'withdrew' should be deleted from the file
  4. Check that numeric columns and word columns match up in terms of blank values.
  5. Keep only the following columns, and name them as such. Note that the wave columns should be the numeric wave data.
    - id
    - last name
    - first name
    - school
    - grade
    - home room
    - w1
    - w2
    - w3
    - w4
  6. Manually change school IDs to school abbreviations.
  7. Save the file as 'step data.csv' into the 'Data' directory.
2. Make sure you have the 'step goals.csv' file in 'Data'.
3. Run the 'export data for star.r' from masterdash and copy the resulting file into the 'Data' directory as 'benchmark and leap scores.csv'.