## Directory for calibration

***

The current version accepts these three categories of pixels, but the calibration directory can contain more than three subdirectories since the name of the useful subdirectories is specified.

There is no constraint on the name and the number of the calibration image files (the program always reads all the files present in the useful subdirectories).

At the end of the learning phase, three files are created in the calibration directory (the first part of the name these files is the name of the directory with the extensions .png (graph of the discriminant analysis), .txt (results of the discriminant analysis), and .RData (file R containing the results of the discriminant analysis, then used by the image analysis procedure).

***
**Warning!**  the calibration directory must contain the sub-directory "background", "limb" and "lesion" with these names (case-sensitive).
