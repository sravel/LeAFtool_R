## Directory for calibration

***

The current version accepts these three categories of pixels, but the calibration folder can contain more than three sub-folders since the name of the useful sub-folders is specified.

There is no constraint on the name and the number of the calibration image files (the program always reads all the files present in the useful sub-folders).

At the end of the learning phase, three files are created in the calibration directory (the first part of the name these files is the name of the directory with the extensions .png (graph of the discriminant analysis), .txt (results of the discriminant analysis), and .RData (file R containing the results of the discriminant analysis, then used by the image analysis procedure).

***
**Warning!**  the calibration folder must contain the sub-folders "background", "limb" and "lesion" with these names (case-sensitive).
