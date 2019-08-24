My Chapter-wise solutions to the problems in the book Structure and Interpretation of Computer Programs(SICP).

Also added a library for handling PGM image format. It is a very basic and simple format, which is why I chose it. The library is required for some exercises in Chapter 2.

Steps for using the library:
1. Find/Create a pgm file. The library assumes that there is a rogers image(an image of the MIT Founder) present in the same folder, named **founder2.pgm**. Change this to your file's address.
2. Use the funtions below, besides etc. to modify the **rogers** image(which will contain whatever image whose address you provided in step 1).
3. When writing the image, use the function **write-binary-pgm-file**, such as **write-binary-pgm-file file-name rogers**. This will copy your image to file-name.
