## inpdfr package

### Purpose of the package
The inpdfr package allows analysing and comparing PDF and/or TXT documents using both classical text mining tools and those from theoretical ecolgy. In the later, words are considered as species and documents as communities, therefore allowing analysis at the community and metacommunity levels.

### How to use the package
Gather some PDF and/or TXT files in a folder. Pointing the working directory to this folder, inpdfr package will extract the text and produce a word occurrence data.frame which will be used to analyse and compare documents. An easy way to start is to use the RGtk2 GUI through the `loadGUI` function. 

### Installation instructions
The package uses XPDF (http://www.foolabs.com/xpdf/download.html) for PDF to text extraction. You need to install XPDF before using `inpdfr` package. Depending on your operating system, you may need to restart your computer after installing XPDF. If you do not want to use XPDF, you can extract the content of your PDF files with the method of your choice and then store the content in TXT files. The only function making use of XPDF is `getPDF` which can be substituted with the `getTXT` function.
install.packages("inpdfr")

### Overview
The inpdfr package provides three cathegories of functions:
- functions to extract and process text into a word-occurrence data.frame,
- functions to analyse the word-occurrence data.frame with standard and ecological tools, and
- functions to use inpdfr through a GTk2 Graphical User Interface.
Further instructions and a complete example are provided in vignette.
