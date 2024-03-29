#This script does everything necessary to build main.tex into a pdf. Some tricks were necessary because of eps->pdf conversions required by the chem numbering package. Note this will not work with automatic atom.io LaTeX builder.

#First swap out proprietry font names to open source in all figures ISO-8859-15

for i in $( find Figures -name "*.eps" ); do
   mv $i $i.dirty
   iconv -c -f WINDOWS-1252 -t UTF-8 $i.dirty |\
   sed 's+Times-Bold+NimbusSanL-Bold+g' |\
   sed 's+Times-Roman+NimbusSanL-Regu+g' |\
   sed 's+Times+NimbusSanL-Regu+g' |\
   sed 's+Helvetica-BoldOblique+NimbusSanL-BoldItal+g' |\
   sed 's+Helvetica-Oblique+NimbusSanL-ReguItal+g' |\
   sed 's+Helvetica-Bold+NimbusSanL-Bold+g' |\
   sed 's+Helvetica-Bold-iso+NimbusSanL-Bold+g' |\
   sed 's+Helvetica+NimbusSanL-Regu+g' |\
   sed 's+Helvetica-iso+NimbusSanL-Regu+g' |\
   sed 's+Symbol+StandardSymL+g' > $i.clean |\
   iconv -c -f UTF-8 -t WINDOWS-1252 $i.clean > $i
   rm $i.dirty
   rm $i.clean
   echo $i
done
