# get numerical values skipping meta data and header

fName<- "D20150704_025933_PQC.eol"
sonde1<- read.table(fName, 
                    skip=14, header=FALSE )

# column headings
colNames<- scan( fName, what="a", 
                 skip=11, nlines=1  )
# units
units<- scan( fName, what="a", 
              skip=12, nlines=1  )

# fix  names of times 
colNames[2:4]<- units[2:4]
# change height to be consistent with package
colNames[colNames == "GeoPoAlt"] <- "alt"
# all in lower case
colNames<- tolower( colNames)
names(sonde1)<- colNames

# clean up negative pressures.
ind<- which( sonde1$press < 0 )
sonde1<- sonde1[-ind, ]
# add the data set header information as an attribute. 
headerInfo<- scan( fName, 
                   what="a", nlines=10, sep="\r"  )
attr( sonde1, "metaData") <- headerInfo
attr( sonde1, "units") <- units
save(sonde1, file= "sonde1.rda")



# column headings
fName<- "D20150704_045923_PQC.eol"

# get numerical values skipping meta data and header
sonde2<- read.table(fName, 
                    skip=14, header=FALSE )



colNames<- scan( fName, what="a", 
                 skip=11, nlines=1  )
# units
units<- scan( fName, what="a", 
              skip=12, nlines=1  )

# fix  names of times 
colNames[2:4]<- units[2:4]
# change height to be consistent with package
colNames[colNames == "GeoPoAlt"] <- "alt"
# all in lower case
colNames<- tolower( colNames)
names(sonde2)<- colNames

# clean up negative pressures.
ind<- which( sonde2$press < 0| sonde2$dewpt == -999 )
if( length( ind)>0){
sonde2<- sonde2[-ind, ]
}

# add the data set header information as an attribute. 
headerInfo<- scan( fName, 
                   what="a", nlines=10, sep="\r"  )
attr( sonde2, "metaData") <- headerInfo
attr( sonde2, "units") <- units
save(sonde2, file= "sonde2.rda")

