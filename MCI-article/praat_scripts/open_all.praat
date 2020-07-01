# This script will open all the files in a given folder.
# Charalambos Themistocleous, 2017
# Date Created: Sat, 10 Dec. 2017


form Open all files in directory
  sentence Directory /Users/charalambosthemistocleous/Desktop/
endform

Create Strings as file list... list 'directory$'*
numberOfFiles = Get number of strings
for ifile to numberOfFiles
	filename$ = Get string... ifile
		# You can add some filename extensions that you want to be excluded to the next line.
		if right$ (filename$, 4) <> ".doc" and right$ (filename$, 4) <> ".xls" and right$ (filename$, 4) <> ".XLS" and right$ (filename$, 4) <> ".TXT" and right$ (filename$, 4) <> ".txt" and right$ (filename$, 4) <> ".dat" and right$ (filename$, 4) <> ".DAT"
			Read from file... 'directory$''filename$'
		endif
	select Strings list
endfor
select Strings list
Remove




