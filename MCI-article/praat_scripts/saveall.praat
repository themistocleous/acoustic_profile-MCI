# This script will save all the files in objects window in a given folder.
# Charalambos Themistocleous, 2008
# Date Created: Sunday, 20 Nov. 2008

form Save all files in directory
  sentence Directory /
endform

select all
clearinfo
total = numberOfSelected ()
for i to total
	object'i' = selected (i)
endfor

for i to total
	select object'i'
	numberCompl$ = selected$ ()
	numberobject$ = right$ (numberCompl$, 11)
        txtFile$ = "" 
	x$ = left$ (numberCompl$, 1)
	if x$ = "S"
		type$ = "Sound"
		extension$ = "wav"
               txtFile$ = replace$(numberCompl$, "Sound ", "", 0)
               printline 'txtFile$'
               Write to WAV file... 'directory$'/'txtFile$'.'extension$'

	elif x$ = "T"
		# type$ = "textgrids"
		extension$ = "TextGrid"
               txtFile$ = replace$(numberCompl$, "TextGrid ", "", 0)
               printline 'txtFile$'
               Write to text file... 'directory$'/'txtFile$'.'extension$'

	elif x$ = "P"
		# type$ = "pitch"
		extension$ = "Pitch"
               txtFile$ = replace$(numberCompl$, "Pitch ", "", 0)
               printline 'txtFile$'
               Write to text file... 'directory$'/'txtFile$'.'extension$'
               
	elif x$ = "M"
		# type$ = "Manipulation"
		extension$ = "Manipulation"
               txtFile$ = replace$(numberCompl$, "Manipulation ", "", 0)
               printline 'txtFile$'
               Write to text file... 'directory$'/'txtFile$'.'extension$'

	else
		# type$ = "misc"
		extension$ = "txt"
               printline 'objName$'
               Write to text file... 'directory$'/'objName$'.'extension$'
	endif

endfor