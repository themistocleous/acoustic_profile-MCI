# This script will save all the files in objects window in a given folder.
# Charalambos Themistocleous, 2017
# Date Created: Sat, 10 Dec. 2017

form Save all files in directory
  sentence Directory /Users/haristhemistocleous/Desktop/speech_platform_eng/sounds/
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
	type$ = "Sound"
	extension$ = "wav"
    txtFile$ = replace$(numberCompl$, "Sound ", "", 0)
    printline 'txtFile$'
    Save as raw 16-bit little-endian file... 'directory$'/'txtFile$'.raw
endfor