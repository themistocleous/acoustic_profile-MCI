#Charalambos Themistocleous
# It is based on Daniel Hirst's "analyse_tier.praat" code.
# This scripts extracts pitch, intensity, and duration. The scripts opens a set of sounds located in a directory and the corresponding text grids. The results are saved in a csv file. 


script_name$ = "information.praat"
date$ = date$()


#define parameters used in the script
form analyse tier
	sentence investigator Charalambos Themistocleous
	sentence Sound_folder /Users/haristhemistocleous/Documents/articles/words_experiment/sounds_words_experiment/
	sentence TextGrid_folder /Users/haristhemistocleous/Documents/articles/words_experiment/sounds_words_experiment/
	sentence outputfile /Users/haristhemistocleous/Documents/articles/words_experiment/sounds_words_experiment.csv
	word Analysis_tier phones
	word Sound_extension .wav
	word TextGrid_extension .TextGrid
	real Time_step 0 (= automatic)
	word Undefined_value NA
	boolean calculate_pitch yes
	boolean automatic_min_max yes
	natural min_pitch 45
	natural max_pitch 750
	optionmenu pitch_units: 1
		option Hertz
		option octaves
		option semitones
	comment Dor log scales:
	optionmenu pitch_reference: 1
		option 1
		option 100
		option 200
		option 440
		option speaker_median
	boolean calculate_intensity yes
	comment For formants
	boolean calculate_formants yes
	natural Number_of_formants 5
	natural Maximum_formant 5500
	positive Window_length 0.05
	positive Pre_emphasis 50
	comment For point tier
	positive Analysis_window 0.1
endform
fileappend "'outputfile$'"  item'tab$'name'tab$'label'tab$'duration'tab$'f0_min'tab$'f0_mean'tab$'f0_max'tab$'intensity_min'tab$'intensity_mean'tab$'intensity_max'tab$'f1'tab$'f2'tab$'f3'newline$'
clearinfo
default_minimum_pitch = 60
default_maximum_pitch = 750

#Read in list of sound files
mySounds = Create Strings as file list... sounds
... 'sound_folder$'/*'sound_extension$'
nSounds = Get number of strings
item = 0

myTextGrids = Create Strings as file list... TextGrids
... 'textGrid_folder$'/*'textGrid_extension$'
nTextGrids = Get number of strings

#check if TextGrid file exists for each sound
# and call treatment
	for iSound from 1 to nSounds
		select mySounds
		sound_name$ = Get string... iSound
		textGrid_name$ = sound_name$ - sound_extension$
		... + textGrid_extension$
		sound$ = sound_folder$ + "/" + sound_name$
		textGrid$ = textGrid_folder$ + "/" + textGrid_name$
		if fileReadable(textGrid$)
			call treatment
		else
			printline # Cannot find TextGrid file
			...  ['textGrid_name$']
		endif
	endfor
else
	if nSounds = 0
		printline Folder ['sound_folder$'] doesn't contain
		... any files with extension ['sound_extension$']
	elsif nTextGrids = 0
		printline Folder ['textGrid_folder$'] doesn't contain
		... any files with extension ['textGrid_extension$']
	endif
endif

#Remove file list
select mySounds
plus myTextGrids
Remove

exit

#subroutine treatment
procedure treatment
	Read from file... 'sound$'
	mySound = selected("Sound")
	name$ = selected$("Sound")
	sound_duration = Get total duration
	Read from file... 'textGrid$'
	myTextGrid = selected("TextGrid")
	textGrid_duration = Get total duration
	if sound_duration != textGrid_duration
		plus mySound
		Scale times
		printline # TextGrid and Sound have different durations
		printline # TextGrid has been scaled to the duration of Sound
	endif
#find number of analysis tier
	select myTextGrid
	call find_analysis_tier
	if tier
#create analysis objects
		select mySound
		printline # file : 'name$'
# - pitch
		if calculate_pitch
			if automatic_min_max
				myPitch = To Pitch... time_step
				... default_minimum_pitch default_maximum_pitch
				call automatic_min_max_pitch
			else
				myPitch = To Pitch... time_step 
				... min_pitch max_pitch
			endif
			median_pitch = Get quantile... 0 0 0.5 Hertz
#			printline # min_pitch: 'min_pitch:0';
#			... median_pitch: 'median_pitch:0'
#			... max_pitch 'max_pitch:0';
			if pitch_reference$ = "speaker_median"
				pitch_reference = median_pitch
			else
				pitch_reference = 'pitch_reference$'
			endif
		endif
# - intensity
		if calculate_intensity
			select mySound
			myIntensity = To Intensity... min_pitch
			... time_step Yes
		endif
		if calculate_formants
			select mySound
			myFormants = To Formant (burg)... time_step
			... number_of_formants maximum_formant
			... window_length pre_emphasis
		endif
#Get time values of beginning and end of each interval
		select myTextGrid
		if isIntervalTier
			nIntervals = Get number of intervals... tier   
		else
			nIntervals = Get number of points... tier
		endif
		for iInterval from 1 to nIntervals
			select myTextGrid
			if isIntervalTier
				label$ = Get label of interval... tier
				... iInterval
				start = Get starting point... tier
				... iInterval
				end = Get end point... tier iInterval
			else
				label$ = Get label of point... tier
				... iInterval
				mid = Get time of point... tier iInterval
				start = mid - analysis_window/2
				if start< 0
					start = 0
				endif
				end = mid + analysis_window/2
				if end > sound_duration
					end = sound_duration
				endif
			endif
#calculate parameters for each non empty interval						
			if label$ != "" and label$ != "_" and
			... label$ != "#"
				item = item+1
# -	duration
				duration = 1000*(end - start)
				call set_undefined duration
				duration$ = value$
# -	pitch
				if calculate_pitch
					select myPitch
					f0_min = Get minimum... start end
					... Hertz Parabolic
					call convert_f0 f0_min
					f0_min = value
					call set_undefined f0_min
					f0_min$ = value$
					f0_mean = Get mean... start end Hertz
					call convert_f0 f0_mean
					f0_mean = value
					call set_undefined f0_mean
					f0_mean$ = value$
					f0_max = Get maximum... start end
					... Hertz Parabolic
					call convert_f0 f0_max
					f0_max = value
					call set_undefined f0_max
					f0_max$ = value$
				endif

# - 	intensity
				if calculate_intensity
					select myIntensity
					intensity_min = Get minimum... start
					... end Parabolic
					call set_undefined intensity_min
					intensity_min$ = value$
					intensity_mean = Get mean... start end
					... energy
					call set_undefined intensity_mean
					intensity_mean$ = value$
				intensity_max = Get maximum... start
					... end Parabolic
					call set_undefined intensity_max
					intensity_max$ = value$								
				endif

# -	formants
				if calculate_formants
					select myFormants
					f1 = Get mean... 1 start end Hertz
					call set_undefined f1
					f1$ = value$
					f2 = Get mean... 2 start end Hertz
					call set_undefined f2
					f2$ = value$
					f3 = Get mean... 3 start end Hertz
					call set_undefined f3
					f3$ = value$
				endif

#print out results
				fileappend "'outputfile$'"  'item''tab$''name$''tab$''label$''tab$''duration$''tab$''f0_min$''tab$''f0_mean$''tab$''f0_max$''tab$''intensity_min$''tab$''intensity_mean$''tab$''intensity_max$''tab$''f1$''tab$''f2$''tab$''f3$''newline$'
			endif
		endfor

#Remove objects
		if calculate_pitch
			select myPitch
			Remove
		endif
		if calculate_intensity
			select myIntensity
			Remove
		endif
		if calculate_formants
			select myFormants
			Remove
		endif
	else
#print warning if TextGrid has no analysis tier
		printline ###TextGrid ['name$'] has no tier
		... ['analysis_tier$']
	endif
#Remove Sound and TextGrid
	select mySound
	plus myTextGrid
	Remove
endproc

procedure convert_f0 value
	value = value/pitch_reference
	if pitch_units$ = "octaves"
		value = log2(value)
	elsif pitch_units$ = "semitones"
		value = 12*log2(value)
	endif
endproc

procedure set_undefined value
	if value = undefined
		value$ = undefined_value$
	elsif pitch_units = 1
		value$ = "'value:0'"
	else 
		value$ = "'value:3'"	
	endif
endproc

procedure automatic_min_max_pitch
	q25 = Get quantile... 0 0 0.25 Hertz
	q75 = Get quantile... 0 0 0.75 Hertz
	min_pitch = 0.75*q25
	max_pitch = 1.5*q75
	Remove
	select mySound
	myPitch = To Pitch... time_step min_pitch
	... max_pitch
endproc

procedure find_analysis_tier
	nTiers = Get number of tiers
	tier = 0
	for iTier from 1 to nTiers
		tier_name$ = Get tier name... iTier
		if tier_name$ = analysis_tier$
			tier = iTier
			isIntervalTier = Is interval tier... tier
		endif
	endfor
endproc

