# This script measures the formant frequencies F1 to F5 using a step of 5 from the vowel onset (0) to the vowel offset (100).
# The script is based on the script "collect_formant_data_from_files.praat" by Mietta Lennes (https://lennes.github.io/spect/#views)
# Copyright 4.7.2003 Mietta Lennes

# Modified by Charalambos Themistocleous 08.08.2014 
# Bug fixes on 11.30.2014


form Analyze formant values from labeled segments in files
	comment Directory of sound files
	text sound_directory /Users/haristhemistocleous/Documents/programs/alz-rj/vowels/cut_sounds_wav/MCI_all/
	sentence Sound_file_extension .wav
	comment Directory of TextGrid files
	text textGrid_directory /Users/haristhemistocleous/Documents/programs/alz-rj/vowels/cut_sounds_wav/MCI_all/
	sentence TextGrid_file_extension .TextGrid
	comment Full path of the resulting text file:
	text resultfile /Users/haristhemistocleous/Documents/programs/alz-rj/vowels/cut_sounds_wav/MCI_all.csv
	comment Which tier do you want to analyse?
	sentence Tier segment
	comment Formant analysis parameters
	positive Time_step 0.01
	integer Maximum_number_of_formants 5
	positive Maximum_formant_(Hz) 5500_(=adult female)
	positive Window_length_(s) 0.025
	real Preemphasis_from_(Hz) 50
endform

Create Strings as file list... list 'sound_directory$'*'sound_file_extension$'
numberOfFiles = Get number of strings

# Check if the result file exists:
if fileReadable (resultfile$)
	pause The result file 'resultfile$' already exists! Do you want to overwrite it?
	filedelete 'resultfile$'
endif

titleline$ = "Filename	Segment	Duration	F1.5	F2.5	F3.5	F4.5	F5.5	F1.10	F2.10	F3.10	F4.10	F5.10	F1.15	F2.15	F3.15	F4.15	F5.15	F1.20	F2.20	F3.20	F4.20	F5.20	F1.25	F2.25	F3.25	F4.25	F5.25	F1.30	F2.30	F3.30	F4.30	F5.30	F1.35	F2.35	F3.35	F4.35	F5.35	F1.40	F2.40	F3.40	F4.40	F5.40	F1.45	F2.45	F3.45	F4.45	F5.45	F1.50	F2.50	F3.50	F4.50	F5.50	F1.55	F2.55	F3.55	F4.55	F5.55	F1.60	F2.60	F3.60	F4.60	F5.60	F1.65	F2.65	F3.65	F4.65	F5.65	F1.70	F2.70	F3.70	F4.70	F5.70	F1.75	F2.75	F3.75	F4.75	F5.75	F1.80	F2.80	F3.80	F4.80	F5.80	F1.85	F2.85	F3.85	F4.85	F5.85	F1.90	F2.90	F3.90	F4.90	F5.90	F1.95	F2.95	F3.95	F4.95	F5.95'newline$'"
fileappend "'resultfile$'" 'titleline$'

# Go through all the sound files, one by one:

for ifile to numberOfFiles
	filename$ = Get string... ifile
	# A sound file is opened from the listing:
	Read from file... 'sound_directory$''filename$'
	# Starting from here, you can add everything that should be
	# repeated for every sound file that was opened:
	soundname$ = selected$ ("Sound", 1)
	To Formant (burg)... time_step maximum_number_of_formants maximum_formant window_length preemphasis_from
	# Open a TextGrid by the same name:
	gridfile$ = "'textGrid_directory$''soundname$''textGrid_file_extension$'"
	if fileReadable (gridfile$)
		Read from file... 'gridfile$'
		# Find the tier number that has the label given in the form:
		call GetTier 'tier$' tier
		numberOfIntervals = Get number of intervals... tier
		# Pass through all intervals in the selected tier:
		for interval to numberOfIntervals
			label$ = Get label of interval... tier interval
			if label$ <> ""
				# if the interval has an unempty label, get its start and end:
				start = Get starting point... tier interval
				end = Get end point... tier interval
				duration = end - start


				time5 = start +  (duration * 0.5)
				time10 = start + (duration * 0.10)
				time15 = start + (duration * 0.15)
				time20 = start + (duration * 0.20)
				time25 = start + (duration * 0.25)
				time30 = start + (duration * 0.30)
				time35 = start + (duration * 0.35)
				time40 = start + (duration * 0.40)
				time45 = start + (duration * 0.45)
				midpoint = (start + end) / 2
				time55 = start + (duration * 0.55)
				time60 = start + (duration * 0.60)
				time65 = start + (duration * 0.65)
				time70 = start + (duration * 0.70)
				time75 = start + (duration * 0.75)
				time80 = start + (duration * 0.80)
				time85 = start + (duration * 0.85)
				time90 = start + (duration * 0.90)
				time95 = start + (duration * 0.95)

				# get the formant values at that interval
				select Formant 'soundname$'

				# 5%
				f1_5 = Get value at time... 1 time5 Hertz Linear
				f2_5 = Get value at time... 2 time5 Hertz Linear
				f3_5 = Get value at time... 3 time5 Hertz Linear
				f4_5 = Get value at time... 4 time5 Hertz Linear
				f5_5 = Get value at time... 5 time5 Hertz Linear

				# 10%
				f1_10 = Get value at time... 1 time10 Hertz Linear
				f2_10 = Get value at time... 2 time10 Hertz Linear
				f3_10 = Get value at time... 3 time10 Hertz Linear
				f4_10 = Get value at time... 4 time10 Hertz Linear
				f5_10 = Get value at time... 5 time10 Hertz Linear

				# 15%
				f1_15 = Get value at time... 1 time15 Hertz Linear
				f2_15 = Get value at time... 2 time15 Hertz Linear
				f3_15 = Get value at time... 3 time15 Hertz Linear
				f4_15 = Get value at time... 4 time15 Hertz Linear
				f5_15 = Get value at time... 5 time15 Hertz Linear

				# 20%
				f1_20 = Get value at time... 1 time20 Hertz Linear
				f2_20 = Get value at time... 2 time20 Hertz Linear
				f3_20 = Get value at time... 3 time20 Hertz Linear
				f4_20 = Get value at time... 4 time20 Hertz Linear
				f5_20 = Get value at time... 5 time20 Hertz Linear

				# 25%
				f1_25 = Get value at time... 1 time25 Hertz Linear
				f2_25 = Get value at time... 2 time25 Hertz Linear
				f3_25 = Get value at time... 3 time25 Hertz Linear
				f4_25 = Get value at time... 4 time25 Hertz Linear
				f5_25 = Get value at time... 5 time25 Hertz Linear

				# 30%
				f1_30 = Get value at time... 1 time30 Hertz Linear
				f2_30 = Get value at time... 2 time30 Hertz Linear
				f3_30 = Get value at time... 3 time30 Hertz Linear
				f4_30 = Get value at time... 4 time30 Hertz Linear
				f5_30 = Get value at time... 5 time30 Hertz Linear

				# 35%
				f1_35 = Get value at time... 1 time35 Hertz Linear
				f2_35 = Get value at time... 2 time35 Hertz Linear
				f3_35 = Get value at time... 3 time35 Hertz Linear
				f4_35 = Get value at time... 4 time35 Hertz Linear
				f5_35 = Get value at time... 5 time35 Hertz Linear

				# 40%
				f1_40 = Get value at time... 1 time40 Hertz Linear
				f2_40 = Get value at time... 2 time40 Hertz Linear
				f3_40 = Get value at time... 3 time40 Hertz Linear
				f4_40 = Get value at time... 4 time40 Hertz Linear
				f5_40 = Get value at time... 5 time40 Hertz Linear

				# 45%
				f1_45 = Get value at time... 1 time45 Hertz Linear
				f2_45 = Get value at time... 2 time45 Hertz Linear
				f3_45 = Get value at time... 3 time45 Hertz Linear
				f4_45 = Get value at time... 4 time45 Hertz Linear
				f5_45 = Get value at time... 5 time45 Hertz Linear

				# 50%
				f1_50 = Get value at time... 1 midpoint Hertz Linear
				f2_50 = Get value at time... 2 midpoint Hertz Linear
				f3_50 = Get value at time... 3 midpoint Hertz Linear
				f4_50 = Get value at time... 4 midpoint Hertz Linear
				f5_50 = Get value at time... 5 midpoint Hertz Linear

				# 55%
				f1_55 = Get value at time... 1 time55 Hertz Linear
				f2_55 = Get value at time... 2 time55 Hertz Linear
				f3_55 = Get value at time... 3 time55 Hertz Linear
				f4_55 = Get value at time... 4 time55 Hertz Linear
				f5_55 = Get value at time... 5 time55 Hertz Linear

				# 60%
				f1_60 = Get value at time... 1 time60 Hertz Linear
				f2_60 = Get value at time... 2 time60 Hertz Linear
				f3_60 = Get value at time... 3 time60 Hertz Linear
				f4_60 = Get value at time... 4 time60 Hertz Linear
				f5_60 = Get value at time... 5 time60 Hertz Linear

				# 65%
				f1_65 = Get value at time... 1 time65 Hertz Linear
				f2_65 = Get value at time... 2 time65 Hertz Linear
				f3_65 = Get value at time... 3 time65 Hertz Linear
				f4_65 = Get value at time... 4 time65 Hertz Linear
				f5_65 = Get value at time... 5 time65 Hertz Linear

				# 70%
				f1_70 = Get value at time... 1 time70 Hertz Linear
				f2_70 = Get value at time... 2 time70 Hertz Linear
				f3_70 = Get value at time... 3 time70 Hertz Linear
				f4_70 = Get value at time... 4 time70 Hertz Linear
				f5_70 = Get value at time... 5 time70 Hertz Linear

				# 75%
				f1_75 = Get value at time... 1 time75 Hertz Linear
				f2_75 = Get value at time... 2 time75 Hertz Linear
				f3_75 = Get value at time... 3 time75 Hertz Linear
				f4_75 = Get value at time... 4 time75 Hertz Linear
				f5_75 = Get value at time... 5 time75 Hertz Linear


				# 80%
				f1_80 = Get value at time... 1 time80 Hertz Linear
				f2_80 = Get value at time... 2 time80 Hertz Linear
				f3_80 = Get value at time... 3 time80 Hertz Linear
				f4_80 = Get value at time... 4 time80 Hertz Linear
				f5_80 = Get value at time... 5 time80 Hertz Linear


				# 85%
				f1_85 = Get value at time... 1 time85 Hertz Linear
				f2_85 = Get value at time... 2 time85 Hertz Linear
				f3_85 = Get value at time... 3 time85 Hertz Linear
				f4_85 = Get value at time... 4 time85 Hertz Linear
				f5_85 = Get value at time... 5 time85 Hertz Linear


				# 90%
				f1_90 = Get value at time... 1 time90 Hertz Linear
				f2_90 = Get value at time... 2 time90 Hertz Linear
				f3_90 = Get value at time... 3 time90 Hertz Linear
				f4_90 = Get value at time... 4 time95 Hertz Linear
				f5_90 = Get value at time... 5 time95 Hertz Linear

				# 95%
				f1_95 = Get value at time... 1 time95 Hertz Linear
				f2_95 = Get value at time... 2 time95 Hertz Linear
				f3_95 = Get value at time... 3 time95 Hertz Linear
				f4_95 = Get value at time... 4 time95 Hertz Linear
				f5_95 = Get value at time... 5 time95 Hertz Linear
# Save result to text file:
				resultline$ = "'soundname$'	'label$'	'duration'	'f1_5'	'f2_5'	'f3_5'	'f4_5'	'f4_5'	'f1_10'	'f2_10'	'f3_10'	'f4_10'	'f5_10'	'f1_15'	'f2_15'	'f3_15'	'f4_15'	'f5_15'	'f1_20'	'f2_20'	'f3_20'	'f4_20'	'f5_20'	'f1_25'	'f2_25'	'f3_25'	'f4_25'	'f5_25'	'f1_30'	'f2_30'	'f3_30'	'f4_30'	'f5_30'	'f1_35'	'f2_35'	'f3_35'	'f4_35'	'f5_35'	'f1_40'	'f2_40'	'f3_40'	'f4_40'	'f5_40'	'f1_45'	'f2_45'	'f3_45'	'f4_45'	'f5_45'	'f1_50'	'f2_50'	'f3_50'	'f4_50'	'f5_50'	'f1_55'	'f2_55'	'f3_55'	'f4_55'	'f5_55'	'f1_60'	'f2_60'	'f3_60'	'f4_60'	'f5_60'	'f1_65'	'f2_65'	'f3_65'	'f4_65'	'f5_65'	'f1_70'	'f2_70'	'f3_70'	'f4_70'	'f5_70'	'f1_75'	'f2_75'	'f3_75'	'f4_75'	'f5_75'	'f1_80'	'f2_80'	'f3_80'	'f4_80'	'f5_80'	'f1_85'	'f2_85'	'f3_85'	'f4_85'	'f5_85'	'f1_90'	'f2_90'	'f3_90'	'f4_90'	'f5_90'	'f1_95'	'f2_95'	'f3_95'	'f4_95'	'f5_95''newline$'"
				fileappend "'resultfile$'" 'resultline$'
				select TextGrid 'soundname$'
			endif
		endfor
		# Remove the TextGrid object from the object list
		select TextGrid 'soundname$'
		Remove
	endif
	# Remove the temporary objects from the object list
	select Sound 'soundname$'
	plus Formant 'soundname$'
	Remove
	select Strings list
	# and go on with the next sound file!
endfor

Remove


# This procedure finds the number of a tier that has a given label.

procedure GetTier name$ variable$
        numberOfTiers = Get number of tiers
        itier = 1
        repeat
                tier$ = Get tier name... itier
                itier = itier + 1
        until tier$ = name$ or itier > numberOfTiers
        if tier$ <> name$
                'variable$' = 0
        else
                'variable$' = itier - 1
        endif

	if 'variable$' = 0
		exit The tier called 'name$' is missing from the file 'soundname$'!
	endif

endproc
