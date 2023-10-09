#################################################################
## Amelie
##	extract acoustic value from VUV textgrid files


# create a text file with one line per voiced part and as columns :

#onsetV 			- onset time of the voiced part
#offsetV 			- offset time of the voiced part
#f0mean 			- mean f0 for the whole voiced part
#f0med 			- median f0 for the whole voiced part
#f0min 			- minimum f0 for the whole voiced part
#f0max 			- maximum f0 for the whole voiced part
#f0middle 		- mean f0 for in 50 ms @ the middle part of the voiced part
#f0sd			- SD of f0 in the whole voiced part [added by Tom Offrede]
#intensityMean 	- average intensity for the whole voiced part
#intensityMax 	- max intensity for the whole voiced part
#f1mean 			- mean f1 for the whole voiced part
#f1std 			- std f1 for the whole voiced part
#f1med 			- median f1 for the whole voiced part
#f1middle 		- mean f1 for in 50 ms @ the middle part of the voiced part
#f2mean		 	- mean f2 for the whole voiced part
#f2std 			- std f2 for the whole voiced part
#f2med 			- std f2 for the whole voiced part
#f2middle 		- mean f2 for in 50 ms @ the middle part of the voiced part
#################################################################

# brings up form that prompts the user to enter directory name
# creates variable




clearinfo



# max interval in which to look for formants, default is 5500 Hz in Praat should be adapted to the speaker
freq = 5500;

# number of formants to look for default is 5500 Hz in Praat should be adapted to the speaker
nform = 5;

soundDir$	= "C:/Users/offredet/Documents/1HU/ExperimentTemperature/Data/SpeechData/processed/ZNV"
textDir$	= soundDir$
# textDir$	= "C:/Users/offredet/Documents/1HU/ExperimentTemperature/Data/SpeechData/ZNV"


# Get the lists of all the sound files in the given directory
Create Strings as file list... list 'soundDir$'/*.wav


# get number of files to process
numberOfFiles = Get number of strings

print 'numberOfFiles'

ifile = 1


# will compute formats, F0 etc. in middle in a inter * 2 sec interval
inter = 0.025

# Deal file by file
while ifile <= numberOfFiles
  
	select Strings list
	fileName$ = Get string... ifile
   
	# strip extension to get basename
	name$ = fileName$ - ".wav"

	Read from file... 'soundDir$'/'fileName$'
	Read from file... 'textDir$'/'name$'_VUV.TextGrid
   
	fileForm$ = textDir$+"/"+name$+".txt"

	# delete file with formants
	filedelete 'fileForm$'
   
	# header, then, one line per voiced part detected    
	fileappend 'fileForm$' onset	offset	f0mean	f0med	f0sd	
	fileappend 'fileForm$' f0min	f0max	f0middle	intensityMean	intensityMax	
	fileappend 'fileForm$' f1mean	f1std	f1med	f1middle	 
	fileappend 'fileForm$' f2mean	f2std	f2med	f2middle	'newline$'
   

	select Sound 'name$'
    	
	To Formant (burg)... 0.0 nform freq 0.025 50

	select Sound 'name$'
	
	To Pitch (ac)... 0.0 117 15 no 0.03 0.45 0.01 0.35 0.14 432

   # Note : pitch detection is used to dectect voiced part
   # the parameters in the command To Picth should be adapted, here they are
		#1 time step (0.0 == default)
		#2 pitch floor (75 == default)
		#3 number of candidates (15 == default)
		#4 very accurate (No)
		#5 silence threshold (0.03)
		#6 voicing threshold (0.45)
		#7 octave cost (0.01)
		#8 octave-jump cost (0.35)	 
		#9 voiced / unvoiced cost (0.14)
		#10 pitch ceiling (600)
    	select Sound 'name$'
    	To Intensity... 100 0.0
   

	select TextGrid 'name$'_VUV
    	nInterv = Get number of intervals... 1
   	
    	for j from 1 to 'nInterv'	

		select TextGrid 'name$'_VUV
        	lab$ = Get label of interval... 1 'j'

		if lab$ == ""
			#	printline vide
		elsif lab$ == "silent"
			#	printline unprocess U
		else
		
			#---- process voiced interval
	    		onsetV  = Get starting point... 1 'j'			       	    
     	    	offsetV = Get end point... 1 'j'

			#---- find middle of the vowel
       		midTps = ('offsetV' + 'onsetV') / 2
		



             	#---- get formants


			#@ middle of the voiced part
            	
			start = midTps - inter
	   		end = midTps + inter
	
	    		select Formant 'name$'
           
            	f1middle = Get mean... 1 'start' 'end' Hertz
	    		
			if  f1middle = undefined
	    			f1middle = 0
            	endif
				
			
			f2middle = Get mean... 2 'start' 'end' Hertz
	    		
			if  f2middle = undefined
	    			f2middle = 0
            	endif

			# for all the vowel
	    		f1mean = Get mean... 1 'onsetV' 'offsetV' Hertz
			f2mean = Get mean... 2 'onsetV' 'offsetV' Hertz
		
			f1med  = Get quantile...  1  'onsetV' 'offsetV' Hertz 0.5 
			f2med  = Get quantile...  2  'onsetV' 'offsetV' Hertz 0.5

			f1std  = Get standard deviation... 1 'onsetV' 'offsetV' Hertz
			f2std  = Get standard deviation... 2 'onsetV' 'offsetV' Hertz
			




			#---- get pitch

	
            	select Pitch 'name$'

			f0mean = Get mean...  'onsetV' 'offsetV' Hertz
			f0med  = Get quantile...  'onsetV' 'offsetV' 0.5 Hertz
			f0sd   = Get standard deviation... 'onsetV' 'offsetV' Hertz
			f0min  = Get minimum...  'onsetV' 'offsetV' Hertz Parabolic
			f0max  = Get maximum...  'onsetV' 'offsetV' Hertz Parabolic

			f0middle = Get mean... 'start' 'end' Hertz



			#---- get intensity

			select Intensity 'name$'
			intensityMean	= Get mean... 'onsetV' 'offsetV' dB
			intensityMax	= Get maximum... 'onsetV' 'offsetV' Parabolic

		           
            	
		fileappend 'fileForm$' 'onsetV:3'	'offsetV:3'	'f0mean:1'	'f0med:1'	'f0sd:1'		
		fileappend 'fileForm$' 'f0min:1'	'f0max:1'	'f0middle:1'	'intensityMean:1'	'intensityMax:1'	
   		fileappend 'fileForm$' 'f1mean:1'	'f1std:1'	'f1med:1'	'f1middle:1'	
   		fileappend 'fileForm$' 'f2mean:1'	'f2std:1'	'f2med:1'	'f2middle:1'	'newline$'
   
    		endif
    	
	endfor
    
     select Formant 'name$'
     plus Intensity 'name$'
     plus Pitch 'name$'
	plus Sound 'name$'
	plus TextGrid 'name$'_VUV
	
	ifile = ifile + 1

	Remove

endwhile



###### After you're done with all files, remove Strings object for complete object cleaning up
select Strings list


########################################################
## END OF SCRIPT
#######################################################
