import csv

# Convert raw data to simple CSV files

def parse_lines(fn, key_trial):
	# Parse raw text file and return list of trials
	#
	# Input
	#  fn : file name
	#  key_trial : presence of this 'field' in JSON is necessary and sufficient condition for a 'trial'

	with open(fn, "rb") as fid: # for files with end of line markers
		lines = [l.strip('\n') for l in fid.readlines()]
	if len(lines)==1: # for files without end of line markers
		with open(fn, "rb") as fid: #reread
			lines = fid.read().splitlines()
	nline = len(lines)

	# only include lines with embedded JSON
	lines = [l for l in lines if '{' in l and '}' in l]
	lines = [l[l.find('{'):l.find('}')+1] for l in lines]

	# remove brackets around JSON
	for l in lines:
		assert l[0]=='{' and l[-1]=='}'
	lines = [l[1:-1] for l in lines]

	# break line into key/value strings
	lines = [l.split(';') for l in lines]

	# convert each trial into a dictionary
	trials = []
	for myline in lines:

		pairs = myline

		# necessary to make field style v2 compatible with v1 (v2 uses quotes around key/value pairs)
		pairs = [p.replace('"','') for p in pairs] # remove quotes from both key and values
		pairs = [p.replace('Subject_ID','Subject ID') for p in pairs] # this field was renamed
		
		# parse the fields 
		pairs = [p.split(':',1) for p in pairs] # split into key and value		
		pairs = { p[0].replace(' ','') : p[1].strip() for p in pairs} # remove spaces from key-name
		# mykeys = [key for key in pairs if 'Correct' in key]
		if key_trial in pairs: # check if this is a trial
			trials.append(pairs)
			
	print "  found " + str(len(trials)) + " trials in " + str(nline) + " lines of raw logs"
	return trials

def exclude_trials(trials,key,value):
	# remove all trials (dicts) with a specific key/value pairing
	#
	trials_new = [t for t in trials if t[key] != value]
	if len(trials_new) < len(trials):
		print "  removed " + str(len(trials) - len(trials_new)) + " trials with {"  + key + " = " + value + "}"
	return trials_new

def rename_subject(trials,key,new_name,old_name):
	flag = True
	for t in trials:
		if t[key] == old_name:
			t[key] = new_name
			if flag:
				print "  Participant " + old_name + " renamed as " + new_name + " to correct incorrect entry."
				print ""
				flag = False
	return trials

def make_simple_csv(fn_out,trials,columns):
	# Write to CSV with row for each trial
	#
	# Input
	#  fn_out : new file name
	#  trials : each trial as a dictionary (output of parse_lines function)
	#  columns : keys to extract for each trial

	# fill in missing values as 'NA'
	for t in trials:
		for c in columns:
			if c not in t:
				t[c] = 'NA'

	# write to CSV`
	fid = csv.writer(open(fn_out, "wb"), quoting=csv.QUOTE_ALL)
	fid.writerow(columns) # header
	for t in trials:
		fid.writerow([t[c] for c in columns])

if __name__ == "__main__":

	# Parameters
	# fn_in_fendle = 'data-flurry/fendle-12-9-16.csv'
	# fn_out_fendle = 'data-flurry/fendle-processed-12-9-16.csv'
	# fields_fendle =  ['SubjectID','CorrectFirstTry?','TrialType','StageType','Object','FoxAssistance','StartTime','ResponsiveStartTime','EndTime'] # fields to include in new csv file

	# fn_in_vocab = 'data-flurry/vocab-12-9-16.csv'
	# fn_out_vocab = 'data-flurry/vocab-processed-12-9-16.csv'
	# fields_vocab = ['SubjectID','Correct','Object_Asked','Correct_Image','Incorrect_Image'] # fields to include in new csv field

	# fn_in_fendle = 'data-flurry/fendle-iPad-2-1-17.csv'
	# fn_out_fendle = 'data-flurry/fendle-iPad-processed-2-1-17.csv'
	# fields_fendle =  ['SubjectID','CorrectFirstTry?','TrialType','StageType','Object','FoxAssistance','StartTime','ResponsiveStartTime','EndTime'] # fields to include in new csv file

	# fn_in_vocab = 'data-flurry/vocab-2-13-17.csv'
	# fn_out_vocab = 'data-flurry/vocab-processed-2-13-17.csv'
	# fields_vocab = ['SubjectID','Correct','Object_Asked','Correct_Image','Incorrect_Image'] # fields to include in new csv field

	# fn_in_fendle = 'data-flurry/fendle-06-12-17.csv'
	# fn_out_fendle = 'data-flurry/fendle-processed-06-12-17.csv'
	# fields_fendle =  ['SubjectID','CorrectFirstTry?','TrialType','StageType','Object','FoxAssistance','StartTime','ResponsiveStartTime','EndTime'] # fields to include in new csv file

	fn_in_fendle = 'data-flurry/fendle-07-12-17.csv'
	fn_out_fendle = 'data-flurry/fendle-processed-07-12-17.csv'
	fields_fendle =  ['SubjectID','CorrectFirstTry?','TrialType','StageType','Object','FoxAssistance','StartTime','ResponsiveStartTime','EndTime'] # fields to include in new csv file

	# fn_in_vocab = 'data-flurry/vocab-06-12-17.csv'
	# fn_out_vocab = 'data-flurry/vocab-processed-06-12-17.csv'
	# fields_vocab = ['SubjectID','Correct','Object_Asked','Correct_Image','Incorrect_Image'] # fields to include in new csv field

	print ""
	print "Preprocessing raw data from files '" + fn_in_fendle	
	S_fendle = parse_lines(fn_in_fendle,key_trial='TrialType')
	print ""
	
	# print "Preprocessing raw data from files '" + fn_in_vocab
	# S_vocab = parse_lines(fn_in_vocab,key_trial='Correct')
	# print ""

	# rename participant
	S_fendle = rename_subject(S_fendle,"SubjectID",new_name="01051704",old_name="01051703")
	S_fendle = rename_subject(S_fendle,"SubjectID",new_name="02101702",old_name="02101701")

	# print "Excluding demo trials for vocab app ...."
	# S_vocab = exclude_trials(S_vocab,'Object_Asked','dog')
	# S_vocab = exclude_trials(S_vocab,'Object_Asked','ball')
	# print ""

	print "Creating CSV file '" + fn_out_fendle
	print ""
	make_simple_csv(fn_out_fendle,S_fendle,fields_fendle)
	# print "Creating CSV file '" + fn_out_vocab
	# print ""
	# make_simple_csv(fn_out_vocab,S_vocab,fields_vocab)