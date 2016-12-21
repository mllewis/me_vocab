import csv

# Convert raw data to simple CSV files

def parse_lines(fn, key_trial):
	# Parse raw text file and return list of trials
	#
	# Input
	#  fn : file name
	#  key_trial : presence of this 'field' in JSON is necessary and sufficient condition for a 'trial'

	with open(fn, "rb") as fid:
		lines = [l.strip('\n') for l in fid.readlines()]

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
		pairs = [p.split(':',1) for p in myline] # split into key and value
		pairs = { p[0].replace(' ','') : p[1].strip() for p in pairs} # remove spaces from key-name
		if key_trial in pairs: # check if this is a trial
			trials.append(pairs)

	return trials

def exclude_trials(trials,key,value):
	# remove all trials (dicts) with a specific key/value pairing
	#
	trials_new = [t for t in trials if t[key] != value]
	if len(trials_new) < len(trials):
		print "  removed " + str(len(trials) - len(trials_new)) + " trials with {"  + key + " = " + value + "}"
	return trials_new

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

	# write to CSV
	fid = csv.writer(open(fn_out, "wb"), quoting=csv.QUOTE_ALL)
	fid.writerow(columns) # header
	for t in trials:
		fid.writerow([t[c] for c in columns])

if __name__ == "__main__":

	# Parameters
	fn_in_fendle = 'data-flurry/fendle-12-9-16.csv'
	fn_out_fendle = 'data-flurry/fendle-processed-12-9-16.csv'
	fields_fendle =  ['SubjectID','CorrectFirstTry?','TrialType','StageType','Object','FoxAssistance','StartTime','ResponsiveStartTime','EndTime'] # fields to include in new csv file

	fn_in_vocab = 'data-flurry/vocab-12-9-16.csv'
	fn_out_vocab = 'data-flurry/vocab-processed-12-9-16.csv'
	fields_vocab = ['SubjectID','Correct','Object_Asked','Correct_Image','Incorrect_Image'] # fields to include in new csv field

	print ""
	print "Preprocessing raw data from files '" + fn_in_fendle + "' and '" + fn_in_vocab + "'...."
	print ""
	S_fendle = parse_lines(fn_in_fendle,key_trial='TrialType')
	S_vocab = parse_lines(fn_in_vocab,key_trial='Correct')

	print "Excluding demo trials for vocab app ...."
	S_vocab = exclude_trials(S_vocab,'Object_Asked','dog')
	S_vocab = exclude_trials(S_vocab,'Object_Asked','ball')
	print ""

	print "Creating CSV files '" + fn_out_fendle + "' and '" + fn_out_vocab +  "'...."
	print ""
	make_simple_csv(fn_out_fendle,S_fendle,fields_fendle)
	make_simple_csv(fn_out_vocab,S_vocab,fields_vocab)