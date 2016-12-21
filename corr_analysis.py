import numpy as np
import matplotlib.pyplot as plt
import pandas as pd
import csv
from scipy import stats

# Analysis for predicting vocab level by ME strength

def get_acc_by_user(df,field_correct,code_correct):
 	# Compute accuracy across a task for each user
	#
	# Input
	#  df : pandas data frame with trials as rows
	#  field_correct : name of the column that records correct vs. incorrect
	#  code_correct : string such as 'Y' or "Yes", meaning this response was correct

	users = list(set(df['SubjectID']))
	trial_count = {}
	trial_acc = {}	
	for myuser in users:
		
		trials = np.array( df.loc[(df.SubjectID == myuser), field_correct] )
		assert len(set(trials))<=2 # make sure this is a binary response
		mycorrect = trials == code_correct
		trial_count[myuser] = np.size(mycorrect)
		trial_acc[myuser] = np.mean(mycorrect) * 100.0

	return trial_acc,trial_count

if __name__ == "__main__":

	# Parameters
	fn_in_fendle_processed = 'data-flurry/fendle-processed-12-9-16.csv' # input data from fendle
	fn_in_vocab_processed = 'data-flurry/vocab-processed-12-9-16.csv'
	fn_in_ages = 'data-flurry/ages-12-9-16.csv' # list of user names and ages

	ntrial_fendle = 19 # full number of trials in an experiment
	ntrial_vocab = 20

	fn_out_summary = 'data-flurry/summary-12-9-16.csv' # output summary of data

	exclude_user = ['1191602', '1191605', '1191606', '11161604', '1261606', '1281601']
		# from Veronica's spreadsheet

	# Main
	print ""
	print "Loading pre-processed ME and vocab data ..."
	print ""
	df_fendle = pd.read_csv(fn_in_fendle_processed)
	df_vocab  = pd.read_csv(fn_in_vocab_processed)

	acc_fendle,count_fendle = get_acc_by_user(df_fendle, field_correct="CorrectFirstTry?", code_correct='Y')
	acc_vocab,count_vocab = get_acc_by_user(df_vocab, field_correct="Correct", code_correct="Yes")	
	with open(fn_in_ages, 'rb') as csvfile:
		myreader = csv.reader(csvfile)
		my_ages = {pairs[0] : float(pairs[1]) for idx,pairs in enumerate(myreader) if idx > 0} # skip header

	print "Find participants that have data in both experiments... "
	print ""
	user_with_both = ( set(acc_fendle.keys()) & set(acc_vocab.keys()) ) - set(exclude_user)
	user_with_both = sorted(list(user_with_both))

	print "Flagging participants with unusual number of trials... "
	mypass = True
	for u in user_with_both:
		if count_fendle[u] != ntrial_fendle:
			print "   User " + u + " had " + str(count_fendle[u]) + " trials in ME task (should have " + str(ntrial_fendle) + ")" 
			mypass = False
		if count_vocab[u] != ntrial_vocab:
			print "   User " + u + " had " + str(count_vocab[u]) + " trials in vocab task (should have " + str(ntrial_vocab) + ")" 
			mypass = False
	if mypass: print "Passed."
	print ""
	
	# vectorize key measures
	v_acc_fendle = []
	v_acc_vocab = []
	v_age = []
	v_count_fendle = []
	v_count_vocab = []
	for u in user_with_both:
		v_acc_fendle.append(acc_fendle[u])
		v_acc_vocab.append(acc_vocab[u])
		v_count_fendle.append(count_fendle[u])
		v_count_vocab.append(count_vocab[u])
		v_age.append(my_ages[u])

	print "Computing simple correlations... "
	n = len(v_acc_vocab)
	(r,p) = stats.pearsonr(v_acc_fendle,v_acc_vocab)
	print "  correlation between vocab and ME is r = " + str(round(r,3)) + ", p = " + str(round(p,3)) + " (n=" + str(n) + ")"

	(r,p) = stats.pearsonr(v_age,v_acc_vocab)
	print "  correlation between vocab and age is r = " + str(round(r,3)) + ", p = " + str(round(p,3)) + " (n=" + str(n) + ")"
	print ""

	print "Writing summary of data to CSV file '" + str(fn_out_summary) +  "'... "
	print ""
	fid = csv.writer(open(fn_out_summary, "wb"), quoting=csv.QUOTE_ALL)
	fid.writerow(['SubjectID','age','vocab_acc','ME_acc','vocab_count','ME_count']) # header
	for idx in range(n):
		fid.writerow( [user_with_both[idx],v_age[idx],v_acc_vocab[idx],v_acc_fendle[idx],v_count_vocab[idx],v_count_fendle[idx]] )


	print "Scatter plots... "	
	plt.figure(1,figsize=(8,4))
	plt.clf()

	plt.subplot(1,2,1)
	plt.scatter(v_acc_fendle,v_acc_vocab)
	plt.xlabel('ME (accuracy)')
	plt.ylabel('vocab (accuracy)')
	plt.xlim((30,105))
	plt.ylim((0,105))

	plt.subplot(1,2,2)
	plt.scatter(v_age,v_acc_vocab)
	plt.ylim((0,105))
	plt.xlabel('age')
	# plt.ylabel('vocab (accuracy)')
	plt.show()
	print ""