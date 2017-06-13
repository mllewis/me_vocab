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
		
		trials = df.loc[(df.SubjectID == myuser), field_correct]
		trials = np.array( trials , dtype=str)		
		trials = trials[trials != 'nan'] # remove incomplete trials
		assert len(set(trials))<=2 # make sure this is a binary response
		mycorrect = trials == code_correct
		trial_count[myuser] = np.size(mycorrect)
		trial_acc[myuser] = np.mean(mycorrect) * 100.0

	return trial_acc,trial_count

def exclude_practice(df):
	# Exclude the practice items in the vocab app.
	# This may not be necessary is importing data from Flurry logs, as the preprocess.py script also has a function for this.
	# This is necessary if important data from iPad logs.
	to_exclude = ['dog','ball'] # the practice items
	nitems = df.shape[0]
	for ex in to_exclude:
		df = df[df.Object_Asked != ex]
	ndrop = nitems - df.shape[0]
	print "Excluding practice trials..."
	print "  dropping " + str(ndrop) + " practice items"
	print ""
	return df

if __name__ == "__main__":

	# Parameters
	# fn_in_fendle_processed = 'data-flurry/fendle-processed-12-9-16.csv' # input data from fendle
	# fn_in_vocab_processed = 'data-flurry/vocab-processed-12-9-16.csv'
	# fn_in_ages = 'data-flurry/ages-12-9-16.csv' # list of user names and ages

	# fn_in_fendle_processed = 'data-flurry/fendle-iPad-processed-2-1-17.csv' # input data from fendle
	# fn_in_vocab_processed = 'data-flurry/vocab-device-results-2-7-17.csv'
	# fn_in_ages = 'data-flurry/ages-2-14-17.csv' # list of user names and ages

	fn_in_fendle_processed = 'data-flurry/fendle-processed-06-12-17.csv' # input data from fendle
	fn_in_vocab_processed = 'data-flurry/vocab-device-results-06-12-17.csv'
	fn_in_ages = 'data-flurry/ages-06-12-17.csv' # list of user names and ages

	ntrial_fendle = 19 # full number of trials in an experiment
	ntrial_vocab = 20

	fn_out_summary = 'data-flurry/summary-06-12-17.csv' # output summary of data

	# exclude_user = ['1191602', '1191605', '1191606', '11161604', '1261606', '1281601'] # from December 2016
 	# exclude_user = ["1191602","1191605","1191606","11161604","1261606","1281601","12131602","01031704","01031705","01051703","01061701","01061702","01061703","01061706","01121705","01121707","01121708","01131703","01171701","01171703","01191701","01191702","01191704","01191706","01271701","01311701","Test"] # from Feb. 2017
 	exclude_user = ["1191602","1191605","1191606","11161604","1261606","1281601","12131602","01031704","01031705","01051703","01061701","01061702","01061703","01061706","01121705","01121707","01131703","01171701","01171703","01191701","01191702","01191704","01191706","01271701","01311701","02071703","02071705","02071706","02091701","02201708","02201709","02201712","02201713","02281703","03021701","03211701","03301701","04111706","04111707","04111708","04181701","05241702","05241704","06021701","06041701","06041702","06041704","06041705","06041708","06041709","06041713","06091702","Test"]

	# Main
	print ""
	print "Loading pre-processed ME and vocab data ..."
	print ""
	df_fendle = pd.read_csv(fn_in_fendle_processed)
	df_vocab  = pd.read_csv(fn_in_vocab_processed)
	df_vocab = exclude_practice(df_vocab)

	acc_fendle,count_fendle = get_acc_by_user(df_fendle, field_correct="CorrectFirstTry?", code_correct='Y')
	acc_vocab,count_vocab = get_acc_by_user(df_vocab, field_correct="Correct", code_correct="Yes")
	
	# get ages
	df_age = pd.read_csv(fn_in_ages)
	v_ids = df_age['SubjectID'].as_matrix()
	v_ages = df_age['Age'].as_matrix()
	my_ages = {v_ids[i] : v_ages[i] for i in range(len(v_ids)) }
	# with open(fn_in_ages, 'rb') as csvfile:
	# 	myreader = csv.reader(csvfile)
	# 	my_ages = {pairs[0] : float(pairs[1]) for idx,pairs in enumerate(myreader) if idx > 0} # skip header

	print "Find participants that have data in both experiments... "
	print "  N = " + str(len(set(acc_fendle.keys()))) + " for Fendle app"
	print "  N = " + str(len(set(acc_vocab.keys()))) + " for vocab app"
	print "  excluding " + str(len(exclude_user)) + " participants"
	user_with_both = ( set(acc_fendle.keys()) & set(acc_vocab.keys()) ) - set(exclude_user)
	user_with_both = sorted(list(user_with_both))
	print "  leaving N = " + str(len(user_with_both)) + " total"
	print ""

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
	print "  correlation between vocab and ME is r = " + str(round(r,3)) + ", p = " + str(round(p,8)) + " (n=" + str(n) + ")"

	(r,p) = stats.pearsonr(v_age,v_acc_vocab)
	print "  correlation between vocab and age is r = " + str(round(r,3)) + ", p = " + str(round(p,8)) + " (n=" + str(n) + ")"
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
	plt.scatter(v_acc_fendle,v_acc_vocab,alpha=0.3)
	plt.xlabel('Mutual Exclusivity strength (% correct)')
	plt.ylabel('Vocabulary strength (% correct)')
	plt.xlim((30,105))
	plt.ylim((0,105))

	plt.subplot(1,2,2)
	plt.scatter(v_age,v_acc_vocab,alpha=0.3)
	plt.ylim((0,105))
	plt.xlabel('Age (in years)')
	# plt.ylabel('vocab (accuracy)')
	plt.show()
	print ""