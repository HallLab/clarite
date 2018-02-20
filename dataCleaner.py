##Deven Orie 
##Dartmouth'19 - CS & Econ Major
##Pipeline clean_rows Cleaner
##Imports Necessary Installations - Numpy, GGPLOT, GG-Grid, Python 2.7

##Recommended to keep DataCleaner Directory on Desktop
from Tkinter import *
import subprocess
import io
import sys
import contextlib
import numpy as np
import csv
import math
import random
import tkFileDialog
from io import StringIO
import os.path
from random import randint
import datetime


with open('logs.txt', 'w') as global_log:
	global_log.write("--------------------------PROGRAM LOGS--------------------------"+'\n')
	global_log.write("THIS LOGS ALL OF YOUR VALUES & FUNCTIONS USED IN THE GUI PROGRAM"+'\n')
	global_log.write("----------------------------------------------------------------"+'\n'+'\n')
	global_log.write("DataCleaner Program Opened"+'\n')
	global_log.write("PROGRAM OPENED :  "+ datetime.datetime.now().strftime("%I:%M%p on %B %d, %Y") + '\n'+'\n')



##Global Constant Values
CONSTANT_INPUT = "a0 = read.delim('/Users/deven/desktop/it.txt', header=TRUE)"
A0_0 = "a0 = read.delim('"
A0_2 = "', header=TRUE)"

IDCOLUMN = "a0$IID <- paste(""id"", row.names(a0), sep=""_"")"



###***************************************************************************************###
   		###************************** Start Program ***********************###

##Choose Global File - Start of Program
def choose_start_file():

	global global_start_file_path
	global_start_file_path = tkFileDialog.askopenfilename()
	choose_start_file_name.set("Data File: " + global_start_file_path)

	global final
	final = A0_0 + global_start_file_path + A0_2

	with open('logs.txt', 'a') as g:
		g.write("File Chosen: "+ global_start_file_path + '\n'+'\n')

	
##Dialogue Function
def dialogue_file_dictionary():
	global global_recode_key_dictionary
	global_recode_key_dictionary = tkFileDialog.askopenfilename()
	choose_dictionary_label_name.set("Dictionary File: " + global_recode_key_dictionary)

	with open('logs.txt', 'a') as g:
		g.write("Recode Key Dictionary Chosen: "+ global_recode_key_dictionary + '\n'+'\n')

	

# def dialogue_file_recode_key():
# 	global recode_key_file
# 	recode_key_file = tkFileDialog.askopenfilename()
# 	choose_file_label_name.set("Data File: " + recode_key_file)
###***************************************************************************************###






###**************************** Welcome to Data Cleaner *********************************###
###******************** (ORDER = Clean,Filter,Visualize,Summary,GUI) ********************###
##IMPORTANT
## Run PRogram ----> python2.7 dataCleaner.py

#All R files, have had their hard values replaced with standard variables
# 1) Functions Read through the R file
# 2) Variables are changed within the R file based on the USER INPUT
# 3) New Values stored in new R file
# 4) Example)   old_R_file.R ----->  new_R_file1.R
# 5) This new R file is the one that is processed/run using SubProcess Python Program

# 6) Output is stored at images, pdf, textdocuments in directory datacleaner_Output
###***************************************************************************************###


###***************************************************************************************###
   		###************************** Clean Data Functions ***********************###
###***************************************************************************************###
def recode_missing():



	
	if os.path.exists(global_start_file_path) == True:

		with open(global_start_file_path, 'r') as file :
			
			fileclean_rows = file.read()
			fileclean_rows = fileclean_rows.replace("\t" + str(missing_val.get()), "\t"+str(missing_val_replacement.get()))
			with open('datacleaner_Output/recode_missing_out.txt', 'w') as file:
				file.write(fileclean_rows)
			with open('logs.txt', 'a') as g:
				g.write("----------------------------------------------------------------" + '\n')
				g.write("Missing Values Replace Function Called" + '\n')
				g.write("Missing Value: "+ str(missing_val.get()) + '\n')
				g.write("Missing Value Replacement: "+ str(missing_val_replacement.get()) + '\n' + '\n')

	else:

		file_name.set("File Path Is Invalid: " + global_start_file_path)


def recode_key():
	iterator = 0
	key_array = []

	with open('logs.txt', 'a') as g:
		g.write("----------------------------------------------------------------" + '\n')

		g.write("Missing Values Dictionary Function Called" + '\n')

	dictionary = global_recode_key_dictionary

	with open(dictionary, 'r') as file :
		fileclean_rows = file.read().split()
		for x in fileclean_rows:
			iterator = iterator + 1
			if iterator % 2 ==0 and iterator > 2:
				key_array.append(x)
				pass

	with open(global_start_file_path, 'r') as file :
		fileclean_rows = file.read()

		for x in key_array:
			print(x)
			fileclean_rows = fileclean_rows.replace('\t'+str(x), "\tNA")

		with open('datacleaner_Output/recode_key_out.txt', 'w') as file:
			file.write(fileclean_rows)







###***************************************************************************************###
		 ###*********************** Filtering Functions *************************###
###***************************************************************************************###
def get_binary():
	f = open('r/get_binary.R','r')
	filedata = f.read()
	f.close()

	newdata = filedata

	f = open('r/gui_generated_scripts/get_binary1.R','w')
	f.write(newdata)
	f.write(final + '\n')
	f.write("newdata <- get_binary(a0, 2)"+ '\n')
	file_output_path = "write.table(newdata,"+ "file" + "=" + '"datacleaner_Output/binary_var_out.txt",' + "sep=" +"'\\t'" + ", row.names=FALSE, quote=FALSE)"



	f.write(file_output_path+ '\n')


	f.close()

	proc = subprocess.call(['Rscript','r/gui_generated_scripts/get_binary1.R'], shell=False)

	with open('logs.txt', 'a') as g:
		g.write("----------------------------------------------------------------" + '\n')

		g.write("Get Binary Variables Function Called" + '\n' +'\n' )


def get_continuous():
	f = open('r/get_continuous.R','r')
	filedata = f.read()
	f.close()

	lower_bound = "a1 = " + str(continuous_var.get())
	file_output_path = "write.table(newdata,"+ "file" + "=" + '"datacleaner_Output/continuous_var_out.txt",' + "sep=" +"'\\t'" + ", row.names=FALSE, quote=FALSE)"

	newdata = filedata

	f = open('r/gui_generated_scripts/get_continuous1.R','w')

	f.write(newdata)
	f.write(final + '\n')
	f.write(lower_bound + '\n')
	f.write("newdata <- get_continuous(a0, a1)"+ '\n')
	f.write(file_output_path+ '\n')

	f.close()



	proc = subprocess.call(['Rscript','r/gui_generated_scripts/get_continuous1.R'], shell=False)
	
	with open('logs.txt', 'a') as g:
		g.write("----------------------------------------------------------------" + '\n')

		g.write("Get Continuous Variables Function Called" + '\n')
		g.write("Lower Bound: " + lower_bound + '\n' +'\n' )


def get_categorical():
	f = open('r/get_categorical.R','r')
	filedata = f.read()
	f.close()
	file_output_path = "write.table(newdata,"+ "file" + "=" + '"datacleaner_Output/get_categorical_out.txt",' + "sep=" +"'\\t'" + ", row.names=FALSE, quote=FALSE)"

	lower_bound = "a1 = " + str(categorical1.get())
	upper_bound = "a2 = " + str(categorical2.get())

	newdata = filedata

	
	f = open('r/gui_generated_scripts/get_categorical1.R','w')
	f.write(newdata)
	f.write(final + '\n')
	f.write(lower_bound + '\n')
	f.write(upper_bound + '\n')
	f.write("newdata <- get_categorical(a0, a1, a2)"+ '\n')
	f.write(file_output_path + '\n')



	f.close()

	proc = subprocess.call(['Rscript','r/gui_generated_scripts/get_categorical1.R'], shell=False)
	
	with open('logs.txt', 'a') as g:
		g.write("----------------------------------------------------------------" + '\n')

		g.write("Get Categorical Variables Function Called" + '\n')
		g.write("Lower Bound: " + lower_bound + '\n')
		g.write("Upper Bound: " + upper_bound + '\n' +'\n' )


def get_check():
	f = open('r/get_check.R','r')
	filedata = f.read()
	f.close()

	file_output_path = "write.table(newdata,"+ "file" + "=" + '"datacleaner_Output/get_check_out.txt",' + "sep=" +"'\\t'" + ", row.names=FALSE, quote=FALSE)"

	lower_bound = "a1 = " + str(ambiguous1.get())
	upper_bound = "a2 = " + str(ambiguous2.get())

	newdata = filedata
	
	f = open('r/gui_generated_scripts/get_check1.R','w')
	f.write(newdata)
	f.write(final + '\n')
	f.write(lower_bound + '\n')
	f.write(upper_bound + '\n')
	f.write("newdata <- get_check(a0, a1, a2)"+ '\n')
	f.write(file_output_path + '\n')



	f.close()
	
	proc = subprocess.call(['Rscript','r/gui_generated_scripts/get_check1.R'], shell=False)

	with open('logs.txt', 'a') as g:
		g.write("----------------------------------------------------------------" + '\n')

		g.write("Get Ambiguous Variables Function Called" + '\n')
		g.write("Lower Bound: " + lower_bound + '\n')
		g.write("Upper Bound: " + upper_bound + '\n' +'\n' )


def sample_keep():
	f = open('r/sample_keep.R','r')
	filedata = f.read()
	f.close()

	file_output_path = "write.table(newdata,"+ "file" + "=" + '"datacleaner_Output/sample_keep_out.txt",' + "sep=" +"'\\t'" + ", row.names=FALSE, quote=FALSE)"

	lower_bound = "a1 = " + str(sample_keep_var.get())
	newdata = filedata

	
	f = open('r/gui_generated_scripts/sample_keep1.R','w')
	f.write(newdata)
	f.write(final + '\n')
	f.write(lower_bound + '\n')
	f.write("newdata <- sample_keep(a0, a1)"+ '\n')
	f.write(file_output_path + '\n')



	f.close()

	proc = subprocess.call(['Rscript','r/gui_generated_scripts/sample_keep1.R'], shell=False)

	with open('logs.txt', 'a') as g:
		g.write("----------------------------------------------------------------" + '\n')

		g.write("Get Ambiguous Variables Function Called" + '\n')
		g.write("Lower Bound: " + lower_bound + '\n' +'\n' )


def transform_list():
	f = open('r/transform_list.R','r')
	filedata = f.read()
	f.close()

	file_output_path = "write.table(newdata,"+ "file" + "=" + '"datacleaner_Output/transform_list.txt",' + "sep=" +"'\\t'" + ", row.names=FALSE, quote=FALSE)"

	argument_1 = "a1 = read.delim('/Users/deven/desktop/it.txt', header=TRUE)"
	a1 = "a1 = read.delim('" + global_transform_key_dictionary + A0_2

	newdata = filedata

	
	f = open('r/gui_generated_scripts/transform_list1.R','w')
	f.write(newdata)
	f.write(final + '\n')
	f.write(a1 + '\n')
	f.write("newdata <- transform_list(a0, a1)"+ '\n')
	f.write(file_output_path + '\n')


	f.close()
	
	proc = subprocess.call(['Rscript','r/gui_generated_scripts/transform_list1.R'], shell=False)
	
	with open('logs.txt', 'a') as g:
		g.write("----------------------------------------------------------------" + '\n')

		g.write("Transform List Function Called" + '\n' + '\n')


def remove_outliers():

	with open('logs.txt', 'a') as g:
		g.write("----------------------------------------------------------------" + '\n')

		g.write("Remove Outliers Function Called" + '\n'+ '\n' )

	# input_file = outlier_file
        matrix = []
        # input_file = '/Users/deven/desktop/miss.txt'

        ##Called within remove outliers

        ##number is the number of clumns 
        ##clean_rows(number)Returns a cleaned row of clean_rows to remove_ourliers where it is added to a matrix
        ##This matrix is then passed to write_matrix_to_textfile, which writes the matrix to a file.
        ##Remove Outliers 
        def clean_rows(number):
                crs = open(global_start_file_path, "r")


                multiplyer = 2.5

                column_array = []
                i = 0

                header = ""

                for columns in ( raw.strip().split() for raw in crs ):

                        column_array.append(columns[number])

                intlist = []
                counter = []
                for x in column_array:
                        try:
                                intlist.append(x)
                                counter.append(float(x))
                                #print(x)
                        except ValueError:
				print("VE")
				#print(str(x) + "\tdo nothing")
                                #intlist.append(x)

                mean = sum(counter) / float(len(counter))
                var  = sum(pow(x-mean,2) for x in counter) / (len(counter)-1)  # variance
                std  = math.sqrt(var)  # standard deviation


                cealing = mean + (multiplyer*std)
                floor = mean - (multiplyer*std)



                for x in range(len(intlist)):
			try:
				float(intlist[x])

	                        if (float(intlist[x]) > cealing or float(intlist[x]) < floor) and x!=0:
                                	intlist[x] = "NA"
			except ValueError:
				intlist[x] = intlist[x]

		#print(intlist)
                return(intlist)


        #This reads in the file, counts the columns, and passes this to call clean_rows(x)
        file = open(global_start_file_path, "r")
        total_columns = file.readline().count('\t') + 1
        file.close()

        #Calls the clean_rows function and appends all rows to a matrix
        ##CALL clean_rows(x)
        for x in range(total_columns):
		print(x)
                matrix.append(clean_rows(x))


        ##Writes Matrix to file
        def write_matrix_to_textfile(a_matrix):
            #print(a_matrix)
            #r = np.transpose(a_matrix)
	    r = zip(*matrix)
            print(r)
            #print(a_row)
            def compile_row_string(a_row):
                return str(a_row).strip("']").strip("['").strip("(").strip(")").replace(" ","\t").replace("'","").replace(",", "")

            with open('datacleaner_Output/remove_outliers.txt', 'w') as f:
                for row in r:
		    #print row
                    f.write(compile_row_string(row)+'\n')

            return True

        write_matrix_to_textfile(matrix)




###***************************************************************************************###
   		###********************** Visualize Data Functions *******************###
###***************************************************************************************###
def histogram():


	f = open('r/hist_plot.R','r')
	filedata = f.read()
	f.close()

	# file_output_path = "write.table(newdata,"+ "file" + "=" + '"datacleaner_Output/histogram_plot.txt",' + "sep=" +"'\\t'" + ", row.names=FALSE, quote=FALSE)"



	#dafault values for the above variables
	number_of_plots_per_page = "a1 = " + str(h1.get())
	number_of_rows_per_page = "a2 = " + str(h2.get())
	numebr_of_columns_per_page = "a3 = " + str(h3.get())
	width_of_plot = "a4 = " + str(h4.get())
	height_of_plot = "a5 = " + str(h5.get())
	resolution_of_plot = "a6 = " + str(h6.get())


	newdata = filedata


	f = open('r/gui_generated_scripts/hist_plot1.R','w')

	f.write(newdata)
	f.write(final + '\n')
	f.write(number_of_plots_per_page + '\n')
	f.write(number_of_rows_per_page + '\n')
	f.write(numebr_of_columns_per_page + '\n')
	f.write(width_of_plot + '\n')
	f.write(height_of_plot + '\n')
	f.write(resolution_of_plot + '\n')
	f.write("quartz()" + '\n')
	f.write("hist_plot(a0, a1, file='datacleaner_Output/hist_plot_out', a2, a3, a4, a5, a6)" + '\n')

	f.close()

	proc = subprocess.call(['Rscript','r/gui_generated_scripts/hist_plot1.R'], shell=False)
	
	with open('logs.txt', 'a') as g:

		g.write("----------------------------------------------------------------" + '\n')

		g.write("Generate Histogram Function Called" + '\n')
		g.write("Number of Plots Per Page: " + str(h1.get()) + '\n')
		g.write("Number of Rows Per Page:  " + str(h2.get()) + '\n')
		g.write("Number of Columns Per Page: " + str(h3.get()) + '\n')
		g.write("Width of Plot: " + str(h4.get()) + '\n')
		g.write("Height of Plot: " + str(h5.get()) + '\n')
		g.write("Resolution of Plot: " + str(h6.get()) + '\n' +'\n' )



def boxplot():

	f = open('r/box_plot.R','r')
	filedata = f.read()
	f.close()

	newdata = filedata



	number_of_plots_per_page = "a1 = " + str(bx1.get())
	number_of_rows_per_page = "a2 = " + str(bx2.get())
	numebr_of_columns_per_page = "a3 = " + str(bx3.get())
	width_of_plot = "a4 = " + str(bx4.get())
	height_of_plot = "a5 = " + str(bx5.get())
	resolution_of_plot = "a6 = " + str(bx6.get())


	f = open('r/gui_generated_scripts/box_plot1.R','w')
	f.write(newdata)
	f.write(final + '\n')
	f.write(number_of_plots_per_page + '\n')
	f.write(number_of_rows_per_page + '\n')
	f.write(numebr_of_columns_per_page + '\n')
	f.write(width_of_plot + '\n')
	f.write(height_of_plot + '\n')
	f.write(resolution_of_plot + '\n')
	f.write("quartz()" + '\n')
	f.write("box_plot(a0, a1, file='datacleaner_Output/boxplot_out', a2, a3, a4, a5, a6)" + '\n')

	f.close()
	proc = subprocess.call(['Rscript','r/gui_generated_scripts/box_plot1.R'], shell=False)

	with open('logs.txt', 'a') as g:
		g.write("----------------------------------------------------------------" + '\n')

		g.write("Generate Box Plot Function Called" + '\n')
		g.write("Number of Plots Per Page: " + str(bx1.get()) + '\n')
		g.write("Number of Rows Per Page:  " + str(bx2.get()) + '\n')
		g.write("Number of Columns Per Page: " + str(bx3.get()) + '\n')
		g.write("Width of Plot: " + str(bx4.get()) + '\n')
		g.write("Height of Plot: " + str(bx5.get()) + '\n')
		g.write("Resolution of Plot: " + str(bx6.get()) + '\n' +'\n' )



def qqplot():
	
	f = open('r/qq_plot.R','r')
	filedata = f.read()
	f.close()

	newdata = filedata

	number_of_plots_per_page = "a1 = " + str(qq1.get())
	number_of_rows_per_page = "a2 = " + str(qq2.get())
	numebr_of_columns_per_page = "a3 = " + str(qq3.get())
	width_of_plot = "a4 = " + str(qq4.get())
	height_of_plot = "a5 = " + str(qq5.get())
	resolution_of_plot = "a6 = " + str(qq6.get())


	f = open('r/gui_generated_scripts/qq_plot1.R','w')
	f.write(newdata)
	f.write(final + '\n')
	f.write(number_of_plots_per_page + '\n')
	f.write(number_of_rows_per_page + '\n')
	f.write(numebr_of_columns_per_page + '\n')
	f.write(width_of_plot + '\n')
	f.write(height_of_plot + '\n')
	f.write(resolution_of_plot + '\n')
	f.write("quartz()" + '\n')
	f.write("qq_plot(a0, a1, file='datacleaner_Output/boxplot_out', a2, a3, a4, a5, a6)" + '\n')

	f.close()
	proc = subprocess.call(['Rscript','r/gui_generated_scripts/qq_plot1.R'], shell=False)

	with open('logs.txt', 'a') as g:
		g.write("----------------------------------------------------------------" + '\n')

		g.write("Generate QQ Plot Function Called" + '\n')
		g.write("Number of Plots Per Page: " + str(qq1.get()) + '\n')
		g.write("Number of Rows Per Page:  " + str(qq2.get()) + '\n')
		g.write("Number of Columns Per Page: " + str(qq3.get()) + '\n')
		g.write("Width of Plot: " + str(qq4.get()) + '\n')
		g.write("Height of Plot: " + str(qq5.get()) + '\n')
		g.write("Resolution of Plot: " + str(qq6.get()) + '\n' +'\n' )



def barplot():
		
	f = open('r/bar_plot.R','r')
	filedata = f.read()
	f.close()

	newdata = filedata


	number_of_plots_per_page = "a1 = " + str(bp1.get())
	number_of_rows_per_page = "a2 = " + str(bp2.get())
	numebr_of_columns_per_page = "a3 = " + str(bp3.get())
	width_of_plot = "a4 = " + str(bp4.get())
	height_of_plot = "a5 = " + str(bp5.get())
	resolution_of_plot = "a6 = " + str(bp6.get())




	f = open('r/gui_generated_scripts/bar_plot1.R','w')
	f.write(newdata)
	f.write(final + '\n')
	f.write(number_of_plots_per_page + '\n')
	f.write(number_of_rows_per_page + '\n')
	f.write(numebr_of_columns_per_page + '\n')
	f.write(width_of_plot + '\n')
	f.write(height_of_plot + '\n')
	f.write(resolution_of_plot + '\n')
	f.write("quartz()" + '\n')
	f.write("bar_plot(a0, a1, file='datacleaner_Output/boxplot_out', a2, a3, a4, a5, a6)" + '\n')


	f.close()
	proc = subprocess.call(['Rscript','r/gui_generated_scripts/bar_plot1.R'], shell=False)

	with open('logs.txt', 'a') as g:
		g.write("----------------------------------------------------------------" + '\n')

		g.write("Generate Bar Plot Function Called" + '\n')
		g.write("Number of Plots Per Page: " + str(bp1.get()) + '\n')
		g.write("Number of Rows Per Page:  " + str(bp2.get()) + '\n')
		g.write("Number of Columns Per Page: " + str(bp3.get()) + '\n')
		g.write("Width of Plot: " + str(bp4.get()) + '\n')
		g.write("Height of Plot: " + str(bp5.get()) + '\n')
		g.write("Resolution of Plot: " + str(bp6.get()) + '\n' +'\n' )




###***************************************************************************************###
			###********************** Summary Functions ************************###
###***************************************************************************************###
def frequency_table():
	f = open('r/freq_tables.R','r')
	filedata = f.read()
	f.close()

	file_output_path = "write.table(newdata,"+ "file" + "=" + '"datacleaner_Output/freq_tables_out.txt",' + "sep=" +"'\\t'" + ", row.names=FALSE, quote=FALSE)"

	newdata = filedata

	f = open('r/gui_generated_scripts/freq_tables1.R','w')
	f.write(newdata)
	f.write(final + '\n')
	f.write("newdata <- freq_tables(a0)"+ '\n')
	f.write(file_output_path + '\n')


	f.close()
	proc = subprocess.call(['Rscript','r/gui_generated_scripts/freq_tables1.R'], shell=False)
	
	with open('logs.txt', 'a') as g:
		g.write("----------------------------------------------------------------" + '\n')

		g.write("Generate Frequency Table Function Called" + '\n'+'\n')

def correlations():
	f = open('r/correlations.R','r')
	filedata = f.read()
	f.close()

	file_output_path = "write.table(newdata,"+ "file" + "=" + '"datacleaner_Output/correlations_out.txt",' + "sep=" +"'\\t'" + ", row.names=FALSE, quote=FALSE)"

	threshold_val = "a1 = " + str(find_correlations_var.get())
	newdata = filedata


	f = open('r/gui_generated_scripts/correlations1.R','w')
	f.write(newdata)
	f.write(final + '\n')
	f.write(threshold_val)
	f.write("newdata <- correlations(a0,a1)"+ '\n')
	f.write(file_output_path + '\n')




	f.close()
	proc = subprocess.call(['Rscript','r/gui_generated_scripts/correlations1.R'], shell=False)
	
	with open('logs.txt', 'a') as g:
		g.write("----------------------------------------------------------------" + '\n')
		g.write("Generate Correlations Function Called" + '\n')
		g.write("Threshold: " + threshold_val+ '\n')


def sample_size():
	f = open('r/sample_size.R','r')
	filedata = f.read()
	f.close()

	file_output_path = "write.table(newdata,"+ "file" + "=" + '"datacleaner_Output/sample_size_out.txt",' + "sep=" +"'\\t'" + ", row.names=FALSE, quote=FALSE)"

	newdata = filedata

	f = open('r/gui_generated_scripts/sample_size1.R','w')
	f.write(newdata)
	f.write(final + '\n')
	f.write("newdata <- sample_size(a0)"+ '\n')
	f.write(file_output_path + '\n')



	f.close()
	proc = subprocess.call(['Rscript','r/summary/sample_size1.R'], shell=False)
	
	with open('logs.txt', 'a') as g:
		g.write("----------------------------------------------------------------" + '\n')
		g.write("Sample Size Function Called" + '\n')
		g.write("Threshold: " + threshold_val+ '\n'+'\n')



def get_levels():
	f = open('r/get_levels.R','r')
	filedata = f.read()
	f.close()
	file_output_path = "write.table(newdata,"+ "file" + "=" + '"datacleaner_Output/get_levels_out.txt",' + "sep=" +"'\\t'" + ", row.names=FALSE, quote=FALSE)"

	newdata = filedata

	f = open('r/gui_generated_scripts/get_levels1.R','w')
	f.write(newdata)
	f.write(final + '\n')
	f.write("newdata <- get_levels(a0)"+ '\n')
	f.write(file_output_path + '\n')


	f.close()
	proc = subprocess.call(['Rscript','r/gui_generated_scripts/get_levels1.R'], shell=False)
	with open('logs.txt', 'a') as g:
		g.write("----------------------------------------------------------------" + '\n')
		g.write("Get Levels Function Called" + '\n')
	
def outliers():
	f = open('r/outliers.R','r')
	filedata = f.read()
	f.close()

	file_output_path = "write.table(newdata,"+ "file" + "=" + '"datacleaner_Output/outlisers.txt",' + "sep=" +"'\\t'" + ", row.names=FALSE, quote=FALSE)"

	threshold_val = "a1 = " + str(find_outliers_var.get())
	newdata = filedata


	f = open('r/gui_generated_scripts/outliers1.R','w')
	f.write(newdata)
	f.write(final + '\n')
	f.write(threshold_val + '\n')
	f.write("newdata <- outliers(a0,a1)"+ '\n')
	f.write(file_output_path + '\n')


	f.close()
	proc = subprocess.call(['Rscript','r/gui_generated_scripts/outliers1.R'], shell=False)
	with open('logs.txt', 'a') as g:
		g.write("----------------------------------------------------------------" + '\n')
		g.write("Threshold: " + threshold_val+ '\n'+'\n')
	
def chisq_tests():
	f = open('r/chisq_tests.R','r')
	filedata = f.read()
	f.close()
	
	file_output_path = "write.table(newdata,"+ "file" + "=" + '"datacleaner_Output/chisq_tests_out.txt",' + "sep=" +"'\\t'" + ", row.names=FALSE, quote=FALSE)"

	newdata = filedata

	f = open('r/gui_generated_scripts/chisq_tests1.R','w')
	f.write(newdata)
	f.write(final + '\n')
	f.write(threshold_val + '\n')
	f.write("newdata <- chisq_tests(a0)"+ '\n')
	f.write(file_output_path + '\n')


	f.close()
	proc = subprocess.call(['Rscript','r/gui_generated_scripts/chisq_tests1.R'], shell=False)
	with open('logs.txt', 'a') as g:
		g.write("Chi Square Function Called" + '\n'+'\n')


###***************************************************************************************###
   		###*************************** GUI CLASS BELOW **************************###
###***************************************************************************************###
class App(Tk):

	def __init__(self, *args, **kwargs):
		Tk.__init__(self, *args, **kwargs)

		#Setup Menu
		MainMenu(self)
		#Setup Frams
		container = Frame(self)
		container.pack(side="top", fill="both", expand=True)
		container.grid_rowconfigure(0, weight=1)		
		container.grid_columnconfigure(0, weight=1)


		self.frames = {}

		for F in (StartPage, CleanData, RecodeMissing, RecodeKey, RemoveOutliers, VisualizeData,FilterData,SummarizeData, HistogramProgram,BoxPlotProgram,QQPlot,BarPlotProgram,CheckPage,MenuOptions,ContinuousProgram,CategoricalProgram,AmbiguousProgram,SampleKeepProgram,TransformData,FindOutliersProgram,FindCorrelationsProgram,VisualizeDataMenu,SummarizeDataMenu,EndPage

):
			frame = F(container, self)
			self.frames[F] = frame
			frame.grid(row=0, column=0, sticky="snew")

		self.show_frame(StartPage)

	def show_frame(self, context):
		frame = self.frames[context]
		frame.tkraise()



class StartPage(Frame):
	def __init__(self,parent, controller):
		Frame.__init__(self,parent)

		label = Label(self, text="Welcome to Data Cleaner")
		label.config(width=200)
		label.config(font=("Courier", 35))
		label.config(fg="cyan4")


		label.pack(pady=10)

		global choose_start_file_name
		choose_start_file_name= StringVar()
		choose_start_file_name.set("Start by Choosing a Data File")

		
		choose_file_label = Label(self, textvariable=choose_start_file_name)
		choose_file_label.config(font=("Courier", 17))
		choose_file_label.pack(pady=20)
		
		choose_file = Button(self,text="Choose File", command= choose_start_file)
		choose_file.pack(pady=20)

		continue_button = Button(self,text="Continue", command= lambda:controller.show_frame(CheckPage))
		continue_button.pack(pady=20)
		# self.button.pack(ipadx=10, ipady=10)


class CheckPage(Frame):
	def __init__(self,parent, controller):
		Frame.__init__(self,parent)

		label = Label(self, text="Do you want to clean your data file?")
		label.config(width=200)
		label.config(font=("Courier", 20))
		label.pack(pady=20)
		label.config(fg="cyan4")



		global var_int 
		var_int = IntVar()
		var_int.set = 0

		def checkbutton_value1():
		    if(var1.get()):
		       var2.set(0)
		       controller.show_frame(CleanData)


		def checkbutton_value2():
		    if(var2.get()):
		       var1.set(0)
		       controller.show_frame(MenuOptions)

		var1=IntVar()
		checkbox_1 = Checkbutton(self, text='Yes   ', variable=var1, command=checkbutton_value1,font=("Courier", 20))
		checkbox_1.pack(pady=10)
		var2=IntVar()
		checkbox_2 = Checkbutton(self, text='No   ', variable=var2, command=checkbutton_value2,font=("Courier", 20))
		checkbox_2.pack(pady=(0,40))


		back = Button(self,text="Back", command= lambda:controller.show_frame(StartPage))
		back.pack()
#Clean File Page
class CleanData(Frame):
	def __init__(self,parent, controller):
		Frame.__init__(self,parent)

		label = Label(self, text="Do you have one missing value to replace in your file?")
		label.config(width=200)
		label.config(font=("Courier", 20))
		label.pack(pady=20)
		label.config(fg="cyan4")


		def checkbutton_value1():
		    if(var1.get()):
		       var2.set(0)
		       controller.show_frame(RecodeMissing)


		def checkbutton_value2():
		    if(var2.get()): 
		       var1.set(0)
		       controller.show_frame(RecodeKey)

		var1=IntVar()
		checkbox_1 = Checkbutton(self, text='Yes   ', variable=var1, command=checkbutton_value1,font=("Courier", 20))
		checkbox_1.pack(pady=10)
		var2=IntVar()
		checkbox_2 = Checkbutton(self, text='No   ', variable=var2, command=checkbutton_value2,font=("Courier", 20))
		checkbox_2.pack(pady=(0,40))

		back = Button(self,text="Back", command= lambda:controller.show_frame(CheckPage))
		back.pack()


class MenuOptions(Frame):
	def __init__(self,parent, controller):
		Frame.__init__(self,parent)

		label = Label(self, text="Do you want to filter your Data?")
		label.config(width=200)
		label.config(font=("Courier", 20))
		label.pack(pady=(10,40))
		label.config(fg="cyan4")


		def checkbutton_value1():
		    if(var1.get()):
		       var2.set(0)
		       controller.show_frame(FilterData)


		def checkbutton_value2():
		    if(var2.get()):
		       var1.set(0)
		       controller.show_frame(VisualizeDataMenu)

		var1=IntVar()
		checkbox_1 = Checkbutton(self, text='Yes   ', variable=var1, command=checkbutton_value1,font=("Courier", 20))
		checkbox_1.pack(pady=10)
		var2=IntVar()
		checkbox_2 = Checkbutton(self, text='No   ', variable=var2, command=checkbutton_value2,font=("Courier", 20))
		checkbox_2.pack(pady=(0,40))


		# filter_data_page = Button(self,text="Filter Data", command= lambda:controller.show_frame(FilterData))
		# filter_data_page.pack(pady=(0,20))


		# visualize_data_page = Button(self,text="Visualize Data", command= lambda:controller.show_frame(VisualizeDataMenu))
		# visualize_data_page.pack(pady=(0,20))

		# summarize_page_button = Button(self,text="Summarize Data", command= lambda:controller.show_frame(SummarizeDataMenu))
		# summarize_page_button.pack(pady=(0,40))

		back = Button(self,text="Back", command= lambda:controller.show_frame(CheckPage))
		back.pack()

class VisualizeDataMenu(Frame):

	def __init__(self,parent, controller):
		Frame.__init__(self,parent)


		label = Label(self, text="Do you want to Visualize your Data?")
		label.config(width=200)
		label.config(font=("Courier", 20))
		label.pack(pady=30)
		label.config(fg="cyan4")

		def checkbutton_value1():
		    if(var1.get()):
		       var2.set(0)
		       controller.show_frame(VisualizeData)


		def checkbutton_value2():
		    if(var2.get()):
		       var1.set(0)
		       controller.show_frame(SummarizeDataMenu)


		var1=IntVar()
		checkbox_1 = Checkbutton(self, text='Yes   ', variable=var1, command=checkbutton_value1,font=("Courier", 20))
		checkbox_1.pack(pady=10)
		var2=IntVar()
		checkbox_2 = Checkbutton(self, text='No   ', variable=var2, command=checkbutton_value2,font=("Courier", 20))
		checkbox_2.pack(pady=(0,40))

		back = Button(self,text="Back", command= lambda:controller.show_frame(MenuOptions))
		back.pack()

class SummarizeDataMenu(Frame):

	def __init__(self,parent, controller):
		Frame.__init__(self,parent)


		label = Label(self, text="Want to make Summaries of your Data?")
		label.config(width=200)
		label.config(font=("Courier", 20))
		label.pack(pady=30)
		label.config(fg="cyan4")

		def checkbutton_value1():
		    if(var1.get()):
		       var2.set(0)
		       controller.show_frame(SummarizeData)


		def checkbutton_value2():
		    if(var2.get()):
		       var1.set(0)
		       controller.show_frame(EndPage)


		var1=IntVar()
		checkbox_1 = Checkbutton(self, text='Yes   ', variable=var1, command=checkbutton_value1,font=("Courier", 20))
		checkbox_1.pack(pady=10)
		var2=IntVar()
		checkbox_2 = Checkbutton(self, text='No   ', variable=var2, command=checkbutton_value2,font=("Courier", 20))
		checkbox_2.pack(pady=(0,40))

		back = Button(self,text="Back", command= lambda:controller.show_frame(VisualizeDataMenu))
		back.pack()

class EndPage(Frame):
	def __init__(self,parent, controller):
		Frame.__init__(self,parent)


		label = Label(self, text="EndPage")
		label.config(width=200)
		label.config(font=("Courier", 20))
		label.pack(pady=30)
		label.config(fg="cyan4")


		back = Button(self,text="Start Over", command= lambda:controller.show_frame(StartPage))
		back.pack()

		back = Button(self,text="Back", command= lambda:controller.show_frame(SummarizeDataMenu))
		back.pack()


#recode missing
class RecodeMissing(Frame):

	def __init__(self,parent, controller):
		Frame.__init__(self,parent)


		label = Label(self, text="Replace Missing Values with Input")
		label.config(width=200)
		label.config(font=("Courier", 20))
		label.pack(pady=30)
		label.config(fg="cyan4")



		global missing_val_replacement 
		missing_val_replacement = StringVar()

		global missing_val 
		missing_val = StringVar()


		replacement_label = Label(self, text="What do want to replace your missing value with?",font=("Courier", 14))
		replacement_label.pack()
		replacement_box  = Entry(self, textvariable=missing_val_replacement, width=15, bg="alice blue")
		replacement_box.pack(pady=(0,35))

		missing_label = Label(self, text="Type in the missing value",font=("Courier", 14))
		missing_label.pack()
		missing_entry_box  = Entry(self, textvariable=missing_val, width=15, bg="alice blue")
		missing_entry_box.pack(pady=(0,30))

		clense_button = Button(self,text="Clense", command= recode_missing)
		clense_button.pack(pady=(0,25))

		continue_button = Button(self,text="Continue", command= lambda:controller.show_frame(MenuOptions))
		continue_button.pack(pady=(0,25))

		back = Button(self,text="Back", command= lambda:controller.show_frame(CleanData))
		back.pack()
#recode key
class RecodeKey(Frame):
	def __init__(self,parent, controller):
		Frame.__init__(self,parent)


		label = Label(self, text="Missing Value Dictionary - Replace Missing")
		label.config(width=200)
		label.config(font=("Courier", 20))
		label.pack(pady=20)
		label.config(fg="cyan4")

		# start_page = Button(self,text="Back to Main Menu", command= lambda:controller.show_frame(StartPage))
		# start_page.pack()

		global choose_dictionary_label_name 
		choose_dictionary_label_name= StringVar()
		choose_dictionary_label_name.set("No Dictionary Chosen")

		choose_dictionary_label = Label(self, textvariable=choose_dictionary_label_name,font=("Courier", 14))
		choose_dictionary_label.pack(pady=5)


		choose_dictionary = Button(self,text="Choose Dictionary", command= dialogue_file_dictionary)
		choose_dictionary.pack(pady=(0,35))	


		clense_button = Button(self,text="Clense", command= recode_key)
		clense_button.pack(pady=(0,25))


		continue_button = Button(self,text="Continue", command= lambda:controller.show_frame(MenuOptions))
		continue_button.pack(pady=(0,25))

		back = Button(self,text="Back", command= lambda:controller.show_frame(CleanData))
		back.pack()
		######


#Remove Outliers
class RemoveOutliers(Frame):
	def __init__(self,parent, controller):
		Frame.__init__(self,parent)

		label = Label(self, text="Remove Outliers",font=("Courier", 20))
		label.pack(pady=20)
		label.config(fg="cyan4")


		global choose_outlier_file_name 
		choose_outlier_file_name= StringVar()
		choose_outlier_file_name.set("No Data File Chosen")
		

		remove_outliers_button = Button(self,text="Remove Outliers", command= remove_outliers)
		remove_outliers_button.pack(pady=(0,45))


		back = Button(self,text="Back", command= lambda:controller.show_frame(FilterData))
		back.pack()


class FilterData(Frame):
	def __init__(self,parent, controller):
		Frame.__init__(self,parent)

		label = Label(self, text="Filter Data Menu")
		label.config(font=("Courier", 20))
		label.pack(pady=20)
		label.config(fg="cyan4")


		remove_outliers = Button(self,text="Remove Outliers", command= lambda:controller.show_frame(RemoveOutliers))
		remove_outliers.pack(pady=(0,15))


		get_binary_button = Button(self,text="Find Binary Variables", command= get_binary)
		get_binary_button.pack(pady=(0,15))

		get_continuous_button = Button(self,text="Find Continuous Variables", command= lambda:controller.show_frame(ContinuousProgram))
		get_continuous_button.pack(pady=(0,15))

		get_categorical_button = Button(self,text="Find Categorical Variables", command= lambda:controller.show_frame(CategoricalProgram))
		get_categorical_button.pack(pady=(0,15))

		get_check_button = Button(self,text="Find Ambiguous Variables", command= lambda:controller.show_frame(AmbiguousProgram))
		get_check_button.pack(pady=(0,15))

		sample_keep_button = Button(self,text="Keep N Samples", command= lambda:controller.show_frame(SampleKeepProgram))
		sample_keep_button.pack(pady=(0,15))

		transform_list_button = Button(self,text="Transform Data", command= lambda:controller.show_frame(TransformData))
		transform_list_button.pack(pady=(0,40))


		global choose_filter_file_name 
		choose_filter_file_name= StringVar()
		choose_filter_file_name.set("No Data File Chosen")
		

		back = Button(self,text="Back", command= lambda:controller.show_frame(MenuOptions))
		back.pack()


def get_key_dictionary():
	global global_transform_key_dictionary
	global_transform_key_dictionary = tkFileDialog.askopenfilename()
	key_dictionary_label.set("Dictionary File: " + global_transform_key_dictionary)


class TransformData(Frame):
	def __init__(self,parent, controller):
		Frame.__init__(self,parent)

		label = Label(self, text="Transform User List of Columns & Transform Type")
		label.config(font=("Courier", 20))
		label.pack(pady=20)
		label.config(fg="cyan4")


		global key_dictionary_label 
		key_dictionary_label= StringVar()
		key_dictionary_label.set("No Dictionary Chosen")

		choose_key_dictionary_label = Label(self, textvariable=key_dictionary_label)
		choose_key_dictionary_label.pack(pady=5)


		choose_key_dictionary = Button(self,text="Choose Key Dictionary", command= get_key_dictionary)
		choose_key_dictionary.pack(pady=(0,20))	


		transform_data_button = Button(self,text="Transform List", command = transform_list)
		transform_data_button.pack(pady=(0,40))

		back = Button(self,text="Back", command= lambda:controller.show_frame(FilterData))
		back.pack()

#Analyze File Data
class VisualizeData(Frame):
	def __init__(self,parent, controller):
		Frame.__init__(self,parent)

		label = Label(self, text="Visualize Data Menu")
		label.config(width=200)
		label.config(font=("Courier", 20))
		label.pack(pady=20)
		label.config(fg="cyan4")



		histogram_button = Button(self,text="Generate Histogram", command= lambda:controller.show_frame(HistogramProgram))
		histogram_button.pack(pady=(0,20))

		box_plot_button = Button(self,text="Generate Box Plot", command= lambda:controller.show_frame(BoxPlotProgram))
		box_plot_button.pack(pady=(0,20))

		qq_plot_button = Button(self,text="Generate QQ Plot", command= lambda:controller.show_frame(QQPlot))
		qq_plot_button.pack(pady=(0,20))

		bar_plot_button = Button(self,text="Generate Bar Plot", command= lambda:controller.show_frame(BarPlotProgram))
		bar_plot_button.pack(pady=(0,40))

		back = Button(self,text="Back", command= lambda:controller.show_frame(VisualizeDataMenu))
		back.pack()



class SummarizeData(Frame):
	def __init__(self,parent, controller):
		Frame.__init__(self,parent)

		label = Label(self, text="Summarize Data Menu")
		label.config(width=200)
		label.config(font=("Courier", 20))
		label.pack(pady=20)
		label.config(fg="cyan4")



		frequency_table_button = Button(self,text="Frequency Table", command= frequency_table)
		frequency_table_button.pack(pady=(0,20))

		correlations_button = Button(self,text="Find Correlations", command= lambda:controller.show_frame(FindCorrelationsProgram))
		correlations_button.pack(pady=(0,20))

		get_levels_button = Button(self,text="Get Levels", command= get_levels)
		get_levels_button.pack(pady=(0,20))

		sample_size_button = Button(self,text="Sample Size", command= sample_size)
		sample_size_button.pack(pady=(0,20))

		outliers_button = Button(self,text="Find Outliers", command= lambda:controller.show_frame(FindOutliersProgram))
		outliers_button.pack(pady=(0,20))

		chisq_tests_button = Button(self,text="Chisq Tests", command= chisq_tests)
		chisq_tests_button.pack(pady=(0,40))

		back = Button(self,text="Back", command= lambda:controller.show_frame(SummarizeDataMenu))
		back.pack()

#Histogram Program
class HistogramProgram(Frame):
	def __init__(self,parent, controller):
		Frame.__init__(self,parent)

		label = Label(self, text="Histogram Program")
		label.config(width=200)
		label.config(font=("Courier", 20))
		label.pack(pady=(10,40))
		label.config(fg="cyan4")


		histogram_button2 = Button(self,text="Generate Histogram", command= histogram)
		histogram_button2.pack(pady=(0,20))

		back = Button(self,text="Back", command= lambda:controller.show_frame(VisualizeData))
		back.pack()

		global h1
		h1= StringVar()
		h1.set("")

		global h2
		h2= StringVar()
		h2.set("")

		global h3
		h3= StringVar()
		h3.set("")

		global h4
		h4= StringVar()
		h4.set("")

		global h5
		h5= StringVar()
		h5.set("")

		global h6
		h6= StringVar()
		h6.set("")

		left_frame = Frame(self)
		left_frame.pack(side=LEFT)
		right_frame = Frame(self)
		right_frame.pack(side=RIGHT)


		plots_label = Label(left_frame, text="# of Plots Per Page",font=("Courier", 14))
		plots_label.pack()
		plots_labelentry_box  = Entry(left_frame, textvariable=h1, width=15, bg="alice blue")
		plots_labelentry_box.pack(padx=(90),pady=(0,20))

		row_label = Label(left_frame, text="# of Rows Per Page",font=("Courier", 14))
		row_label.pack()
		row_label_entry_box  = Entry(left_frame, textvariable=h2, width=15, bg="alice blue")
		row_label_entry_box.pack(padx=(90),pady=(0,20))

		column_label = Label(left_frame, text="# of Columns Per Page",font=("Courier", 14))
		column_label.pack()
		column_label_entry_box  = Entry(left_frame, textvariable=h3, width=15, bg="alice blue")
		column_label_entry_box.pack(padx=(90),pady=(0,20))

		width_label = Label(right_frame, text="Width of Plot",font=("Courier", 14))
		width_label.pack(padx=(0,100))
		width_label_entry_box  = Entry(right_frame, textvariable=h4, width=15, bg="alice blue")
		width_label_entry_box.pack(padx=(0,100),pady=(0,20))

		height_label = Label(right_frame, text="Height of Plot",font=("Courier", 14))
		height_label.pack(padx=(0,100))
		height_label_entry_box  = Entry(right_frame, textvariable=h5, width=15, bg="alice blue")
		height_label_entry_box.pack(padx=(0,100),pady=(0,20))

		resolution_label = Label(right_frame, text="Resolution of Plot",font=("Courier", 14))
		resolution_label.pack(padx=(0,100))
		resolution_label_entry_box  = Entry(right_frame, textvariable=h6, width=15, bg="alice blue")
		resolution_label_entry_box.pack(padx=(0,100),pady=(0,20))

		# tfield = tkinter.Text(self)
		# tfield.pack()
		# for line in os.popen("run_command", 'r'):
		# 	tfield.insert("end", line)

#Box Plot Program
class BoxPlotProgram(Frame):
	def __init__(self,parent, controller):
		Frame.__init__(self,parent)

		label = Label(self, text="Box Plot Program")
		label.config(width=200)
		label.config(font=("Courier", 20))
		label.pack(pady=(10,40))
		label.config(fg="cyan4")


		box_plot_button = Button(self,text="Generate Box Plot", command= boxplot)
		box_plot_button.pack(pady=(0,20))

		back = Button(self,text="Back", command= lambda:controller.show_frame(VisualizeData))
		back.pack()

		global bx1
		bx1= StringVar()
		bx1.set("")

		global bx2
		bx2= StringVar()
		bx2.set("")

		global bx3
		bx3= StringVar()
		bx3.set("")

		global bx4
		bx4= StringVar()
		bx4.set("")

		global bx5
		bx5= StringVar()
		bx5.set("")

		global bx6
		bx6= StringVar()
		bx6.set("")

		left_frame = Frame(self)
		left_frame.pack(side=LEFT)
		right_frame = Frame(self)
		right_frame.pack(side=RIGHT)


		plots_label = Label(left_frame, text="# of Plots Per Page",font=("Courier", 14))
		plots_label.pack()
		plots_labelentry_box  = Entry(left_frame, textvariable=bx1, width=15, bg="alice blue")
		plots_labelentry_box.pack(padx=(90),pady=(0,20))

		row_label = Label(left_frame, text="# of Rows Per Page",font=("Courier", 14))
		row_label.pack()
		row_label_entry_box  = Entry(left_frame, textvariable=bx2, width=15, bg="alice blue")
		row_label_entry_box.pack(padx=(90),pady=(0,20))

		column_label = Label(left_frame, text="# of Columns Per Page",font=("Courier", 14))
		column_label.pack()
		column_label_entry_box  = Entry(left_frame, textvariable=bx3, width=15, bg="alice blue")
		column_label_entry_box.pack(padx=(90),pady=(0,20))

		width_label = Label(right_frame, text="Width of Plot",font=("Courier", 14))
		width_label.pack(padx=(0,100))
		width_label_entry_box  = Entry(right_frame, textvariable=bx4, width=15, bg="alice blue")
		width_label_entry_box.pack(padx=(0,100),pady=(0,20))

		height_label = Label(right_frame, text="Height of Plot",font=("Courier", 14))
		height_label.pack(padx=(0,100))
		height_label_entry_box  = Entry(right_frame, textvariable=bx5, width=15, bg="alice blue")
		height_label_entry_box.pack(padx=(0,100),pady=(0,20))

		resolution_label = Label(right_frame, text="Resolution of Plot",font=("Courier", 14))
		resolution_label.pack(padx=(0,100))
		resolution_label_entry_box  = Entry(right_frame, textvariable=bx6, width=15, bg="alice blue")
		resolution_label_entry_box.pack(padx=(0,100),pady=(0,20))
#QQ Plot 
class QQPlot(Frame):
	def __init__(self,parent, controller):
		Frame.__init__(self,parent)


		label = Label(self, text="QQPlot Program")
		label.config(width=200)
		label.config(font=("Courier", 20))
		label.pack(pady=(10,40))
		label.config(fg="cyan4")

		

		qq_plot_button = Button(self,text="Generate QQPlot", command= qqplot)
		qq_plot_button.pack(pady=(0,20))

		back = Button(self,text="Back", command= lambda:controller.show_frame(VisualizeData))
		back.pack()

		global qq1
		qq1= StringVar()
		qq1.set("")

		global qq2
		qq2= StringVar()
		qq2.set("")

		global qq3
		qq3= StringVar()
		qq3.set("")

		global qq4
		qq4= StringVar()
		qq4.set("")

		global qq5
		qq5= StringVar()
		qq5.set("")

		global qq6
		qq6= StringVar()
		qq6.set("")

		left_frame = Frame(self)
		left_frame.pack(side=LEFT)
		right_frame = Frame(self)
		right_frame.pack(side=RIGHT)


		plots_label = Label(left_frame, text="# of Plots Per Page",font=("Courier", 14))
		plots_label.pack()
		plots_labelentry_box  = Entry(left_frame, textvariable=qq1, width=15, bg="alice blue")
		plots_labelentry_box.pack(padx=(90),pady=(0,20))

		row_label = Label(left_frame, text="# of Rows Per Page",font=("Courier", 14))
		row_label.pack()
		row_label_entry_box  = Entry(left_frame, textvariable=qq2, width=15, bg="alice blue")
		row_label_entry_box.pack(padx=(90),pady=(0,20))

		column_label = Label(left_frame, text="# of Columns Per Page",font=("Courier", 14))
		column_label.pack()
		column_label_entry_box  = Entry(left_frame, textvariable=qq3, width=15, bg="alice blue")
		column_label_entry_box.pack(padx=(90),pady=(0,20))

		width_label = Label(right_frame, text="Width of Plot",font=("Courier", 14))
		width_label.pack(padx=(0,100))
		width_label_entry_box  = Entry(right_frame, textvariable=qq4, width=15, bg="alice blue")
		width_label_entry_box.pack(padx=(0,100),pady=(0,20))

		height_label = Label(right_frame, text="Height of Plot",font=("Courier", 14))
		height_label.pack(padx=(0,100))
		height_label_entry_box  = Entry(right_frame, textvariable=qq5, width=15, bg="alice blue")
		height_label_entry_box.pack(padx=(0,100),pady=(0,20))

		resolution_label = Label(right_frame, text="Resolution of Plot",font=("Courier", 14))
		resolution_label.pack(padx=(0,100))
		resolution_label_entry_box  = Entry(right_frame, textvariable=qq6, width=15, bg="alice blue")
		resolution_label_entry_box.pack(padx=(0,100),pady=(0,20))

#Bar Plot Program
class BarPlotProgram(Frame):
	def __init__(self,parent, controller):
		Frame.__init__(self,parent)

		label = Label(self, text="Bar Plot Program")
		label.config(width=200)
		label.config(font=("Courier", 20))
		label.pack(pady=(10,40))
		label.config(fg="cyan4")


	
		bar_plot_button = Button(self,text="Generate Bar Plot", command= barplot)
		bar_plot_button.pack(pady=(0,20))

		back = Button(self,text="Back", command= lambda:controller.show_frame(VisualizeData))
		back.pack()

		global bp1
		bp1= StringVar()
		bp1.set("")

		global bp2
		bp2= StringVar()
		bp2.set("")

		global bp3
		bp3= StringVar()
		bp3.set("")

		global bp4
		bp4= StringVar()
		bp4.set("")

		global bp5
		bp5= StringVar()
		bp5.set("")

		global bp6
		bp6= StringVar()
		bp6.set("")

		left_frame = Frame(self)
		left_frame.pack(side=LEFT)
		right_frame = Frame(self)
		right_frame.pack(side=RIGHT)


		plots_label = Label(left_frame, text="# of Plots Per Page",font=("Courier", 14))
		plots_label.pack()
		plots_labelentry_box  = Entry(left_frame, textvariable=bp1, width=15, bg="alice blue")
		plots_labelentry_box.pack(padx=(90),pady=(0,20))

		row_label = Label(left_frame, text="# of Rows Per Page",font=("Courier", 14))
		row_label.pack()
		row_label_entry_box  = Entry(left_frame, textvariable=bp2, width=15, bg="alice blue")
		row_label_entry_box.pack(padx=(90),pady=(0,20))

		column_label = Label(left_frame, text="# of Columns Per Page",font=("Courier", 14))
		column_label.pack()
		column_label_entry_box  = Entry(left_frame, textvariable=bp3, width=15, bg="alice blue")
		column_label_entry_box.pack(padx=(90),pady=(0,20))

		width_label = Label(right_frame, text="Width of Plot",font=("Courier", 14))
		width_label.pack(padx=(0,100))
		width_label_entry_box  = Entry(right_frame, textvariable=bp4, width=15, bg="alice blue")
		width_label_entry_box.pack(padx=(0,100),pady=(0,20))

		height_label = Label(right_frame, text="Height of Plot",font=("Courier", 14))
		height_label.pack(padx=(0,100))
		height_label_entry_box  = Entry(right_frame, textvariable=bp5, width=15, bg="alice blue")
		height_label_entry_box.pack(padx=(0,100),pady=(0,20))

		resolution_label = Label(right_frame, text="Resolution of Plot",font=("Courier", 14))
		resolution_label.pack(padx=(0,100))
		resolution_label_entry_box  = Entry(right_frame, textvariable=bp6, width=15, bg="alice blue")
		resolution_label_entry_box.pack(padx=(0,100),pady=(0,20))


##1
class ContinuousProgram(Frame):
	def __init__(self,parent, controller):
		Frame.__init__(self,parent)

		label = Label(self, text="Get Continuous Variables")
		label.config(width=200)
		label.config(font=("Courier", 20))
		label.pack(pady=(10,40))
		label.config(fg="cyan4")



		global continuous_var
		continuous_var= StringVar()
		continuous_var.set("")

		continuous_label = Label(self, text="Minimum # of Unique Values to be Considered Continuous",font=("Courier", 14))
		continuous_label.pack()
		continuous_label_entry_box  = Entry(self, textvariable=continuous_var, width=15, bg="alice blue")
		continuous_label_entry_box.pack(pady=(0,20))


		get_continuous_button = Button(self,text="Get Continuous Variables", command= get_continuous)
		get_continuous_button.pack(pady=(0,20))

		back = Button(self,text="Back", command= lambda:controller.show_frame(FilterData))
		back.pack()

class CategoricalProgram(Frame):
	def __init__(self,parent, controller):
		Frame.__init__(self,parent)

		label = Label(self, text="Get Catrgorical Variables")
		label.config(width=200)
		label.config(font=("Courier", 20))
		label.pack(pady=(10,40))
		label.config(fg="cyan4")



		global categorical1
		categorical1= StringVar()
		categorical1.set("")
		categorical1_label = Label(self, text="Min # of Values to be Considered Catrgorical",font=("Courier", 14))
		categorical1_label.pack()
		categorical1_label_entry_box  = Entry(self, textvariable=categorical1, width=15, bg="alice blue")
		categorical1_label_entry_box.pack(pady=(0,20))


		global categorical2
		categorical2= StringVar()
		categorical2.set("")
		categorical2_label = Label(self, text="Max # of Values to be Considered Catrgorical",font=("Courier", 14))
		categorical2_label.pack()
		categorical2_label_entry_box  = Entry(self, textvariable=categorical2, width=15, bg="alice blue")
		categorical2_label_entry_box.pack(pady=(0,20))


		get_categorical_button = Button(self,text="Get Categorical Variables", command= get_categorical)
		get_categorical_button.pack(pady=(0,20))

		back = Button(self,text="Back", command= lambda:controller.show_frame(FilterData))
		back.pack()

class AmbiguousProgram(Frame):
	def __init__(self,parent, controller):
		Frame.__init__(self,parent)

		label = Label(self, text="Identify Ambiguous Variables",font=("Courier", 14))
		label.config(width=200)
		label.config(font=("Courier", 20))
		label.pack(pady=(10,40))
		label.config(fg="cyan4")


		global ambiguous1
		ambiguous1= StringVar()
		ambiguous1.set("")
		ambiguous1_label = Label(self, text="Min # of Levels Desired",font=("Courier", 14))
		ambiguous1_label.pack()
		ambiguous1_label_entry_box  = Entry(self, textvariable=ambiguous1, width=15, bg="alice blue")
		ambiguous1_label_entry_box.pack(pady=(0,20))


		global ambiguous2
		ambiguous2= StringVar()
		ambiguous2.set("")
		ambiguous2_label = Label(self, text="Max # of Levels Desired",font=("Courier", 14))
		ambiguous2_label.pack()
		ambiguous2_label_entry_box  = Entry(self, textvariable=ambiguous2, width=15, bg="alice blue")
		ambiguous2_label_entry_box.pack(pady=(0,20))


		get_check_button = Button(self,text="Check Ambiguous Variables", command= get_check)
		get_check_button.pack(pady=(0,20))

		back = Button(self,text="Back", command= lambda:controller.show_frame(FilterData))
		back.pack()

##1
class SampleKeepProgram(Frame):
	def __init__(self,parent, controller):
		Frame.__init__(self,parent)

		label = Label(self, text="Keep Varibles with n Samples")
		label.config(width=200)
		label.config(font=("Courier", 20))
		label.pack(pady=(10,40))
		label.config(fg="cyan4")


		global sample_keep_var
		sample_keep_var= StringVar()
		sample_keep_var.set("")

		sample_keep_var_label = Label(self, text="Min # of Samples per Variable",font=("Courier", 14))
		sample_keep_var_label.pack()
		sample_keep_var_label_entry_box  = Entry(self, textvariable=sample_keep_var, width=15, bg="alice blue")
		sample_keep_var_label_entry_box.pack(pady=(0,20))

		get_check_button = Button(self,text="Keep N Samples", command= sample_keep)
		get_check_button.pack(pady=(0,40))


		back = Button(self,text="Back", command= lambda:controller.show_frame(FilterData))
		back.pack()


class FindOutliersProgram(Frame):
	def __init__(self,parent, controller):
		Frame.__init__(self,parent)

		label = Label(self, text="Find Outliers")
		label.config(width=200)
		label.config(font=("Courier", 20))
		label.pack(pady=(10,40))
		label.config(fg="cyan4")


		global find_outliers_var
		find_outliers_var= StringVar()
		find_outliers_var.set("")

		find_outliers_var_label = Label(self, text="Number of Standard Deviations From Mean",font=("Courier", 14))
		find_outliers_var_label.pack()
		find_outliers_var_label_entry_box  = Entry(self, textvariable=find_outliers_var, width=15, bg="alice blue")
		find_outliers_var_label_entry_box.pack(pady=(0,20))


		get_check_button = Button(self,text="Find Outliers", command= outliers)
		get_check_button.pack(pady=(0,40))


		back = Button(self,text="Back", command= lambda:controller.show_frame(SummarizeData))
		back.pack()

class FindCorrelationsProgram(Frame):
	def __init__(self,parent, controller):
		Frame.__init__(self,parent)

		label = Label(self, text="Find Correlations")
		label.config(width=200)
		label.config(font=("Courier", 20))
		label.pack(pady=(10,40))
		label.config(fg="cyan4")


		global find_correlations_var
		find_correlations_var= StringVar()
		find_correlations_var.set("")

		find_correlations_var_label = Label(self, text="Correlation Threshold",font=("Courier", 14))
		find_correlations_var_label.pack()
		find_correlations_var_label_entry_box  = Entry(self, textvariable=find_correlations_var, width=15, bg="alice blue")
		find_correlations_var_label_entry_box.pack(pady=(0,20))



		get_check_button = Button(self,text="Find Correlations", command= correlations)
		get_check_button.pack(pady=(0,40))


		back = Button(self,text="Back", command= lambda:controller.show_frame(SummarizeData))
		back.pack()
class MainMenu:
	def __init__(self, master):
		menubar = Menu(master)
		filemenu = Menu(menubar, tearoff=0)
		filemenu.add_command(label="Exit", command=master.quit)
		menubar.add_cascade(label="File", menu=filemenu)
		master.config(menu=menubar)


	
##To call the GUI Application
app = App()
app.title('DataCleaner')
app.geometry("725x500")
app.mainloop()














