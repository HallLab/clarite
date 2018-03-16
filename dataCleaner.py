##Deven Orie 
##Dartmouth'19 - CS & Econ Major
##Pipeline clean_rows Cleaner
##Imports Necessary Installations - Python2, or 2,7

##Recommended to keep DataCleaner Directory on Desktop

import ttk as ttk
from Tkinter import *
import Tkinter as tk
import subprocess
import io
import sys
import contextlib
import csv
import tkFileDialog
from io import StringIO
import os.path
import datetime


# from PIL import ImageTk
IN_WIDTH = 19
IN_HEIGHT = 18


global time_stamp
time_stamp = datetime.datetime.now().strftime("%Y-%m-%d %H:%M:%S")


if not os.path.exists("Logs"):
    os.makedirs("Logs")
if not os.path.exists("R/GUI_Scripts"):
    os.makedirs("R/GUI_Scripts")
if not os.path.exists("GUI_Output"):
    os.makedirs("GUI_Output")

global log_file_name
log_file_name = "Logs/" + time_stamp + "-"'logs.txt'
with open(log_file_name, 'w') as global_log:
	global_log.write("LIVE LOG REPORT - PROGRAM OPENED :  " + datetime.datetime.now().strftime("%I:%M%p on %B %d, %Y") + '\n')
	global_log.write("LOGS INPUT VALUES, STDOUT, STDERR, R SCRIPT & FUNC CALLS (GUI PROGRAM)"+'\n'+'\n')


##Global Constant Values
CONSTANT_INPUT = "a0 = read.delim('/Users/deven/desktop/it.txt', header=TRUE)"
A0_0 = "a0 = read.delim('"
A0_2 = "', header=TRUE)"

# GGPLOT_LIBRARY = "library(ggplot2)"
# GRIDEXTRA_LIBRARY = "library(gridExtra)"
# RCOLORBREWER_LIBRARY = "library(RColorBrewer)"



IDCOLUMN = "a0$IID <- paste(""id"", row.names(a0), sep=""_"")"

global_file_prefix = "emptyPrefix"


GLOBAL_OUTPUT_DIRECTORY = "GUI_Output"


GLOBAL_OUTPUT_DIRECTORY_FORMATTED = ""
###***************************************************************************************###
   		###************************** Start Program ***********************###

##Choose Global File - Start of Program
def choose_start_file():
	global global_file_prefix



	global global_start_file_path
	global_start_file_path = tkFileDialog.askopenfilename()
	choose_start_file_name.set("Data File: " + global_start_file_path)

	global final
	final = A0_0 + global_start_file_path + A0_2

	with open(log_file_name, 'a') as g:
		g.write("---------------------------------------------------------------" + datetime.datetime.now().strftime("%I:%M:%S %p") + "---------------------------------------------------------------" + '\n')
		g.write("File Chosen: "+ global_start_file_path + '\n')
		g.write("Output Folder: "+ GLOBAL_OUTPUT_DIRECTORY + '\n')

		if os.path.splitext(os.path.basename(global_start_file_path))[0] != "":
			# if global_file_prefix == "emptyPrefix":
			global_file_prefix = os.path.splitext(os.path.basename(global_start_file_path))[0] + "_"
			global GLOBAL_OUTPUT_DIRECTORY_FORMATTED 
			GLOBAL_OUTPUT_DIRECTORY_FORMATTED = "'" + GLOBAL_OUTPUT_DIRECTORY  + "/" + global_file_prefix
		print(global_file_prefix)
	refresh_logs()

def output_directory_change():
	global GLOBAL_OUTPUT_DIRECTORY
	GLOBAL_OUTPUT_DIRECTORY = str(change_output_directory_var.get())
	choose_directory_file_name.set("Output Folder:  " + GLOBAL_OUTPUT_DIRECTORY)
	
	with open(log_file_name, 'a') as g:
		g.write("---------------------------------------------------------------" + datetime.datetime.now().strftime("%I:%M:%S %p") + "---------------------------------------------------------------" + '\n')
		g.write("Output Folder Changed: "+ GLOBAL_OUTPUT_DIRECTORY + '\n')
	refresh_logs()

	if not os.path.exists(GLOBAL_OUTPUT_DIRECTORY):
    		os.makedirs(GLOBAL_OUTPUT_DIRECTORY)

	global GLOBAL_OUTPUT_DIRECTORY_FORMATTED 
	GLOBAL_OUTPUT_DIRECTORY_FORMATTED = "'" + GLOBAL_OUTPUT_DIRECTORY  + "/" + global_file_prefix
	o_toplevel.destroy()

def choose_output_directory():

	global o_toplevel 
	o_toplevel = Toplevel()
	o_toplevel.geometry("255x215")

	label1 = Label(o_toplevel,bg="white",text = "Change Output Folder")
	label1.grid(row=0, column=0, columnspan=2)
	label1.config(font=("Comfortaa", 18))
	label1.config(fg="cyan4")


	global change_output_directory_var
	change_output_directory_var= StringVar()
	change_output_directory_var.set("")

	change_output_directory_var_label = Label(o_toplevel, text="Output Folder Name",font=("Comfortaa", 14))
	change_output_directory_var_label.grid(row=1, column=0)
	change_output_directory_var_label_entry_box  = Entry(o_toplevel, textvariable=change_output_directory_var, width=15, bg="alice blue")
	change_output_directory_var_label_entry_box.grid(row=1, column=1)


	change_button = Button(o_toplevel,text="Change", command= output_directory_change)
	change_button.grid(row=2, column=0,columnspan=2)

##Dialogue Function
def dialogue_file_dictionary():
	global global_recode_key_dictionary
	global_recode_key_dictionary = tkFileDialog.askopenfilename()
	choose_dictionary_label_name.set("Dictionary File: " + global_recode_key_dictionary)

	with open(log_file_name, 'a') as g:
		g.write("Recode Key Dictionary Chosen: "+ global_recode_key_dictionary + '\n'+'\n')
	refresh_logs()
	

def column_file_chosen():

	global column_file_name 
	column_file_name = tkFileDialog.askopenfilename()
	cofilter1.set("File: " + column_file_name)


	with open(log_file_name, 'a') as g:
		g.write("---------------------------------------------------------------" + datetime.datetime.now().strftime("%I:%M:%S %p") + "---------------------------------------------------------------" + '\n')
		g.write("Column File Uploaded: "+ column_file_name + '\n')
	refresh_logs()

	
def sample_file_chosen():

	global sample_file_name 
	sample_file_name = tkFileDialog.askopenfilename()
	rowfilter1.set("File: " + sample_file_name)


	with open(log_file_name, 'a') as g:
		g.write("---------------------------------------------------------------" + datetime.datetime.now().strftime("%I:%M:%S %p") + "---------------------------------------------------------------" + '\n')
		g.write("Column File Uploaded: "+ sample_file_name + '\n')
	refresh_logs()


def ewas1_file_chosen():

	global global_ewas_filename_1
	global_ewas_filename_1 = tkFileDialog.askopenfilename()
	if global_ewas_filename_1 == "":
		ewas_choosefile_1.set("No File Uploaded")
	else:
		ewas_choosefile_1.set("File Uploaded")

	with open(log_file_name, 'a') as g:
		g.write("---------------------------------------------------------------" + datetime.datetime.now().strftime("%I:%M:%S %p") + "---------------------------------------------------------------" + '\n')
		g.write("Categorical File Uploaded: "+ global_ewas_filename_1 + '\n')
	refresh_logs()


def ewas2_file_chosen():

	global global_ewas_filename_2
	global_ewas_filename_2 = tkFileDialog.askopenfilename()

	if global_ewas_filename_2 == "":
		ewas_choosefile_2.set("No File Uploaded")
	else:
		ewas_choosefile_2.set("File Uploaded")

	with open(log_file_name, 'a') as g:
		g.write("---------------------------------------------------------------" + datetime.datetime.now().strftime("%I:%M:%S %p") + "---------------------------------------------------------------" + '\n')
		g.write("Continuous File Uploaded: "+ global_ewas_filename_2 + '\n')
	refresh_logs()


def merge_data_choose_file():

	global merge_data_file
	merge_data_file = tkFileDialog.askopenfilename()
	second_file_upload_merge.set("File: " + merge_data_file)
	
	with open(log_file_name, 'a') as g:
		g.write("---------------------------------------------------------------" + datetime.datetime.now().strftime("%I:%M:%S %p") + "---------------------------------------------------------------" + '\n')
		g.write("Second File Uploaded - Merge Data File: "+ merge_data_file + '\n')
	refresh_logs()


def recode_missing_file_upload():

	global key_recode_file
	key_recode_file = tkFileDialog.askopenfilename()
	recode_key_file_var.set("File: " + key_recode_file)
	
	with open(log_file_name, 'a') as g:
		g.write("---------------------------------------------------------------" + datetime.datetime.now().strftime("%I:%M:%S %p") + "---------------------------------------------------------------" + '\n')
		g.write("Recode Key File Uploaded- File: "+ key_recode_file + '\n')
	refresh_logs()


def upload_tranformation_file():

	global transformation_file
	transformation_file = tkFileDialog.askopenfilename()
	transformation_file_var.set("File: " + transformation_file)
	
	with open(log_file_name, 'a') as g:
		g.write("---------------------------------------------------------------" + datetime.datetime.now().strftime("%I:%M:%S %p") + "---------------------------------------------------------------" + '\n')
		g.write("Transformation File Uploaded: " + transformation_file + '\n')
	refresh_logs()

def optional_file_mplot():

	global option_groups_file_mplot
	option_groups_file_mplot = tkFileDialog.askopenfilename()
	if option_groups_file_mplot != "":
		manhattan_plot_choosefile.set("File Uploaded")
	else:
		manhattan_plot_choosefile.set("No File Chosen")

	# manhattan_plot_choosefile.set("File: " + os.path.splitext(os.path.basename(option_groups_file_mplot))[0])
	
	with open(log_file_name, 'a') as g:
		g.write("---------------------------------------------------------------" + datetime.datetime.now().strftime("%I:%M:%S %p") + "---------------------------------------------------------------" + '\n')
		g.write("Optional Groups File Uploaded: " + option_groups_file_mplot + '\n')
	refresh_logs()


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

# 6) Output is stored at images, pdf, textdocuments in directory GUI_Output
###***************************************************************************************###


###***************************************************************************************###
   		###************************** Clean Data Functions ***********************###
###***************************************************************************************###
def recode_missing():

	f = open('r/recode_missing.R','r')
	filedata = f.read()
	f.close()

	newdata = filedata

	argument_1 = "a1 = " + "'" + str(rc_specific_var.get()) + "'"

	f = open('r/GUI_Scripts/recode_missing1.R','w')
	f.write(newdata)
	f.write(final + '\n')
	f.write(argument_1 + '\n')

	f.write("newdata <- recode_missing(a0, a1)"+ '\n')
	file_output_path = "write.table(newdata,"+ "file" + "=" + GLOBAL_OUTPUT_DIRECTORY_FORMATTED +  "SpecificMiss.txt" + "'," + "sep=" +"'\\t'" + ", row.names=FALSE, quote=FALSE)"

	f.write(file_output_path+ '\n')


	f.close()

	with open(log_file_name, 'a') as g:
		g.write("---------------------------------------------------------------" + datetime.datetime.now().strftime("%I:%M:%S %p") + "---------------------------------------------------------------" + '\n')

		g.write("Specific Recode Missing Function Called" + '\n' +'\n' )
	

	s_out = open(log_file_name, "a")
	proc = subprocess.call(['Rscript','r/GUI_Scripts/recode_missing1.R'], shell=False,stdout=s_out, stderr=s_out)

	refresh_logs()


def recode_key():

	f = open('r/recode_key.R','r')
	filedata = f.read()
	f.close()

	newdata = filedata

	argument_1 = "a1 = " + "'" + str(recode_key_file_var.get()) + "'"

	f = open('r/GUI_Scripts/recode_key1.R','w')
	f.write(newdata)
	f.write(final + '\n')
	f.write(argument_1 + '\n')

	f.write("newdata <- recode_key(a0, a1)"+ '\n')
	file_output_path = "write.table(newdata,"+ "file" + "=" + GLOBAL_OUTPUT_DIRECTORY_FORMATTED +  "StandardMiss.txt" + "'," + "sep=" +"'\\t'" + ", row.names=FALSE, quote=FALSE)"

	f.write(file_output_path+ '\n')

	f.close()

	with open(log_file_name, 'a') as g:
		g.write("---------------------------------------------------------------" + datetime.datetime.now().strftime("%I:%M:%S %p") + "---------------------------------------------------------------" + '\n')

		g.write("Recode Key Function Called" + '\n' +'\n' )
	

	s_out = open(log_file_name, "a")
	proc = subprocess.call(['Rscript','r/GUI_Scripts/recode_key1.R'], shell=False,stdout=s_out, stderr=s_out)

	refresh_logs()




###***************************************************************************************###
		 ###*********************** Filtering Functions *************************###
###***************************************************************************************###
def get_binary():
	f = open('r/get_binary.R','r')
	filedata = f.read()
	f.close()

	newdata = filedata

	f = open('r/GUI_Scripts/get_binary1.R','w')
	f.write(newdata)
	f.write(final + '\n')
	f.write("newdata <- get_binary(a0, 2)"+ '\n')
	file_output_path = "write.table(newdata,"+ "file" + "=" + GLOBAL_OUTPUT_DIRECTORY_FORMATTED +  "Binary.txt" + "'," + "sep=" +"'\\t'" + ", row.names=FALSE, quote=FALSE)"



	f.write(file_output_path+ '\n')


	f.close()

	with open(log_file_name, 'a') as g:
		g.write("---------------------------------------------------------------" + datetime.datetime.now().strftime("%I:%M:%S %p") + "---------------------------------------------------------------" + '\n')

		g.write("Get Binary Variables Function Called" + '\n' +'\n' )
	

	s_out = open(log_file_name, "a")
	proc = subprocess.call(['Rscript','r/GUI_Scripts/get_binary1.R'], shell=False,stdout=s_out, stderr=s_out)

	refresh_logs()

def get_continuous():
	f = open('r/get_continuous.R','r')
	filedata = f.read()
	f.close()

	lower_bound = "a1 = " + str(continuous_var.get())
	file_output_path = "write.table(newdata,"+ "file" + "=" + GLOBAL_OUTPUT_DIRECTORY_FORMATTED +  "Continuous" + str(continuous_var.get()) +".txt" + "'," + "sep=" +"'\\t'" + ", row.names=FALSE, quote=FALSE)"

	newdata = filedata

	f = open('r/GUI_Scripts/get_continuous1.R','w')

	f.write(newdata)
	f.write(final + '\n')
	f.write(lower_bound + '\n')
	f.write("newdata <- get_continuous(a0, a1)"+ '\n')
	f.write(file_output_path+ '\n')

	f.close()


	
	with open(log_file_name, 'a') as g:
		g.write("---------------------------------------------------------------" + datetime.datetime.now().strftime("%I:%M:%S %p") + "---------------------------------------------------------------" + '\n')

		g.write("Get Continuous Variables Function Called" + '\n')
		g.write("Lower Bound: " + lower_bound + '\n' +'\n' )




	s_out = open(log_file_name, "a")
	proc = subprocess.call(['Rscript','r/GUI_Scripts/get_continuous1.R'], shell=False,stdout=s_out, stderr=s_out)

	refresh_logs()

def get_categorical():
	f = open('r/get_categorical.R','r')
	filedata = f.read()
	f.close()
	file_output_path = "write.table(newdata,"+ "file" + "=" + GLOBAL_OUTPUT_DIRECTORY_FORMATTED +  "Categorical"+str(categorical1.get())+"_" +str(categorical2.get())+".txt" + "'," + "sep=" +"'\\t'" + ", row.names=FALSE, quote=FALSE)"

	lower_bound = "a1 = " + str(categorical1.get())
	upper_bound = "a2 = " + str(categorical2.get())

	newdata = filedata

	
	f = open('r/GUI_Scripts/get_categorical1.R','w')
	f.write(newdata)
	f.write(final + '\n')
	f.write(lower_bound + '\n')
	f.write(upper_bound + '\n')
	f.write("newdata <- get_categorical(a0, a1, a2)"+ '\n')
	f.write(file_output_path + '\n')



	f.close()

	
	with open(log_file_name, 'a') as g:
		g.write("---------------------------------------------------------------" + datetime.datetime.now().strftime("%I:%M:%S %p") + "---------------------------------------------------------------" + '\n')

		g.write("Get Categorical Variables Function Called" + '\n')
		g.write("Lower Bound: " + lower_bound + '\n')
		g.write("Upper Bound: " + upper_bound + '\n' +'\n' )


	s_out = open(log_file_name, "a")
	proc = subprocess.call(['Rscript','r/GUI_Scripts/get_categorical1.R'], shell=False,stdout=s_out, stderr=s_out)

	refresh_logs()

def get_check():
	f = open('r/get_check.R','r')
	filedata = f.read()
	f.close()

	file_output_path = "write.table(newdata,"+ "file" + "=" + GLOBAL_OUTPUT_DIRECTORY_FORMATTED +  "Ambiguous"+str(ambiguous1.get()) +"_"+str(ambiguous2.get()) +".txt" +"',"+ "sep=" +"'\\t'" + ", row.names=FALSE, quote=FALSE)"

	lower_bound = "a1 = " + str(ambiguous1.get())
	upper_bound = "a2 = " + str(ambiguous2.get())

	newdata = filedata
	
	f = open('r/GUI_Scripts/get_check1.R','w')
	f.write(newdata)
	f.write(final + '\n')
	f.write(lower_bound + '\n')
	f.write(upper_bound + '\n')
	f.write("newdata <- get_check(a0, a1, a2)"+ '\n')
	f.write(file_output_path + '\n')



	f.close()
	

	with open(log_file_name, 'a') as g:
		g.write("---------------------------------------------------------------" + datetime.datetime.now().strftime("%I:%M:%S %p") + "---------------------------------------------------------------" + '\n')

		g.write("Get Ambiguous Variables Function Called" + '\n')
		g.write("Lower Bound: " + lower_bound + '\n')
		g.write("Upper Bound: " + upper_bound + '\n' +'\n' )
	
	s_out = open(log_file_name, "a")
	proc = subprocess.call(['Rscript','r/GUI_Scripts/get_check1.R'], shell=False,stdout=s_out, stderr=s_out)

	refresh_logs()

def min_n():
	f = open('r/min_n.R','r')
	filedata = f.read()
	f.close()

	file_output_path = "write.table(newdata,"+ "file" + "=" + GLOBAL_OUTPUT_DIRECTORY_FORMATTED +  "MinSamples.txt" + "'," + "sep=" +"'\\t'" + ", row.names=FALSE, quote=FALSE)"

	lower_bound = "a1 = " + str(min_n_var.get())
	newdata = filedata

	
	f = open('r/GUI_Scripts/min_n1.R','w')
	f.write(newdata)
	f.write(final + '\n')
	f.write(lower_bound + '\n')
	f.write("newdata <- min_n(a0, a1)"+ '\n')
	f.write(file_output_path + '\n')



	f.close()


	with open(log_file_name, 'a') as g:
		g.write("---------------------------------------------------------------" + datetime.datetime.now().strftime("%I:%M:%S %p") + "---------------------------------------------------------------" + '\n')

		g.write("Get Ambiguous Variables Function Called" + '\n')
		g.write("Lower Bound: " + lower_bound + '\n' +'\n' )

	s_out = open(log_file_name, "a")
	proc = subprocess.call(['Rscript','r/GUI_Scripts/min_n1.R'], shell=False,stdout=s_out, stderr=s_out)

	refresh_logs()

def transvar():
	f = open('r/transvar.R','r')
	filedata = f.read()
	f.close()

	file_output_path = "write.table(newdata,"+ "file" + "=" + GLOBAL_OUTPUT_DIRECTORY_FORMATTED +  "Transformation.txt" + "'," + "sep=" +"'\\t'" + ", row.names=FALSE, quote=FALSE)"

	argument_1 = "a1 = read.delim('/Users/deven/desktop/it.txt', header=TRUE)"
	a1 = "a1 = read.delim('" + str(transformation_file_var.get()) + A0_2

	newdata = filedata

	
	f = open('r/GUI_Scripts/transvar1.R','w')
	f.write(newdata)
	f.write(final + '\n')
	f.write(a1 + '\n')
	f.write("newdata <- transvar(a0, a1)"+ '\n')
	f.write(file_output_path + '\n')


	f.close()
	
	
	with open(log_file_name, 'a') as g:
		g.write("---------------------------------------------------------------" + datetime.datetime.now().strftime("%I:%M:%S %p") + "---------------------------------------------------------------" + '\n')

		g.write("Variable Specific Transformation Function Called" + '\n' + '\n')

	s_out = open(log_file_name, "a")
	proc = subprocess.call(['Rscript','r/GUI_Scripts/transvar1.R'], shell=False,stdout=s_out, stderr=s_out)

	refresh_logs()

# def remove_outliers():



###***************************************************************************************###
   		###********************** Visualize Data Functions *******************###
###***************************************************************************************###
def histogram():


	f = open('r/hist_plot.R','r')
	filedata = f.read()
	f.close()

	# file_output_path = "write.table(newdata,"+ "file" + "=" + '"GUI_Output/histogram_plot.txt",' + "sep=" +"'\\t'" + ", row.names=FALSE, quote=FALSE)"


	#dafault values for the above variables
	number_of_plots_per_page = "a1 = " + str(h1.get())
	number_of_rows_per_page = "a2 = " + str(h2.get())
	numebr_of_columns_per_page = "a3 = " + str(h3.get())
	width_of_plot = "a4 = " + str(h4.get())
	height_of_plot = "a5 = " + str(h5.get())
	resolution_of_plot = "a6 = " + str(h6.get())


	newdata = filedata


	f = open('r/GUI_Scripts/hist_plot1.R','w')
	# f.write(GGPLOT_LIBRARY + '\n')
	# f.write(GRIDEXTRA_LIBRARY + '\n')
	f.write(newdata)
	f.write(final + '\n')
	f.write(number_of_plots_per_page + '\n')
	f.write(number_of_rows_per_page + '\n')
	f.write(numebr_of_columns_per_page + '\n')
	f.write(width_of_plot + '\n')
	f.write(height_of_plot + '\n')
	f.write(resolution_of_plot + '\n')
	f.write("quartz()" + '\n')
	f.write("hist_plot(a0, a1, file=" + GLOBAL_OUTPUT_DIRECTORY_FORMATTED +  "Histogram" + "'," + "a2, a3, a4, a5, a6)" + '\n')

	f.close()

	
	with open(log_file_name, 'a') as g:

		g.write("---------------------------------------------------------------" + datetime.datetime.now().strftime("%I:%M:%S %p") + "---------------------------------------------------------------" + '\n')

		g.write("Generate Histogram Function Called" + '\n')
		g.write("Number of Plots Per Page: " + str(h1.get()) + '\n')
		g.write("Number of Rows Per Page:  " + str(h2.get()) + '\n')
		g.write("Number of Columns Per Page: " + str(h3.get()) + '\n')
		g.write("Width of Plot: " + str(h4.get()) + '\n')
		g.write("Height of Plot: " + str(h5.get()) + '\n')
		g.write("Resolution of Plot: " + str(h6.get()) + '\n' +'\n' )
	
	s_out = open(log_file_name, "a")
	proc = subprocess.call(['Rscript','r/GUI_Scripts/hist_plot1.R'], shell=False,stdout=s_out, stderr=s_out)

	refresh_logs()

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


	f = open('r/GUI_Scripts/box_plot1.R','w')
	# f.write(GGPLOT_LIBRARY + '\n')
	# f.write(GRIDEXTRA_LIBRARY + '\n')
	f.write(newdata)
	f.write(final + '\n')
	f.write(number_of_plots_per_page + '\n')
	f.write(number_of_rows_per_page + '\n')
	f.write(numebr_of_columns_per_page + '\n')
	f.write(width_of_plot + '\n')
	f.write(height_of_plot + '\n')
	f.write(resolution_of_plot + '\n')
	f.write("quartz()" + '\n')
	f.write("box_plot(a0, a1, file=" + GLOBAL_OUTPUT_DIRECTORY_FORMATTED +  "BoxPlot" + "'," + "a2, a3, a4, a5, a6)" + '\n')

	f.close()

	with open(log_file_name, 'a') as g:
		g.write("---------------------------------------------------------------" + datetime.datetime.now().strftime("%I:%M:%S %p") + "---------------------------------------------------------------" + '\n')

		g.write("Generate Box Plot Function Called" + '\n')
		g.write("Number of Plots Per Page: " + str(bx1.get()) + '\n')
		g.write("Number of Rows Per Page:  " + str(bx2.get()) + '\n')
		g.write("Number of Columns Per Page: " + str(bx3.get()) + '\n')
		g.write("Width of Plot: " + str(bx4.get()) + '\n')
		g.write("Height of Plot: " + str(bx5.get()) + '\n')
		g.write("Resolution of Plot: " + str(bx6.get()) + '\n' +'\n' )
	
	s_out = open(log_file_name, "a")
	proc = subprocess.call(['Rscript','r/GUI_Scripts/box_plot1.R'], shell=False,stdout=s_out, stderr=s_out)

	refresh_logs()

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


	f = open('r/GUI_Scripts/qq_plot1.R','w')
	# f.write(GGPLOT_LIBRARY + '\n')
	# f.write(GRIDEXTRA_LIBRARY + '\n')
	f.write(newdata)
	f.write(final + '\n')
	f.write(number_of_plots_per_page + '\n')
	f.write(number_of_rows_per_page + '\n')
	f.write(numebr_of_columns_per_page + '\n')
	f.write(width_of_plot + '\n')
	f.write(height_of_plot + '\n')
	f.write(resolution_of_plot + '\n')
	f.write("quartz()" + '\n')
	f.write("qqplot(a0, a1, file=" + GLOBAL_OUTPUT_DIRECTORY_FORMATTED +  "QQPlot" + "'," + "a2, a3, a4, a5, a6)" + '\n')

	f.close()

	with open(log_file_name, 'a') as g:
		g.write("---------------------------------------------------------------" + datetime.datetime.now().strftime("%I:%M:%S %p") + "---------------------------------------------------------------" + '\n')

		g.write("Generate QQ Plot Function Called" + '\n')
		g.write("Number of Plots Per Page: " + str(qq1.get()) + '\n')
		g.write("Number of Rows Per Page:  " + str(qq2.get()) + '\n')
		g.write("Number of Columns Per Page: " + str(qq3.get()) + '\n')
		g.write("Width of Plot: " + str(qq4.get()) + '\n')
		g.write("Height of Plot: " + str(qq5.get()) + '\n')
		g.write("Resolution of Plot: " + str(qq6.get()) + '\n' +'\n' )
	
	s_out = open(log_file_name, "a")
	proc = subprocess.call(['Rscript','r/GUI_Scripts/qq_plot1.R'], shell=False,stdout=s_out, stderr=s_out)

	refresh_logs()


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




	f = open('r/GUI_Scripts/bar_plot1.R','w')
	# f.write(GGPLOT_LIBRARY + '\n')
	# f.write(GRIDEXTRA_LIBRARY + '\n')
	f.write(newdata)
	f.write(final + '\n')
	f.write(number_of_plots_per_page + '\n')
	f.write(number_of_rows_per_page + '\n')
	f.write(numebr_of_columns_per_page + '\n')
	f.write(width_of_plot + '\n')
	f.write(height_of_plot + '\n')
	f.write(resolution_of_plot + '\n')
	f.write("quartz()" + '\n')
	f.write("bar_plot(a0, a1, file=" + GLOBAL_OUTPUT_DIRECTORY_FORMATTED +  "BarPlot" + "'," + "a2, a3, a4, a5, a6)" + '\n')


	f.close()

	with open(log_file_name, 'a') as g:
		g.write("---------------------------------------------------------------" + datetime.datetime.now().strftime("%I:%M:%S %p") + "---------------------------------------------------------------" + '\n')

		g.write("Generate Bar Plot Function Called" + '\n')
		g.write("Number of Plots Per Page: " + str(bp1.get()) + '\n')
		g.write("Number of Rows Per Page:  " + str(bp2.get()) + '\n')
		g.write("Number of Columns Per Page: " + str(bp3.get()) + '\n')
		g.write("Width of Plot: " + str(bp4.get()) + '\n')
		g.write("Height of Plot: " + str(bp5.get()) + '\n')
		g.write("Resolution of Plot: " + str(bp6.get()) + '\n' +'\n' )

	
	s_out = open(log_file_name, "a")
	
	proc = subprocess.call(['Rscript','r/GUI_Scripts/bar_plot1.R'], shell=False, stdout=s_out, stderr=s_out)

	refresh_logs()


###***************************************************************************************###
			###********************** Summary Functions ************************###
###***************************************************************************************###
def frequency_table():
	f = open('r/freq_tables.R','r')
	filedata = f.read()
	f.close()

	file_output_path = "write.table(newdata,"+ "file" + "=" + GLOBAL_OUTPUT_DIRECTORY_FORMATTED +  "Freq.txt" + "'," + "sep=" +"'\\t'" + ", row.names=FALSE, quote=FALSE)"

	newdata = filedata

	f = open('r/GUI_Scripts/freq_tables1.R','w')
	f.write(newdata)
	f.write(final + '\n')
	f.write("newdata <- freq_tables(a0)"+ '\n')
	f.write(file_output_path + '\n')


	f.close()
	
	with open(log_file_name, 'a') as g:
		g.write("---------------------------------------------------------------" + datetime.datetime.now().strftime("%I:%M:%S %p") + "---------------------------------------------------------------" + '\n')

		g.write("Generate Frequency Table Function Called" + '\n'+'\n')

	s_out = open(log_file_name, "a")
	
	proc = subprocess.call(['Rscript','r/GUI_Scripts/freq_tables1.R'], shell=False, stdout=s_out, stderr=s_out)

	refresh_logs()

def correlations():
	f = open('r/correlations.R','r')
	filedata = f.read()
	f.close()

	file_output_path = "write.table(newdata,"+ "file" + "=" + GLOBAL_OUTPUT_DIRECTORY_FORMATTED +  "Correlations"+ str(find_correlations_var.get()) + ".txt" + "'," + "sep=" +"'\\t'" + ", row.names=FALSE, quote=FALSE)"

	threshold_val = "a1 = " + str(find_correlations_var.get())
	newdata = filedata


	f = open('r/GUI_Scripts/correlations1.R','w')
	f.write(newdata)
	f.write(final + '\n')
	f.write(threshold_val+ '\n')
	f.write("newdata <- correlations(a0,a1)"+ '\n')
	f.write(file_output_path + '\n')

	f.close()
	
	with open(log_file_name, 'a') as g:
		g.write("---------------------------------------------------------------" + datetime.datetime.now().strftime("%I:%M:%S %p") + "---------------------------------------------------------------" + '\n')
		g.write("Generate Correlations Function Called" + '\n')
		g.write("Threshold: " + threshold_val+ '\n')
	
	s_out = open(log_file_name, "a")
	
	proc = subprocess.call(['Rscript','r/GUI_Scripts/correlations1.R'], shell=False, stdout=s_out, stderr=s_out)

	refresh_logs()
	
def sample_size():
	f = open('r/sample_size.R','r')
	filedata = f.read()
	f.close()

	file_output_path = "write.table(newdata,"+ "file" + "=" + GLOBAL_OUTPUT_DIRECTORY_FORMATTED +  "SampleSize.txt" + "'," + "sep=" +"'\\t'" + ", row.names=FALSE, quote=FALSE)"

	newdata = filedata

	f = open('r/GUI_Scripts/sample_size1.R','w')
	f.write(newdata)
	f.write(final + '\n')
	f.write("newdata <- sample_size(a0)"+ '\n')
	f.write(file_output_path + '\n')



	f.close()
	
	with open(log_file_name, 'a') as g:
		g.write("---------------------------------------------------------------" + datetime.datetime.now().strftime("%I:%M:%S %p") + "---------------------------------------------------------------" + '\n')
		g.write("Sample Size Function Called" + '\n')
		# g.write("Threshold: " + threshold_val+ '\n'+'\n')
	
	s_out = open(log_file_name, "a")
	
	proc = subprocess.call(['Rscript','r/GUI_Scripts/sample_size1.R'], shell=False, stdout=s_out, stderr=s_out)

	refresh_logs()


def get_uniq():

	f = open('r/get_uniq.R','r')
	filedata = f.read()
	f.close()
	file_output_path = "write.table(newdata,"+ "file" + "=" + GLOBAL_OUTPUT_DIRECTORY_FORMATTED + "Unique.txt" + "'," + "sep=" +"'\\t'" + ", row.names=FALSE, quote=FALSE)"

	newdata = filedata

	f = open('r/GUI_Scripts/get_uniq1.R','w')
	f.write(newdata)
	f.write(final + '\n')
	f.write("newdata <- get_uniq(a0)"+ '\n')
	f.write(file_output_path + '\n')


	f.close()

	with open(log_file_name, 'a') as g:
		g.write("---------------------------------------------------------------" + datetime.datetime.now().strftime("%I:%M:%S %p") + "---------------------------------------------------------------" + '\n')
		g.write("Get Unique Values Function Called" + '\n')
	
	s_out = open(log_file_name, "a")
	
	proc = subprocess.call(['Rscript','r/GUI_Scripts/get_uniq1.R'], shell=False, stdout=s_out, stderr=s_out)

	refresh_logs()


def outlier_impact():
	f = open('r/outlier_impact.R','r')
	filedata = f.read()
	f.close()

	file_output_path = "write.table(newdata,"+ "file" + "=" + GLOBAL_OUTPUT_DIRECTORY_FORMATTED +  "OutlierImpact"+ str(find_outliers_var.get())+ ".txt" + "'," + "sep=" +"'\\t'" + ", row.names=FALSE, quote=FALSE)"

	threshold_val = "a1 = " + str(find_outliers_var.get())
	newdata = filedata


	f = open('r/GUI_Scripts/outlier_impact1.R','w')
	f.write(newdata)
	f.write(final + '\n')
	f.write(threshold_val + '\n')
	f.write("newdata <- outlier_impact(a0,a1)"+ '\n')
	f.write(file_output_path + '\n')

	f.close()
	with open(log_file_name, 'a') as g:
		g.write("---------------------------------------------------------------" + datetime.datetime.now().strftime("%I:%M:%S %p") + "---------------------------------------------------------------" + '\n')
		g.write("Threshold: " + threshold_val+ '\n'+'\n')

	s_out = open(log_file_name, "a")
	
	proc = subprocess.call(['Rscript','r/GUI_Scripts/outlier_impact1.R'], shell=False, stdout=s_out, stderr=s_out)

	refresh_logs()

def chisq_tests():
	f = open('r/chisq_tests.R','r')
	filedata = f.read()
	f.close()
	
	file_output_path = "write.table(newdata,"+ "file" + "=" + GLOBAL_OUTPUT_DIRECTORY_FORMATTED +  "chisq.txt" + "'," + "sep=" +"'\\t'" + ", row.names=FALSE, quote=FALSE)"

	newdata = filedata

	f = open('r/GUI_Scripts/chisq_tests1.R','w')
	f.write(newdata)
	f.write(final + '\n')
	# f.write(threshold_val + '\n')
	f.write("newdata <- chisq_tests(a0)"+ '\n')
	f.write(file_output_path + '\n')


	f.close()
	with open(log_file_name, 'a') as g:
		g.write("Chi-squared Test Called" + '\n'+'\n')
	
	s_out = open(log_file_name, "a")
	
	proc = subprocess.call(['Rscript','r/GUI_Scripts/chisq_tests1.R'], shell=False, stdout=s_out, stderr=s_out)

	refresh_logs()


#Variable Filter - colfilter()
def col_filter():

	f = open('r/colfilter.R','r')
	filedata = f.read()
	f.close()

	file_output_path = "write.table(newdata,"+ "file" + "=" + GLOBAL_OUTPUT_DIRECTORY_FORMATTED +  "VariableFiltered.txt" + "'," + "sep=" +"'\\t'" + ", row.names=FALSE, quote=FALSE)"


	a0_1 = "a1 = read.delim('"
	argument_1 = a0_1 + column_file_name+ A0_2


	newdata = filedata
	
	f = open('r/GUI_Scripts/colfilter1.R','w')
	f.write(newdata)
	f.write(final + '\n')
	f.write(argument_1 + '\n')
	f.write("newdata <- colfilter(a0, a1, FALSE)"+ '\n')
	f.write(file_output_path + '\n')

	f.close()
	
	with open(log_file_name, 'a') as g:
		g.write("---------------------------------------------------------------" + datetime.datetime.now().strftime("%I:%M:%S %p") + "---------------------------------------------------------------" + '\n')

		g.write("Filter Columns Called" + '\n')
		g.write("File: " + column_file_name + '\n'+'\n' )
	
	s_out = open(log_file_name, "a")
	proc = subprocess.call(['Rscript','r/GUI_Scripts/colfilter1.R'], shell=False,stdout=s_out, stderr=s_out)

	refresh_logs()

#Sample Filter - rowfilter
def row_filter():

	f = open('r/rowfilter.R','r')
	filedata = f.read()
	f.close()

	file_output_path = "write.table(newdata,"+ "file" + "=" + GLOBAL_OUTPUT_DIRECTORY_FORMATTED +  "SampleFiltered.txt" + "'," + "sep=" +"'\\t'" + ", row.names=FALSE, quote=FALSE)"

	a0_1 = "a1 = read.delim('"
	argument_1 = a0_1 + sample_file_name + A0_2


	if str(rowfilterMenuVar.get()) == "True":
		argument_2 = "a2 = FALSE"
	else:
		argument_2 = "a2 = TRUE"


	newdata = filedata
	
	f = open('r/GUI_Scripts/rowfilter1.R','w')
	f.write(newdata)
	f.write(final + '\n')
	f.write(argument_1 + '\n')
	f.write(argument_2 + '\n')
	f.write("newdata <- rowfilter(a0, a1, a2)"+ '\n')
	f.write(file_output_path + '\n')


	f.close()
	
	with open(log_file_name, 'a') as g:
		g.write("---------------------------------------------------------------" + datetime.datetime.now().strftime("%I:%M:%S %p") + "---------------------------------------------------------------" + '\n')

		g.write("Filter Samples Called" + '\n')
		g.write("File: " + sample_file_name + '\n'+'\n' )
	
	s_out = open(log_file_name, "a")
	proc = subprocess.call(['Rscript','r/GUI_Scripts/rowfilter1.R'], shell=False,stdout=s_out, stderr=s_out)

	refresh_logs()

def merge_data():

	f = open('r/merge_data.R','r')
	filedata = f.read()
	f.close()

	file_output_path = "write.table(newdata,"+ "file" + "=" + GLOBAL_OUTPUT_DIRECTORY_FORMATTED +  "Merged.txt" + "'," + "sep=" +"'\\t'" + ", row.names=FALSE, quote=FALSE)"

	a0_1 = "a1 = read.delim('"
	argument_1 = a0_1 + merge_data_file + A0_2


	if str(mergeVar.get()) == "True":
		argument_2 = "a2 = TRUE"

	else:
		argument_2 = "a2 = FALSE"


	newdata = filedata
	
	f = open('r/GUI_Scripts/merge_data1.R','w')
	f.write(newdata)
	f.write('\n')
	f.write(final + '\n')
	f.write(argument_1 + '\n')
	f.write(argument_2 + '\n')
	f.write("newdata <- merge_data(a0, a1, a2)"+ '\n')
	f.write(file_output_path + '\n')
	f.close()

	with open(log_file_name, 'a') as g:
		g.write("---------------------------------------------------------------" + datetime.datetime.now().strftime("%I:%M:%S %p") + "---------------------------------------------------------------" + '\n')

		g.write("Merge Data Called" + '\n')
	
	s_out = open(log_file_name, "a")
	proc = subprocess.call(['Rscript','r/GUI_Scripts/merge_data1.R'], shell=False,stdout=s_out, stderr=s_out)

	refresh_logs()

#EWAS
def ewas():

	f = open('r/ewas.R','r')
	filedata = f.read()
	f.close()

	file_output_path = "write.table(newdata,"+ "file" + "=" + GLOBAL_OUTPUT_DIRECTORY_FORMATTED +  "ewas.txt" + "'," + "sep=" +"'\\t'" + ", row.names=FALSE, quote=FALSE)"

	if global_ewas_filename_1 == "":
		argument_1 = "a1 = NULL"
	else:
		a0_1 = "a1 = read.delim('"
		argument_1 = a0_1 + global_ewas_filename_1 + A0_2


	if global_ewas_filename_2 == "":
		argument_2 = "a2 = NULL"
	else:
		a0_2 = "a2 = read.delim('"
		argument_2 = a0_2 + global_ewas_filename_2 + A0_2


	argument_3 = "a3 = " + "'" + str(phenotype_var.get()) + "'"

	if global_covariate_string == "":
		argument_4 = "a4 = NULL"
	else:
		argument_4 = "a4 <- " + "c(" + global_covariate_string + ")"

	if str(regressionVar.get()) == "Linear":

		argument_5 = "a5 = 'gaussian'"
	else:
		argument_5 = "a5 = 'binomial'"



	if str(correctionVar.get()) == "Both":
		argument_6 = "a6 <- " + "c(" + "'bonferroni'" + "," + "'fdr')"
	else:
		argument_6 = "a6 <- " + "c(" + "'" + str(correctionVar.get()) + "'" + ")"


	newdata = filedata
	
	f = open('r/GUI_Scripts/ewas1.R','w')
	f.write(newdata)
	# f.write(final + '\n')
	f.write(argument_1 + '\n')
	f.write(argument_2 + '\n')
	f.write(argument_3 + '\n')
	f.write(argument_4 + '\n')
	f.write(argument_5 + '\n')
	f.write(argument_6 + '\n')

	f.write("newdata <- ewas(a1,a2,a3,a4,a5,a6)"+ '\n')
	f.write(file_output_path + '\n')


	f.close()
	

	with open(log_file_name, 'a') as g:
		g.write("---------------------------------------------------------------" + datetime.datetime.now().strftime("%I:%M:%S %p") + "---------------------------------------------------------------" + '\n')

		g.write("EWAS Called" + '\n')

	
	s_out = open(log_file_name, "a")
	proc = subprocess.call(['Rscript','r/GUI_Scripts/ewas1.R'], shell=False,stdout=s_out, stderr=s_out)

	refresh_logs()



def remove_outliers():

	f = open('r/remove_outliers.R','r')
	filedata = f.read()
	f.close()

	file_output_path = "write.table(newdata,"+ "file" + "=" + GLOBAL_OUTPUT_DIRECTORY_FORMATTED +  "SampleFiltered.txt" + "'," + "sep=" +"'\\t'" + ", row.names=FALSE, quote=FALSE)"

	argument_1 = "a1 = " + str(remove_outliers_var.get()) 


	newdata = filedata
	
	f = open('r/GUI_Scripts/remove_outliers1.R','w')
	f.write(newdata)
	f.write(final + '\n')
	f.write(argument_1 + '\n')
	f.write("newdata <- remove_outliers(a0, a1)"+ '\n')
	f.write(file_output_path + '\n')

	f.close()
	
	with open(log_file_name, 'a') as g:
		g.write("---------------------------------------------------------------" + datetime.datetime.now().strftime("%I:%M:%S %p") + "---------------------------------------------------------------" + '\n')

		g.write("Remove Outliers Called" + '\n')
	
	s_out = open(log_file_name, "a")
	proc = subprocess.call(['Rscript','r/GUI_Scripts/remove_outliers1.R'], shell=False,stdout=s_out, stderr=s_out)

	refresh_logs()



###***************************************************************************************###
   		###*************************** GUI PROGRAM BELOW **************************###
###***************************************************************************************###

root = Tk()
root.geometry("880x705")
root.title('CLARITE')
n = ttk.Notebook(root)

#Title of Program
title_label = Label(root,bg="white",text = "CLARITE")
title_label.config(font=("Comfortaa", 20))
title_label.pack(pady=15)
title_label.config(fg="cyan4")
title_label.pack(fill=X)


place_holder = Label(root, text="")
place_holder.config(font=("Comfortaa", 14))
place_holder.pack()
place_holder_2 = Label(root, text="")
place_holder_2.config(font=("Comfortaa", 14))
place_holder_2.pack()
place_holder_3 = Label(root, text="")
place_holder_3.config(font=("Comfortaa", 14))
place_holder_3.pack()



#Choose Initial File
global choose_start_file_name
choose_start_file_name= StringVar()
choose_start_file_name.set("Start by Choosing a Data File")
choose_file = Button(root,text="Choose File", command= choose_start_file)
choose_file.place(x=281,y=47)

choose_file_label = Label(root, textvariable=choose_start_file_name)
choose_file_label.config(font=("Comfortaa", 14))
choose_file_label.place(x=390,y=53)



global choose_directory_file_name
choose_directory_file_name= StringVar()
choose_directory_file_name.set("Output Folder:  " + GLOBAL_OUTPUT_DIRECTORY)

#Choose Output Directory
output_directory_label = Label(root,bg="white",textvariable=choose_directory_file_name)
output_directory_label.config(font=("Comfortaa", 13))
output_directory_label.config(fg="black")
output_directory_label.place(x=390,y=86)

choose_file = Button(root,text="Change", command= choose_output_directory)
choose_file.place(x=281,y=81)
choose_file.config(width = 8)





#Creates the three tabs
f1 = Frame(n,width=300,height=300)
f2 = Frame(n,width=300,height=300)
f3 = Frame(n,width=300,height=300)
n.add(f1,text="Descriptive")
n.add(f2,text="Quality Control")
n.add(f3,text="Association")



###***************************************************************************************###
   		###*************************** GUI PROGRAM  **************************###
###******************************Descriptive - General**********************************************###
###***************************************************************************************###
def get_uniq_popup():
	toplevel = Toplevel()
	toplevel.geometry("245x205")

	label1 = Label(toplevel,bg="white",text = "Get Unique Values")
	label1.config(font=("Comfortaa", 18))
	label1.pack(pady=(0,15))
	label1.config(fg="cyan4")
	label1.pack(fill=X)

	# grous_file_button = Button(toplevel,width=IN_WIDTH, height = IN_HEIGHT,command= lambda: instructions_popup("Get Unique Values", "man/get_uniq.Rd"))
	# image = ImageTk.PhotoImage(file="instruction.png")
	# grous_file_button.config(image=image)
	# grous_file_button.image = image
	# grous_file_button.place(x=210, y=0)

	run_u_button=Button(toplevel,text="Run", command=get_uniq)
	run_u_button.pack(fill=X)




	# toplevel.destroy()

def sample_size_popup():
	toplevel = Toplevel()
	toplevel.geometry("245x205")

	label1 = Label(toplevel,bg="white",text = "Sample Size")
	label1.config(font=("Comfortaa", 18))
	label1.pack(pady=(0,15))
	label1.config(fg="cyan4")
	label1.pack(fill=X)

	run_levels_button=Button(toplevel,text="Run", command=sample_size)
	run_levels_button.pack(fill=X)
	# toplevel.destroy()


###***************************************************************************************###
#Creating the buttons for the left section of Frame One
left_frame = Frame(f1)
left_frame.pack(side=LEFT)
right_frame = Frame(f1)
right_frame.pack(side=RIGHT)

l1 = Label(left_frame,bg="white",text = "General")
l1.config(font=("Comfortaa", 20))
l1.pack(pady=(0,5))
l1.pack(padx=(75,0))
l1.config(fg="cyan4")
l1.pack(fill=X)

get_uniq_button = Button(left_frame,text="Get Unique Values", command= get_uniq_popup)
get_uniq_button.pack(padx=(75,0))
get_uniq_button.config(width = 13)

Sample_Size = Button(left_frame,text="Sample Size", command= sample_size_popup)
Sample_Size.pack(padx=(75,0))
Sample_Size.config(width = 13)




###***************************************************************************************###
   		###*************************** GUI PROGRAM  **************************###
###******************************Descriptive - Categorical**********************************************###
###***************************************************************************************###
def frequency_table_popup():

	toplevel = Toplevel()
	toplevel.geometry("245x205")

	label1 = Label(toplevel,bg="white",text = "Frequency Table")
	label1.config(font=("Comfortaa", 18))
	label1.pack(pady=(0,15))
	label1.config(fg="cyan4")
	label1.pack(fill=X)

	run_levels_button=Button(toplevel,text="Run", command=frequency_table)
	run_levels_button.pack(fill=X)

def bar_plot_popup():

	toplevel = Toplevel()
	toplevel.geometry("300x235")

	label1 = Label(toplevel,bg="white",text = "Bar Plot")
	label1.grid(row=0, column=0, columnspan=2)
	label1.config(font=("Comfortaa", 18))
	label1.config(fg="cyan4")

	
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

	plots_label = Label(toplevel, text="Plots Per Page",font=("Comfortaa", 14))
	plots_label.grid(row=1, column=0)

	plots_labelentry_box  = Entry(toplevel, textvariable=bp1, width=15, bg="alice blue")
	plots_labelentry_box.grid(row=1, column=1)

	row_label = Label(toplevel, text="Rows Per Page",font=("Comfortaa", 14))
	row_label.grid(row=2, column=0)
	row_label_entry_box  = Entry(toplevel, textvariable=bp2, width=15, bg="alice blue")
	row_label_entry_box.grid(row=2, column=1)

	column_label = Label(toplevel, text="Columns Per Page",font=("Comfortaa", 14))
	column_label.grid(row=3, column=0)
	column_label_entry_box  = Entry(toplevel, textvariable=bp3, width=15, bg="alice blue")
	column_label_entry_box.grid(row=3, column=1)

	width_label = Label(toplevel, text="Width of Plot",font=("Comfortaa", 14))
	width_label.grid(row=4, column=0)
	width_label_entry_box  = Entry(toplevel, textvariable=bp4, width=15, bg="alice blue")
	width_label_entry_box.grid(row=4, column=1)

	height_label = Label(toplevel, text="Height of Plot",font=("Comfortaa", 14))
	height_label.grid(row=5, column=0)
	height_label_entry_box  = Entry(toplevel, textvariable=bp5, width=15, bg="alice blue")
	height_label_entry_box.grid(row=5, column=1)

	resolution_label = Label(toplevel, text="Resolution of Plot",font=("Comfortaa", 14))
	resolution_label.grid(row=6, column=0)
	resolution_label_entry_box  = Entry(toplevel, textvariable=bp6, width=15, bg="alice blue")
	resolution_label_entry_box.grid(row=6, column=1)


	run_barplot_button=Button(toplevel,text="Run", command=barplot, width=13)
	run_barplot_button.grid(row=7, column=0,columnspan=2)


def chi_square_popup():

	toplevel = Toplevel()
	toplevel.geometry("245x205")

	label1 = Label(toplevel,bg="white",text = "Chi-squared Test")
	label1.config(font=("Comfortaa", 18))
	label1.pack(pady=(0,15))
	label1.config(fg="cyan4")
	label1.pack(fill=X)

	run_levels_button=Button(toplevel,text="Run", command=chisq_tests)
	run_levels_button.pack(fill=X)


###***************************************************************************************###
#Creating the buttons for the middle section of Frame One
l2 = Label(f1,bg="white",text = "Categorical/Binary")
l2.config(font=("Comfortaa", 20))
l2.pack(pady=(55,5))
l2.config(fg="cyan4")
l2.pack(fill=X)

frequency_table_button = Button(f1,text="Frequency Table", command= frequency_table_popup)
frequency_table_button.pack(padx=(0,0))
frequency_table_button.config(width = 13)

chi_square_test_button = Button(f1,text="Chi-squared Test", command= chi_square_popup)
chi_square_test_button.pack(padx=(0,0))
chi_square_test_button.config(width = 13)

bar_plot_button = Button(f1,text="Bar Plot", command= bar_plot_popup)
bar_plot_button.pack(padx=(0,0))
bar_plot_button.config(width = 13)




###***************************************************************************************###
   		###*************************** GUI PROGRAM  **************************###
###******************************Descriptive - Continuous**********************************************###
###***************************************************************************************###
def correlations_popup():

	toplevel = Toplevel()
	toplevel.geometry("325x215")

	label1 = Label(toplevel,bg="white",text = "Correlations")
	label1.grid(row=0, column=0, columnspan=2)
	label1.config(font=("Comfortaa", 18))
	label1.config(fg="cyan4")



	global find_correlations_var
	find_correlations_var= StringVar()
	find_correlations_var.set("")

	find_correlations_var_label = Label(toplevel, text="Correlation Threshold",font=("Comfortaa", 14))
	find_correlations_var_label.grid(row=1, column=0)
	find_correlations_var_label_entry_box  = Entry(toplevel, textvariable=find_correlations_var, width=15, bg="alice blue")
	find_correlations_var_label_entry_box.grid(row=1, column=1)


	get_check_button = Button(toplevel,text="Correlations", command= correlations)
	get_check_button.grid(row=2, column=0,columnspan=2)



def outliers_popup():

	toplevel = Toplevel()
	toplevel.geometry("255x215")

	label1 = Label(toplevel,bg="white",text = "Outlier Impact")
	label1.grid(row=0, column=0, columnspan=2)
	label1.config(font=("Comfortaa", 18))
	label1.config(fg="cyan4")


	global find_outliers_var
	find_outliers_var= StringVar()
	find_outliers_var.set("")

	find_outliers_var_label = Label(toplevel, text="SD From Mean",font=("Comfortaa", 14))
	find_outliers_var_label.grid(row=1, column=0)
	find_outliers_var_label_entry_box  = Entry(toplevel, textvariable=find_outliers_var, width=15, bg="alice blue")
	find_outliers_var_label_entry_box.grid(row=1, column=1)


	get_check_button = Button(toplevel,text="Outlier Impact", command= outlier_impact)
	get_check_button.grid(row=2, column=0,columnspan=2)



def box_plot_popup():

	toplevel = Toplevel()
	toplevel.geometry("300x235")

	label1 = Label(toplevel,bg="white",text = "Box Plot")
	label1.grid(row=0, column=0, columnspan=2)
	label1.config(font=("Comfortaa", 18))
	label1.config(fg="cyan4")

	
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

	plots_label = Label(toplevel, text="Plots Per Page",font=("Comfortaa", 14))
	plots_label.grid(row=1, column=0)
	plots_labelentry_box  = Entry(toplevel, textvariable=bx1, width=15, bg="alice blue")
	plots_labelentry_box.grid(row=1, column=1)

	row_label = Label(toplevel, text="Rows Per Page",font=("Comfortaa", 14))
	row_label.grid(row=2, column=0)
	row_label_entry_box  = Entry(toplevel, textvariable=bx2, width=15, bg="alice blue")
	row_label_entry_box.grid(row=2, column=1)

	column_label = Label(toplevel, text="Columns Per Page",font=("Comfortaa", 14))
	column_label.grid(row=3, column=0)
	column_label_entry_box  = Entry(toplevel, textvariable=bx3, width=15, bg="alice blue")
	column_label_entry_box.grid(row=3, column=1)

	width_label = Label(toplevel, text="Width of Plot",font=("Comfortaa", 14))
	width_label.grid(row=4, column=0)
	width_label_entry_box  = Entry(toplevel, textvariable=bx4, width=15, bg="alice blue")
	width_label_entry_box.grid(row=4, column=1)

	height_label = Label(toplevel, text="Height of Plot",font=("Comfortaa", 14))
	height_label.grid(row=5, column=0)
	height_label_entry_box  = Entry(toplevel, textvariable=bx5, width=15, bg="alice blue")
	height_label_entry_box.grid(row=5, column=1)

	resolution_label = Label(toplevel, text="Resolution of Plot",font=("Comfortaa", 14))
	resolution_label.grid(row=6, column=0)
	resolution_label_entry_box  = Entry(toplevel, textvariable=bx6, width=15, bg="alice blue")
	resolution_label_entry_box.grid(row=6, column=1)


	box_plot_button = Button(toplevel,text="Run", command= boxplot)
	box_plot_button.grid(row=7, column=0,columnspan=2)



def histogram_popup():

	toplevel = Toplevel()
	toplevel.geometry("300x235")

	label1 = Label(toplevel,bg="white",text = "Histogram")
	label1.grid(row=0, column=0, columnspan=2)
	label1.config(font=("Comfortaa", 18))
	label1.config(fg="cyan4")


	plots_label = Label(toplevel, text="Plots Per Page",font=("Comfortaa", 14))
	plots_label.grid(row=1, column=0)


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


	plots_label = Label(toplevel, text="Plots Per Page",font=("Comfortaa", 14))
	plots_label.grid(row=1, column=0)
	plots_labelentry_box  = Entry(toplevel, textvariable=h1, width=15, bg="alice blue")
	plots_labelentry_box.grid(row=1, column=1)

	row_label = Label(toplevel, text="Rows Per Page",font=("Comfortaa", 14))
	row_label.grid(row=2, column=0)
	row_label_entry_box  = Entry(toplevel, textvariable=h2, width=15, bg="alice blue")
	row_label_entry_box.grid(row=2, column=1)

	column_label = Label(toplevel, text="Columns Per Page",font=("Comfortaa", 14))
	column_label.grid(row=3, column=0)
	column_label_entry_box  = Entry(toplevel, textvariable=h3, width=15, bg="alice blue")
	column_label_entry_box.grid(row=3, column=1)

	width_label = Label(toplevel, text="Width of Plot",font=("Comfortaa", 14))
	width_label.grid(row=4, column=0)
	width_label_entry_box  = Entry(toplevel, textvariable=h4, width=15, bg="alice blue")
	width_label_entry_box.grid(row=4, column=1)

	height_label = Label(toplevel, text="Height of Plot",font=("Comfortaa", 14))
	height_label.grid(row=5, column=0)
	height_label_entry_box  = Entry(toplevel, textvariable=h5, width=15, bg="alice blue")
	height_label_entry_box.grid(row=5, column=1)

	resolution_label = Label(toplevel, text="Resolution of Plot",font=("Comfortaa", 14))
	resolution_label.grid(row=6, column=0)
	resolution_label_entry_box  = Entry(toplevel, textvariable=h6, width=15, bg="alice blue")
	resolution_label_entry_box.grid(row=6, column=1)


	histogram_button2 = Button(toplevel,text="Run", command= histogram, width=12)
	histogram_button2.grid(row=7, column=0,columnspan=2)


def qq_plot_popup():

	toplevel = Toplevel()
	toplevel.geometry("300x235")

	label1 = Label(toplevel,bg="white",text = "QQ Plot")
	label1.grid(row=0, column=0, columnspan=2)
	label1.config(font=("Comfortaa", 18))
	label1.config(fg="cyan4")


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


	plots_label = Label(toplevel, text="Plots Per Page",font=("Comfortaa", 14))
	plots_label.grid(row=1, column=0)
	plots_labelentry_box  = Entry(toplevel, textvariable=qq1, width=15, bg="alice blue")
	plots_labelentry_box.grid(row=1, column=1)

	row_label = Label(toplevel, text="Rows Per Page",font=("Comfortaa", 14))
	row_label.grid(row=2, column=0)
	row_label_entry_box  = Entry(toplevel, textvariable=qq2, width=15, bg="alice blue")
	row_label_entry_box.grid(row=2, column=1)

	column_label = Label(toplevel, text="Columns Per Page",font=("Comfortaa", 14))
	column_label.grid(row=3, column=0)
	column_label_entry_box  = Entry(toplevel, textvariable=qq3, width=15, bg="alice blue")
	column_label_entry_box.grid(row=3, column=1)

	width_label = Label(toplevel, text="Width of Plot",font=("Comfortaa", 14))
	width_label.grid(row=4, column=0)
	width_label_entry_box  = Entry(toplevel, textvariable=qq4, width=15, bg="alice blue")
	width_label_entry_box.grid(row=4, column=1)

	height_label = Label(toplevel, text="Height of Plot",font=("Comfortaa", 14))
	height_label.grid(row=5, column=0)
	height_label_entry_box  = Entry(toplevel, textvariable=qq5, width=15, bg="alice blue")
	height_label_entry_box.grid(row=5, column=1)

	resolution_label = Label(toplevel, text="Resolution of Plot",font=("Comfortaa", 14))
	resolution_label.grid(row=6, column=0)
	resolution_label_entry_box  = Entry(toplevel, textvariable=qq6, width=15, bg="alice blue")
	resolution_label_entry_box.grid(row=6, column=1)


	qq_plot_button = Button(toplevel,text="Run", command= qqplot, width=12)
	qq_plot_button.grid(row=7, column=0,columnspan=2)



###***************************************************************************************###
#Creating the buttons for the right section of Frame One
l3 = Label(right_frame,bg="white",text = "Continuous")
l3.config(font=("Comfortaa", 20))
l3.pack(pady=(53,5))
l3.pack(padx=(0,75))
l3.config(fg="cyan4")
l3.pack(fill=X)

correlations_button = Button(right_frame,text="Correlations", command= correlations_popup)
correlations_button.pack(padx=(0,75))
correlations_button.config(width = 13)

outliers_button = Button(right_frame,text="Outliers", command= outliers_popup)
outliers_button.pack(padx=(0,75))
outliers_button.config(width = 13)

histogram_button = Button(right_frame,text="Histogram", command= histogram_popup)
histogram_button.pack(padx=(0,75))
histogram_button.config(width = 13)

box_plot_button = Button(right_frame,text="Box Plot", command= box_plot_popup)
box_plot_button.pack(side=LEFT)
box_plot_button.config(width = 5)

qq_pot_button = Button(right_frame,text="QQPlot", command= qq_plot_popup)
qq_pot_button.pack(padx=(0,75))
qq_pot_button.config(width = 5)




###***************************************************************************************###
   		###*************************** GUI PROGRAM  **************************###
###******************************Quality Control - General**********************************************###
###***************************************************************************************###

def colfilter_popup():

	toplevel = Toplevel()
	toplevel.geometry("300x235")

	label1 = Label(toplevel,bg="white",text = "Variable Filter")
	label1.grid(row=0, column=0, columnspan=2)
	label1.config(font=("Comfortaa", 18))
	label1.config(fg="cyan4")


	global cofilter1
	cofilter1= StringVar()
	cofilter1.set("No File Chosen")
	cofilter1_label = Label(toplevel, textvariable=cofilter1,font=("Comfortaa", 14))
	cofilter1_label.grid(row=1, column=1)


	upload_columns_button = Button(toplevel,text="Choose File", command= column_file_chosen)
	upload_columns_button.grid(row=1, column=0)

	get_check_button = Button(toplevel,text="Run", command= col_filter)
	get_check_button.grid(row=2, column=0,columnspan=2)



def rowfilter_popup():

	toplevel = Toplevel()
	toplevel.geometry("300x235")

	label1 = Label(toplevel,bg="white",text = "Sample Filter")
	label1.grid(row=0, column=0, columnspan=2)
	label1.config(font=("Comfortaa", 18))
	label1.config(fg="cyan4")

	global rowfilter1
	rowfilter1= StringVar()
	rowfilter1.set("No File Chosen")
	rowfilter1_label = Label(toplevel, textvariable=rowfilter1,font=("Comfortaa", 14))
	rowfilter1_label.grid(row=1, column=1)

	upload_columns_button = Button(toplevel,text="Choose File", command= sample_file_chosen)
	upload_columns_button.grid(row=1, column=0)
	

	global rowfilter2
	rowfilter2= StringVar()
	rowfilter2.set("")
	rowfilter2_label = Label(toplevel, text="Exclude",font=("Comfortaa", 14))
	rowfilter2_label.grid(row=2, column=0)


	#Correction Drop Down Menu
	global rowfilterMenuVar
	rowfilterMenuVar = tk.StringVar()
	rowfilterMenuVar_choices = ("True", "False")
	rowfilterMenuVar.set("Select")
	rowfilterMenuVar_choices = tk.OptionMenu(toplevel, rowfilterMenuVar, *rowfilterMenuVar_choices)
	rowfilterMenuVar_choices.grid(row=2, column=1)

	get_check_button = Button(toplevel,text="Run", command= row_filter)
	get_check_button.grid(row=3, column=0,columnspan=2)


def get_continuous_popup():

	toplevel = Toplevel()
	toplevel.geometry("400x235")

	label = Label(toplevel, text="Get Continuous Variables")
	label.grid(row=0, column=0, columnspan=2)
	label.config(font=("Comfortaa", 20))
	label.config(fg="cyan4")


	global continuous_var
	continuous_var= StringVar()
	continuous_var.set("")

	continuous_label = Label(toplevel, text="Min Values Considered Continuous",font=("Comfortaa", 14))
	continuous_label.grid(row=1, column=0)
	continuous_label_entry_box  = Entry(toplevel, textvariable=continuous_var, width=15, bg="alice blue")
	continuous_label_entry_box.grid(row=1, column=1)


	get_continuous_button = Button(toplevel,text="Get Continuous Variables", command= get_continuous)
	get_continuous_button.grid(row=2, column=0,columnspan=2)


#Get Ambiguous Popup
def get_check_popup():


	toplevel = Toplevel()
	toplevel.geometry("300x235")

	label1 = Label(toplevel,bg="white",text = "Get Ambiguous")
	label1.grid(row=0, column=0, columnspan=2)
	label1.config(font=("Comfortaa", 18))
	label1.config(fg="cyan4")


	global ambiguous1
	ambiguous1= StringVar()
	ambiguous1.set("")
	ambiguous1_label = Label(toplevel, text="Min Levels Desired",font=("Comfortaa", 14))
	ambiguous1_label.grid(row=1, column=0)
	ambiguous1_label_entry_box  = Entry(toplevel, textvariable=ambiguous1, width=15, bg="alice blue")
	ambiguous1_label_entry_box.grid(row=1, column=1)


	global ambiguous2
	ambiguous2= StringVar()
	ambiguous2.set("")
	ambiguous2_label = Label(toplevel, text="Max Levels Desired",font=("Comfortaa", 14))
	ambiguous2_label.grid(row=2, column=0)
	ambiguous2_label_entry_box  = Entry(toplevel, textvariable=ambiguous2, width=15, bg="alice blue")
	ambiguous2_label_entry_box.grid(row=2, column=1)


	get_check_button = Button(toplevel,text="Run", command= get_check)
	get_check_button.grid(row=3, column=0,columnspan=2)



def get_categorical_popup():

	toplevel = Toplevel()
	toplevel.geometry("440x235")

	label1 = Label(toplevel,bg="white",text = "Get Categorical")
	label1.grid(row=0, column=0, columnspan=2)
	label1.config(font=("Comfortaa", 18))
	label1.config(fg="cyan4")


	global categorical1
	categorical1= StringVar()
	categorical1.set("")
	categorical1_label = Label(toplevel, text="Min Values to be Considered Categorical",font=("Comfortaa", 14))
	categorical1_label.grid(row=1, column=0)
	categorical1_label_entry_box  = Entry(toplevel, textvariable=categorical1, width=15, bg="alice blue")
	categorical1_label_entry_box.grid(row=1, column=1)


	global categorical2
	categorical2= StringVar()
	categorical2.set("")
	categorical2_label = Label(toplevel, text="Max Values to be Considered Categorical",font=("Comfortaa", 14))
	categorical2_label.grid(row=2, column=0)
	categorical2_label_entry_box  = Entry(toplevel, textvariable=categorical2, width=15, bg="alice blue")
	categorical2_label_entry_box.grid(row=2, column=1)


	run_get_categorical_button=Button(toplevel,text="Run", command=get_categorical, width=12)
	run_get_categorical_button.grid(row=3, column=0,columnspan=2)

def get_binary_popup():
	toplevel = Toplevel()
	toplevel.geometry("300x235")

	label = Label(toplevel, text="Get Binary Variables")
	label.grid(row=0, column=0, columnspan=2)
	label.config(font=("Comfortaa", 20))
	label.config(fg="cyan4")


	get_binary_button = Button(toplevel,text="Run", command= get_binary)
	get_binary_button.grid(row=1, column=0,columnspan=2)



def sample_size_filter_popup():
	toplevel = Toplevel()
	toplevel.geometry("300x235")

	label1 = Label(toplevel,bg="white",text = "Sample Size Filter")
	label1.grid(row=0, column=0, columnspan=2)
	label1.config(font=("Comfortaa", 18))
	label1.config(fg="cyan4")


	sample_size_filter_button = Button(toplevel,text="Run", command= sample_size)
	sample_size_filter_button.grid(row=1, column=0,columnspan=2)



def merge_variables_popup():
	toplevel = Toplevel()
	toplevel.geometry("300x235")

	label1 = Label(toplevel,bg="white",text = "Merge Variables")
	label1.grid(row=0, column=0, columnspan=2)
	label1.config(font=("Comfortaa", 18))
	label1.config(fg="cyan4")


	choose_second_file_button = Button(toplevel,text="Choose File", command= merge_data_choose_file)
	choose_second_file_button.grid(row=1, column=0)

	global second_file_upload_merge
	second_file_upload_merge= StringVar()
	second_file_upload_merge.set("No File Chosen")
	second_file_upload_merge_label = Label(toplevel, textvariable=second_file_upload_merge,font=("Choose File", 14))
	second_file_upload_merge_label.grid(row=1, column=1)

	union_var_label = Label(toplevel, text="Sample Inclusion",font=("Comfortaa", 14))
	union_var_label.grid(row=2, column=0)


	#Correction Drop Down Menu
	global mergeVar
	mergeVar = tk.StringVar()
	mergeVar_choices = ("Union", "Intersect")
	mergeVar.set("Select")
	mergeVar_menu = tk.OptionMenu(toplevel, mergeVar, *mergeVar_choices)
	mergeVar_menu.grid(row=2, column=1)


	run_get_categorical_button=Button(toplevel,text="Run", command=merge_data, width=12)
	run_get_categorical_button.grid(row=3, column=0,columnspan=2)



	
#RecodeKey
def recode_missing_popup():
	toplevel = Toplevel()
	toplevel.geometry("300x235")

	label1 = Label(toplevel,bg="white",text = "Recode Missing")
	label1.grid(row=0, column=0, columnspan=2)
	label1.config(font=("Comfortaa", 18))
	label1.config(fg="cyan4")


	choose_key_file_button = Button(toplevel,text="Choose File", command= recode_missing_file_upload)
	choose_key_file_button.grid(row=1, column=0)

	global recode_key_file_var
	recode_key_file_var= StringVar()
	recode_key_file_var.set("No File Chosen")
	recode_key_file_var_label = Label(toplevel, textvariable=recode_key_file_var,font=("Choose File", 14))
	recode_key_file_var_label.grid(row=1, column=1)



	rc_button=Button(toplevel,text="Run", command=recode_key, width=12)
	rc_button.grid(row=2, column=0,columnspan=2)





#Recode_Missing
def specific_recode_missing_popup():

	toplevel = Toplevel()
	toplevel.geometry("325x235")

	label = Label(toplevel, text="Recode Mising - Specific")
	label.grid(row=0, column=0, columnspan=2)
	label.config(font=("Comfortaa", 20))
	label.config(fg="cyan4")


	global rc_specific_var
	rc_specific_var= StringVar()
	rc_specific_var.set("")

	rc_specific_var_label = Label(toplevel, text="Missing Value to Recode",font=("Comfortaa", 14))
	rc_specific_var_label.grid(row=1, column=0)
	rc_specific_var_label_entry_box  = Entry(toplevel, textvariable=rc_specific_var, width=15, bg="alice blue")
	rc_specific_var_label_entry_box.grid(row=1, column=1)


	rc_specific_button = Button(toplevel,text="Run", command= recode_missing)
	rc_specific_button.grid(row=2, column=0,columnspan=2)



def min_n_popup():

	toplevel = Toplevel()
	toplevel.geometry("275x205")


	label = Label(toplevel, text="Min # of Samples")
	label.grid(row=0, column=0, columnspan=2)
	label.config(font=("Comfortaa", 20))
	label.config(fg="cyan4")


	global min_n_var
	min_n_var= StringVar()
	min_n_var.set("")

	min_n_var_label = Label(toplevel, text="Min # of Samples",font=("Comfortaa", 14))
	min_n_var_label.grid(row=1, column=0)
	min_n_var_label_entry_box  = Entry(toplevel, textvariable=min_n_var, width=15, bg="alice blue")
	min_n_var_label_entry_box.grid(row=1, column=1)

	get_check_button = Button(toplevel,text="Run", command= min_n)
	get_check_button.grid(row=2, column=0,columnspan=2)


###***************************************************************************************###
#Creating the buttons for the left section of Frame TWO
f2_left_frame = Frame(f2)
f2_left_frame.pack(side=LEFT)
f2_right_frame = Frame(f2)
f2_right_frame.pack(side=RIGHT)


f2_l1 = Label(f2_left_frame,bg="white",text = "General")
f2_l1.config(font=("Comfortaa", 20))
f2_l1.pack(pady=(0,5))
f2_l1.pack(padx=(116,0))
f2_l1.config(fg="cyan4")
f2_l1.pack(fill=X)


Colfilter = Button(f2_left_frame,text="Variable Filter", command= colfilter_popup)
Colfilter.pack(padx=(0,0))
Colfilter.config(width = 13)

rowfilter = Button(f2_left_frame,text="Sample Filter", command= rowfilter_popup)
rowfilter.pack(padx=(0,0))
rowfilter.config(width = 13)

get_continuous_button = Button(f2_left_frame,text="Get Continuous", command= get_continuous_popup)
get_continuous_button.pack(padx=(0,0))
get_continuous_button.config(width = 13)

get_categorical_button = Button(f2_left_frame,text="Get Categorical", command= get_categorical_popup)
get_categorical_button.pack(padx=(0,0))
get_categorical_button.config(width = 13)

get_check_button = Button(f2_left_frame,text="Get Ambiguous", command= get_check_popup)
get_check_button.pack(padx=(0,0))
get_check_button.config(width = 13)


merge_variables_button = Button(f2,text="Merge Variables", command= merge_variables_popup)
merge_variables_button.place(x=174,y=89)
merge_variables_button.config(width = 13)


get_binary_button = Button(f2,text="Get Binary", command= get_binary_popup)
get_binary_button.place(x=174,y=118)
get_binary_button.config(width = 13)


min_n_button = Button(f2,text="Min # of Samples", command= min_n_popup)
min_n_button.place(x=174,y=145)
min_n_button.config(width = 13)


recode_missing_button = Button(f2,text="Variable Missing", command= recode_missing_popup)
recode_missing_button.place(x=174,y=172)
recode_missing_button.config(width = 13)


specific_recode_missing_button = Button(f2,text="Single Missing", command= specific_recode_missing_popup)
specific_recode_missing_button.place(x=174,y=202)
specific_recode_missing_button.config(width = 13)





###***************************************************************************************###
   		###*************************** GUI PROGRAM  **************************###
###******************************Quality Control - Categorical**********************************************###
###***************************************************************************************###
def min_cat_n():
	print("min_cat_n_called")



def min_cat_n_popup():

	toplevel = Toplevel()
	toplevel.geometry("330x205")


	label = Label(toplevel, text="Min Category Size")
	label.grid(row=0, column=0, columnspan=2)
	label.config(font=("Comfortaa", 20))
	label.config(fg="cyan4")


	global min_cat_n_var
	min_cat_n_var= StringVar()
	min_cat_n_var.set("")

	min_cat_n_var_label = Label(toplevel, text="Min Samples per Category",font=("Comfortaa", 14))
	min_cat_n_var_label.grid(row=1, column=0)
	min_cat_n_var_label_entry_box  = Entry(toplevel, textvariable=min_cat_n_var, width=15, bg="alice blue")
	min_cat_n_var_label_entry_box.grid(row=1, column=1)

	get_check_button = Button(toplevel,text="Run", command= min_cat_n)
	get_check_button.grid(row=2, column=0,columnspan=2)

###***************************************************************************************###
#Creating the buttons for the middle section of Frame TWO
f2_l2 = Label(f2,bg="white",text = "Categorical/Binary")
f2_l2.config(font=("Comfortaa", 20))
f2_l2.pack(pady=(51,5))
f2_l2.pack(padx=(140,0))

f2_l2.config(fg="cyan4")
# f2_l2.pack(fill=X)


min_cat_n_button = Button(f2,text="Min Category Size", command= min_cat_n_popup)
min_cat_n_button.pack(padx=(140,0))
min_cat_n_button.config(width = 13)


###***************************************************************************************###
   		###*************************** GUI PROGRAM  **************************###
###******************************Quality Control - Continuous**********************************************###
###***************************************************************************************###


def remove_outliers_popup():

	toplevel = Toplevel()
	toplevel.geometry("300x235")

	label = Label(toplevel, text="Remove Outliers")
	label.grid(row=0, column=0, columnspan=2)
	label.config(font=("Comfortaa", 20))
	label.config(fg="cyan4")


	global remove_outliers_var
	remove_outliers_var= StringVar()
	remove_outliers_var.set("")

	remove_outliers_var_label = Label(toplevel, text="SD From Mean",font=("Comfortaa", 14))
	remove_outliers_var_label.grid(row=1, column=0)
	remove_outliers_var_label_entry_box  = Entry(toplevel, textvariable=remove_outliers_var, width=15, bg="alice blue")
	remove_outliers_var_label_entry_box.grid(row=1, column=1)


	remove_outliers_var_button = Button(toplevel,text="Run", command= remove_outliers)
	remove_outliers_var_button.grid(row=2, column=0,columnspan=2)



#Variable Specific Transformation Popup
def transvar_popup():

	toplevel = Toplevel()
	toplevel.geometry("300x235")

	label1 = Label(toplevel,bg="white",text = "Variable Transformation")
	label1.grid(row=0, column=0, columnspan=2)
	label1.config(font=("Comfortaa", 18))
	label1.config(fg="cyan4")


	choose_second_file_button = Button(toplevel,text="Choose File", command= upload_tranformation_file)
	choose_second_file_button.grid(row=1, column=0)

	global transformation_file_var
	transformation_file_var= StringVar()
	transformation_file_var.set("No File Chosen")
	transformation_file_var_label = Label(toplevel, textvariable=transformation_file_var,font=("Choose File", 14))
	transformation_file_var_label.grid(row=1, column=1)


	transformation_button=Button(toplevel,text="Run", command=transvar)
	transformation_button.grid(row=2, column=0, columnspan=2)


###***************************************************************************************###
#Creating the buttons for the right section of Frame TWO
f2_l3 = Label(f2_right_frame,bg="white",text = "Continuous")
f2_l3.config(font=("Comfortaa", 20))
f2_l3.pack(pady=(0,5))
f2_l3.pack(padx=(0,30))
f2_l3.config(fg="cyan4")
f2_l3.pack(fill=X)


remove_outliers_button = Button(f2_right_frame,text="Remove Outliers", command= remove_outliers_popup)
remove_outliers_button.pack(padx=(0,30))
remove_outliers_button.config(width = 16)

transvar_button = Button(f2_right_frame,text="Variable Transformation", command= transvar_popup)
transvar_button.pack(padx=(0,30))
transvar_button.config(width = 16)




###***************************************************************************************###
   		###*************************** GUI PROGRAM  **************************###
###****************************** Associations - EWAS  **********************************************###
###***************************************************************************************###


f3_left_frame = Frame(f3)
f3_left_frame.pack(side=LEFT)
f3_right_frame = Frame(f3)
f3_right_frame.pack(side=RIGHT)

label = Label(f3_left_frame, text="Environment-Wide Association Study")
label.grid(row=0, column=0, columnspan=2)
label.config(font=("Comfortaa", 20))
label.config(fg="cyan4")




###********************************GUI BOX Covariates***************************************###
ychange = -55
global listNodes_covariates 
listNodes_covariates = Listbox(f3, width=19, height=3, font=("Helvetica", 12))
listNodes_covariates.place(x=363,y=199+ ychange)

scrollbar_covariates  = Scrollbar(f3, orient="vertical")
scrollbar_covariates.config(command=listNodes_covariates.yview)
scrollbar_covariates.pack(side="right", fill="y")
listNodes_covariates.config(yscrollcommand=scrollbar_covariates.set)

global_covariate_string = ""
def add_phenotype_to_list():
	#Add the strig from entry box into the listNodes box
	listNodes_covariates.insert(END, str(co_variates_var.get()))
	#Moves the scroller down as more data is added to the box
	listNodes_covariates.see(END)

	#Concatenate all entry string into one string
	global global_covariate_string
	if not global_covariate_string:
		global_covariate_string = global_covariate_string +  "'" + str(co_variates_var.get()) + "'"
	else:
		global_covariate_string = global_covariate_string + "," +"'" + str(co_variates_var.get()) + "'"
	co_variates_var_label_entry_box.delete(0, 'end')


#Covariate Variable 
global co_variates_var
co_variates_var= StringVar()
co_variates_var.set("")
global co_variates_var_label_entry_box  
co_variates_var_label_entry_box = Entry(f3, textvariable=co_variates_var, width=15, bg="alice blue")
co_variates_var_label_entry_box.place(x=362,y=246+ ychange,width=76)

#Add Button for covariate list
add_phenotype_button = Button(f3,text="Add", command= add_phenotype_to_list)
add_phenotype_button.place(x=438,y=246+ ychange,width=45)

co_variates_var_label = Label(f3, text="covariates",font=("Comfortaa", 14))
co_variates_var_label.place(x=383,y=179+ ychange)


###***************************************************************************************###


#file one upload
global ewas_choosefile_1
ewas_choosefile_1= StringVar()
ewas_choosefile_1.set("No File Chosen")
ewas_choosefile_1_label = Label(f3_left_frame, textvariable=ewas_choosefile_1,font=("Comfortaa", 14))
ewas_choosefile_1_label.grid(row=1, column=1)
upload_ewas1_button = Button(f3_left_frame,text="Choose Categorical file", command= ewas1_file_chosen)
upload_ewas1_button.grid(row=1, column=0)


#file two upload
global ewas_choosefile_2
ewas_choosefile_2= StringVar()
ewas_choosefile_2.set("No File Chosen")
ewas_choosefile_2_label = Label(f3_left_frame, textvariable=ewas_choosefile_2,font=("Comfortaa", 14))
ewas_choosefile_2_label.grid(row=2, column=1)
upload_ewas2_button = Button(f3_left_frame,text="Choose Continuous file", command= ewas2_file_chosen)
upload_ewas2_button.grid(row=2, column=0)



regression_label = Label(f3_left_frame, text="Regression ",font=("Comfortaa", 14))
regression_label.grid(row=3, column=0)

correction_label = Label(f3_left_frame, text="Multiple Test Correction ",font=("Comfortaa", 14))
correction_label.grid(row=4, column=0)

#Regression Drop Down Menu
regressionVar = tk.StringVar()
regression_choices = ("Linear", "Logistic")
regressionVar.set("Select")
regression_menu = tk.OptionMenu(f3_left_frame, regressionVar, *regression_choices)
regression_menu.grid(row=3, column=1)


#Correction Drop Down Menu
correctionVar = tk.StringVar()
correction_choices = ("bonferroni", "fdr", "both")
correctionVar.set("Select")
correction_menu = tk.OptionMenu(f3_left_frame, correctionVar, *correction_choices)
correction_menu.grid(row=4, column=1)


#Phenotypes
global phenotype_var
phenotype_var= StringVar()
phenotype_var.set("")
phenotype_var_label = Label(f3_left_frame, text="Phenotype ",font=("Comfortaa", 14))
phenotype_var_label.grid(row=5, column=0)
phenotype_var_label_entry_box  = Entry(f3_left_frame, textvariable=phenotype_var, width=15, bg="alice blue")
phenotype_var_label_entry_box.grid(row=5, column=1)
phenotype_var_label_entry_box.config(width=9)


#Formatting Purposes
place_holder1 = Label(f3_left_frame, text="",font=("Comfortaa", 14))
place_holder1.grid(row=6, column=3)


ewas_button = Button(f3_left_frame,text="Run", command= ewas)
ewas_button.grid(row=7, column=0,columnspan=3)
ewas_button.config(width=13)





###***************************************************************************************###
   		###*************************** GUI PROGRAM  **************************###
###****************************** Associations - MPLOT  **********************************************###
###***************************************************************************************###


def man_plot():

	f = open('r/eman.R','r')
	filedata = f.read()
	f.close()

	if str(ewas_output_Var.get()) == "Yes":
		argument_1 = "a1 = TRUE"
	else:
		argument_1 = "a1 = FALSE"


	#optional
	if option_groups_file_mplot != "":
		a0_2_man = "a2 = read.delim('"
		argument_2 = a0_2_man + option_groups_file_mplot + A0_2
	else:
		argument_2 = "a2 = NULL"


	#optional
	if str(p_value_threshold_var.get()) == "":
		argument_3 = "a3 = NULL" 
	else:
		argument_3 = "a3 = " + str(p_value_threshold_var.get())

	#optional
	if str(plot_title_var.get()) != "":
		argument_4 = "a4 = " + "'" + str(plot_title_var.get()) + "'" 
	else:
		argument_4 = "a4 = NULL"


	if str(moreColorsVar.get()) == "Standard Colors":
		argument_5 = "a5 = TRUE"
	else:
		argument_5 = "a5 = FALSE"

	#argument_6 = the file 

	argument_7 = "a7 = " + str(man_height_of_plot_var.get())
	argument_8 = "a8 = " + str(man_width_of_plot_var.get())
	argument_9 = "a9 = " + str(man_res_of_plot_var.get())



	newdata = filedata	
	f = open('r/GUI_Scripts/eman1.R','w')
	f.write(newdata)
	# f.write(GGPLOT_LIBRARY + '\n')
	# f.write(GRIDEXTRA_LIBRARY + '\n')
	# if str(moreColorsVar.get()) != "Standard Colors":
	# 	f.write(RCOLORBREWER_LIBRARY + '\n')
	f.write(final + '\n')
	f.write(argument_1 + '\n')
	f.write(argument_2 + '\n')
	f.write(argument_3 + '\n')
	f.write(argument_4 + '\n')
	f.write(argument_5 + '\n')
	# f.write(argument_6 + '\n')
	f.write(argument_7 + '\n')
	f.write(argument_8 + '\n')
	f.write(argument_9 + '\n')

	f.write("newdata <- eman(a0,a1,a2,a3,a4,a5," + "file=" + GLOBAL_OUTPUT_DIRECTORY_FORMATTED +  "ManPlot" + "'," + "a7,a8,a9)"+ '\n')


	f.close()
	

	with open(log_file_name, 'a') as g:
		g.write("---------------------------------------------------------------" + datetime.datetime.now().strftime("%I:%M:%S %p") + "---------------------------------------------------------------" + '\n')

		g.write("EWAS Called" + '\n')

	
	s_out = open(log_file_name, "a")
	proc = subprocess.call(['Rscript','r/GUI_Scripts/eman1.R'], shell=False,stdout=s_out, stderr=s_out)

	refresh_logs()




label = Label(f3_right_frame, text="Manhattan Plot")
label.grid(row=0, column=0, columnspan=2)
label.config(font=("Comfortaa", 20))
label.config(fg="cyan4")


global man_height_of_plot_var
man_height_of_plot_var= StringVar()
man_height_of_plot_var.set("")
plot_height_label = Label(f3_right_frame, text="* Height of Plot ",font=("Comfortaa", 14))
plot_height_label.grid(row=1, column=0)
plot_height_label_entry_box  = Entry(f3_right_frame, textvariable=man_height_of_plot_var, width=15, bg="alice blue")
plot_height_label_entry_box.grid(row=1, column=1)



global man_width_of_plot_var
man_width_of_plot_var= StringVar()
man_width_of_plot_var.set("")
plot_width_label = Label(f3_right_frame, text="* Width of Plot ",font=("Comfortaa", 14))
plot_width_label.grid(row=2, column=0)
plot_width_label_entry_box  = Entry(f3_right_frame, textvariable=man_width_of_plot_var, width=15, bg="alice blue")
plot_width_label_entry_box.grid(row=2, column=1)



global man_res_of_plot_var
man_res_of_plot_var= StringVar()
man_res_of_plot_var.set("")
plot_resolution_label = Label(f3_right_frame, text="* Resolution of Plot ",font=("Comfortaa", 14))
plot_resolution_label.grid(row=3, column=0)
plot_resolution_label_entry_box  = Entry(f3_right_frame, textvariable=man_res_of_plot_var, width=15, bg="alice blue")
plot_resolution_label_entry_box.grid(row=3, column=1)




#Colors in Program
moreColorsVar_label = Label(f3_right_frame, text="* Plot Colors ",font=("Comfortaa", 14))
moreColorsVar_label.grid(row=4, column=0)

global moreColorsVar
moreColorsVar = tk.StringVar()
moreColorVar_choices = ("Standard Colors", "Expanded Colors")
moreColorsVar.set("Select")
moreColorsVar_menu = tk.OptionMenu(f3_right_frame, moreColorsVar, *moreColorVar_choices)
moreColorsVar_menu.grid(row=4, column=1)

ewas_output_Var_label = Label(f3_right_frame, text="* EWAS output ",font=("Comfortaa", 14))
ewas_output_Var_label.grid(row=5, column=0)

ewas_output_Var = tk.StringVar()
ewas_output_Var_choices = ("Yes", "No")
ewas_output_Var.set("Select")

ewas_output_Var_menu = tk.OptionMenu(f3_right_frame, ewas_output_Var, *ewas_output_Var_choices)
ewas_output_Var_menu.grid(row=5, column=1)


#Groups File - Optional
global manhattan_plot_choosefile
manhattan_plot_choosefile= StringVar()
manhattan_plot_choosefile.set("No File Chosen")
manhattan_plot_choosefile_label = Label(f3_right_frame, textvariable=manhattan_plot_choosefile,font=("Comfortaa", 14))
manhattan_plot_choosefile_label.grid(row=6, column=1)
groups_file_button = Button(f3_right_frame,text="Choose Groups File", command= optional_file_mplot)
groups_file_button.grid(row=6, column=0)
global option_groups_file_mplot
option_groups_file_mplot = ""


#P value - Optional
global p_value_threshold_var
p_value_threshold_var= StringVar()
p_value_threshold_var.set("")
pvalue_threshold_label = Label(f3_right_frame, text="P Value Threshold ",font=("Comfortaa", 14))
pvalue_threshold_label.grid(row=7, column=0)
pvalue_threshold_label_entry_box  = Entry(f3_right_frame, textvariable=p_value_threshold_var, width=15, bg="alice blue")
pvalue_threshold_label_entry_box.grid(row=7, column=1)


#Title - Optional
global plot_title_var
plot_title_var= StringVar()
plot_title_var.set("")
plot_title_label = Label(f3_right_frame, text="Plot Title ",font=("Comfortaa", 14))
plot_title_label.grid(row=8, column=0)
plot_title_label_entry_box  = Entry(f3_right_frame, textvariable=plot_title_var, width=15, bg="alice blue")
plot_title_label_entry_box.grid(row=8, column=1)



manhattan_plot_button = Button(f3_right_frame,text="Run", command= man_plot)
manhattan_plot_button.grid(row=9, column=0,columnspan=2)
manhattan_plot_button.config(width=13)


# red_symbol_1 = Label(f3, text="*",font=("Comfortaa", 14))
# red_symbol_1.config(fg="red")
# red_symbol_1.place(x=646, y=46)

# red_symbol_2 = Label(f3, text="*",font=("Comfortaa", 14))
# red_symbol_2.config(fg="red")
# red_symbol_2.place(x=645, y=75)

# red_symbol_3 = Label(f3, text="*",font=("Comfortaa", 14))
# red_symbol_3.config(fg="red")
# red_symbol_3.place(x=660, y=102)

# red_symbol_4 = Label(f3, text="*",font=("Comfortaa", 14))
# red_symbol_4.config(fg="red")
# red_symbol_4.place(x=640, y=132)

# red_symbol_5 = Label(f3, text="*",font=("Comfortaa", 14))
# red_symbol_5.config(fg="red")
# red_symbol_5.place(x=645, y=160)

# self.entry.configure(highlightbackground="red", highlightcolor="red")


###***************************************************************************************###
   		###*************************** Logging PROGRAM  **************************###
###****************************** Logs all inputs & STDOUT/ERROR  **********************************************###
###***************************************************************************************###

def refresh_logs():

	listNodes.delete(0,END)
    	with open(log_file_name) as f:
		for line in f:

			listNodes.insert(END, str(line))

	listNodes.see(END)

def log_updater():


	log_label = Label(root,bg="white",text = "Live Logs")
	log_label.config(font=("Comfortaa", 15))
	log_label.pack(pady=(0,0))
	log_label.config(fg="black")
	log_label.pack(fill=X)


	frame = Frame(root)
	frame.pack()

	global listNodes 
	listNodes = Listbox(frame, width=80, height=10, font=("Helvetica", 17))
	listNodes.pack(side="left", fill="y")

	scrollbar = Scrollbar(frame, orient="vertical")
	scrollbar.config(command=listNodes.yview)
	scrollbar.pack(side="right", fill="y")

	listNodes.config(yscrollcommand=scrollbar.set)

	refresh_logs()






def instructions_popup(title, name_of_instr):

	toplevel = Toplevel()
	toplevel.geometry("410x435")

	log_label = Label(toplevel,bg="white",text = title + " Instructions")
	log_label.config(font=("Comfortaa", 15))
	log_label.pack(pady=(5,0))
	log_label.config(fg="cyan4")
	log_label.pack(fill=X)


	frame = Frame(root)
	frame.pack()

	global instructionNodes 
	instructionNodes = Listbox(toplevel, width=66, height=5, font=("Helvetica", 12))
	instructionNodes.pack(side="left", fill="y")

	scrollbar = Scrollbar(toplevel, orient="vertical")
	scrollbar.config(command=instructionNodes.yview)
	scrollbar.pack(side="right", fill="y")

	instructionNodes.config(yscrollcommand=scrollbar.set)

    	with open(name_of_instr) as f:
		for line in f:

			instructionNodes.insert(END, str(line))




# grous_file_button = Button(root,width=IN_WIDTH, height = IN_HEIGHT,command= lambda: instructions_popup("Bar Plot", "man/bar_plot.Rd"))
# image = ImageTk.PhotoImage(file="instruction.png")
# grous_file_button.config(image=image)
# grous_file_button.image = image
# grous_file_button.pack()


n.pack(fill=X)
log_updater()
root.mainloop()



###***************************************************************************************###
   		###*************************** OLD GUI CLASS BELOW **************************###
###***************************************************************************************###
# class App(Tk):

# 	def __init__(self, *args, **kwargs):
# 		Tk.__init__(self, *args, **kwargs)

# 		#Setup Menu
# 		MainMenu(self)
# 		#Setup Frams
# 		container = Frame(self)
# 		container.pack(side="top", fill="both", expand=True)
# 		container.grid_rowconfigure(0, weight=1)		
# 		container.grid_columnconfigure(0, weight=1)


# 		self.frames = {}

# 		for F in (StartPage, CleanData, RecodeMissing, RecodeKey, RemoveOutliers, VisualizeData,FilterData,SummarizeData, HistogramProgram,BoxPlotProgram,QQPlot,BarPlotProgram,CheckPage,MenuOptions,ContinuousProgram,CategoricalProgram,AmbiguousProgram,SampleKeepProgram,TransformData,FindOutliersProgram,FindCorrelationsProgram,VisualizeDataMenu,SummarizeDataMenu,EndPage

# ):
# 			frame = F(container, self)
# 			self.frames[F] = frame
# 			frame.grid(row=0, column=0, sticky="snew")

# 		self.show_frame(StartPage)

# 	def show_frame(self, context):
# 		frame = self.frames[context]
# 		frame.tkraise()



# class StartPage(Frame):
# 	def __init__(self,parent, controller):
# 		Frame.__init__(self,parent)

# 		label = Label(self, text="Welcome to Data Cleaner")
# 		label.config(width=200)
# 		label.config(font=("Comfortaa", 35))
# 		label.config(fg="cyan4")


# 		label.pack(pady=10)

# 		global choose_start_file_name
# 		choose_start_file_name= StringVar()
# 		choose_start_file_name.set("Start by Choosing a Data File")

		
# 		choose_file_label = Label(self, textvariable=choose_start_file_name)
# 		choose_file_label.config(font=("Comfortaa", 15))
# 		choose_file_label.pack(pady=20)
		
# 		choose_file = Button(self,text="Chose File", command= choose_start_file)
# 		choose_file.pack(pady=20)

# 		continue_button = Button(self,text="Continue", command= lambda:controller.show_frame(CheckPage))
# 		continue_button.pack(pady=20)
		# self.button.pack(ipadx=10, ipady=10)


# class CheckPage(Frame):
# 	def __init__(self,parent, controller):
# 		Frame.__init__(self,parent)

# 		label = Label(self, text="Do you want to clean your data file?")
# 		label.config(width=200)
# 		label.config(font=("Comfortaa", 20))
# 		label.pack(pady=20)
# 		label.config(fg="cyan4")



# 		global var_int 
# 		var_int = IntVar()
# 		var_int.set = 0

# 		def checkbutton_value1():
# 		    if(var1.get()):
# 		       var2.set(0)
# 		       controller.show_frame(CleanData)
# 		       with open(log_file_name, 'a') as g:
# 					g.write("---------------------------------------------------------------" + datetime.datetime.now().strftime("%I:%M:%S %p") + "---------------------------------------------------------------" + '\n')
# 					g.write("Do you want to clean your data file:  USER Selected ----->   YES" + '\n')
# 					g.write('\n'+'\n')


# 		def checkbutton_value2():
# 		    if(var2.get()):
# 		       var1.set(0)
# 		       controller.show_frame(MenuOptions)
# 		       with open(log_file_name, 'a') as g:
# 					g.write("---------------------------------------------------------------" + datetime.datetime.now().strftime("%I:%M:%S %p") + "---------------------------------------------------------------" + '\n')
# 					g.write("Do you want to clean your data file:  USER Selected ----->   NO" + '\n')
# 					g.write('\n'+'\n')


# 		var1=IntVar()
# 		checkbox_1 = Checkbutton(self, text='Yes   ', variable=var1, command=checkbutton_value1,font=("Comfortaa", 20))
# 		checkbox_1.pack(pady=10)
# 		var2=IntVar()
# 		checkbox_2 = Checkbutton(self, text='No   ', variable=var2, command=checkbutton_value2,font=("Comfortaa", 20))
# 		checkbox_2.pack(pady=(0,40))


# 		back = Button(self,text="Back", command= lambda:controller.show_frame(StartPage))
# 		back.pack()
# #Clean File Page
# class CleanData(Frame):
# 	def __init__(self,parent, controller):
# 		Frame.__init__(self,parent)

# 		label = Label(self, text="Do you have one missing value to replace in your file?")
# 		label.config(width=200)
# 		label.config(font=("Comfortaa", 20))
# 		label.pack(pady=20)
# 		label.config(fg="cyan4")


# 		def checkbutton_value1():
# 		    if(var1.get()):
# 		       var2.set(0)
# 		       controller.show_frame(RecodeMissing)


# 		def checkbutton_value2():
# 		    if(var2.get()): 
# 		       var1.set(0)
# 		       controller.show_frame(RecodeKey)

# 		var1=IntVar()
# 		checkbox_1 = Checkbutton(self, text='Yes   ', variable=var1, command=checkbutton_value1,font=("Comfortaa", 20))
# 		checkbox_1.pack(pady=10)
# 		var2=IntVar()
# 		checkbox_2 = Checkbutton(self, text='No   ', variable=var2, command=checkbutton_value2,font=("Comfortaa", 20))
# 		checkbox_2.pack(pady=(0,40))

# 		back = Button(self,text="Back", command= lambda:controller.show_frame(CheckPage))
# 		back.pack()


# class MenuOptions(Frame):
# 	def __init__(self,parent, controller):
# 		Frame.__init__(self,parent)

# 		label = Label(self, text="Do you want to filter your Data?")
# 		label.config(width=200)
# 		label.config(font=("Comfortaa", 20))
# 		label.pack(pady=(10,40))
# 		label.config(fg="cyan4")


# 		def checkbutton_value1():
# 		    if(var1.get()):
# 		       var2.set(0)
# 		       controller.show_frame(FilterData)
# 		       with open(log_file_name, 'a') as g:
# 					g.write("---------------------------------------------------------------" + datetime.datetime.now().strftime("%I:%M:%S %p") + "---------------------------------------------------------------" + '\n')
# 					g.write("Do you want to filter your data file:  USER Selected ----->   YES" + '\n')
# 					g.write('\n'+'\n')


# 		def checkbutton_value2():
# 		    if(var2.get()):
# 		       var1.set(0)
# 		       controller.show_frame(VisualizeDataMenu)
# 		       with open(log_file_name, 'a') as g:
# 					g.write("---------------------------------------------------------------" + datetime.datetime.now().strftime("%I:%M:%S %p") + "---------------------------------------------------------------" + '\n')
# 					g.write("Do you want to filter your data file:  USER Selected ----->   NO" + '\n')
# 					g.write('\n'+'\n')

# 		var1=IntVar()
# 		checkbox_1 = Checkbutton(self, text='Yes   ', variable=var1, command=checkbutton_value1,font=("Comfortaa", 20))
# 		checkbox_1.pack(pady=10)
# 		var2=IntVar()
# 		checkbox_2 = Checkbutton(self, text='No   ', variable=var2, command=checkbutton_value2,font=("Comfortaa", 20))
# 		checkbox_2.pack(pady=(0,40))


# 		# filter_data_page = Button(self,text="Filter Data", command= lambda:controller.show_frame(FilterData))
# 		# filter_data_page.pack(pady=(0,20))


# 		# visualize_data_page = Button(self,text="Visualize Data", command= lambda:controller.show_frame(VisualizeDataMenu))
# 		# visualize_data_page.pack(pady=(0,20))

# 		# summarize_page_button = Button(self,text="Summarize Data", command= lambda:controller.show_frame(SummarizeDataMenu))
# 		# summarize_page_button.pack(pady=(0,40))

# 		back = Button(self,text="Back", command= lambda:controller.show_frame(CheckPage))
# 		back.pack()

# class VisualizeDataMenu(Frame):

# 	def __init__(self,parent, controller):
# 		Frame.__init__(self,parent)


# 		label = Label(self, text="Do you want to Visualize your Data?")
# 		label.config(width=200)
# 		label.config(font=("Comfortaa", 20))
# 		label.pack(pady=30)
# 		label.config(fg="cyan4")

# 		def checkbutton_value1():
# 		    if(var1.get()):
# 		       var2.set(0)
# 		       controller.show_frame(VisualizeData)
# 		       with open(log_file_name, 'a') as g:
# 					g.write("---------------------------------------------------------------" + datetime.datetime.now().strftime("%I:%M:%S %p") + "---------------------------------------------------------------" + '\n')
# 					g.write("Do you want to visualize your data file:  USER Selected ----->   YES" + '\n')
# 					g.write('\n'+'\n')


# 		def checkbutton_value2():
# 		    if(var2.get()):
# 		       var1.set(0)
# 		       controller.show_frame(SummarizeDataMenu)
# 		       with open(log_file_name, 'a') as g:
# 					g.write("---------------------------------------------------------------" + datetime.datetime.now().strftime("%I:%M:%S %p") + "---------------------------------------------------------------" + '\n')
# 					g.write("Do you want to visualize your data file:  USER Selected ----->   NO" + '\n')
# 					g.write('\n'+'\n')

# 		var1=IntVar()
# 		checkbox_1 = Checkbutton(self, text='Yes   ', variable=var1, command=checkbutton_value1,font=("Comfortaa", 20))
# 		checkbox_1.pack(pady=10)
# 		var2=IntVar()
# 		checkbox_2 = Checkbutton(self, text='No   ', variable=var2, command=checkbutton_value2,font=("Comfortaa", 20))
# 		checkbox_2.pack(pady=(0,40))

# 		back = Button(self,text="Back", command= lambda:controller.show_frame(MenuOptions))
# 		back.pack()

# class SummarizeDataMenu(Frame):

# 	def __init__(self,parent, controller):
# 		Frame.__init__(self,parent)


# 		label = Label(self, text="Want to make Summaries of your Data?")
# 		label.config(width=200)
# 		label.config(font=("Comfortaa", 20))
# 		label.pack(pady=30)
# 		label.config(fg="cyan4")

# 		def checkbutton_value1():
# 		    if(var1.get()):
# 		       var2.set(0)
# 		       controller.show_frame(SummarizeData)
# 		       with open(log_file_name, 'a') as g:
# 					g.write("---------------------------------------------------------------" + datetime.datetime.now().strftime("%I:%M:%S %p") + "---------------------------------------------------------------" + '\n')
# 					g.write("Do you want to summarize your data file:  USER Selected ----->   YES" + '\n')
# 					g.write('\n'+'\n')


# 		def checkbutton_value2():
# 		    if(var2.get()):
# 		       var1.set(0)
# 		       controller.show_frame(EndPage)
# 		       with open(log_file_name, 'a') as g:
# 					g.write("---------------------------------------------------------------" + datetime.datetime.now().strftime("%I:%M:%S %p") + "---------------------------------------------------------------" + '\n')
# 					g.write("Do you want to summarize your data file:  USER Selected ----->   NO" + '\n')
# 					g.write('\n'+'\n')


# 		var1=IntVar()
# 		checkbox_1 = Checkbutton(self, text='Yes   ', variable=var1, command=checkbutton_value1,font=("Comfortaa", 20))
# 		checkbox_1.pack(pady=10)
# 		var2=IntVar()
# 		checkbox_2 = Checkbutton(self, text='No   ', variable=var2, command=checkbutton_value2,font=("Comfortaa", 20))
# 		checkbox_2.pack(pady=(0,40))

# 		back = Button(self,text="Back", command= lambda:controller.show_frame(VisualizeDataMenu))
# 		back.pack()

# class EndPage(Frame):
# 	def __init__(self,parent, controller):
# 		Frame.__init__(self,parent)


# 		label = Label(self, text="EndPage")
# 		label.config(width=200)
# 		label.config(font=("Comfortaa", 20))
# 		label.pack(pady=30)
# 		label.config(fg="cyan4")


# 		back = Button(self,text="Start Over", command= lambda:controller.show_frame(StartPage))
# 		back.pack()

# 		back = Button(self,text="Back", command= lambda:controller.show_frame(SummarizeDataMenu))
# 		back.pack()


# #recode missing
# class RecodeMissing(Frame):

# 	def __init__(self,parent, controller):
# 		Frame.__init__(self,parent)


# 		label = Label(self, text="Replace Missing Values with Input")
# 		label.config(width=200)
# 		label.config(font=("Comfortaa", 20))
# 		label.pack(pady=30)
# 		label.config(fg="cyan4")



# 		global missing_val_replacement 
# 		missing_val_replacement = StringVar()

# 		global missing_val 
# 		missing_val = StringVar()


# 		replacement_label = Label(self, text="What do want to replace your missing value with?",font=("Comfortaa", 14))
# 		replacement_label.pack()
# 		replacement_box  = Entry(self, textvariable=missing_val_replacement, width=15, bg="alice blue")
# 		replacement_box.pack(pady=(0,35))

# 		missing_label = Label(self, text="Type in the missing value",font=("Comfortaa", 14))
# 		missing_label.pack()
# 		missing_entry_box  = Entry(self, textvariable=missing_val, width=15, bg="alice blue")
# 		missing_entry_box.pack(pady=(0,30))

# 		clense_button = Button(self,text="Clense", command= recode_missing)
# 		clense_button.pack(pady=(0,25))

# 		continue_button = Button(self,text="Continue", command= lambda:controller.show_frame(MenuOptions))
# 		continue_button.pack(pady=(0,25))

# 		back = Button(self,text="Back", command= lambda:controller.show_frame(CleanData))
# 		back.pack()
# #recode key
# class RecodeKey(Frame):
# 	def __init__(self,parent, controller):
# 		Frame.__init__(self,parent)


# 		label = Label(self, text="Missing Value Dictionary - Replace Missing")
# 		label.config(width=200)
# 		label.config(font=("Comfortaa", 20))
# 		label.pack(pady=20)
# 		label.config(fg="cyan4")

# 		# start_page = Button(self,text="Back to Main Menu", command= lambda:controller.show_frame(StartPage))
# 		# start_page.pack()

# 		global choose_dictionary_label_name 
# 		choose_dictionary_label_name= StringVar()
# 		choose_dictionary_label_name.set("No Dictionary Chosen")

# 		choose_dictionary_label = Label(self, textvariable=choose_dictionary_label_name,font=("Comfortaa", 14))
# 		choose_dictionary_label.pack(pady=5)


# 		choose_dictionary = Button(self,text="Choose Dictionary", command= dialogue_file_dictionary)
# 		choose_dictionary.pack(pady=(0,35))	


# 		clense_button = Button(self,text="Clense", command= recode_key)
# 		clense_button.pack(pady=(0,25))


# 		continue_button = Button(self,text="Continue", command= lambda:controller.show_frame(MenuOptions))
# 		continue_button.pack(pady=(0,25))

# 		back = Button(self,text="Back", command= lambda:controller.show_frame(CleanData))
# 		back.pack()
# 		######


# #Remove Outliers
# class RemoveOutliers(Frame):
# 	def __init__(self,parent, controller):
# 		Frame.__init__(self,parent)

# 		label = Label(self, text="Remove Outliers",font=("Comfortaa", 20))
# 		label.pack(pady=20)
# 		label.config(fg="cyan4")


# 		global choose_outlier_file_name 
# 		choose_outlier_file_name= StringVar()
# 		choose_outlier_file_name.set("No Data File Chosen")
		

# 		remove_outliers_button = Button(self,text="Remove Outliers", command= remove_outliers)
# 		remove_outliers_button.pack(pady=(0,45))


# 		back = Button(self,text="Back", command= lambda:controller.show_frame(FilterData))
# 		back.pack()


# class FilterData(Frame):
# 	def __init__(self,parent, controller):
# 		Frame.__init__(self,parent)

# 		label = Label(self, text="Filter Data Menu")
# 		label.config(font=("Comfortaa", 20))
# 		label.pack(pady=20)
# 		label.config(fg="cyan4")


# 		remove_outliers = Button(self,text="Remove Outliers", command= lambda:controller.show_frame(RemoveOutliers))
# 		remove_outliers.pack(pady=(0,15))


# 		get_binary_button = Button(self,text="Find Binary Variables", command= get_binary)
# 		get_binary_button.pack(pady=(0,15))

# 		get_continuous_button = Button(self,text="Find Continuous Variables", command= lambda:controller.show_frame(ContinuousProgram))
# 		get_continuous_button.pack(pady=(0,15))

# 		get_categorical_button = Button(self,text="Find Categorical Variables", command= lambda:controller.show_frame(CategoricalProgram))
# 		get_categorical_button.pack(pady=(0,15))

# 		get_check_button = Button(self,text="Find Ambiguous Variables", command= lambda:controller.show_frame(AmbiguousProgram))
# 		get_check_button.pack(pady=(0,15))

# 		min_n_button = Button(self,text="Keep N Samples", command= lambda:controller.show_frame(SampleKeepProgram))
# 		min_n_button.pack(pady=(0,15))

# 		transvar_button = Button(self,text="Transform Data", command= lambda:controller.show_frame(TransformData))
# 		transvar_button.pack(pady=(0,40))


# 		global choose_filter_file_name 
# 		choose_filter_file_name= StringVar()
# 		choose_filter_file_name.set("No Data File Chosen")
		

# 		back = Button(self,text="Back", command= lambda:controller.show_frame(MenuOptions))
# 		back.pack()


# def get_key_dictionary():
# 	global global_transform_key_dictionary
# 	global_transform_key_dictionary = tkFileDialog.askopenfilename()
# 	key_dictionary_label.set("Dictionary File: " + global_transform_key_dictionary)


# class TransformData(Frame):
# 	def __init__(self,parent, controller):
# 		Frame.__init__(self,parent)

# 		label = Label(self, text="Transform User List of Columns & Transform Type")
# 		label.config(font=("Comfortaa", 20))
# 		label.pack(pady=20)
# 		label.config(fg="cyan4")


# 		global key_dictionary_label 
# 		key_dictionary_label= StringVar()
# 		key_dictionary_label.set("No Dictionary Chosen")

# 		choose_key_dictionary_label = Label(self, textvariable=key_dictionary_label)
# 		choose_key_dictionary_label.pack(pady=5)


# 		choose_key_dictionary = Button(self,text="Choose Key Dictionary", command= get_key_dictionary)
# 		choose_key_dictionary.pack(pady=(0,20))	


# 		transform_data_button = Button(self,text="Variable Specific Transformation", command = transvar)
# 		transform_data_button.pack(pady=(0,40))

# 		back = Button(self,text="Back", command= lambda:controller.show_frame(FilterData))
# 		back.pack()

# #Analyze File Data
# class VisualizeData(Frame):
# 	def __init__(self,parent, controller):
# 		Frame.__init__(self,parent)

# 		label = Label(self, text="Visualize Data Menu")
# 		label.config(width=200)
# 		label.config(font=("Comfortaa", 20))
# 		label.pack(pady=20)
# 		label.config(fg="cyan4")



# 		histogram_button = Button(self,text="Generate Histogram", command= lambda:controller.show_frame(HistogramProgram))
# 		histogram_button.pack(pady=(0,20))

# 		box_plot_button = Button(self,text="Generate Box Plot", command= lambda:controller.show_frame(BoxPlotProgram))
# 		box_plot_button.pack(pady=(0,20))

# 		qq_plot_button = Button(self,text="Generate QQ Plot", command= lambda:controller.show_frame(QQPlot))
# 		qq_plot_button.pack(pady=(0,20))

# 		bar_plot_button = Button(self,text="Generate Bar Plot", command= lambda:controller.show_frame(BarPlotProgram))
# 		bar_plot_button.pack(pady=(0,40))

# 		back = Button(self,text="Back", command= lambda:controller.show_frame(VisualizeDataMenu))
# 		back.pack()



# class SummarizeData(Frame):
# 	def __init__(self,parent, controller):
# 		Frame.__init__(self,parent)

# 		label = Label(self, text="Summarize Data Menu")
# 		label.config(width=200)
# 		label.config(font=("Comfortaa", 20))
# 		label.pack(pady=20)
# 		label.config(fg="cyan4")



# 		frequency_table_button = Button(self,text="Frequency Table", command= frequency_table)
# 		frequency_table_button.pack(pady=(0,20))

# 		correlations_button = Button(self,text="Correlations", command= lambda:controller.show_frame(FindCorrelationsProgram))
# 		correlations_button.pack(pady=(0,20))

# 		get_uniq_button = Button(self,text="Get Unique Values", command= get_uniq)
# 		get_uniq_button.pack(pady=(0,20))

# 		sample_size_button = Button(self,text="Sample Size", command= sample_size)
# 		sample_size_button.pack(pady=(0,20))

# 		outliers_button = Button(self,text="Find Outliers", command= lambda:controller.show_frame(FindOutliersProgram))
# 		outliers_button.pack(pady=(0,20))

# 		chisq_tests_button = Button(self,text="Chisq Tests", command= chisq_tests)
# 		chisq_tests_button.pack(pady=(0,40))

# 		back = Button(self,text="Back", command= lambda:controller.show_frame(SummarizeDataMenu))
# 		back.pack()

# #Histogram Program
# class HistogramProgram(Frame):
# 	def __init__(self,parent, controller):
# 		Frame.__init__(self,parent)

# 		label = Label(self, text="Histogram Program")
# 		label.config(width=200)
# 		label.config(font=("Comfortaa", 20))
# 		label.pack(pady=(10,40))
# 		label.config(fg="cyan4")


# 		histogram_button2 = Button(self,text="Generate Histogram", command= histogram)
# 		histogram_button2.pack(pady=(0,20))

# 		back = Button(self,text="Back", command= lambda:controller.show_frame(VisualizeData))
# 		back.pack()

# 		global h1
# 		h1= StringVar()
# 		h1.set("")

# 		global h2
# 		h2= StringVar()
# 		h2.set("")

# 		global h3
# 		h3= StringVar()
# 		h3.set("")

# 		global h4
# 		h4= StringVar()
# 		h4.set("")

# 		global h5
# 		h5= StringVar()
# 		h5.set("")

# 		global h6
# 		h6= StringVar()
# 		h6.set("")

# 		left_frame = Frame(self)
# 		left_frame.pack(side=LEFT)
# 		right_frame = Frame(self)
# 		right_frame.pack(side=RIGHT)


# 		plots_label = Label(left_frame, text="# of Plots Per Page",font=("Comfortaa", 14))
# 		plots_label.pack()
# 		plots_labelentry_box  = Entry(left_frame, textvariable=h1, width=15, bg="alice blue")
# 		plots_labelentry_box.pack(padx=(90),pady=(0,20))

# 		row_label = Label(left_frame, text="# of Rows Per Page",font=("Comfortaa", 14))
# 		row_label.pack()
# 		row_label_entry_box  = Entry(left_frame, textvariable=h2, width=15, bg="alice blue")
# 		row_label_entry_box.pack(padx=(90),pady=(0,20))

# 		column_label = Label(left_frame, text="# of Columns Per Page",font=("Comfortaa", 14))
# 		column_label.pack()
# 		column_label_entry_box  = Entry(left_frame, textvariable=h3, width=15, bg="alice blue")
# 		column_label_entry_box.pack(padx=(90),pady=(0,20))

# 		width_label = Label(right_frame, text="Width of Plot",font=("Comfortaa", 14))
# 		width_label.pack(padx=(0,100))
# 		width_label_entry_box  = Entry(right_frame, textvariable=h4, width=15, bg="alice blue")
# 		width_label_entry_box.pack(padx=(0,100),pady=(0,20))

# 		height_label = Label(right_frame, text="Height of Plot",font=("Comfortaa", 14))
# 		height_label.pack(padx=(0,100))
# 		height_label_entry_box  = Entry(right_frame, textvariable=h5, width=15, bg="alice blue")
# 		height_label_entry_box.pack(padx=(0,100),pady=(0,20))

# 		resolution_label = Label(right_frame, text="Resolution of Plot",font=("Comfortaa", 14))
# 		resolution_label.pack(padx=(0,100))
# 		resolution_label_entry_box  = Entry(right_frame, textvariable=h6, width=15, bg="alice blue")
# 		resolution_label_entry_box.pack(padx=(0,100),pady=(0,20))

# 		# tfield = tkinter.Text(self)
# 		# tfield.pack()
# 		# for line in os.popen("run_command", 'r'):
# 		# 	tfield.insert("end", line)

# #Box Plot Program
# class BoxPlotProgram(Frame):
# 	def __init__(self,parent, controller):
# 		Frame.__init__(self,parent)

# 		label = Label(self, text="Box Plot Program")
# 		label.config(width=200)
# 		label.config(font=("Comfortaa", 20))
# 		label.pack(pady=(10,40))
# 		label.config(fg="cyan4")


# 		box_plot_button = Button(self,text="Generate Box Plot", command= boxplot)
# 		box_plot_button.pack(pady=(0,20))

# 		back = Button(self,text="Back", command= lambda:controller.show_frame(VisualizeData))
# 		back.pack()

# 		global bx1
# 		bx1= StringVar()
# 		bx1.set("")

# 		global bx2
# 		bx2= StringVar()
# 		bx2.set("")

# 		global bx3
# 		bx3= StringVar()
# 		bx3.set("")

# 		global bx4
# 		bx4= StringVar()
# 		bx4.set("")

# 		global bx5
# 		bx5= StringVar()
# 		bx5.set("")

# 		global bx6
# 		bx6= StringVar()
# 		bx6.set("")

# 		left_frame = Frame(self)
# 		left_frame.pack(side=LEFT)
# 		right_frame = Frame(self)
# 		right_frame.pack(side=RIGHT)


# 		plots_label = Label(left_frame, text="# of Plots Per Page",font=("Comfortaa", 14))
# 		plots_label.pack()
# 		plots_labelentry_box  = Entry(left_frame, textvariable=bx1, width=15, bg="alice blue")
# 		plots_labelentry_box.pack(padx=(90),pady=(0,20))

# 		row_label = Label(left_frame, text="# of Rows Per Page",font=("Comfortaa", 14))
# 		row_label.pack()
# 		row_label_entry_box  = Entry(left_frame, textvariable=bx2, width=15, bg="alice blue")
# 		row_label_entry_box.pack(padx=(90),pady=(0,20))

# 		column_label = Label(left_frame, text="# of Columns Per Page",font=("Comfortaa", 14))
# 		column_label.pack()
# 		column_label_entry_box  = Entry(left_frame, textvariable=bx3, width=15, bg="alice blue")
# 		column_label_entry_box.pack(padx=(90),pady=(0,20))

# 		width_label = Label(right_frame, text="Width of Plot",font=("Comfortaa", 14))
# 		width_label.pack(padx=(0,100))
# 		width_label_entry_box  = Entry(right_frame, textvariable=bx4, width=15, bg="alice blue")
# 		width_label_entry_box.pack(padx=(0,100),pady=(0,20))

# 		height_label = Label(right_frame, text="Height of Plot",font=("Comfortaa", 14))
# 		height_label.pack(padx=(0,100))
# 		height_label_entry_box  = Entry(right_frame, textvariable=bx5, width=15, bg="alice blue")
# 		height_label_entry_box.pack(padx=(0,100),pady=(0,20))

# 		resolution_label = Label(right_frame, text="Resolution of Plot",font=("Comfortaa", 14))
# 		resolution_label.pack(padx=(0,100))
# 		resolution_label_entry_box  = Entry(right_frame, textvariable=bx6, width=15, bg="alice blue")
# 		resolution_label_entry_box.pack(padx=(0,100),pady=(0,20))
# #QQ Plot 
# class QQPlot(Frame):
# 	def __init__(self,parent, controller):
# 		Frame.__init__(self,parent)


# 		label = Label(self, text="QQPlot Program")
# 		label.config(width=200)
# 		label.config(font=("Comfortaa", 20))
# 		label.pack(pady=(10,40))
# 		label.config(fg="cyan4")

		

# 		qq_plot_button = Button(self,text="Generate QQPlot", command= qqplot)
# 		qq_plot_button.pack(pady=(0,20))

# 		back = Button(self,text="Back", command= lambda:controller.show_frame(VisualizeData))
# 		back.pack()

# 		global qq1
# 		qq1= StringVar()
# 		qq1.set("")

# 		global qq2
# 		qq2= StringVar()
# 		qq2.set("")

# 		global qq3
# 		qq3= StringVar()
# 		qq3.set("")

# 		global qq4
# 		qq4= StringVar()
# 		qq4.set("")

# 		global qq5
# 		qq5= StringVar()
# 		qq5.set("")

# 		global qq6
# 		qq6= StringVar()
# 		qq6.set("")

# 		left_frame = Frame(self)
# 		left_frame.pack(side=LEFT)
# 		right_frame = Frame(self)
# 		right_frame.pack(side=RIGHT)


# 		plots_label = Label(left_frame, text="# of Plots Per Page",font=("Comfortaa", 14))
# 		plots_label.pack()
# 		plots_labelentry_box  = Entry(left_frame, textvariable=qq1, width=15, bg="alice blue")
# 		plots_labelentry_box.pack(padx=(90),pady=(0,20))

# 		row_label = Label(left_frame, text="# of Rows Per Page",font=("Comfortaa", 14))
# 		row_label.pack()
# 		row_label_entry_box  = Entry(left_frame, textvariable=qq2, width=15, bg="alice blue")
# 		row_label_entry_box.pack(padx=(90),pady=(0,20))

# 		column_label = Label(left_frame, text="# of Columns Per Page",font=("Comfortaa", 14))
# 		column_label.pack()
# 		column_label_entry_box  = Entry(left_frame, textvariable=qq3, width=15, bg="alice blue")
# 		column_label_entry_box.pack(padx=(90),pady=(0,20))

# 		width_label = Label(right_frame, text="Width of Plot",font=("Comfortaa", 14))
# 		width_label.pack(padx=(0,100))
# 		width_label_entry_box  = Entry(right_frame, textvariable=qq4, width=15, bg="alice blue")
# 		width_label_entry_box.pack(padx=(0,100),pady=(0,20))

# 		height_label = Label(right_frame, text="Height of Plot",font=("Comfortaa", 14))
# 		height_label.pack(padx=(0,100))
# 		height_label_entry_box  = Entry(right_frame, textvariable=qq5, width=15, bg="alice blue")
# 		height_label_entry_box.pack(padx=(0,100),pady=(0,20))

# 		resolution_label = Label(right_frame, text="Resolution of Plot",font=("Comfortaa", 14))
# 		resolution_label.pack(padx=(0,100))
# 		resolution_label_entry_box  = Entry(right_frame, textvariable=qq6, width=15, bg="alice blue")
# 		resolution_label_entry_box.pack(padx=(0,100),pady=(0,20))

# #Bar Plot Program
# class BarPlotProgram(Frame):
# 	def __init__(self,parent, controller):
# 		Frame.__init__(self,parent)

# 		label = Label(self, text="Bar Plot Program")
# 		label.config(width=200)
# 		label.config(font=("Comfortaa", 20))
# 		label.pack(pady=(10,40))
# 		label.config(fg="cyan4")


	
# 		bar_plot_button = Button(self,text="Generate Bar Plot", command= barplot)
# 		bar_plot_button.pack(pady=(0,20))

# 		back = Button(self,text="Back", command= lambda:controller.show_frame(VisualizeData))
# 		back.pack()

# 		global bp1
# 		bp1= StringVar()
# 		bp1.set("")

# 		global bp2
# 		bp2= StringVar()
# 		bp2.set("")

# 		global bp3
# 		bp3= StringVar()
# 		bp3.set("")

# 		global bp4
# 		bp4= StringVar()
# 		bp4.set("")

# 		global bp5
# 		bp5= StringVar()
# 		bp5.set("")

# 		global bp6
# 		bp6= StringVar()
# 		bp6.set("")

# 		left_frame = Frame(self)
# 		left_frame.pack(side=LEFT)
# 		right_frame = Frame(self)
# 		right_frame.pack(side=RIGHT)


# 		plots_label = Label(left_frame, text="# of Plots Per Page",font=("Comfortaa", 14))
# 		plots_label.pack()
# 		plots_labelentry_box  = Entry(left_frame, textvariable=bp1, width=15, bg="alice blue")
# 		plots_labelentry_box.pack(padx=(90),pady=(0,20))

# 		row_label = Label(left_frame, text="# of Rows Per Page",font=("Comfortaa", 14))
# 		row_label.pack()
# 		row_label_entry_box  = Entry(left_frame, textvariable=bp2, width=15, bg="alice blue")
# 		row_label_entry_box.pack(padx=(90),pady=(0,20))

# 		column_label = Label(left_frame, text="# of Columns Per Page",font=("Comfortaa", 14))
# 		column_label.pack()
# 		column_label_entry_box  = Entry(left_frame, textvariable=bp3, width=15, bg="alice blue")
# 		column_label_entry_box.pack(padx=(90),pady=(0,20))

# 		width_label = Label(right_frame, text="Width of Plot",font=("Comfortaa", 14))
# 		width_label.pack(padx=(0,100))
# 		width_label_entry_box  = Entry(right_frame, textvariable=bp4, width=15, bg="alice blue")
# 		width_label_entry_box.pack(padx=(0,100),pady=(0,20))

# 		height_label = Label(right_frame, text="Height of Plot",font=("Comfortaa", 14))
# 		height_label.pack(padx=(0,100))
# 		height_label_entry_box  = Entry(right_frame, textvariable=bp5, width=15, bg="alice blue")
# 		height_label_entry_box.pack(padx=(0,100),pady=(0,20))

# 		resolution_label = Label(right_frame, text="Resolution of Plot",font=("Comfortaa", 14))
# 		resolution_label.pack(padx=(0,100))
# 		resolution_label_entry_box  = Entry(right_frame, textvariable=bp6, width=15, bg="alice blue")
# 		resolution_label_entry_box.pack(padx=(0,100),pady=(0,20))


# ##1
# class ContinuousProgram(Frame):
# 	def __init__(self,parent, controller):
# 		Frame.__init__(self,parent)

# 		label = Label(self, text="Get Continuous Variables")
# 		label.config(width=200)
# 		label.config(font=("Comfortaa", 20))
# 		label.pack(pady=(10,40))
# 		label.config(fg="cyan4")



# 		global continuous_var
# 		continuous_var= StringVar()
# 		continuous_var.set("")

# 		continuous_label = Label(self, text="Minimum # of Unique Values to be Considered Continuous",font=("Comfortaa", 14))
# 		continuous_label.pack()
# 		continuous_label_entry_box  = Entry(self, textvariable=continuous_var, width=15, bg="alice blue")
# 		continuous_label_entry_box.pack(pady=(0,20))


# 		get_continuous_button = Button(self,text="Get Continuous Variables", command= get_continuous)
# 		get_continuous_button.pack(pady=(0,20))

# 		back = Button(self,text="Back", command= lambda:controller.show_frame(FilterData))
# 		back.pack()

# class CategoricalProgram(Frame):
# 	def __init__(self,parent, controller):
# 		Frame.__init__(self,parent)

# 		label = Label(self, text="Get Catrgorical Variables")
# 		label.config(width=200)
# 		label.config(font=("Comfortaa", 20))
# 		label.pack(pady=(10,40))
# 		label.config(fg="cyan4")



# 		global categorical1
# 		categorical1= StringVar()
# 		categorical1.set("")
# 		categorical1_label = Label(self, text="Min # of Values to be Considered Catrgorical",font=("Comfortaa", 14))
# 		categorical1_label.pack()
# 		categorical1_label_entry_box  = Entry(self, textvariable=categorical1, width=15, bg="alice blue")
# 		categorical1_label_entry_box.pack(pady=(0,20))


# 		global categorical2
# 		categorical2= StringVar()
# 		categorical2.set("")
# 		categorical2_label = Label(self, text="Max # of Values to be Considered Catrgorical",font=("Comfortaa", 14))
# 		categorical2_label.pack()
# 		categorical2_label_entry_box  = Entry(self, textvariable=categorical2, width=15, bg="alice blue")
# 		categorical2_label_entry_box.pack(pady=(0,20))


# 		get_categorical_button = Button(self,text="Get Categorical Variables", command= get_categorical)
# 		get_categorical_button.pack(pady=(0,20))

# 		back = Button(self,text="Back", command= lambda:controller.show_frame(FilterData))
# 		back.pack()

# class AmbiguousProgram(Frame):
# 	def __init__(self,parent, controller):
# 		Frame.__init__(self,parent)

# 		label = Label(self, text="Identify Ambiguous Variables",font=("Comfortaa", 14))
# 		label.config(width=200)
# 		label.config(font=("Comfortaa", 20))
# 		label.pack(pady=(10,40))
# 		label.config(fg="cyan4")


# 		global ambiguous1
# 		ambiguous1= StringVar()
# 		ambiguous1.set("")
# 		ambiguous1_label = Label(self, text="Min # of Levels Desired",font=("Comfortaa", 14))
# 		ambiguous1_label.pack()
# 		ambiguous1_label_entry_box  = Entry(self, textvariable=ambiguous1, width=15, bg="alice blue")
# 		ambiguous1_label_entry_box.pack(pady=(0,20))


# 		global ambiguous2
# 		ambiguous2= StringVar()
# 		ambiguous2.set("")
# 		ambiguous2_label = Label(self, text="Max # of Levels Desired",font=("Comfortaa", 14))
# 		ambiguous2_label.pack()
# 		ambiguous2_label_entry_box  = Entry(self, textvariable=ambiguous2, width=15, bg="alice blue")
# 		ambiguous2_label_entry_box.pack(pady=(0,20))


# 		get_check_button = Button(self,text="Check Ambiguous Variables", command= get_check)
# 		get_check_button.pack(pady=(0,20))

# 		back = Button(self,text="Back", command= lambda:controller.show_frame(FilterData))
# 		back.pack()

# ##1
# class SampleKeepProgram(Frame):
# 	def __init__(self,parent, controller):
# 		Frame.__init__(self,parent)

# 		label = Label(self, text="Keep Varibles with n Samples")
# 		label.config(width=200)
# 		label.config(font=("Comfortaa", 20))
# 		label.pack(pady=(10,40))
# 		label.config(fg="cyan4")


# 		global min_n_var
# 		min_n_var= StringVar()
# 		min_n_var.set("")

# 		min_n_var_label = Label(self, text="Min # of Samples per Variable",font=("Comfortaa", 14))
# 		min_n_var_label.pack()
# 		min_n_var_label_entry_box  = Entry(self, textvariable=min_n_var, width=15, bg="alice blue")
# 		min_n_var_label_entry_box.pack(pady=(0,20))

# 		get_check_button = Button(self,text="Keep N Samples", command= min_n)
# 		get_check_button.pack(pady=(0,40))


# 		back = Button(self,text="Back", command= lambda:controller.show_frame(FilterData))
# 		back.pack()


# class FindOutliersProgram(Frame):
# 	def __init__(self,parent, controller):
# 		Frame.__init__(self,parent)

# 		label = Label(self, text="Find Outliers")
# 		label.config(width=200)
# 		label.config(font=("Comfortaa", 20))
# 		label.pack(pady=(10,40))
# 		label.config(fg="cyan4")


# 		global find_outliers_var
# 		find_outliers_var= StringVar()
# 		find_outliers_var.set("")

# 		find_outliers_var_label = Label(self, text="Number of Standard Deviations From Mean",font=("Comfortaa", 14))
# 		find_outliers_var_label.pack()
# 		find_outliers_var_label_entry_box  = Entry(self, textvariable=find_outliers_var, width=15, bg="alice blue")
# 		find_outliers_var_label_entry_box.pack(pady=(0,20))


# 		get_check_button = Button(self,text="Find Outliers", command= outliers)
# 		get_check_button.pack(pady=(0,40))


# 		back = Button(self,text="Back", command= lambda:controller.show_frame(SummarizeData))
# 		back.pack()

# class FindCorrelationsProgram(Frame):
# 	def __init__(self,parent, controller):
# 		Frame.__init__(self,parent)

# 		label = Label(self, text="Correlations")
# 		label.config(width=200)
# 		label.config(font=("Comfortaa", 20))
# 		label.pack(pady=(10,40))
# 		label.config(fg="cyan4")


# 		global find_correlations_var
# 		find_correlations_var= StringVar()
# 		find_correlations_var.set("")

# 		find_correlations_var_label = Label(self, text="Correlation Threshold",font=("Comfortaa", 14))
# 		find_correlations_var_label.pack()
# 		find_correlations_var_label_entry_box  = Entry(self, textvariable=find_correlations_var, width=15, bg="alice blue")
# 		find_correlations_var_label_entry_box.pack(pady=(0,20))



# 		get_check_button = Button(self,text="Correlations", command= correlations)
# 		get_check_button.pack(pady=(0,40))


# 		back = Button(self,text="Back", command= lambda:controller.show_frame(SummarizeData))
# 		back.pack()
# class MainMenu:
# 	def __init__(self, master):
# 		menubar = Menu(master)
# 		filemenu = Menu(menubar, tearoff=0)
# 		filemenu.add_command(label="Exit", command=master.quit)
# 		menubar.add_cascade(label="File", menu=filemenu)
# 		master.config(menu=menubar)


	
##To call the GUI Application
# app = App()
# app.title('DataCleaner')
# app.geometry("725x500")
# app.mainloop()







