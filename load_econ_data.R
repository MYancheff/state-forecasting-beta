folder = "state-legislative-data/Economic Data/"
filename = "Nevada Annual Real Median Househod Income Data 1984-2017 BEA code MEHOINUSNVA672N.csv"
path = paste(folder, filename,sep="")
data = read_csv(path)
