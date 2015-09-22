import os
import re
import sys

report_type = sys.argv[1]

zip_location = "web_dump/"
out_file = "NY_NJ-" + report_type + "_weather.csv"
zip_files = os.listdir(zip_location)
num_files = len(zip_files)

counter = 1
for f in zip_files:
    d_str = re.findall("\d+",f)[0]
    d_type = d_str + report_type + ".txt"
    in_file = zip_location + f
    cmd = " ".join(["unzip", "-p", in_file, d_type, "|grep","-f","NY_NJ-WBAN_codes.txt",">>",out_file])
    print "Processing:" + str(counter) + "/" + str(num_files)
    os.system(cmd)
    counter += 1

