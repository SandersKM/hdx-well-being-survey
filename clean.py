# -*- coding: utf-8 -*-
"""
Created on Sat Apr 21 17:11:22 2018

@author: kates
"""
filenames = ["disability", "first_gen", "gender", "international", "major", 
             "race1", "race2", "religion", "SES", "sexuality1", "sexuality2", 
             "trans", "transfer", "year"]
for f in filenames:
    data = open("C:/Users/kates/Desktop/HWBI/Subgroup_Comparisons/"+ f + ".txt")
    newdata = open("C:/Users/kates/Desktop/HWBI/Subgroup_Comparisons/"+ f + ".csv", "w")
    lines = data.readlines()
    new = ""
    for i in lines[1:]:
        n = i.split(",")[1:]
        new += ",".join(n)
        print(new)
    newdata.write(new)
    data.close()
    newdata.close()