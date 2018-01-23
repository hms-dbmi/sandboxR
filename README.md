
# Using sandboxR package to explore and sort phenotypics data from dbGap

## Introduction
### Load the package


```R
#install.packages("devtools", repos = "https://cran.rstudio.com/")
library("devtools")
install_github("hms-dbmi/sanboxR", force = TRUE)
library(sandboxR)
```

### Get the list of the function for this new package


```R
lsf.str("package:sandboxR")
```

## 1. Explore dbGap studies
### Let's try to explore the "Jackson Heart Study" cohort that exists on dbGap.
##### The dbGap search engine can be tricky, that's why we created the function "browse.dbgap", who helps you find the studies related to the term that you search on your web browser.
Note that if you run this function in a juopyterhub environment, it will return a url since jupyterhub doesn't have access to your local browser.


```R
search.dbgap("Jackson")
```

##### dbGap returns the list of the studies related to your term. As you see, there are 6 studies associated with the "Jackson Heart Study" (JHS). One of these study is the main one aka the "parent study", whereas the other ones are substudies. In this case, phs000286.v5.p1 is the parent study. Firslty, we can use the phs.version() function in order to be sure that this is the latest version of the study. We can abbreviate the phs name by giving just the digit, or we can use the full dbGap id.


```R
phs.version("286")
```

##### The is.parent() function is usefull to test if a study is a parent study or a substudy


```R
is.parent("000286") # JHS main cohort
is.parent("phs499") # substudy "CARe" for JHS
```

#### If you don't know the parent study of a substudy, try parent.study()


```R
parent.study("phs000499")
```

##### On the other side, use sub.study() to get the name and IDs of the substudies from a parent one


```R
sub.study("286") # note here that the substudy "TOPMed" is missing because it has not been fully integrated yet
```

##### If you want to get the name of a study from its dbGap id, use study.name()


```R
study.name("286")
```

##### Finally, you can watch your study on dbGap with browse.dbgap(). If a website exists for this study, you can browse it using browse.study()


```R
browse.dbgap("286")
browse.study("286")
```

## 2. Explore the characteristics of your study
##### For each dbGap study, there can be multiple consent groups that will have there specificities. Use consent.groups to know the number and the name of the consent groups in the study that you are exploring. Let's keep focusing on JHS.


```R
JHS <- "phs000286"
consent.groups(JHS)
```

##### Use n.pop() to know the number of patient included in each groups.


```R
n.pop(JHS)
n.pop(JHS, consentgroups = FALSE)
```

##### Use n.tables() and n.variables() to get the number of datatables in your study and the total number of variables (n.variables may takes a while as it goes into the study files to count the actual number of variables).


```R
n.tables(JHS)
n.variables(JHS)
```

#### datatables.dict() will return a data frame with the datatables IDs (phtxxxxxx) and description of your study (may takes a while).


```R
tablesdict <- datatables.dict(JHS)
head(tablesdict)
```

#### variables.dict() will return a data frame with the variables IDs (phvxxxxxx), their name in the study, the datatable where they come from and their description (may takes even more time).


```R
vardict <- variables.dict(JHS)
head(vardict)
```

Now that we have explore our datasets, let's use sandboxR in order to clean our variables, and to gather them into a tree that will be easier to use for researchers. Note that for chapter 3, we will need to move and create a lot of files on your environment. It will be easier to use on your local computer than in the Jupyterhub environment.

## 3. Build your sandboxR tree
### 3.1. Export your data from dbGap
In order to get your data from dbGap, you will need to request an access and to get a decryption key. This has to be done here: https://dbgap.ncbi.nlm.nih.gov/aa/wga.cgi?login=&page=login
### 3.2. Decrypt your files
We found that the decryption system from dbGap can be tricky. We created dbgap.decrypt() in order to easily decrypt the files that you have downloaded. Note that the "files" argument can be a file or a folder containing multiple encrypted files. Also, this function works only for Mac OS at this moment.


```R
key <- "path/to/your/key.ngc"
files <- "path/to/the/files/you/want/to/decrypt.ncbi_enc"
dbgap.decrypt(file, key)
```


### 3.3 Create your first sandbox map
Once the dbgap files decrypted, you will have one folder per consent groups containing one file per datatable. The goal of this function is to create a folder with a "0_map.csv" file who will map all your variables, and a subfolder "study_tree" containing one .csv file per variable in your study, gathered by datatables.


```R
cg <- c("path/to/the/first/folder/containing/a/consent/group", "second/folder", ...)
destination <- "path/were/your/tree/will/be/located"
sandbox(JHS, cg, destination)
```


### 3.4.a Modify your tree
Once your first tree has been created,  you can easily modify the arrangement of your variables by creating new subdirectories, and by dragging and dropping your variable files. You can also change the name of your directories and variables. Be careful not to delete a variable file in this process.

Then, use the function TreeToMap() in order to reflect your modifications in your "0_map.csv" file.

```R
path <- "Pathway/to/the/folder/where/the/map/and/the/tree/are/located"
TreeToMap(path)
```


### 3.4.b Modify your map
Similarly, you can modify the name of your files directly in the "0_map.csv" files. Modify the 5th column "data_label" to change the name of your variables. Use then the MapToTree() function in order to reflect your modifications in your tree.


```R
MapToTree(path)
```


##### Be careful to apply your changes with MapToTree or TreeToMap BEFORE switching from 3.4.a to 3.4.b and from 3.4.b to 3.4.a
## 3.5 Fixing and repairing issues
Each time you will use TreeToMap(), the old map will be saved with a time stamp in a hidden folder called ".oldmaps". Use the function list.oldmaps() in order to list your previous maps. Use look.oldmaps() in order to view one of these maps as a data.frame. Use recover.map() to change your tree and your "0_map.csv" according to one of your previous maps.


```R
list.oldmaps(path)
look.oldmaps(path, "olmap YYYY-MM-DD HHMM AM")
recover.map(path, "olmap YYYY-MM-DD HHMM AM")
```
