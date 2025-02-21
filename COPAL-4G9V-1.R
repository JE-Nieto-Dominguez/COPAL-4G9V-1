#Cow Pairing Algorithm for up to Size 4 Groups and 9 Modifiable Variables. Version 1. 
#(COPAL-4G9V-1)

#Credits ----------------------------------------------------------------------

#This code was written on February 2025, by José Eduardo Nieto Domínguez 
#(https://orcid.org/0009-0003-9136-1839))
#
#If you require any assistance in running it, or have any questions,
#please email me at: "nietodominguez.je@gmail.com"
#
#I will be happy to help, in spanish or english!
#
#P.S. Enjoy this ASCII Elegant Penguin!
#     __
#   _|_|_      Best regards!
#   ('v')             -JEND.
#  //-x-\\
#  (\_=_/)
#   ^^ ^^

#Working Directory ------------------------------------------------------------

#NOTE: Please update line 26 with the Working Directory you are going to use.
#For personal replicability, I am leaving my own local working directory here.

setwd("C:/Users/nieto/OneDrive/Documents/Investigación/Análisis de datos/Algoritmo_Copal")

#Libraries for this R Project -------------------------------------------------

#This project uses the tidyverse and openxlsx libraries.
#If you don't have them installed on your computer, before doing anything else,
#please remove the "#" character on the following lines of code and run them:
#install.packages("tidyverse")
#install.packages("openxlsx")

#I am leaving the installation line commented, since I already have them.

library(tidyverse) #Used extensively in this code.
library(openxlsx)  #Used to save the resulting data frames as
                   #Excel files (xlsx).
                   #This was asked by my colleague.

#Variables modifiable at the user's discretion --------------------------------

#PLEASE READ THE NOTES IN THIS SECTION BEFORE CHANGING ANYTHING. 

#NOTE1: This code uses 9 placeholder variable names -- 4 of them are also used
#as filters --, plus an ID one. They are explained further below. All 8 of the
#placeholder ones are expected to be modified, when necessary, to reflect 
#the actual variables you are using in your current experiment(s).

#NOTE2: For this code to work, YOU MUST HAVE all 10 variables in your original 
#csv file (see the "Main data frame" section below, for more information).

#NOTE3: To correctly modify and replace these variables along the entire code,
#You can press Ctrl + F to open the Find and Replace menu in RStudio.
#It is a small bar that, once you press Ctrl + F, appears above this code. 
#Once you have this bar open, simply use it to change all the 

#These variables are mentioned 150+ times along the entire code so, PLEASE,
#make sure you have replaced your variables correctly.

#The variables are:

# ID = A combination of numbers and / or letters used to identify a single cow.
# Variable_A = Used to calculate means, standard deviations and coefficients of variance.
# Variable_B = Used for the Same purpose as Variable_A.
# Variable_C = Used for the Same purpose as Variable_A.
# Variable_D = Used for the same purpose as Variable_A.
# Variable_E = Used for the same purpose as Variable_A.
# FilterMinMax_O = A special variable that is used as a filter in this code.
                  #It has both lower and upper limits accepted.
# FilterMinMax_P = Used for the same purpose as FilterMinMax_P.
# FilterEquals_Y = A special variable that is used as a filter in this code.
                 #The code will try and pair the cows that share 
                 #the same values between them.
# FilterEquals_Z = Used for the same purpose as FilterEquals_Y.

#The variable names are written exactly as they appear in this code.
#If you have correctly replaced them (see NOTE3), then their names here
#should be changed to appear as the new names you decided to use.

#Values modifiable at the user's discretion -----------------------------------

#Changing these values will directly effect the result of this code.
#You are free to change them.

Filter_FilterMinMax_O_LowerLimit <- 1 #The lowest FilterMinMax_O value you wish to admit.
Filter_FilterMinMax_O_UpperLimit <- 1 #The highest FilterMinMax_O value you wish to admit.

Filter_FilterMinMax_P_LowerLimit <- 1 #The lowest FilterMinMax_P value you wish to admit.
Filter_FilterMinMax_P_UpperLimit <- 1 #The highest FilterMinMax_P value you wish to admit.

PairingsChosen <- 12 #How many total pairings you wish to have at the end.
                     #Please consider the number has to be less than, 
                     #the total of observations contained in DF_TotalPairings.
                     #If this condition is not met, the code will not work.
                     #It is adviced to run the code a few times until you find
                     #the number that works for you.
                     
                     #For this specific example, 12 PairingsChosen works just fine.

MaxCV <- 20 #The highest Coefficient of Variation you wish to admit for 
            #the variables on your pairings.

GroupSizeWanted <- 2 #The size of the groups you want COPAL-UTS4G-V1 to make.
                     #OPTIONS: 
                     #2 (for groups with COW1 + COW2)
                     #3 (for groups with COW1 + COW2 + COW3)
                     #4 (for groups with COW1 + COW2 + COW3 + COW4)

                     #NOTE: This code only works with those three options.
                     #If you save any other number, it will not work.

#Main data frame --------------------------------------------------------------

#NOTE1: Please make sure you have the following csv file on your working directory.
#If you don't, this code will not work.

DF_Main <- read.csv("Random-EXP-H0125_JEND_EDIT_EXAMPLE.csv")

#If you wish to run this code using a different file, please make sure it has
#the same column names as this one. If it does not have it, the code will not work.

#NOTE2: Per the "Variables modifiable at the user's discretion" section, your
#csv file MUST have 9 variables with the names used in this code. Those you
#rename (see that section for more details), must be written EXACTLY the same
#in both the csv file and this code.

#NOTE3: In case you do not rename a variable (i.e. you keep the original name,
#like "FilterMinMax_O", or "Variable_A"), they MUST be present in your csv file
#with the placeholder name (again: like "FilterMinMax_O", or "Variable_A").
#In order to keep them from influencing the other variables, 
#have 1's as the value in every row / observation for each unused variable.
#If that isn't the case, the code will not work.

#I am aware that changing names and creating extra columns with 1's in them
#can be seen as extra work, or even extra hassle. However, I think this
#makes the code usable in a lot of cases.

#The alternative was to have a very specific code, bespoke for a single
#experiment. We chose not following that path, hoping for the most re usability
#of the code.

#Applying filters to FilterMinMax_O values on the main data frame ------------------------

#The filter's values can be changed on the "Values modifiable at the user's 
#discretion" section.

DF_1stFilter <- DF_Main |>                 
  filter(
    FilterMinMax_O >= Filter_FilterMinMax_O_LowerLimit & 
    FilterMinMax_O <= Filter_FilterMinMax_O_UpperLimit &
    FilterMinMax_P >= Filter_FilterMinMax_P_LowerLimit & 
    FilterMinMax_P <= Filter_FilterMinMax_P_UpperLimit
  )

#Final data frame template ------------------------------------------------------

#NOTE: I chose to create this data frame here, so the rest of the code can use it.
#Its original form has all the columns we need as a final product, plus
#a single row with lots of zeros. The final form, after the code has finished
#running, will have the original row removed and, instead of it, all the pairings
#signaled in "PairingsChosen" will be there.

#This template will change Depending on the value saved as GroupSizeWanted.
#(for more details, please go to the 
#"Values modifiable at the user's discretion" section).

if (GroupSizeWanted == 2) { 
  
  #Create a a template for COW1 + COW2
  DF_TotalPairings <- data.frame(
    COW1 = 0,
    COW2 = 0,
    FilterMinMax_O_1 = 0,
    FilterMinMax_O_2 = 0,
    MN_FilterMinMax_O = 0,
    SD_FilterMinMax_O = 0,
    CV_FilterMinMax_O = 0,
    Variable_A_1 = 0,
    Variable_A_2 = 0,
    MN_Variable_A = 0,
    SD_Variable_A = 0,
    CV_Variable_A = 0,
    FilterEquals_Y_1 = 0,
    FilterEquals_Y_2 = 0,
    MN_FilterEquals_Y = 0,
    SD_FilterEquals_Y = 0,
    CV_FilterEquals_Y = 0,
    Variable_B_1 = 0,
    Variable_B_2 = 0,
    MN_Variable_B = 0,
    SD_Variable_B = 0,
    CV_Variable_B = 0,
    Variable_C_1 = 0,
    Variable_C_2 = 0,
    MN_Variable_C = 0,
    SD_Variable_C = 0,
    CV_Variable_C = 0,
    FilterMinMax_P_1 = 0,
    FilterMinMax_P_2 = 0,
    MN_FilterMinMax_P = 0,
    SD_FilterMinMax_P = 0,
    CV_FilterMinMax_P = 0,
    FilterEquals_Z_1 = 0,
    FilterEquals_Z_2 = 0,
    MN_FilterEquals_Z = 0,
    SD_FilterEquals_Z = 0,
    CV_FilterEquals_Z = 0,
    Variable_D_1 = 0,
    Variable_D_2 = 0,
    MN_Variable_D = 0,
    SD_Variable_D = 0,
    CV_Variable_D = 0,
    Variable_E_1 = 0,
    Variable_E_2 = 0,
    MN_Variable_E = 0,
    SD_Variable_E = 0,
    CV_Variable_E = 0,
    CV_TOTAL = 0
  )
} else if (GroupSizeWanted == 3) { 
  
  #Create a a template for COW1 + COW2 + COW3
  DF_TotalPairings <- data.frame(
    COW1 = 0,
    COW2 = 0,
    COW3 = 0,
    FilterMinMax_O_1 = 0,
    FilterMinMax_O_2 = 0,
    FilterMinMax_O_3 = 0,
    MN_FilterMinMax_O = 0,
    SD_FilterMinMax_O = 0,
    CV_FilterMinMax_O = 0,
    Variable_A_1 = 0,
    Variable_A_2 = 0,
    Variable_A_3 = 0,
    MN_Variable_A = 0,
    SD_Variable_A = 0,
    CV_Variable_A = 0,
    FilterEquals_Y_1 = 0,
    FilterEquals_Y_2 = 0,
    FilterEquals_Y_3 = 0,
    MN_FilterEquals_Y = 0,
    SD_FilterEquals_Y = 0,
    CV_FilterEquals_Y = 0,
    Variable_B_1 = 0,
    Variable_B_2 = 0,
    Variable_B_3 = 0,
    MN_Variable_B = 0,
    SD_Variable_B = 0,
    CV_Variable_B = 0,
    Variable_C_1 = 0,
    Variable_C_2 = 0,
    Variable_C_3 = 0,
    MN_Variable_C = 0,
    SD_Variable_C = 0,
    CV_Variable_C = 0,
    FilterMinMax_P_1 = 0,
    FilterMinMax_P_2 = 0,
    FilterMinMax_P_3 = 0,
    MN_FilterMinMax_P = 0,
    SD_FilterMinMax_P = 0,
    CV_FilterMinMax_P = 0,
    FilterEquals_Z_1 = 0,
    FilterEquals_Z_2 = 0,
    FilterEquals_Z_3 = 0,
    MN_FilterEquals_Z = 0,
    SD_FilterEquals_Z = 0,
    CV_FilterEquals_Z = 0,
    Variable_D_1 = 0,
    Variable_D_2 = 0,
    Variable_D_3 = 0,
    MN_Variable_D = 0,
    SD_Variable_D = 0,
    CV_Variable_D = 0,
    Variable_E_1 = 0,
    Variable_E_2 = 0,
    Variable_E_3 = 0,
    MN_Variable_E = 0,
    SD_Variable_E = 0,
    CV_Variable_E = 0,
    CV_TOTAL = 0
  )
} else if (GroupSizeWanted == 4) { 
  
  #Create a a template for COW1 + COW2 + COW3 + COW4
  DF_TotalPairings <- data.frame(
    COW1 = 0,
    COW2 = 0,
    COW3 = 0,
    COW4 = 0, 
    FilterMinMax_O_1 = 0,
    FilterMinMax_O_2 = 0,
    FilterMinMax_O_3 = 0,
    FilterMinMax_O_4 = 0,
    MN_FilterMinMax_O = 0,
    SD_FilterMinMax_O = 0,
    CV_FilterMinMax_O = 0,
    Variable_A_1 = 0,
    Variable_A_2 = 0,
    Variable_A_3 = 0,
    Variable_A_4 = 0,
    MN_Variable_A = 0,
    SD_Variable_A = 0,
    CV_Variable_A = 0,
    FilterEquals_Y_1 = 0,
    FilterEquals_Y_2 = 0,
    FilterEquals_Y_3 = 0,
    FilterEquals_Y_4 = 0,
    MN_FilterEquals_Y = 0,
    SD_FilterEquals_Y = 0,
    CV_FilterEquals_Y = 0,
    Variable_B_1 = 0,
    Variable_B_2 = 0,
    Variable_B_3 = 0,
    Variable_B_4 = 0,
    MN_Variable_B = 0,
    SD_Variable_B = 0,
    CV_Variable_B = 0,
    Variable_C_1 = 0,
    Variable_C_2 = 0,
    Variable_C_3 = 0,
    Variable_C_4 = 0,
    MN_Variable_C = 0,
    SD_Variable_C = 0,
    CV_Variable_C = 0,
    FilterMinMax_P_1 = 0,
    FilterMinMax_P_2 = 0,
    FilterMinMax_P_3 = 0,
    FilterMinMax_P_4 = 0,
    MN_FilterMinMax_P = 0,
    SD_FilterMinMax_P = 0,
    CV_FilterMinMax_P = 0,
    FilterEquals_Z_1 = 0,
    FilterEquals_Z_2 = 0,
    FilterEquals_Z_3 = 0,
    FilterEquals_Z_4 = 0,
    MN_FilterEquals_Z = 0,
    SD_FilterEquals_Z = 0,
    CV_FilterEquals_Z = 0,
    Variable_D_1 = 0,
    Variable_D_2 = 0,
    Variable_D_3 = 0,
    Variable_D_4 = 0,
    MN_Variable_D = 0,
    SD_Variable_D = 0,
    CV_Variable_D = 0,
    Variable_E_1 = 0,
    Variable_E_2 = 0,
    Variable_E_3 = 0,
    Variable_E_4 = 0,
    MN_Variable_E = 0,
    SD_Variable_E = 0,
    CV_Variable_E = 0,
    CV_TOTAL = 0
  )
} else {
  cat("Error: The value in 'GroupSizeWanted' must be 2, 3 or 4.
      Please go to the 'Values modifiable at the user's discretion' section
      and change 'GroupSizeWanted' to a valid number.\n")
}

#Null Cows templates -----------------------------------------------------------

#NOTE: Only for GroupSizeWanted 3 and 4.

#This template will change Depending on the value saved as GroupSizeWanted.
#(for more details, please go to the 
#"Values modifiable at the user's discretion" section).

if (GroupSizeWanted == 2) { 
  
  cat("Null Cows not needed for this GroupSizeWanted.\n")
  
} else if (GroupSizeWanted == 3) { 
  
  COW3_Null <- data.frame(
    COW3 = 0,
    FilterMinMax_O_3 = 0,
    Variable_A_3 = 0,
    FilterEquals_Y_3 = 0,
    Variable_B_3 = 0,
    Variable_C_3 = 0,
    FilterMinMax_P_3 = 0,
    FilterEquals_Z_3 = 0,
    Variable_D_3 = 0,
    Variable_E_3 = 0
  )
} else if (GroupSizeWanted == 4) {
  
  COW3_Null <- data.frame(
    COW3 = 0,
    FilterMinMax_O_3 = 0,
    Variable_A_3 = 0,
    FilterEquals_Y_3 = 0,
    Variable_B_3 = 0,
    Variable_C_3 = 0,
    FilterMinMax_P_3 = 0,
    FilterEquals_Z_3 = 0,
    Variable_D_3 = 0,
    Variable_E_3 = 0
  )
  
  COW4_Null <- data.frame(
    COW4 = 0,
    FilterMinMax_O_4 = 0,
    Variable_A_4 = 0,
    FilterEquals_Y_4 = 0,
    Variable_B_4 = 0,
    Variable_C_4 = 0,
    FilterMinMax_P_4 = 0,
    FilterEquals_Z_4 = 0,
    Variable_D_4 = 0,
    Variable_E_4 = 0
  )
} else {
  
  DF_1stFilter <- 0
    
  cat("Error: The value in 'GroupSizeWanted' must be 2, 3 or 4.
      Please go to the 'Values modifiable at the user's discretion' section
      and change 'GroupSizeWanted' to a valid number.\n")
}


#Pair the cows in DF_1stFilter using a 40-step loop  ---------------------------

#This is the main part of the code.
#It will loop as long as DF_1stFilter has 2 or more rows.

while (nrow(DF_1stFilter) >= 2) { #THE LOOP'S CODE STARTS HERE -----------------
  
  #STEP 1 - Save the first row in DF_1stFilter------------------------------------
  
  Cow_to_pair <- DF_1stFilter[1, ]
  
  #STEP 2 - Create a data frame to compare Cow_to_pair with the rest of the cows----
  
  DF_Comparisons_COW2 <- DF_1stFilter |> 
    mutate(
      COW1 = Cow_to_pair$ID,
      COW2 = ID,
      FilterMinMax_O_1 = Cow_to_pair$FilterMinMax_O,
      FilterMinMax_O_2 = FilterMinMax_O,
      Variable_A_1 = Cow_to_pair$Variable_A,
      Variable_A_2 = Variable_A,
      FilterEquals_Y_1 = Cow_to_pair$FilterEquals_Y,
      FilterEquals_Y_2 = FilterEquals_Y,
      Variable_B_1 = Cow_to_pair$Variable_B,
      Variable_B_2 = Variable_B,
      Variable_C_1 = Cow_to_pair$Variable_C,
      Variable_C_2 = Variable_C,
      FilterMinMax_P_1 = Cow_to_pair$FilterMinMax_P,
      FilterMinMax_P_2 = FilterMinMax_P,
      FilterEquals_Z_1 = Cow_to_pair$FilterEquals_Z,
      FilterEquals_Z_2 = FilterEquals_Z,
      Variable_D_1 = Cow_to_pair$Variable_D,
      Variable_D_2 = Variable_D,
      Variable_E_1 = Cow_to_pair$Variable_E,
      Variable_E_2 = Variable_E
    )
  
  #STEP 3 - Calculate MN, SD and CV ----------------------------------------------
  
  #MN is a variable's mean
  #SD is a variable's standard deviation
  #CV is a variable's coefficient of variation
  
  DF_Comparisons_COW2 <- DF_Comparisons_COW2 |> 
    mutate(
      #FilterMinMax_O
      MN_FilterMinMax_O = rowMeans(across(c(FilterMinMax_O_1, FilterMinMax_O_2))),
      SD_FilterMinMax_O = apply(across(c(FilterMinMax_O_1, FilterMinMax_O_2)), 1, sd),
      CV_FilterMinMax_O = (SD_FilterMinMax_O / MN_FilterMinMax_O) * 100,
      #Variable_A
      MN_Variable_A = rowMeans(across(c(Variable_A_1, Variable_A_2))),
      SD_Variable_A = apply(across(c(Variable_A_1, Variable_A_2)), 1, sd),
      CV_Variable_A = (SD_Variable_A / MN_Variable_A) * 100,
      #FilterEquals_Y
      MN_FilterEquals_Y = rowMeans(across(c(FilterEquals_Y_1, FilterEquals_Y_2))),
      SD_FilterEquals_Y = apply(across(c(FilterEquals_Y_1, FilterEquals_Y_2)), 1, sd),
      CV_FilterEquals_Y = (SD_FilterEquals_Y / MN_FilterEquals_Y) * 100,
      #Variable_B
      MN_Variable_B = rowMeans(across(c(Variable_B_1, Variable_B_2))),
      SD_Variable_B = apply(across(c(Variable_B_1, Variable_B_2)), 1, sd),
      CV_Variable_B = (SD_Variable_B / MN_Variable_B) * 100,
      #Variable_C
      MN_Variable_C = rowMeans(across(c(Variable_C_1, Variable_C_2))),
      SD_Variable_C = apply(across(c(Variable_C_1, Variable_C_2)), 1, sd),
      CV_Variable_C = (SD_Variable_C / MN_Variable_C) * 100,
      #FilterMinMax_P
      MN_FilterMinMax_P = rowMeans(across(c(FilterMinMax_P_1, FilterMinMax_P_2))),
      SD_FilterMinMax_P = apply(across(c(FilterMinMax_P_1, FilterMinMax_P_2)), 1, sd),
      CV_FilterMinMax_P = (SD_FilterMinMax_P / MN_FilterMinMax_P) * 100,
      #FilterEquals_Z
      MN_FilterEquals_Z = rowMeans(across(c(FilterEquals_Z_1, FilterEquals_Z_2))),
      SD_FilterEquals_Z = apply(across(c(FilterEquals_Z_1, FilterEquals_Z_2)), 1, sd),
      CV_FilterEquals_Z = (SD_FilterEquals_Z / MN_FilterEquals_Z) * 100,
      #Variable_D
      MN_Variable_D = rowMeans(across(c(Variable_D_1, Variable_D_2))),
      SD_Variable_D = apply(across(c(Variable_D_1, Variable_D_2)), 1, sd),
      CV_Variable_D = (SD_Variable_D / MN_Variable_D) * 100,
      #Variable_E
      MN_Variable_E = rowMeans(across(c(Variable_E_1, Variable_E_2))),
      SD_Variable_E = apply(across(c(Variable_E_1, Variable_E_2)), 1, sd),
      CV_Variable_E = (SD_Variable_E / MN_Variable_E) * 100
    )
  
  #STEP 4 - Calculate CV_TOTAL ---------------------------------------------------
  
  #This value will be used to find the ideal pairing for Cow_to_pair
  
  DF_Comparisons_COW2 <- DF_Comparisons_COW2 |> 
    mutate(
      CV_TOTAL = CV_FilterMinMax_O + CV_Variable_A + CV_FilterEquals_Y +
                 CV_Variable_B + CV_Variable_C + CV_FilterMinMax_P +
                 CV_FilterEquals_Z + CV_Variable_D + CV_Variable_E
    )
  
  #STEP 5 - Clean up DF_Comparisons_COW2 -------------------------------------
  
  #We do so by removing the first row, because it represents Cow_to_pair
  #being compared with itself. Thus, it is an invalid pairing.
  
  #We also reorder the data frame.
  
  DF_Comparisons_COW2 <- DF_Comparisons_COW2[-1, ]
  DF_Comparisons_COW2 <- DF_Comparisons_COW2 |> 
    select(
      COW1,
      COW2,
      FilterMinMax_O_1,
      FilterMinMax_O_2,
      MN_FilterMinMax_O,
      SD_FilterMinMax_O,
      CV_FilterMinMax_O,
      Variable_A_1,
      Variable_A_2,
      MN_Variable_A,
      SD_Variable_A,
      CV_Variable_A,
      FilterEquals_Y_1,
      FilterEquals_Y_2,
      MN_FilterEquals_Y,
      SD_FilterEquals_Y,
      CV_FilterEquals_Y,
      Variable_B_1,
      Variable_B_2,
      MN_Variable_B,
      SD_Variable_B,
      CV_Variable_B,
      Variable_C_1,
      Variable_C_2,
      MN_Variable_C,
      SD_Variable_C,
      CV_Variable_C,
      FilterMinMax_P_1,
      FilterMinMax_P_2,
      MN_FilterMinMax_P,
      SD_FilterMinMax_P,
      CV_FilterMinMax_P,
      FilterEquals_Z_1,
      FilterEquals_Z_2,
      MN_FilterEquals_Z,
      SD_FilterEquals_Z,
      CV_FilterEquals_Z,
      Variable_D_1,
      Variable_D_2,
      MN_Variable_D,
      SD_Variable_D,
      CV_Variable_D,
      Variable_E_1,
      Variable_E_2,
      MN_Variable_E,
      SD_Variable_E,
      CV_Variable_E,
      CV_TOTAL
    )
  
  #STEP 6 - Filter variables using the value saved as MaxCV -----
  
  #The filter's values can be changed on the "Values modifiable at the user's 
  #discretion" section.
  #The resulting filtered data frame is saved as DF_2ndFilter
  
  DF_2ndFilter <- DF_Comparisons_COW2 |> 
    filter(
      CV_FilterMinMax_O <= MaxCV &
        CV_Variable_A <= MaxCV &
        CV_FilterEquals_Y <= MaxCV &
        CV_Variable_B <= MaxCV &
        CV_Variable_C <= MaxCV &
        CV_FilterMinMax_P <= MaxCV &
        CV_FilterEquals_Z <= MaxCV &
        CV_Variable_D <= MaxCV &
        CV_Variable_E <= MaxCV
    )
  
  #STEP 7 - Check if DF_2ndFilter has zero rows --------------------------------
  
  if (nrow(DF_2ndFilter) == 0) { 
    
    #Remove the row containing Cow_to_pair from DF_1stFilter and restart loop
    DF_1stFilter <- DF_1stFilter[-1, ]
    
    next 
  }
  
  #STEP 8 - Filter DF_2ndFilter to find any pairings with the same FilterEquals_Y and FilterEquals_Z values ----
  
  #The resulting filtered data frame is saved as DF_3rdFilter
  
  DF_3rdFilter <- DF_2ndFilter |> 
    filter(
      FilterEquals_Y_1 == FilterEquals_Y_2 &
      FilterEquals_Z_1 == FilterEquals_Z_2
    )
  
  #STEP 9 - Count the amount of rows in DF_3rdFilter -----------------------------
  
  #We do this to determine if we have any ideal pairing where both cows have the 
  #same FilterEquals_Y value. This is not always the case, hence STEP 9.
  
  Rows_in_3rdFilter <- nrow(DF_3rdFilter)
  
  #STEP 10 - Choose the most adequate filter for this specific cow pairing --------
  
  if (Rows_in_3rdFilter == 0L) {
    DF_ChosenFilter1 <- DF_2ndFilter #Meaning there are no cows with the same FilterEquals_Y
  } else { 
    DF_ChosenFilter1 <- DF_3rdFilter #Meaning there is at least one other cow with
    #the same FilterEquals_Y as COW1
  }
  
  #Whichever filter is chosen, it will be used to find the most adequate cow to pair
  #with COW1.
  
  #STEP 11 - Determine the lowest CV_TOTAL value in DF_ChosenFilter1 --------------
  
  #The resulting value is saved as min_value_CV_TOTAL1
  min_value_CV_TOTAL1 <- min(DF_ChosenFilter1$CV_TOTAL)
  
  #STEP 12 - Find the most adequate cow to be paired with COW1 -------------------
  
  #We filter DF_ChosenFilter1, using min_value_CV_TOTAL1
  #The resulting row is saved as Cow_1stPair
  
  Cow_1stPair <- DF_ChosenFilter1 |> 
    filter(
      CV_TOTAL == min_value_CV_TOTAL1
    )
  
  
  #STEP 13 - Remove COW1's data from DF_1stFilter --------------------------------
  
  DF_1stFilter <- DF_1stFilter |> 
    filter(
      ID != Cow_1stPair$COW1
    )
  
  #STEP 14 - Remove COW2's data from DF_1stFilter --------------------------------
  
  DF_1stFilter <- DF_1stFilter |> 
    filter(
      ID != Cow_1stPair$COW2
    )
  
  #STEP 15 - Decide course of action based on GroupSizeWanted --------------------
  
  if (GroupSizeWanted == 2) { 
    
    #Then, this is as far as the steps go.
    #Save Cow_1stPair as Cows_Paired.
    Cows_Paired <- Cow_1stPair
  
    #Bind Cows_Paired to DF_TotalPairings.
    #This way, after the loop is completed, DF_TotalPairings will have all 
    #the pairings created using steps 1 to 15.
    DF_TotalPairings <- bind_rows(DF_TotalPairings, Cows_Paired)
    
    #Start the loop again
    next 
    }
  
  #STEP 16 - Create a data frame to compare Cow_1stPair with the other cows ----
  
  DF_Comparisons_COW3 <- DF_1stFilter |> 
    mutate(
      COW1 = Cow_1stPair$COW1,
      COW2 = Cow_1stPair$COW2,
      COW3 = ID,
      FilterMinMax_O_1 = Cow_1stPair$FilterMinMax_O_1,
      FilterMinMax_O_2 = Cow_1stPair$FilterMinMax_O_2,
      FilterMinMax_O_3 = FilterMinMax_O,
      Variable_A_1 = Cow_1stPair$Variable_A_1,
      Variable_A_2 = Cow_1stPair$Variable_A_2,
      Variable_A_3 = Variable_A,
      FilterEquals_Y_1 = Cow_1stPair$FilterEquals_Y_1,
      FilterEquals_Y_2 = Cow_1stPair$FilterEquals_Y_2,
      FilterEquals_Y_3 = FilterEquals_Y,
      Variable_B_1 = Cow_1stPair$Variable_B_1,
      Variable_B_2 = Cow_1stPair$Variable_B_2,
      Variable_B_3 = Variable_B,
      Variable_C_1 = Cow_1stPair$Variable_C_1,
      Variable_C_2 = Cow_1stPair$Variable_C_2,
      Variable_C_3 = Variable_C,
      FilterMinMax_P_1 = Cow_1stPair$FilterMinMax_P_1,
      FilterMinMax_P_2 = Cow_1stPair$FilterMinMax_P_2,
      FilterMinMax_P_3 = FilterMinMax_P,
      FilterEquals_Z_1 = Cow_1stPair$FilterEquals_Z_1,
      FilterEquals_Z_2 = Cow_1stPair$FilterEquals_Z_2,
      FilterEquals_Z_3 = FilterEquals_Z,
      Variable_D_1 = Cow_1stPair$Variable_D_1,
      Variable_D_2 = Cow_1stPair$Variable_D_2,
      Variable_D_3 = Variable_D,
      Variable_E_1 = Cow_1stPair$Variable_E_1,
      Variable_E_2 = Cow_1stPair$Variable_E_2,
      Variable_E_3 = Variable_E
    )
  
  #STEP 17 - Calculate new MN, SD and CV ---------------------------------------
  
  #MN is a variable's mean
  #SD is a variable's standard deviation
  #CV is a variable's coefficient of variation
  
  
  DF_Comparisons_COW3 <- DF_Comparisons_COW3 |> 
    mutate(
      #FilterMinMax_O
      MN_FilterMinMax_O = rowMeans(across(c(FilterMinMax_O_1, FilterMinMax_O_2, FilterMinMax_O_3))),
      SD_FilterMinMax_O = apply(across(c(FilterMinMax_O_1, FilterMinMax_O_2, FilterMinMax_O_3)), 1, sd),
      CV_FilterMinMax_O = (SD_FilterMinMax_O / MN_FilterMinMax_O) * 100,
      #Variable_A
      MN_Variable_A = rowMeans(across(c(Variable_A_1, Variable_A_2, Variable_A_3))),
      SD_Variable_A = apply(across(c(Variable_A_1, Variable_A_2, Variable_A_3)), 1, sd),
      CV_Variable_A = (SD_Variable_A / MN_Variable_A) * 100,
      #FilterEquals_Y
      MN_FilterEquals_Y = rowMeans(across(c(FilterEquals_Y_1, FilterEquals_Y_2, FilterEquals_Y_3))),
      SD_FilterEquals_Y = apply(across(c(FilterEquals_Y_1, FilterEquals_Y_2, FilterEquals_Y_3)), 1, sd),
      CV_FilterEquals_Y = (SD_FilterEquals_Y / MN_FilterEquals_Y) * 100,
      #Variable_B
      MN_Variable_B = rowMeans(across(c(Variable_B_1, Variable_B_2, Variable_B_3))),
      SD_Variable_B = apply(across(c(Variable_B_1, Variable_B_2, Variable_B_3)), 1, sd),
      CV_Variable_B = (SD_Variable_B / MN_Variable_B) * 100,
      #Variable_C
      MN_Variable_C = rowMeans(across(c(Variable_C_1, Variable_C_2, Variable_C_3))),
      SD_Variable_C = apply(across(c(Variable_C_1, Variable_C_2, Variable_C_3)), 1, sd),
      CV_Variable_C = (SD_Variable_C / MN_Variable_C) * 100,
      #FilterMinMax_P
      MN_FilterMinMax_P = rowMeans(across(c(FilterMinMax_P_1, FilterMinMax_P_2, FilterMinMax_P_3))),
      SD_FilterMinMax_P = apply(across(c(FilterMinMax_P_1, FilterMinMax_P_2, FilterMinMax_P_3)), 1, sd),
      CV_FilterMinMax_P = (SD_FilterMinMax_P / MN_FilterMinMax_P) * 100,
      #FilterEquals_Z
      MN_FilterEquals_Z = rowMeans(across(c(FilterEquals_Z_1, FilterEquals_Z_2, FilterEquals_Z_3))),
      SD_FilterEquals_Z = apply(across(c(FilterEquals_Z_1, FilterEquals_Z_2, FilterEquals_Z_3)), 1, sd),
      CV_FilterEquals_Z = (SD_FilterEquals_Z / MN_FilterEquals_Z) * 100,
      #Variable_D
      MN_Variable_D = rowMeans(across(c(Variable_D_1, Variable_D_2, Variable_D_3))),
      SD_Variable_D = apply(across(c(Variable_D_1, Variable_D_2, Variable_D_3)), 1, sd),
      CV_Variable_D = (SD_Variable_D / MN_Variable_D) * 100,
      #Variable_E
      MN_Variable_E = rowMeans(across(c(Variable_E_1, Variable_E_2, Variable_E_3))),
      SD_Variable_E = apply(across(c(Variable_E_1, Variable_E_2, Variable_E_3)), 1, sd),
      CV_Variable_E = (SD_Variable_E / MN_Variable_E) * 100
    )
  
  #STEP 18 - Calculate new CV_TOTAL --------------------------------------------
  
  DF_Comparisons_COW3 <- DF_Comparisons_COW3 |> 
    mutate(
      CV_TOTAL = CV_FilterMinMax_O + CV_Variable_A + CV_FilterEquals_Y +
        CV_Variable_B + CV_Variable_C + CV_FilterMinMax_P +
        CV_FilterEquals_Z + CV_Variable_D + CV_Variable_E
    )
  
  #STEP 19 - Reorder DF_Comparisons_COW3 ---------------------------------------
  
  DF_Comparisons_COW3 <- DF_Comparisons_COW3 |> 
    select(
      COW1,
      COW2,
      COW3,
      FilterMinMax_O_1,
      FilterMinMax_O_2,
      FilterMinMax_O_3,
      MN_FilterMinMax_O,
      SD_FilterMinMax_O,
      CV_FilterMinMax_O,
      Variable_A_1,
      Variable_A_2,
      Variable_A_3,
      MN_Variable_A,
      SD_Variable_A,
      CV_Variable_A,
      FilterEquals_Y_1,
      FilterEquals_Y_2,
      FilterEquals_Y_3,
      MN_FilterEquals_Y,
      SD_FilterEquals_Y,
      CV_FilterEquals_Y,
      Variable_B_1,
      Variable_B_2,
      Variable_B_3,
      MN_Variable_B,
      SD_Variable_B,
      CV_Variable_B,
      Variable_C_1,
      Variable_C_2,
      Variable_C_3,
      MN_Variable_C,
      SD_Variable_C,
      CV_Variable_C,
      FilterMinMax_P_1,
      FilterMinMax_P_2,
      FilterMinMax_P_3,
      MN_FilterMinMax_P,
      SD_FilterMinMax_P,
      CV_FilterMinMax_P,
      FilterEquals_Z_1,
      FilterEquals_Z_2,
      FilterEquals_Z_3,
      MN_FilterEquals_Z,
      SD_FilterEquals_Z,
      CV_FilterEquals_Z,
      Variable_D_1,
      Variable_D_2,
      Variable_D_3,
      MN_Variable_D,
      SD_Variable_D,
      CV_Variable_D,
      Variable_E_1,
      Variable_E_2,
      Variable_E_3,
      MN_Variable_E,
      SD_Variable_E,
      CV_Variable_E,
      CV_TOTAL
    )
  
  #STEP 20 - Filter new CV_FilterMinMax_O, CV_Variable_A and CV_FilterEquals_Y using the value saved as MaxCV -----
  
  #The filter's values can be changed on the "Values modifiable at the user's 
  #discretion" section.
  #The resulting filtered data frame is saved as DF_4thFilter
  
  DF_4thFilter <- DF_Comparisons_COW3 |> 
    filter(
      CV_FilterMinMax_O <= MaxCV &
        CV_Variable_A <= MaxCV &
        CV_FilterEquals_Y <= MaxCV &
        CV_Variable_B <= MaxCV &
        CV_Variable_C <= MaxCV &
        CV_FilterMinMax_P <= MaxCV &
        CV_FilterEquals_Z <= MaxCV &
        CV_Variable_D <= MaxCV &
        CV_Variable_E <= MaxCV
    )
  
  #Check if DF_4thFilter has zero rows and the value in GroupSizeWanted
  
  if (nrow(DF_4thFilter) == 0 & GroupSizeWanted == 3) { 
    
    #Keep the first pair and group them with two null cows
    Cows_Paired <- Cow_1stPair |> 
      mutate(
        COW1 = COW1,
        COW2 = COW2,
        COW3 = COW3_Null$COW3,
        FilterMinMax_O_1 = FilterMinMax_O_1,
        FilterMinMax_O_2 = FilterMinMax_O_2,
        FilterMinMax_O_3 = COW3_Null$FilterMinMax_O_3,
        MN_FilterMinMax_O = MN_FilterMinMax_O,
        SD_FilterMinMax_O = SD_FilterMinMax_O,
        CV_FilterMinMax_O = CV_FilterMinMax_O,
        Variable_A_1 = Variable_A_1,
        Variable_A_2 = Variable_A_2,
        Variable_A_3 = COW3_Null$Variable_A_3,
        MN_Variable_A = MN_Variable_A,
        SD_Variable_A = SD_Variable_A,
        CV_Variable_A = CV_Variable_A,
        FilterEquals_Y_1 = FilterEquals_Y_1,
        FilterEquals_Y_2 = FilterEquals_Y_2,
        FilterEquals_Y_3 = COW3_Null$FilterEquals_Y_3,
        MN_FilterEquals_Y = MN_FilterEquals_Y,
        SD_FilterEquals_Y = SD_FilterEquals_Y,
        CV_FilterEquals_Y = CV_FilterEquals_Y,
        Variable_B_1 = Variable_B_1,
        Variable_B_2 = Variable_B_2,
        Variable_B_3 = COW3_Null$Variable_B_3,
        MN_Variable_B = MN_Variable_B,
        SD_Variable_B = SD_Variable_B,
        CV_Variable_B = CV_Variable_B,
        Variable_C_1 = Variable_C_1,
        Variable_C_2 = Variable_C_2,
        Variable_C_3 = COW3_Null$Variable_C_3,
        MN_Variable_C = MN_Variable_C,
        SD_Variable_C = SD_Variable_C,
        CV_Variable_C = CV_Variable_C,
        FilterMinMax_P_1 = FilterMinMax_P_1,
        FilterMinMax_P_2 = FilterMinMax_P_2,
        FilterMinMax_P_3 = COW3_Null$FilterMinMax_P_3,
        MN_FilterMinMax_P = MN_FilterMinMax_P,
        SD_FilterMinMax_P = SD_FilterMinMax_P,
        CV_FilterMinMax_P = CV_FilterMinMax_P,
        FilterEquals_Z_1 = FilterEquals_Z_1,
        FilterEquals_Z_2 = FilterEquals_Z_2,
        FilterEquals_Z_3 = COW3_Null$FilterEquals_Z_3,
        MN_FilterEquals_Z = MN_FilterEquals_Z,
        SD_FilterEquals_Z = SD_FilterEquals_Z,
        CV_FilterEquals_Z = CV_FilterEquals_Z,
        Variable_D_1 = Variable_D_1,
        Variable_D_2 = Variable_D_2,
        Variable_D_3 = COW3_Null$Variable_D_3,
        MN_Variable_D = MN_Variable_D,
        SD_Variable_D = SD_Variable_D,
        CV_Variable_D = CV_Variable_D,
        Variable_E_1 = Variable_E_1,
        Variable_E_2 = Variable_E_2,
        Variable_E_3 = COW3_Null$Variable_E_3,
        MN_Variable_E = MN_Variable_E,
        SD_Variable_E = SD_Variable_E,
        CV_Variable_E = CV_Variable_E,
        CV_TOTAL = CV_TOTAL
      )
    
    #Bind Cows_Paired to DF_TotalPairings
    DF_TotalPairings <- bind_rows(DF_TotalPairings, Cows_Paired)
    
    #and restart loop
    next 
  } 
  
  if (nrow(DF_4thFilter) == 0 & GroupSizeWanted == 4) { 
    
    #Keep the first pair and group them with two null cows
    Cows_Paired <- Cow_1stPair |> 
      mutate(
        COW1 = COW1,
        COW2 = COW2,
        COW3 = COW3_Null$COW3,
        COW4 = COW4_Null$COW4,
        FilterMinMax_O_1 = FilterMinMax_O_1,
        FilterMinMax_O_2 = FilterMinMax_O_2,
        FilterMinMax_O_3 = COW3_Null$FilterMinMax_O_3,
        FilterMinMax_O_4 = COW4_Null$FilterMinMax_O_4,
        MN_FilterMinMax_O = MN_FilterMinMax_O,
        SD_FilterMinMax_O = SD_FilterMinMax_O,
        CV_FilterMinMax_O = CV_FilterMinMax_O,
        Variable_A_1 = Variable_A_1,
        Variable_A_2 = Variable_A_2,
        Variable_A_3 = COW3_Null$Variable_A_3,
        Variable_A_4 = COW4_Null$Variable_A_4,
        MN_Variable_A = MN_Variable_A,
        SD_Variable_A = SD_Variable_A,
        CV_Variable_A = CV_Variable_A,
        FilterEquals_Y_1 = FilterEquals_Y_1,
        FilterEquals_Y_2 = FilterEquals_Y_2,
        FilterEquals_Y_3 = COW3_Null$FilterEquals_Y_3,
        FilterEquals_Y_4 = COW4_Null$FilterEquals_Y_4,
        MN_FilterEquals_Y = MN_FilterEquals_Y,
        SD_FilterEquals_Y = SD_FilterEquals_Y,
        CV_FilterEquals_Y = CV_FilterEquals_Y,
        Variable_B_1 = Variable_B_1,
        Variable_B_2 = Variable_B_2,
        Variable_B_3 = COW3_Null$Variable_B_3,
        Variable_B_4 = COW4_Null$Variable_B_4,
        MN_Variable_B = MN_Variable_B,
        SD_Variable_B = SD_Variable_B,
        CV_Variable_B = CV_Variable_B,
        Variable_C_1 = Variable_C_1,
        Variable_C_2 = Variable_C_2,
        Variable_C_3 = COW3_Null$Variable_C_3,
        Variable_C_4 = COW4_Null$Variable_C_4,
        MN_Variable_C = MN_Variable_C,
        SD_Variable_C = SD_Variable_C,
        CV_Variable_C = CV_Variable_C,
        FilterMinMax_P_1 = FilterMinMax_P_1,
        FilterMinMax_P_2 = FilterMinMax_P_2,
        FilterMinMax_P_3 = COW3_Null$FilterMinMax_P_3,
        FilterMinMax_P_4 = COW4_Null$FilterMinMax_P_4,
        MN_FilterMinMax_P = MN_FilterMinMax_P,
        SD_FilterMinMax_P = SD_FilterMinMax_P,
        CV_FilterMinMax_P = CV_FilterMinMax_P,
        FilterEquals_Z_1 = FilterEquals_Z_1,
        FilterEquals_Z_2 = FilterEquals_Z_2,
        FilterEquals_Z_3 = COW3_Null$FilterEquals_Z_3,
        FilterEquals_Z_4 = COW4_Null$FilterEquals_Z_4,
        MN_FilterEquals_Z = MN_FilterEquals_Z,
        SD_FilterEquals_Z = SD_FilterEquals_Z,
        CV_FilterEquals_Z = CV_FilterEquals_Z,
        Variable_D_1 = Variable_D_1,
        Variable_D_2 = Variable_D_2,
        Variable_D_3 = COW3_Null$Variable_D_3,
        Variable_D_4 = COW4_Null$Variable_D_4,
        MN_Variable_D = MN_Variable_D,
        SD_Variable_D = SD_Variable_D,
        CV_Variable_D = CV_Variable_D,
        Variable_E_1 = Variable_E_1,
        Variable_E_2 = Variable_E_2,
        Variable_E_3 = COW3_Null$Variable_E_3,
        Variable_E_4 = COW4_Null$Variable_E_4,
        MN_Variable_E = MN_Variable_E,
        SD_Variable_E = SD_Variable_E,
        CV_Variable_E = CV_Variable_E,
        CV_TOTAL = CV_TOTAL
      )
    
    #Bind Cows_Paired to DF_TotalPairings
    DF_TotalPairings <- bind_rows(DF_TotalPairings, Cows_Paired)
    
    #and restart loop
    next 
  }
  
  #STEP 21 - Filter DF_4thFilter to find any pairings with the same FilterEquals_Y and FilterEquals_Z values ----
  
  #The resulting filtered data frame is saved as DF_5thFilter
  
  DF_5thFilter <- DF_4thFilter |> 
    filter(
      FilterEquals_Y_1 == FilterEquals_Y_2 &
        FilterEquals_Y_2 == FilterEquals_Y_3 &
        FilterEquals_Z_1 == FilterEquals_Z_2 &
        FilterEquals_Z_2 == FilterEquals_Z_3
    )
  
  #STEP 22 - Count the amount of rows in DF_5thFilter -----------------------------
  
  #We do this to determine if we have any ideal pairing where the three cows have
  #the same FilterEquals_Y value. This is not always the case, hence STEP 22.
  
  Rows_in_5thFilter <- nrow(DF_5thFilter)
  
  #STEP 23 - Choose the most adequate filter for the pairing with COW3 ---------
  
  if (Rows_in_5thFilter == 0L) {
    DF_ChosenFilter2 <- DF_4thFilter #Meaning there are no cows with the same FilterEquals_Y
  } else { 
    DF_ChosenFilter2 <- DF_5thFilter #Meaning there is at least one other cow with
    #the same FilterEquals_Y as COW1 and COW2
  }
  
  #Whichever filter is chosen, it will be used to find the most adequate cow to pair
  #with COW1 and COW2
  
  #STEP 24 - Determine the lowest CV_TOTAL value in DF_ChosenFilter2 --------------
  
  #The resulting value is saved as min_value_CV_TOTAL2
  min_value_CV_TOTAL2 <- min(DF_ChosenFilter2$CV_TOTAL)
  
  #STEP 25 - Find the most adequate cow to be paired with COW1 and COW2 -----------
  
  #We filter DF_ChosenFilter2, using min_value_CV_TOTAL2
  #The resulting row is saved as Cows_Paired
  
  Cow_2ndPair <- DF_ChosenFilter2 |> 
    filter(
      CV_TOTAL == min_value_CV_TOTAL2
    )
  
  #STEP 26 - Remove COW3's data from DF_1stFilter --------------------------------
  
  DF_1stFilter <- DF_1stFilter |> 
    filter(
      ID != Cow_2ndPair$COW3
    )
  
  #STEP 27 - Decide course of action based on GroupSizeWanted --------------------
  
  if (GroupSizeWanted == 3) { 
    
    #Then, this is as far as the steps go.
    #Save Cow_2ndPair as Cows_Paired.
    Cows_Paired <- Cow_2ndPair
    
    #Bind Cows_Paired to DF_TotalPairings.
    #This way, after the loop is completed, DF_TotalPairings will have all 
    #the pairings created using steps 1 to 15.
    DF_TotalPairings <- bind_rows(DF_TotalPairings, Cows_Paired)
    
    #Start the loop again
    next 
  }
  
  #STEP 28 - Create a data frame to compare Cow_2ndPair with the other cows ----
  
  DF_Comparisons_COW4 <- DF_1stFilter |> 
    mutate(
      COW1 = Cow_2ndPair$COW1,
      COW2 = Cow_2ndPair$COW2,
      COW3 = Cow_2ndPair$COW3,
      COW4 = ID,
      FilterMinMax_O_1 = Cow_2ndPair$FilterMinMax_O_1,
      FilterMinMax_O_2 = Cow_2ndPair$FilterMinMax_O_2,
      FilterMinMax_O_3 = Cow_2ndPair$FilterMinMax_O_3,
      FilterMinMax_O_4 = FilterMinMax_O,
      Variable_A_1 = Cow_2ndPair$Variable_A_1,
      Variable_A_2 = Cow_2ndPair$Variable_A_2,
      Variable_A_3 = Cow_2ndPair$Variable_A_3,
      Variable_A_4 = Variable_A,
      FilterEquals_Y_1 = Cow_2ndPair$FilterEquals_Y_1,
      FilterEquals_Y_2 = Cow_2ndPair$FilterEquals_Y_2,
      FilterEquals_Y_3 = Cow_2ndPair$FilterEquals_Y_3,
      FilterEquals_Y_4 = FilterEquals_Y,
      Variable_B_1 = Cow_2ndPair$Variable_B_1,
      Variable_B_2 = Cow_2ndPair$Variable_B_2,
      Variable_B_3 = Cow_2ndPair$Variable_B_3,
      Variable_B_4 = Variable_B,
      Variable_C_1 = Cow_2ndPair$Variable_C_1,
      Variable_C_2 = Cow_2ndPair$Variable_C_2,
      Variable_C_3 = Cow_2ndPair$Variable_C_3,
      Variable_C_4 = Variable_C,
      FilterMinMax_P_1 = Cow_2ndPair$FilterMinMax_P_1,
      FilterMinMax_P_2 = Cow_2ndPair$FilterMinMax_P_2,
      FilterMinMax_P_3 = Cow_2ndPair$FilterMinMax_P_3,
      FilterMinMax_P_4 = FilterMinMax_P,
      FilterEquals_Z_1 = Cow_2ndPair$FilterEquals_Z_1,
      FilterEquals_Z_2 = Cow_2ndPair$FilterEquals_Z_2,
      FilterEquals_Z_3 = Cow_2ndPair$FilterEquals_Z_3,
      FilterEquals_Z_4 = FilterEquals_Z,
      Variable_D_1 = Cow_2ndPair$Variable_D_1,
      Variable_D_2 = Cow_2ndPair$Variable_D_2,
      Variable_D_3 = Cow_2ndPair$Variable_D_3,
      Variable_D_4 = Variable_D,
      Variable_E_1 = Cow_2ndPair$Variable_E_1,
      Variable_E_2 = Cow_2ndPair$Variable_E_2,
      Variable_E_3 = Cow_2ndPair$Variable_E_3,
      Variable_E_4 = Variable_E
    )
  
  #STEP 29 - Calculate MN, SD and CV for the third time  -----------------------
  
  #MN is a variable's mean
  #SD is a variable's standard deviation
  #CV is a variable's coefficient of variation
  
  
  DF_Comparisons_COW4 <- DF_Comparisons_COW4 |> 
    mutate(
      #FilterMinMax_O
      MN_FilterMinMax_O = rowMeans(across(c(FilterMinMax_O_1, FilterMinMax_O_2, FilterMinMax_O_3, FilterMinMax_O_4))),
      SD_FilterMinMax_O = apply(across(c(FilterMinMax_O_1, FilterMinMax_O_2, FilterMinMax_O_3, FilterMinMax_O_4)), 1, sd),
      CV_FilterMinMax_O = (SD_FilterMinMax_O / MN_FilterMinMax_O) * 100,
      #Variable_A
      MN_Variable_A = rowMeans(across(c(Variable_A_1, Variable_A_2, Variable_A_3, Variable_A_4))),
      SD_Variable_A = apply(across(c(Variable_A_1, Variable_A_2, Variable_A_3, Variable_A_4)), 1, sd),
      CV_Variable_A = (SD_Variable_A / MN_Variable_A) * 100,
      #FilterEquals_Y
      MN_FilterEquals_Y = rowMeans(across(c(FilterEquals_Y_1, FilterEquals_Y_2, FilterEquals_Y_3, FilterEquals_Y_4))),
      SD_FilterEquals_Y = apply(across(c(FilterEquals_Y_1, FilterEquals_Y_2, FilterEquals_Y_3, FilterEquals_Y_4)), 1, sd),
      CV_FilterEquals_Y = (SD_FilterEquals_Y / MN_FilterEquals_Y) * 100,
      #Variable_B
      MN_Variable_B = rowMeans(across(c(Variable_B_1, Variable_B_2, Variable_B_3, Variable_B_4))),
      SD_Variable_B = apply(across(c(Variable_B_1, Variable_B_2, Variable_B_3, Variable_B_4)), 1, sd),
      CV_Variable_B = (SD_Variable_B / MN_Variable_B) * 100,
      #Variable_C
      MN_Variable_C = rowMeans(across(c(Variable_C_1, Variable_C_2, Variable_C_3, Variable_C_4))),
      SD_Variable_C = apply(across(c(Variable_C_1, Variable_C_2, Variable_C_3, Variable_C_4)), 1, sd),
      CV_Variable_C = (SD_Variable_C / MN_Variable_C) * 100,
      #FilterMinMax_P
      MN_FilterMinMax_P = rowMeans(across(c(FilterMinMax_P_1, FilterMinMax_P_2, FilterMinMax_P_3, FilterMinMax_P_4))),
      SD_FilterMinMax_P = apply(across(c(FilterMinMax_P_1, FilterMinMax_P_2, FilterMinMax_P_3, FilterMinMax_P_4)), 1, sd),
      CV_FilterMinMax_P = (SD_FilterMinMax_P / MN_FilterMinMax_P) * 100,
      #FilterEquals_Z
      MN_FilterEquals_Z = rowMeans(across(c(FilterEquals_Z_1, FilterEquals_Z_2, FilterEquals_Z_3, FilterEquals_Z_4))),
      SD_FilterEquals_Z = apply(across(c(FilterEquals_Z_1, FilterEquals_Z_2, FilterEquals_Z_3, FilterEquals_Z_4)), 1, sd),
      CV_FilterEquals_Z = (SD_FilterEquals_Z / MN_FilterEquals_Z) * 100,
      #Variable_D
      MN_Variable_D = rowMeans(across(c(Variable_D_1, Variable_D_2, Variable_D_3, Variable_D_4))),
      SD_Variable_D = apply(across(c(Variable_D_1, Variable_D_2, Variable_D_3, Variable_D_4)), 1, sd),
      CV_Variable_D = (SD_Variable_D / MN_Variable_D) * 100,
      #Variable_E
      MN_Variable_E = rowMeans(across(c(Variable_E_1, Variable_E_2, Variable_E_3, Variable_E_4))),
      SD_Variable_E = apply(across(c(Variable_E_1, Variable_E_2, Variable_E_3, Variable_E_4)), 1, sd),
      CV_Variable_E = (SD_Variable_E / MN_Variable_E) * 100
    )
  
  #STEP 30 - Calculate CV_TOTAL for the third time -----------------------------
  
  DF_Comparisons_COW4 <- DF_Comparisons_COW4 |> 
    mutate(
      CV_TOTAL = CV_FilterMinMax_O + CV_Variable_A + CV_FilterEquals_Y +
        CV_Variable_B + CV_Variable_C + CV_FilterMinMax_P +
        CV_FilterEquals_Z + CV_Variable_D + CV_Variable_E
    )
  
  #STEP 31 - Reorder DF_Comparisons_COW4 ---------------------------------------
  
  DF_Comparisons_COW4 <- DF_Comparisons_COW4 |> 
    select(
      COW1,
      COW2,
      COW3,
      COW4,
      FilterMinMax_O_1,
      FilterMinMax_O_2,
      FilterMinMax_O_3,
      FilterMinMax_O_4,
      MN_FilterMinMax_O,
      SD_FilterMinMax_O,
      CV_FilterMinMax_O,
      Variable_A_1,
      Variable_A_2,
      Variable_A_3,
      Variable_A_4,
      MN_Variable_A,
      SD_Variable_A,
      CV_Variable_A,
      FilterEquals_Y_1,
      FilterEquals_Y_2,
      FilterEquals_Y_3,
      FilterEquals_Y_4,
      MN_FilterEquals_Y,
      SD_FilterEquals_Y,
      CV_FilterEquals_Y,
      Variable_B_1,
      Variable_B_2,
      Variable_B_3,
      Variable_B_4,
      MN_Variable_B,
      SD_Variable_B,
      CV_Variable_B,
      Variable_C_1,
      Variable_C_2,
      Variable_C_3,
      Variable_C_4,
      MN_Variable_C,
      SD_Variable_C,
      CV_Variable_C,
      FilterMinMax_P_1,
      FilterMinMax_P_2,
      FilterMinMax_P_3,
      FilterMinMax_P_4,
      MN_FilterMinMax_P,
      SD_FilterMinMax_P,
      CV_FilterMinMax_P,
      FilterEquals_Z_1,
      FilterEquals_Z_2,
      FilterEquals_Z_3,
      FilterEquals_Z_4,
      MN_FilterEquals_Z,
      SD_FilterEquals_Z,
      CV_FilterEquals_Z,
      Variable_D_1,
      Variable_D_2,
      Variable_D_3,
      Variable_D_4,
      MN_Variable_D,
      SD_Variable_D,
      CV_Variable_D,
      Variable_E_1,
      Variable_E_2,
      Variable_E_3,
      Variable_E_4,
      MN_Variable_E,
      SD_Variable_E,
      CV_Variable_E,
      CV_TOTAL
    )
  
  #STEP 32 - Filter new CV_FilterMinMax_O, CV_Variable_A and CV_FilterEquals_Y using the value saved as MaxCV -----
  
  #The filter's values can be changed on the "Values modifiable at the user's 
  #discretion" section.
  #The resulting filtered data frame is saved as DF_6thFilter
  
  DF_6thFilter <- DF_Comparisons_COW4 |> 
    filter(
      CV_FilterMinMax_O <= MaxCV &
        CV_Variable_A <= MaxCV &
        CV_FilterEquals_Y <= MaxCV &
        CV_Variable_B <= MaxCV &
        CV_Variable_C <= MaxCV &
        CV_FilterMinMax_P <= MaxCV &
        CV_FilterEquals_Z <= MaxCV &
        CV_Variable_D <= MaxCV &
        CV_Variable_E <= MaxCV
    )
  
  #Check if DF_6thFilter has zero rows
  if (nrow(DF_6thFilter) == 0) { 
    
    #Keep the second pair and group them with a null cow
    Cows_Paired <- Cow_2ndPair |> 
      mutate(
        COW1 = COW1,
        COW2 = COW2,
        COW3 = COW3,
        COW4 = COW4_Null$COW4,
        FilterMinMax_O_1 = FilterMinMax_O_1,
        FilterMinMax_O_2 = FilterMinMax_O_2,
        FilterMinMax_O_3 = FilterMinMax_O_3,
        FilterMinMax_O_4 = COW4_Null$FilterMinMax_O_4,
        MN_FilterMinMax_O = MN_FilterMinMax_O,
        SD_FilterMinMax_O = SD_FilterMinMax_O,
        CV_FilterMinMax_O = CV_FilterMinMax_O,
        Variable_A_1 = Variable_A_1,
        Variable_A_2 = Variable_A_2,
        Variable_A_3 = Variable_A_3,
        Variable_A_4 = COW4_Null$Variable_A_4,
        MN_Variable_A = MN_Variable_A,
        SD_Variable_A = SD_Variable_A,
        CV_Variable_A = CV_Variable_A,
        FilterEquals_Y_1 = FilterEquals_Y_1,
        FilterEquals_Y_2 = FilterEquals_Y_2,
        FilterEquals_Y_3 = FilterEquals_Y_3,
        FilterEquals_Y_4 = COW4_Null$FilterEquals_Y_4,
        MN_FilterEquals_Y = MN_FilterEquals_Y,
        SD_FilterEquals_Y = SD_FilterEquals_Y,
        CV_FilterEquals_Y = CV_FilterEquals_Y,
        Variable_B_1 = Variable_B_1,
        Variable_B_2 = Variable_B_2,
        Variable_B_3 = Variable_B_3,
        Variable_B_4 = COW4_Null$Variable_B_4,
        MN_Variable_B = MN_Variable_B,
        SD_Variable_B = SD_Variable_B,
        CV_Variable_B = CV_Variable_B,
        Variable_C_1 = Variable_C_1,
        Variable_C_2 = Variable_C_2,
        Variable_C_3 = Variable_C_3,
        Variable_C_4 = COW4_Null$Variable_C_4,
        MN_Variable_C = MN_Variable_C,
        SD_Variable_C = SD_Variable_C,
        CV_Variable_C = CV_Variable_C,
        FilterMinMax_P_1 = FilterMinMax_P_1,
        FilterMinMax_P_2 = FilterMinMax_P_2,
        FilterMinMax_P_3 = FilterMinMax_P_3,
        FilterMinMax_P_4 = COW4_Null$FilterMinMax_P_4,
        MN_FilterMinMax_P = MN_FilterMinMax_P,
        SD_FilterMinMax_P = SD_FilterMinMax_P,
        CV_FilterMinMax_P = CV_FilterMinMax_P,
        FilterEquals_Z_1 = FilterEquals_Z_1,
        FilterEquals_Z_2 = FilterEquals_Z_2,
        FilterEquals_Z_3 = FilterEquals_Z_3,
        FilterEquals_Z_4 = COW4_Null$FilterEquals_Z_4,
        MN_FilterEquals_Z = MN_FilterEquals_Z,
        SD_FilterEquals_Z = SD_FilterEquals_Z,
        CV_FilterEquals_Z = CV_FilterEquals_Z,
        Variable_D_1 = Variable_D_1,
        Variable_D_2 = Variable_D_2,
        Variable_D_3 = Variable_D_3,
        Variable_D_4 = COW4_Null$Variable_D_4,
        MN_Variable_D = MN_Variable_D,
        SD_Variable_D = SD_Variable_D,
        CV_Variable_D = CV_Variable_D,
        Variable_E_1 = Variable_E_1,
        Variable_E_2 = Variable_E_2,
        Variable_E_3 = Variable_E_3,
        Variable_E_4 = COW4_Null$Variable_E_4,
        MN_Variable_E = MN_Variable_E,
        SD_Variable_E = SD_Variable_E,
        CV_Variable_E = CV_Variable_E,
        CV_TOTAL = CV_TOTAL
      )
    
    #Bind Cows_Paired to DF_TotalPairings
    DF_TotalPairings <- bind_rows(DF_TotalPairings, Cows_Paired)
    
    #and restart loop
    next 
  }
  
  #STEP 33 - Filter DF_6thFilter to find any pairings with the same FilterEquals_Y values ----
  
  #The resulting filtered data frame is saved as DF_7thFilter
  
  DF_7thFilter <- DF_6thFilter |> 
    filter(
      FilterEquals_Y_1 == FilterEquals_Y_2 &
        FilterEquals_Y_2 == FilterEquals_Y_3 &
        FilterEquals_Y_3 == FilterEquals_Y_4 &
        FilterEquals_Z_1 == FilterEquals_Z_2 &
        FilterEquals_Z_2 == FilterEquals_Z_3 &
        FilterEquals_Z_3 == FilterEquals_Z_4
    )
  
  #STEP 34 - Count the amount of rows in DF_7thFilter -----------------------------
  
  #We do this to determine if we have any ideal pairing where the four cows have
  #the same FilterEquals_Y value. This is not always the case, hence STEP 33.
  
  Rows_in_7thFilter <- nrow(DF_7thFilter)
  
  #STEP 35 - Choose the most adequate filter for the pairing with COW4 ---------
  
  if (Rows_in_7thFilter == 0L) {
    DF_ChosenFilter3 <- DF_6thFilter #Meaning there are no cows with the same FilterEquals_Y
  } else { 
    DF_ChosenFilter3 <- DF_7thFilter #Meaning there is at least one other cow with
    #the same FilterEquals_Y as COW1, COW2 and COW3
  }
  
  #Whichever filter is chosen, it will be used to find the most adequate cow to pair
  #with COW1, COW2 and COW3
  
  #STEP 36 - Determine the lowest CV_TOTAL value in DF_ChosenFilter3 --------------
  
  #The resulting value is saved as min_value_CV_TOTAL2
  min_value_CV_TOTAL3 <- min(DF_ChosenFilter3$CV_TOTAL)
  
  #STEP 37 - Find the most adequate cow to be paired with COW1, COW2 and COW3 ----
  
  #We filter DF_ChosenFilter3, using min_value_CV_TOTAL3
  #The resulting row is saved as Cows_Paired
  
  Cows_Paired <- DF_ChosenFilter3 |> 
    filter(
      CV_TOTAL == min_value_CV_TOTAL3
    )
  
  #STEP 38 - Bind Cows_Paired to DF_TotalPairings ---------------------------------
  
  #This way, after the loop is completed, DF_TotalPairings will have all the pairings
  #created using steps 1 to 36.
  
  DF_TotalPairings <- bind_rows(DF_TotalPairings, Cows_Paired)
  
  #STEP 39 - Remove COW4's data from DF_1stFilter --------------------------------
  
  DF_1stFilter <- DF_1stFilter |> 
    filter(
      ID != Cows_Paired$COW4
    )
  
  #STEP 40 - Calculate the amount of rows left in DF_1stFilter -------------------
  
  #After steps 13, 14, 25 and 40 DF_1stFilter must have 4 less rows.
  #Eventually, the row number will decrease until the loop is over.
  
  print(nrow(DF_1stFilter))
  
} #THE LOOP'S CODE ENDS HERE ---------------------------------------------------

#Remove the first row in DF_TotalPairings --------------------------------------

#The first row is the "original" one, created with the template for the data frame.
#By doing this, we have only the cow pairings created with the 15-step loop. 

DF_TotalPairings <- DF_TotalPairings[-1,]

#Randomly choose from DF_TotalPairings the amount of PairingsChosen ------------

#The PairingsChosen value can be changed on the "Values modifiable at the user's 
#discretion" section.

#We use set.seed in order to always get the same random selection.
#PLEASE DO NOT DELETE THIS.

set.seed(1)

DF_Final <- DF_TotalPairings[sample(
  nrow(DF_TotalPairings),
  PairingsChosen),]

#Save DF_TotalPairings and DF_Final as csv and xlms files ----------------------

#Save DF_Final and DF_TotalPairings as csv files

write_csv(DF_Final, "DF_Final.csv") 
write_csv(DF_TotalPairings, "DF_TotalPairings.csv") 

#Save DF_Final and DF_TotalPairings as xlsx files
#NOTE: You have to load the openxlsx library for this to work

write.xlsx(DF_Final, "DF_Final.xlsx") 
write.xlsx(DF_TotalPairings, "DF_TotalPairings.xlsx")

#Confirmation message ----------------------------------------------------------
cat("All done!\n")