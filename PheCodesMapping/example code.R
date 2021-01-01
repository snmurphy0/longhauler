load("phecode_map_file.rda")

# Example: find the phecode corresponding to ICD9 code "ICD9:038.40":

map_file_icd9_10_all$Phecode[which(map_file_icd9_10_all$ICD == "ICD9:038.40")]

# There are two findings: "038_" and "038_1", corresponding to "038." and "038.1"
# (see https://phewascatalog.org/phecodes)
# Here we use '_' to replace '.' in the phecode as we want to avoid it being converted
# to some numerical value by R.