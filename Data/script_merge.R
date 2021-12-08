
data1 = read.csv("aircrafts.csv")
names(data1)
dim(data1)

data2 = read.csv("occurrences.csv")
names(data2)
dim(data2)

# Merge
data <- merge(data1[, 2:(ncol(data1)-1)], data2[, 2:(ncol(data2)-1)], by="occurrence_id")
names(data)
dim(data)

# Create merged csv
write.csv(data, file = "aircrafts_occurrences_merged.csv", row.names = FALSE)

# Check
merged = read.csv("aircrafts_occurrences_merged.csv")
names(merged)
dim(merged)
all.equal(data, merged)
