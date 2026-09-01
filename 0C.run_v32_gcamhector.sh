#!/usr/bin/env bash
# This script requires that you have the ability to run command line 
# hector locally, and that the correct version is installed. 
# The git tag --describe should return v3.2.0-12-g4bc2383 


# Define separate paths
HECTOR_EXEC="/Users/dorh012/Documents/Hector-WD/command_line/hector/src/hector"
INPUT_DIR="/Users/dorh012/Documents/GCAM-WD/CMPs/CMP406/inputs/old"
INI_FILE="hector-gcam.ini"

# Run Hector with the full paths
"$HECTOR_EXEC" "$INPUT_DIR/$INI_FILE"

# This will write a outputstream file to the output dir, rename and move 
# that file to the master-GCAM directory. 
OUTPUT_DIR="./output"
DEST_DIR="./master-GCAM"
NEW_NAME="gcamhector_outputstream.csv"

# Find the file containing "outputstream" in its name
file=$(find "$OUTPUT_DIR" -maxdepth 1 -type f -iname "*outputstream*")

if [[ -n "$file" ]]; then
    echo "Found: $file"
    mv "$file" "$DEST_DIR/$NEW_NAME"
else
    echo "No outputstream file found."
fi

