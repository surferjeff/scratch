package main

import (
	"bufio"
	"fmt"
	"os"
)

func check(err error, msg string, args ...interface{}) {
	if err != nil {
		formattedMessage := fmt.Sprintf(msg, args...)
		panic(fmt.Sprintf("%s: %v", formattedMessage, err))
	}
}

func main() {
	fileName := "sample.txt"

	// Open the file
	file, err := os.Open(fileName)
	check(err, "failed to open file %s", fileName)
	defer file.Close()

	// Read the file line by line
	var lines []string
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		lines = append(lines, scanner.Text())
	}
	check(scanner.Err(), "error while scanning file %s", fileName)

}
