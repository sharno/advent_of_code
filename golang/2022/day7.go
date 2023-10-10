package main

import (
	"fmt"
	"os"
	"strconv"
	"strings"
	"unicode"
)

func day7() {
	bytes, _ := os.ReadFile("day7.txt")
	lines := strings.Split(string(bytes), "\r\n")

	currentDir := []string{""}
	fs := map[string]map[string]int{}
	for _, line := range lines {
		if line == "$ cd .." {
			currentDir = currentDir[:len(currentDir)-1]
		} else if line == "$ cd /" {
			currentDir = []string{""}
		} else if line[:4] == "$ cd" {
			currentDir = append(currentDir, line[5:])
		} else if unicode.IsDigit(rune(line[0])) {
			sizeName := strings.Split(line, " ")
			currentDirName := strings.Join(currentDir, "/")
			fileName := currentDirName + sizeName[1]
			fileSize, _ := strconv.Atoi(sizeName[0])
			for i := range currentDir {
				dir := strings.Join(currentDir[0:i+1], "/")
				files := fs[dir]
				if files == nil {
					files = map[string]int{}
					fs[dir] = files
				}
				files[fileName] = fileSize
			}
		}
	}

	part1 := 0
	for _, files := range fs {
		dirSize := 0
		for _, fileSize := range files {
			dirSize += fileSize
		}
		if dirSize <= 100000 {
			part1 += dirSize
		}
	}
	fmt.Println(part1)

	// part 2
	totalDiskSpace := 70000000
	neededDiskSpace := 30000000
	usedDiskSpace := 0
	for _, size := range fs[""] {
		usedDiskSpace += size
	}
	toDelete := neededDiskSpace - (totalDiskSpace - usedDiskSpace)

	part2 := totalDiskSpace
	for _, files := range fs {
		dirSize := 0
		for _, fileSize := range files {
			dirSize += fileSize
		}
		if dirSize >= toDelete && dirSize < part2 {
			part2 = dirSize
		}
	}
	fmt.Println(part2)
}
