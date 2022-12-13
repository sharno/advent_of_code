package main

import (
	"fmt"
	"os"
	"strconv"
	"strings"
)

func day10() {
	bytes, _ := os.ReadFile("day10.txt")
	lines := strings.Split(string(bytes), "\r\n")

	cycle := 1
	register := 1
	cycles := []int{20, 60, 100, 140, 180, 220, 9999999999}
	crt := []rune{}
	signalSum := 0
	for _, line := range lines {
		num := 0
		if line[0] == 'a' {
			// part 2
			if cycle%40 >= register && cycle%40 <= register+2 {
				crt = append(crt, '#')
			} else {
				crt = append(crt, '.')
			}
			if (cycle+1)%40 >= register && (cycle+1)%40 <= register+2 {
				crt = append(crt, '#')
			} else {
				crt = append(crt, '.')
			}
			cycle += 2
			numstr := strings.Split(line, " ")[1]
			num, _ = strconv.Atoi(numstr)
		} else {
			// part 2
			if cycle%40 >= register && cycle%40 <= register+2 {
				crt = append(crt, '#')
			} else {
				crt = append(crt, '.')
			}
			cycle++
		}

		// part 1
		if cycle > cycles[0] {
			signalSum += cycles[0] * register
			cycles = cycles[1:]
		}
		register += num
	}

	// part 1
	fmt.Println(signalSum)

	// part 2
	for i := range crt {
		if i%40 == 0 {
			fmt.Println()
		}
		fmt.Print(string(crt[i]))
	}
}
