package main

import (
	"fmt"
	"os"
	"strconv"
	"strings"
	"unicode"
)

func day5() {
	bytes, _ := os.ReadFile("day5.txt")
	lines := strings.Split(string(bytes), "\n")

	stepsMode := false
	stacks := make([][]rune, 9)
	steps := [][3]int{}
	for _, line := range lines {
		if line == "" || line[0] == ' ' {
			stepsMode = true
			continue
		}

		if stepsMode {
			// st eps
			var n int
			var from int
			var to int
			if unicode.IsDigit(rune(line[6])) {
				n, _ = strconv.Atoi(line[5:7])
				from, _ = strconv.Atoi(line[13:14])
				to, _ = strconv.Atoi(line[18:19])
			} else {
				n, _ = strconv.Atoi(line[5:6])
				from, _ = strconv.Atoi(line[12:13])
				to, _ = strconv.Atoi(line[17:18])
			}
			step := [3]int{n, from, to}
			steps = append(steps, step)
		} else {
			// stacks
			i := 1
			for i < 35 {
				stackIndex := (i - 1) / 4
				if line[i] != ' ' {
					stacks[stackIndex] = append(stacks[stackIndex], rune(line[i]))
				}
				i += 4
			}
		}
	}
	for i, stack := range stacks {
		stacks[i] = reverse(stack)
	}

	stacks2 := make([][]rune, len(stacks))
	for i := range stacks {
		stacks2[i] = make([]rune, len(stacks[i]))
		copy(stacks2[i], stacks[i])
	}

	// part 1
	for _, step := range steps {
		for i := 0; i < step[0]; i++ {
			from := step[1] - 1
			to := step[2] - 1

			// pop
			crate := stacks[from][len(stacks[from])-1]
			stacks[from] = stacks[from][:len(stacks[from])-1]
			// push
			stacks[to] = append(stacks[to], crate)
		}
	}
	printTopOfStacks(stacks)

	stacks = stacks2
	// part 2
	for _, step := range steps {
		numOfCrates := step[0]
		from := step[1] - 1
		to := step[2] - 1

		stackLength := len(stacks[from])
		// pop
		crates := stacks[from][stackLength-numOfCrates : stackLength]
		stacks[from] = stacks[from][:stackLength-numOfCrates]
		// push
		stacks[to] = append(stacks[to], crates...)
	}
	printTopOfStacks(stacks)
}

func reverse(stack []rune) []rune {
	reversed := make([]rune, 0)
	for i := len(stack) - 1; i >= 0; i-- {
		reversed = append(reversed, stack[i])
	}
	return reversed
}

func printTopOfStacks(stacks [][]rune) {
	res := []rune{}
	for _, stack := range stacks {
		res = append(res, stack[len(stack)-1])
	}
	fmt.Println(string(res))
}
