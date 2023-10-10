package main

import (
	"fmt"
	"os"
	"strconv"
	"strings"
)

func day4() {
	bytes, _ := os.ReadFile("day4.txt")
	lines := strings.Split(string(bytes), "\n")

	// parse input
	elves := [][]int{}
	for _, line := range lines {
		ranges := strings.Split(line, ",")
		es := []int{}
		for _, r := range ranges {
			nums := strings.Split(r, "-")
			fst, _ := strconv.Atoi(nums[0])
			snd, _ := strconv.Atoi(nums[1])
			es = append(es, fst)
			es = append(es, snd)
		}
		elves = append(elves, es)
	}

	// part 1
	count := 0
	for _, es := range elves {
		if (es[0] >= es[2] && es[1] <= es[3]) || (es[2] >= es[0] && es[3] <= es[1]) {
			count++
		}
	}
	fmt.Println(count)

	// part 2
	count = 0
	for _, es := range elves {
		if (es[0] >= es[2] && es[0] <= es[3]) ||
			(es[1] >= es[2] && es[1] <= es[3]) ||
			(es[2] >= es[0] && es[2] <= es[1]) ||
			(es[3] >= es[0] && es[3] <= es[1]) {
			count++
		}
	}
	fmt.Println(count)
}
