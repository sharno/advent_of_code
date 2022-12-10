package main

import (
	"fmt"
	"os"
	"strings"
)

func day3() {
	bytes, _ := os.ReadFile("day3.txt")
	lines := strings.Split(string(bytes), "\n")

	// part 1
	sum := 0
	for _, line := range lines {
		sum += day3Item([]string{line[:len(line)/2], line[len(line)/2:]})
	}
	fmt.Println(sum)

	// part 2
	groups := [][]string{}
	group := []string{}
	for _, line := range lines {
		group = append(group, line)
		if len(group) == 3 {
			groups = append(groups, group)
			group = []string{}
		}
	}

	sum = 0
	for _, group := range groups {
		sum += day3Item(group)
	}
	fmt.Println(sum)
}

func day3Item(group []string) int {
	set := stringToSet(group[0])
	for _, line := range group {
		set = intersection(set, stringToSet(line))
	}
	var item rune
	for k := range set {
		item = k
	}
	return runeCode(item)
}

func stringToSet(str string) map[rune]void {
	set := map[rune]void{}
	for _, c := range str {
		set[c] = void{}
	}
	return set
}

func intersection(set1 map[rune]void, set2 map[rune]void) map[rune]void {
	set := map[rune]void{}
	for k, v := range set1 {
		_, ok := set2[k]
		if ok {
			set[k] = v
		}
	}
	return set
}

func runeCode(c rune) int {
	if c < 'a' {
		return int(c - 'A' + 27)
	} else {
		return int(c - 'a' + 1)
	}
}
