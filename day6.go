package main

import (
	"fmt"
	"os"
	"strings"
)

func day6() {
	bytes, _ := os.ReadFile("day6.txt")
	lines := strings.Split(string(bytes), "\n")

	line := lines[0]
	fmt.Println(detectSignal6(line))
	fmt.Println(detectSignal6_14(line))
}

func detectSignal6(s string) int {
	window := []rune{}
	for i, c := range s {
		if i > 3 {
			window = window[1:]
		}
		window = append(window, c)

		set := map[rune]void{}
		for _, cc := range window {
			set[cc] = void{}
		}

		if len(set) > 3 {
			return i + 1
		}
	}
	panic("no solution!")
}

func detectSignal6_14(s string) int {
	window := []rune{}
	for i, c := range s {
		if i > 13 {
			window = window[1:]
		}
		window = append(window, c)

		set := map[rune]void{}
		for _, cc := range window {
			set[cc] = void{}
		}

		if len(set) > 13 {
			return i + 1
		}
	}
	panic("no solution!")
}
