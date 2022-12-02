package main

import (
	"fmt"
	"os"
	"strings"
)

func day2() {
	bytes, _ := os.ReadFile("day2input")
	lines := strings.Split(string(bytes), "\n")

  score1 := 0
  for _, line := range lines {
    score1 += day2score1(line)
  }
  fmt.Println(score1)

  score2 := 0
  for _, line := range lines {
    score2 += day2score2(line)
  }
  fmt.Println(score2)
}

func day2score1(s string) int {
  switch s {
    case "A X": return 3 + 1
    case "B X": return 0 + 1
    case "C X": return 6 + 1
    case "A Y": return 6 + 2
    case "B Y": return 3 + 2
    case "C Y": return 0 + 2
    case "A Z": return 0 + 3
    case "B Z": return 6 + 3
    case "C Z": return 3 + 3
  }
  panic("shouldn't come here")
}

func day2score2(s string) int {
  switch s {
    case "A X": return 3
    case "B X": return 1
    case "C X": return 2
    case "A Y": return 1 + 3
    case "B Y": return 2 + 3
    case "C Y": return 3 + 3
    case "A Z": return 2 + 6
    case "B Z": return 3 + 6
    case "C Z": return 1 + 6
  }
  panic("shouldn't come here")
}
