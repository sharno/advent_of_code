package main

import (
	"fmt"
	"os"
	"strconv"
	"strings"
)

func day1() {
	bytes, _ := os.ReadFile("day1input")
  lines := strings.Split(string(bytes), "\n")
  
  currentElf := 0
  max := 0
  max2 := 0
  max3 := 0
  for _, line := range lines {
    // at an empty line we got the total of the elf so we check the maxes
    if line == "" {
      if currentElf > max {
        max3 = max2
        max2 = max
        max = currentElf
      } else if currentElf > max2 {
        max3 = max2
        max2 = currentElf
      } else if currentElf > max3 {
        max3 = currentElf
      }

      // reset to the new elf
      currentElf = 0
      continue
    }
    
    num, _ := strconv.Atoi(line)
    currentElf += num
  }
	fmt.Println(max)
	fmt.Println(max + max2 + max3)
}
