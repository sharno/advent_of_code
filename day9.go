package main

import (
	"fmt"
	"math"
	"os"
	"strconv"
	"strings"
)

type pair struct {
	x int
	y int
}
type step struct {
	direction rune
	times     int
}

func day9() {
	bytes, _ := os.ReadFile("day9.txt")
	lines := strings.Split(string(bytes), "\r\n")

	steps := []step{}
	for _, line := range lines {
		times, _ := strconv.Atoi(line[2:])
		steps = append(steps, step{rune(line[0]), times})
	}

	set := map[pair]void{}
	snake := []pair{{0, 0}, {0, 0}}
	for _, step := range steps {
		for s := 0; s < step.times; s++ {
			switch step.direction {
			case 'R':
				snake[0].x++
			case 'L':
				snake[0].x--
			case 'U':
				snake[0].y++
			case 'D':
				snake[0].y--
			}

			for i := 1; i < len(snake); i++ {
				totalDistance := math.Abs(float64(snake[i-1].x)-float64(snake[i].x)) +
					math.Abs(float64(snake[i-1].y)-float64(snake[i].y))
				if totalDistance > 2 {
					snake[i].x += int(math.Round(float64(snake[i-1].x-snake[i].x) / 2))
					snake[i].y += int(math.Round(float64(snake[i-1].y-snake[i].y) / 2))
				} else if totalDistance > 1 {
					snake[i].x += (snake[i-1].x - snake[i].x) / 2
					snake[i].y += (snake[i-1].y - snake[i].y) / 2
				}
			}

			set[snake[len(snake)-1]] = void{}
		}
	}
	fmt.Println(len(set))

	// part 2 : copied the code above and made the snake of 10 nodes instead of 2
	set = map[pair]void{}
	snake = []pair{{0, 0}, {0, 0}, {0, 0}, {0, 0}, {0, 0}, {0, 0}, {0, 0}, {0, 0}, {0, 0}, {0, 0}}
	for _, step := range steps {
		for s := 0; s < step.times; s++ {
			switch step.direction {
			case 'R':
				snake[0].x++
			case 'L':
				snake[0].x--
			case 'U':
				snake[0].y++
			case 'D':
				snake[0].y--
			}

			for i := 1; i < len(snake); i++ {
				totalDistance := math.Abs(float64(snake[i-1].x)-float64(snake[i].x)) +
					math.Abs(float64(snake[i-1].y)-float64(snake[i].y))
				if totalDistance > 2 {
					snake[i].x += int(math.Round(float64(snake[i-1].x-snake[i].x) / 2))
					snake[i].y += int(math.Round(float64(snake[i-1].y-snake[i].y) / 2))
				} else if totalDistance > 1 {
					snake[i].x += (snake[i-1].x - snake[i].x) / 2
					snake[i].y += (snake[i-1].y - snake[i].y) / 2
				}
			}

			set[snake[len(snake)-1]] = void{}
		}
	}
	fmt.Println(len(set))
}
