package main

import (
	"fmt"
	"os"
	"strconv"
	"strings"
)

func day8() {
	bytes, _ := os.ReadFile("day8.txt")
	lines := strings.Split(string(bytes), "\r\n")

	grid := [][]int{}
	for _, line := range lines {
		row := []int{}
		for _, c := range line {
			h, _ := strconv.Atoi(string(c))
			row = append(row, h)
		}
		grid = append(grid, row)
	}

	type pair struct {
		fst int
		snd int
	}
	set := map[pair]void{}
	for i := 0; i < len(grid); i++ {
		tallest := -1
		for j := 0; j < len(grid[0]); j++ {
			if grid[i][j] > tallest {
				tallest = grid[i][j]
				set[pair{i, j}] = void{}
			}
		}
	}
	for j := 0; j < len(grid[0]); j++ {
		tallest := -1
		for i := 0; i < len(grid); i++ {
			if grid[i][j] > tallest {
				tallest = grid[i][j]
				set[pair{i, j}] = void{}
			}
		}
	}
	for j := len(grid[0]) - 1; j >= 0; j-- {
		tallest := -1
		for i := len(grid) - 1; i >= 0; i-- {
			if grid[i][j] > tallest {
				tallest = grid[i][j]
				set[pair{i, j}] = void{}
			}
		}
	}
	for i := len(grid) - 1; i >= 0; i-- {
		tallest := -1
		for j := len(grid[0]) - 1; j >= 0; j-- {
			if grid[i][j] > tallest {
				tallest = grid[i][j]
				set[pair{i, j}] = void{}
			}
		}
	}

	fmt.Println(len(set))

	bestScore := 0
	for i := 0; i < len(grid); i++ {
		for j := 0; j < len(grid[0]); j++ {
			score := scenicScore(i, j, grid)
			if score > bestScore {
				bestScore = score
			}
		}
	}
	fmt.Println(bestScore)
}

func scenicScore(x int, y int, grid [][]int) int {
	h := grid[x][y]
	right := 0
	for j := y + 1; j < len(grid[0]); j++ {
		right++
		if grid[x][j] >= h {
			break
		}
	}
	down := 0
	for i := x + 1; i < len(grid); i++ {
		down++
		if grid[i][y] >= h {
			break
		}
	}
	up := 0
	for i := x - 1; i >= 0; i-- {
		up++
		if grid[i][y] >= h {
			break
		}
	}
	left := 0
	for j := y - 1; j >= 0; j-- {
		left++
		if grid[x][j] >= h {
			break
		}
	}

	return up * down * left * right
}
