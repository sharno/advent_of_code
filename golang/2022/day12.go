package main

import (
	"fmt"
	"math"
	"os"
	"strings"
)

func day12() {
	bytes, _ := os.ReadFile("day12.txt")
	lines := strings.Split(string(bytes), "\r\n")

	start := pair{}
	for i := range lines {
		for j := range lines[i] {
			if lines[i][j] == 'S' {
				start = pair{i, j}
			}
		}
	}

	grid := [][]rune{}
	for _, l := range lines {
		grid = append(grid, []rune(l))
	}

	fmt.Println(bfs(start, grid))

	as := []pair{}
	for i := range grid {
		for j := range grid[i] {
			if grid[i][j] == 'a' {
				as = append(as, pair{i, j})
			}
		}
	}
	shortest := math.MaxInt
	for _, p := range as {
		path := bfs(p, grid)
		if path < shortest {
			shortest = path
		}
	}
	fmt.Println(shortest)
}

func bfs(start pair, grid [][]rune) int {
	steps := 0
	visited := map[pair]void{}
	q := []*pair{&start}
	for len(q) > 0 {
		newQ := []*pair{}
		for _, p := range q {
			if p == nil {
				continue
			}

			if grid[p.x][p.y] == 'E' {
				return steps
			}

			if _, ok := visited[*p]; ok {
				continue
			}

			if canGo(p, up(*p), grid) {
				newQ = append(newQ, up(*p))
			}
			if canGo(p, down(*p, grid), grid) {
				newQ = append(newQ, down(*p, grid))
			}
			if canGo(p, left(*p), grid) {
				newQ = append(newQ, left(*p))
			}
			if canGo(p, right(*p, grid), grid) {
				newQ = append(newQ, right(*p, grid))
			}
			visited[*p] = void{}
		}
		steps++
		q = newQ
	}

	// some points don't have a path to the destination, we return a very high number
	return math.MaxInt
}

func up(p pair) *pair {
	if p.x == 0 {
		return nil
	} else {
		return &pair{p.x - 1, p.y}
	}
}
func down(p pair, grid [][]rune) *pair {
	if p.x == len(grid)-1 {
		return nil
	} else {
		return &pair{p.x + 1, p.y}
	}
}

func left(p pair) *pair {
	if p.y == 0 {
		return nil
	} else {
		return &pair{p.x, p.y - 1}
	}
}
func right(p pair, grid [][]rune) *pair {
	if p.y == len(grid[0])-1 {
		return nil
	} else {
		return &pair{p.x, p.y + 1}
	}
}

func canGo(origin *pair, dest *pair, g [][]rune) bool {
	if origin == nil || dest == nil {
		return false
	}
	if g[origin.x][origin.y] == 'S' && g[dest.x][dest.y] == 'a' {
		return true
	}
	if g[dest.x][dest.y] == 'E' {
		return g[origin.x][origin.y] == 'z'
	}
	if g[dest.x][dest.y]-g[origin.x][origin.y] > 1 {
		return false
	}
	return true
}
