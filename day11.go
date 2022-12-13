package main

import (
	"fmt"
	"os"
	"strconv"
	"strings"
)

type monkey struct {
	items     []int
	operation []string
	test      int
	ifTrue    int
	ifFalse   int
	inspected int
}

func day11() {
	bytes, _ := os.ReadFile("day11.txt")
	lines := strings.Split(string(bytes), "\r\n")

	// parse
	monkeys := []monkey{}
	m := monkey{}
	for _, line := range lines {
		if line == "" {
			continue
		}

		if line[:3] == "  S" {
			itemsString := strings.Split(strings.Split(line, ": ")[1], ", ")
			for _, itemString := range itemsString {
				item, _ := strconv.Atoi(itemString)
				m.items = append(m.items, item)
			}
		} else if line[:3] == "  O" {
			tokens := strings.Split(strings.Split(line, "= old ")[1], " ")
			m.operation = tokens
		} else if line[:3] == "  T" {
			test, _ := strconv.Atoi(strings.Split(line, "by ")[1])
			m.test = test
		} else if line[:8] == "    If t" {
			target, _ := strconv.Atoi(string(line[len(line)-1]))
			m.ifTrue = target
		} else if line[:8] == "    If f" {
			target, _ := strconv.Atoi(string(line[len(line)-1]))
			m.ifFalse = target
			monkeys = append(monkeys, m)
			m = monkey{}
		}
	}
	monkeys2 := make([]monkey, len(monkeys))
	copy(monkeys2, monkeys)

	// rounds
	for round := 0; round < 20; round++ {
		for i, m := range monkeys {
			for _, item := range m.items {
				worry := evalOperation(item, m.operation) / 3
				if worry%m.test == 0 {
					monkeys[m.ifTrue].items = append(monkeys[m.ifTrue].items, worry)
				} else {
					monkeys[m.ifFalse].items = append(monkeys[m.ifFalse].items, worry)
				}
			}
			monkeys[i].inspected += len(m.items)
			monkeys[i].items = []int{}
		}
	}

	h1 := 0
	h2 := 0
	for _, m := range monkeys {
		if m.inspected > h1 {
			h2 = h1
			h1 = m.inspected
		} else if m.inspected > h2 {
			h2 = m.inspected
		}
	}
	fmt.Println(h1 * h2)

	// rounds 2

	// magic number is the multiplication of all the prime numbers that we test with
	// if we use this number to get the remainder we can keep our number low
	magicNum := 1
	for _, m := range monkeys2 {
		magicNum *= m.test
	}
	for round := 0; round < 10000; round++ {
		for i, m := range monkeys2 {
			for _, item := range m.items {
				worry := evalOperation(item, m.operation)
				if worry%m.test == 0 {
					monkeys2[m.ifTrue].items = append(monkeys2[m.ifTrue].items, worry%magicNum)
				} else {
					monkeys2[m.ifFalse].items = append(monkeys2[m.ifFalse].items, worry%magicNum)
				}
			}
			monkeys2[i].inspected += len(m.items)
			monkeys2[i].items = []int{}
		}
	}

	h1 = 0
	h2 = 0
	for _, m := range monkeys2 {
		if m.inspected > h1 {
			h2 = h1
			h1 = m.inspected
		} else if m.inspected > h2 {
			h2 = m.inspected
		}
	}
	fmt.Println(h1 * h2)
}

func evalOperation(old int, expr []string) int {
	e := 0
	if expr[1] == "old" {
		e = old
	} else {
		e, _ = strconv.Atoi(expr[1])
	}

	if expr[0] == "*" {
		return old * e
	} else {
		return old + e
	}
}
