package main

import (
	"fmt"
	"strconv"
)

// Represents the mapping from stone number to its count.
type StoneMap map[uint64]uint64

// Simulates a single blink operation.
func blink(stones StoneMap) StoneMap {
	newStones := make(StoneMap)
	for stone, count := range stones {
		if stone == 0 {
			newStones[1] += count
		} else {
			text := strconv.FormatUint(stone, 10)
			if len(text)%2 == 0 {
				halfLength := len(text) / 2
				firstHalf, _ := strconv.ParseUint(text[:halfLength], 10, 64)
				secondHalf, _ := strconv.ParseUint(text[halfLength:], 10, 64)
				newStones[firstHalf] += count
				newStones[secondHalf] += count
			} else {
				newStones[stone*2024] += count
			}
		}
	}
	return newStones
}

func main() {
	// Input array
	input := []uint64{0, 37551, 469, 63, 1, 791606, 2065, 9983586}

	// Initialize stones map for part 1
	stones := make(StoneMap)
	for _, n := range input {
		stones[n] = 1
	}

	// Perform 25 blink operations for part 1
	for i := 0; i < 25; i++ {
		stones = blink(stones)
	}

	// Calculate the sum of values in part 1
	part1Sum := uint64(0)
	for _, count := range stones {
		part1Sum += count
	}
	fmt.Printf("part1: %d\n", part1Sum)

	// Perform 50 additional blink operations for part 2
	for i := 0; i < 50; i++ {
		stones = blink(stones)
	}

	// Calculate the sum of values in part 2
	part2Sum := uint64(0)
	for _, count := range stones {
		part2Sum += count
	}
	fmt.Printf("part2: %d\n", part2Sum)
}
