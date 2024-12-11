use std::collections::HashMap;

// Adds a stone with the specified count to the map.
fn add_stone(stone: u64, count: u64, stones: &mut HashMap<u64, u64>) {
    *stones.entry(stone).or_insert(0) += count;
}

// Simulates a single blink operation.
fn blink(stones: &HashMap<u64, u64>) -> HashMap<u64, u64> {
    let mut new_stones = HashMap::with_capacity(stones.len() * 2);

    for (&stone, &count) in stones {
        if stone == 0 {
            add_stone(1, count, &mut new_stones);
        } else {
            let text = stone.to_string();
            if text.len() % 2 == 0 {
                let half_length = text.len() / 2;
                let first_half: u64 = text[..half_length].parse().unwrap();
                let second_half: u64 = text[half_length..].parse().unwrap();
                add_stone(first_half, count, &mut new_stones);
                add_stone(second_half, count, &mut new_stones);
            } else {
                add_stone(stone * 2024, count, &mut new_stones);
            }
        }
    }

    new_stones
}

fn main() {
    // Input array
    let input = [0u64, 37551, 469, 63, 1, 791606, 2065, 9983586];

    // Initialize stones map for part 1
    let mut stones: HashMap<u64, u64> = HashMap::new();
    for &n in &input {
        add_stone(n, 1, &mut stones);
    }

    // Perform 25 blink operations for part 1
    for _ in 0..25 {
        stones = blink(&stones);
    }

    // Calculate the sum of values in part 1
    let part1_sum: u64 = stones.values().sum();
    println!("part1: {}", part1_sum);

    // Perform 50 additional blink operations for part 2
    for _ in 0..50 {
        stones = blink(&stones);
    }

    // Calculate the sum of values in part 2
    let part2_sum: u64 = stones.values().sum();
    println!("part2: {}", part2_sum);
}
