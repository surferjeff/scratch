use std::fs;

#[derive(PartialEq)]
// Define the maze walk results
enum WalkResult {
    Escaped,
    InfiniteLoop,
}

// Constants for path masking
const PATH_MASK: u8 = 0b01100000;

// Function to turn right
fn turn_right(step: (i32, i32, char)) -> (i32, i32, char) {
    match step {
        (-1, 0, 'a') => (0, 1, 'b'),
        (0, 1, 'b') => (1, 0, 'd'),
        (1, 0, 'd') => (0, -1, 'h'),
        (0, -1, 'h') => (-1, 0, 'a'),
        _ => panic!("Invalid step"),
    }
}

// Find the starting position
fn find_start_position(input: &[String]) -> (usize, usize) {
    input
        .iter()
        .enumerate()
        .find_map(|(row, line)| line.chars().position(|c| c == '^').map(|col| (row, col)))
        .expect("Start position not found")
}

// Walk the maze
fn walk(lines: &mut Vec<Vec<char>>, rows: usize, cols: usize, start_pos: (usize, usize)) -> WalkResult {
    let mut loop_recursive = |mut step: (i32, i32, char), mut row: usize, mut col: usize| -> WalkResult {
        loop {
            let (i, j, c) = step;

            let foot_print = lines[row][col];
            let foot_print_byte = foot_print as u8;
            let new_foot_print = if PATH_MASK & foot_print_byte == PATH_MASK {
                char::from(foot_print_byte | (c as u8))
            } else {
                c
            };

            lines[row][col] = new_foot_print;
            let next_row = row as i32 + i;
            let next_col = col as i32 + j;

            if foot_print == new_foot_print {
                return WalkResult::InfiniteLoop;
            } else if next_row < 0 || next_row >= rows as i32 || next_col < 0 || next_col >= cols as i32 {
                return WalkResult::Escaped;
            } else if lines[next_row as usize][next_col as usize] == '#' {
                step = turn_right(step);
            } else {
                row = next_row as usize;
                col = next_col as usize;
            }
        }
    };

    loop_recursive((-1, 0, 'a'), start_pos.0, start_pos.1)
}

fn main() {
    // Read input from file
    let input: Vec<String> = fs::read_to_string("input.txt")
        .expect("Failed to read file")
        .lines()
        .map(String::from)
        .collect();

    let rows = input.len();
    let cols = input[0].len();
    let start_pos = find_start_position(&input);

    // Part 1: Count path footprints
    let mut lines: Vec<Vec<char>> = input.iter().map(|line| line.chars().collect()).collect();
    walk(&mut lines, rows, cols, start_pos);

    let is_path_char = |c: char| PATH_MASK == (c as u8 & PATH_MASK);
    let part1_count: usize = lines
        .iter()
        .flat_map(|row| row.iter())
        .filter(|&&c| is_path_char(c))
        .count();

    println!("part1 {}", part1_count);

    // Part 2: Find infinite loops.
    let is_infinite_loop = |row: usize, col: usize| {
        let mut lines: Vec<Vec<char>> = input.iter().map(|line| line.chars().collect()).collect();
        lines[row][col] = '#';
        walk(&mut lines, rows, cols, start_pos) == WalkResult::InfiniteLoop
    };

    let part2_count: usize = (0..rows)
        .flat_map(|row| (0..cols).map(move |col| (row, col)))
        .filter(|&(row, col)| is_infinite_loop(row, col))
        .count();

    println!("part2 {}", part2_count);
}
