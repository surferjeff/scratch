use std::collections::HashSet;
use std::fs;
use regex::Regex;

#[derive(Debug, Clone, Copy)]
struct Robot {
    // Position
    px: i32,
    py: i32,
    // Velocity
    vx: i32,
    vy: i32,
}

fn parse_input(path: &str) -> Vec<Robot> {
    let text = fs::read_to_string(path).expect("Failed to read input file");
    let re = Regex::new(r"p=(?P<px>-?\d+),(?P<py>-?\d+) v=(?P<vx>-?\d+),(?P<vy>-?\d+)")
        .expect("Invalid regex");

    re.captures_iter(&text)
        .map(|caps| Robot {
            px: caps["px"].parse().unwrap(),
            py: caps["py"].parse().unwrap(),
            vx: caps["vx"].parse().unwrap(),
            vy: caps["vy"].parse().unwrap(),
        })
        .collect()
}

fn simulate(width: i32, height: i32, seconds: i32, robots: &mut [Robot]) {
    for robot in robots {
        robot.px = (robot.px + robot.vx * seconds).rem_euclid(width);
        robot.py = (robot.py + robot.vy * seconds).rem_euclid(height);
    }
}

fn calc_safety_factor(robots: &[Robot], width: i32, height: i32) -> i32 {
    let half_width = width / 2;
    let half_height = height / 2;

    let mut q1 = 0;
    let mut q2 = 0;
    let mut q3 = 0;
    let mut q4 = 0;

    for robot in robots {
        match (robot.px < half_width, robot.py < half_height) {
            (true, true) => q1 += 1,
            (true, false) => q2 += 1,
            (false, true) => q3 += 1,
            (false, false) => q4 += 1,
        }
    }

    q1 * q2 * q3 * q4
}

fn render_robots(width: usize, height: usize, robots: &[Robot]) {
    let mut arena = vec![vec!['.'; width]; height];

    for robot in robots {
        if (robot.px as usize) < width && (robot.py as usize) < height {
            arena[robot.py as usize][robot.px as usize] = '*';
        }
    }

    for line in arena {
        println!("{}", line.into_iter().collect::<String>());
    }
}

fn robots_overlap(robots: &[Robot], width: usize, height: usize) -> bool {
    let mut arena = vec![0u64; (width * height) / 64 + 1 ];

    for robot in robots {
        let offset = robot.py as usize * width + robot.px as usize;
        let word_offset = offset / 64;
        let bit_mask = 1u64 << (offset % 64);
        let word = arena[word_offset];
        if word & bit_mask != 0 {
            return true;
        }
        arena[word_offset] = word | bit_mask;
    }

    false
}

fn main() {
    let mut robots = parse_input("input.txt");
    println!("{} robots", robots.len());

    simulate(101, 103, 100, &mut robots);
    println!("Safety Factor: {}", calc_safety_factor(&robots, 101, 103));

    for i in 100..=10000 {
        simulate(101, 103, 1, &mut robots);
        if !robots_overlap(&robots, 101, 103) {
            println!("Seconds: {}", i);
            render_robots(101, 103, &robots);
            println!();
        }
    }
}
