// solution: 53268

use std::fs;

fn main() {
    let file_path = "input";
    let input = fs::read_to_string(file_path).expect("make sure input file exists");

    let digits = vec![
        ("zero",  0),
        ("one",   1),
        ("two",   2),
        ("three", 3),
        ("four",  4),
        ("five",  5),
        ("six",   6),
        ("seven", 7),
        ("eight", 8),
        ("nine",  9)
    ];

    let mut res: u64 = 0;
    for line in input.lines() {
        let line = line.as_bytes();
        let mut i = 0;
        let mut nums = Vec::new();

        while i < line.len() {
            if (line[i] as char).is_numeric() {
                nums.push(line[i] - ('0' as u8));
            }
            else {
                for (s, n) in &digits {
                    if i + s.len() <= line.len() && s.as_bytes() == &line[i .. i + s.len()] {
                        nums.push(*n);
                    }
                }
            }
            i += 1;
        }
        let first = nums[0];
        let last = nums[nums.len()-1];
        // println!("{first}{last}");
        let num = first * 10 + last;
        res += num as u64;
    }
    println!("{res}");
}
