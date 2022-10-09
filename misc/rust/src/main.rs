use std::io;
use std::fs::File;
use std::io::BufRead;

#[derive(Debug, Copy, Clone)]
struct Range {
    min: i32,
    max: i32
}

#[derive(Debug, Copy, Clone)]
struct Cube {
    x: Range,
    y: Range,
    z: Range
}

#[derive(Debug, Copy, Clone)]
struct Cmd {
    status: bool,
    cube: Cube
}

fn read_input() -> Vec<String> {
    let file = File::open("/Users/nickm/src/aoc/input/day22.txt").unwrap();
    let reader = io::BufReader::new(file);
    let mut vec = Vec::new();
    for line in reader.lines() {
        if let Ok(line) = line {
            vec.push(line);
        }
    }
    vec
}

fn parse_range(s: &str) -> Range {
    let (min_str, max_str) = s[2..].split_once("..").unwrap();
    Range{min: min_str.parse().unwrap(), max: max_str.parse::<i32>().unwrap() + 1}
}

fn parse_line(s: &String) -> Cmd {
    let (status_str, cubes_str) = s.split_once(" ").unwrap();
    let status = status_str == "on";
    let r : Vec<_> = cubes_str.split(",").map(parse_range).collect();
    let cube = Cube{x: r[0].clone(), y: r[1], z: r[2]};
    return Cmd{status, cube};
}

fn make_axis<T>(cmds: &[Cmd], f: T) -> Vec<i32>
where T: Fn(&Cmd) -> Range
{
    let mut v : Vec<_> = cmds.iter()
        .map(f)
        .flat_map(|r| [r.min, r.max])
        .collect();

    v.sort();
    v.dedup();
    return v;
}

fn map_int(axis: &[i32], n: i32) -> i32 {
    axis.binary_search(&n).unwrap() as i32
}

fn map_range(axis: &[i32], r: Range) -> Range {
    Range{min: map_int(axis, r.min), max: map_int(axis, r.max)}
}

fn in_initial_area(cmd: &Cmd) -> bool {
    let c = cmd.cube;
    return 
        c.x.min >= -50 && c.x.max <= 50 && 
        c.y.min >= -50 && c.y.max <= 50 && 
        c.z.min >= -50 && c.z.max <= 50;
}

fn run_cmds(cmds: &[Cmd]) -> usize {
    let x_axis = make_axis(cmds, |c| c.cube.x);
    let y_axis = make_axis(cmds, |c| c.cube.y);
    let z_axis = make_axis(cmds, |c| c.cube.z);

    let xs = x_axis.len();
    let ys = y_axis.len();
    let zs = z_axis.len();
    let mut reactor = vec![false; xs * ys * zs];
    
    for cmd in cmds {
        let xr = map_range(&x_axis, cmd.cube.x);
        let yr = map_range(&y_axis, cmd.cube.y);
        let zr = map_range(&z_axis, cmd.cube.z);
        for x in xr.min..xr.max {
            for y in yr.min..yr.max {
                for z in zr.min..zr.max {
                    let i = (x as usize) + ((y as usize) * xs) + ((z as usize) * xs * ys);
                    reactor[i] = cmd.status;
                }
            }
        }
    }

    let mut i = 0;
    let mut count = 0;
    for z in 0..zs {
        for y in 0..ys {
            for x in 0..xs {
                if reactor[i] {
                    let x = (x_axis[x + 1] - x_axis[x]) as usize;
                    let y = (y_axis[y + 1] - y_axis[y]) as usize;
                    let z = (z_axis[z + 1] - z_axis[z]) as usize;
                    count = count + (x * y * z);
                }
                i += 1;
            }
        }
    }

    count
}

fn main() {
   
    let cmds : Vec<_> = read_input().iter().map(parse_line).collect();

    //let part1_commands : Vec<_> = cmds.iter().copied().filter(|c| in_initial_area(c)).collect();

    let t1 = std::time::Instant::now();
    let count = run_cmds(&cmds);
    let t2 = std::time::Instant::now();
    let elapsed = t2 - t1;
    println!("Count: {} {}", count, elapsed.as_millis());
    
}
