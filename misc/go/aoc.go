package main

import (
	"fmt"
	"sort"
	"strings"
	"time"
)

type Range struct {
	min int
	max int
}

type Cube struct {
	x, y, z Range
}

type Cmd struct {
	status bool
	cube   Cube
}

func parseRange(s string) Range {
	tokens := strings.Split(s[2:], "..")
	return Range{
		min: parseInt(tokens[0]),
		max: parseInt(tokens[1]) + 1,
	}
}

func parseCube(s string) Cube {
	tokens := strings.Split(s, ",")
	return Cube{
		x: parseRange(tokens[0]),
		y: parseRange(tokens[1]),
		z: parseRange(tokens[2]),
	}
}

func parseInput(lines []string) []*Cmd {
	var cmds []*Cmd
	for _, s := range lines {
		tokens := strings.SplitN(s, " ", 2)
		cmds = append(cmds, &Cmd{
			status: tokens[0] == "on",
			cube:   parseCube(tokens[1]),
		})
	}
	return cmds
}

func makeAxis(cmds []*Cmd, f func(*Cmd) Range) []int {
	m := make(map[int]bool)
	for _, c := range cmds {
		r := f(c)
		m[r.min] = true
		m[r.max] = true
	}

	a := make([]int, 0, len(m))
	for i := range m {
		a = append(a, i)
	}

	sort.Ints(a)
	return a
}

func mapInt(axis []int, n int) int {
	return sort.SearchInts(axis, n)
}

func mapRange(axis []int, r Range) Range {
	return Range{
		min: mapInt(axis, r.min),
		max: mapInt(axis, r.max),
	}
}

func inInitialArea(cmd *Cmd) bool {
	c := &cmd.cube
	return c.x.min >= -50 && c.x.max <= 50 &&
		c.y.min >= -50 && c.y.max <= 50 &&
		c.z.min >= -50 && c.z.max <= 50
}

func runCmds(cmds []*Cmd) int64 {
	xAxis := makeAxis(cmds, func(c *Cmd) Range { return c.cube.x })
	yAxis := makeAxis(cmds, func(c *Cmd) Range { return c.cube.y })
	zAxis := makeAxis(cmds, func(c *Cmd) Range { return c.cube.z })

	xs := len(xAxis)
	ys := len(yAxis)
	zs := len(zAxis)
	reactor := make([]bool, xs*ys*zs)

	for _, cmd := range cmds {
		xr := mapRange(xAxis, cmd.cube.x)
		yr := mapRange(yAxis, cmd.cube.y)
		zr := mapRange(zAxis, cmd.cube.z)
		for x := xr.min; x < xr.max; x++ {
			for y := yr.min; y < yr.max; y++ {
				for z := zr.min; z < zr.max; z++ {
					i := x + (y * xs) + (z * xs * ys)
					reactor[i] = cmd.status
				}
			}
		}
	}

	i := 0
	var count int64
	for z := 0; z < zs; z++ {
		for y := 0; y < ys; y++ {
			for x := 0; x < xs; x++ {
				if reactor[i] {
					xi := xAxis[x+1] - xAxis[x]
					yi := yAxis[y+1] - yAxis[y]
					zi := zAxis[z+1] - zAxis[z]
					count += int64(xi * yi * zi)
				}
				i++
			}
		}
	}

	return count
}

func main() {
	cmds := parseInput(readFile("/Users/nickm/src/aoc/input/day22.txt"))
	t1 := time.Now()
	count := runCmds(cmds)
	d := time.Since(t1)
	fmt.Printf("%d: %d msec\n", count, d.Milliseconds())
}
