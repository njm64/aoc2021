import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.function.Function;
import java.util.stream.Collectors;

public class Day22 {

    static class Range {
        Range(int min, int max) {
            this.min = min;
            this.max = max;
        }

        Range(String s) {
            final String[] tokens = s.substring(2).split("\\.\\.");
            min = Integer.parseInt(tokens[0]);
            max = Integer.parseInt(tokens[1]) + 1;
        }

        boolean inInitRange() {
            return min >= -50 && max <= 50;
        }

        final int min, max;
    }

    static class Cube {
        Cube(String s) {
            final String[] tokens = s.split(",");
            x = new Range(tokens[0]);
            y = new Range(tokens[1]);
            z = new Range(tokens[2]);
        }

        final Range x, y, z;

        boolean inInitRange() {
            return x.inInitRange() && y.inInitRange() && z.inInitRange();
        }
    }

    static class Cmd {
        Cmd(String line) {
            final String[] tokens = line.split(" ");
            status = tokens[0].equals("on");
            cube = new Cube(tokens[1]);
        }

        final boolean status;
        final Cube cube;
    }

    List<Integer> makeAxis(List<Cmd> cmds, Function<Cmd, Range> f) {
        final List<Integer> list = new ArrayList<>();
        for(Cmd cmd : cmds) {
            final Range r = f.apply(cmd);
            list.add(r.min);
            list.add(r.max);
        }
        return list.stream().distinct().sorted().collect(Collectors.toList());
    }

    int mapInt(List<Integer> axis, int n) {
        int i = Collections.binarySearch(axis, n);
        if(i < 0) {
            throw new RuntimeException();
        }
        return i;
    }

    Range mapRange(List<Integer> axis, Range r) {
        return new Range(mapInt(axis, r.min), mapInt(axis, r.max));
    }

    long runCmds(List<Cmd> cmds) {
        final var xAxis = makeAxis(cmds, (c) -> c.cube.x );
        final var yAxis = makeAxis(cmds, (c) -> c.cube.y );
        final var zAxis = makeAxis(cmds, (c) -> c.cube.z );

        final int xs = xAxis.size();
        final int ys = yAxis.size();
        final int zs = zAxis.size();

        final boolean[] reactor = new boolean[xs * ys * zs];

        for(Cmd cmd : cmds) {
            final Range xr = mapRange(xAxis, cmd.cube.x);
            final Range yr = mapRange(yAxis, cmd.cube.y);
            final Range zr = mapRange(zAxis, cmd.cube.z);
            for(int x = xr.min; x < xr.max; x++) {
                for(int y = yr.min; y < yr.max; y++) {
                    for(int z = zr.min; z < zr.max; z++) {
                        final int i = x + (y * xs) + (z * xs * ys);
                        reactor[i] = cmd.status;
                    }
                }
            }
        }

        int i = 0;
        long count = 0;
        for(int z = 0; z < zs; z++) {
            for(int y = 0; y < ys; y++) {
                for(int x = 0; x < xs; x++) {
                    if(reactor[i]) {
                        final long xi = xAxis.get(x + 1) - xAxis.get(x);
                        final long yi = yAxis.get(y + 1) - yAxis.get(y);
                        final long zi = zAxis.get(z + 1) - zAxis.get(z);
                        count += xi * yi * zi;
                    }
                    i++;
                }
            }
        }

        return count;
    }


    public static void main(String[] args) {
        Day22 day = new Day22();

        List<Cmd> cmds = new ArrayList<>();

        try {
            BufferedReader r = new BufferedReader(new FileReader("/Users/nickm/src/aoc/input/day22.txt"));
            String line;
            while((line = r.readLine()) != null) {
                Cmd cmd = new Cmd(line);
               // if(cmd.cube.inInitRange()) {
                    cmds.add(cmd);
               // }
            }
        } catch(IOException e) {
            System.out.printf("Failed to read input");
            System.exit(0);
        }

        long t1 = System.currentTimeMillis();
        long result = day.runCmds(cmds);
        long t2 = System.currentTimeMillis();

        System.out.printf("%d %d", result, t2 - t1);
    }
}
