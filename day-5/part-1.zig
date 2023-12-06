const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;

const RangeMap = struct {
    entries: std.ArrayListUnmanaged(Entry) = .{},

    fn append(map: *RangeMap, allocator: Allocator, entry: Entry) !void {
        try map.entries.append(allocator, entry);
    }

    fn get(map: RangeMap, source: u32) u32 {
        return for (map.entries.items) |entry| {
            if (source >= entry.source_start and source - entry.source_start < entry.len) {
                break entry.dest_start + (source - entry.source_start);
            }
        } else source;
    }

    const Entry = struct {
        source_start: u32,
        dest_start: u32,
        len: u32,
    };
};

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var seeds: std.ArrayListUnmanaged(u32) = .{};
    var seed_to_soil: RangeMap = .{};
    var soil_to_fertilizer: RangeMap = .{};
    var fertilizer_to_water: RangeMap = .{};
    var water_to_light: RangeMap = .{};
    var light_to_temperature: RangeMap = .{};
    var temperature_to_humidity: RangeMap = .{};
    var humidity_to_location: RangeMap = .{};

    var input = try std.fs.cwd().openFile("input.txt", .{});
    defer input.close();
    var buffered_input = std.io.bufferedReader(input.reader());
    const input_reader = buffered_input.reader();
    var line_buf: [1000]u8 = undefined;
    var current_map: ?*RangeMap = null;
    while (try input_reader.readUntilDelimiterOrEof(&line_buf, '\n')) |line| {
        if (mem.startsWith(u8, line, "seeds: ")) {
            var seed_iter = mem.splitScalar(u8, line["seeds: ".len..], ' ');
            while (seed_iter.next()) |seed| {
                try seeds.append(allocator, std.fmt.parseInt(u32, seed, 10) catch return error.InvalidInput);
            }
        } else if (mem.eql(u8, line, "seed-to-soil map:")) {
            current_map = &seed_to_soil;
        } else if (mem.eql(u8, line, "soil-to-fertilizer map:")) {
            current_map = &soil_to_fertilizer;
        } else if (mem.eql(u8, line, "fertilizer-to-water map:")) {
            current_map = &fertilizer_to_water;
        } else if (mem.eql(u8, line, "water-to-light map:")) {
            current_map = &water_to_light;
        } else if (mem.eql(u8, line, "light-to-temperature map:")) {
            current_map = &light_to_temperature;
        } else if (mem.eql(u8, line, "temperature-to-humidity map:")) {
            current_map = &temperature_to_humidity;
        } else if (mem.eql(u8, line, "humidity-to-location map:")) {
            current_map = &humidity_to_location;
        } else if (line.len > 0) {
            var parts: std.BoundedArray(u32, 3) = .{};
            var parts_iter = mem.splitScalar(u8, line, ' ');
            while (parts_iter.next()) |part| {
                parts.append(std.fmt.parseInt(u32, part, 10) catch return error.InvalidInput) catch return error.InvalidInput;
            }
            if (parts.len != parts.capacity()) return error.InvalidInput;
            if (current_map) |*map| {
                try map.*.append(allocator, .{
                    .source_start = parts.get(1),
                    .dest_start = parts.get(0),
                    .len = parts.get(2),
                });
            } else return error.InvalidInput;
        }
    }

    var min_location: u32 = std.math.maxInt(u32);
    for (seeds.items) |seed| {
        const soil = seed_to_soil.get(seed);
        const fertilizer = soil_to_fertilizer.get(soil);
        const water = fertilizer_to_water.get(fertilizer);
        const light = water_to_light.get(water);
        const temperature = light_to_temperature.get(light);
        const humidity = temperature_to_humidity.get(temperature);
        const location = humidity_to_location.get(humidity);
        if (location < min_location) min_location = location;
    }
    std.debug.print("{}\n", .{min_location});
}
