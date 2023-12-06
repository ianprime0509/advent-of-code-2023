const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;

const Range = struct {
    start: u64,
    len: u64,
};

const RangeMap = struct {
    entries: std.ArrayListUnmanaged(Entry) = .{},

    fn append(map: *RangeMap, allocator: Allocator, entry: Entry) !void {
        try map.entries.append(allocator, entry);
    }

    fn normalize(map: *RangeMap) void {
        std.sort.heap(Entry, map.entries.items, {}, struct {
            fn lessThan(_: void, a: Entry, b: Entry) bool {
                return a.source_start < b.source_start;
            }
        }.lessThan);
    }

    fn getRanges(map: RangeMap, range: Range, results: *std.ArrayList(Range)) !void {
        var remaining_range = range;
        for (map.entries.items) |entry| {
            if (remaining_range.start >= entry.source_start + entry.len) continue;
            if (remaining_range.start < entry.source_start) {
                const unmapped_len = @min(remaining_range.len, entry.source_start - remaining_range.start + remaining_range.len);
                try results.append(.{ .start = remaining_range.start, .len = unmapped_len });
                remaining_range.start = entry.source_start;
                remaining_range.len -= unmapped_len;
                if (remaining_range.len == 0) return;
            }
            const mapped_len = @min(entry.len - (remaining_range.start - entry.source_start), remaining_range.len);
            try results.append(.{
                .start = entry.dest_start + (remaining_range.start - entry.source_start),
                .len = mapped_len,
            });
            remaining_range.len -= mapped_len;
            remaining_range.start += mapped_len;
            if (remaining_range.len == 0) return;
        }
        if (remaining_range.len > 0) {
            try results.append(remaining_range); // Unmapped
        }
    }

    const Entry = struct {
        source_start: u64,
        dest_start: u64,
        len: u64,
    };
};

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var seeds = std.ArrayList(Range).init(allocator);
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
            while (seed_iter.next()) |seed_start_str| {
                const seed_start = std.fmt.parseInt(u64, seed_start_str, 10) catch return error.InvalidInput;
                const seed_len_str = seed_iter.next() orelse return error.InvalidInput;
                const seed_len = std.fmt.parseInt(u64, seed_len_str, 10) catch return error.InvalidInput;
                try seeds.append(.{ .start = seed_start, .len = seed_len });
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
            var parts: std.BoundedArray(u64, 3) = .{};
            var parts_iter = mem.splitScalar(u8, line, ' ');
            while (parts_iter.next()) |part| {
                parts.append(std.fmt.parseInt(u64, part, 10) catch return error.InvalidInput) catch return error.InvalidInput;
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
    seed_to_soil.normalize();
    soil_to_fertilizer.normalize();
    fertilizer_to_water.normalize();
    water_to_light.normalize();
    light_to_temperature.normalize();
    temperature_to_humidity.normalize();
    humidity_to_location.normalize();

    var soils = std.ArrayList(Range).init(allocator);
    for (seeds.items) |seed| {
        try seed_to_soil.getRanges(seed, &soils);
    }
    var fertilizers = std.ArrayList(Range).init(allocator);
    for (soils.items) |soil| {
        try soil_to_fertilizer.getRanges(soil, &fertilizers);
    }
    var waters = std.ArrayList(Range).init(allocator);
    for (fertilizers.items) |fertilizer| {
        try fertilizer_to_water.getRanges(fertilizer, &waters);
    }
    var lights = std.ArrayList(Range).init(allocator);
    for (waters.items) |water| {
        try water_to_light.getRanges(water, &lights);
    }
    var temperatures = std.ArrayList(Range).init(allocator);
    for (lights.items) |light| {
        try light_to_temperature.getRanges(light, &temperatures);
    }
    var humidities = std.ArrayList(Range).init(allocator);
    for (temperatures.items) |temperature| {
        try temperature_to_humidity.getRanges(temperature, &humidities);
    }
    var locations = std.ArrayList(Range).init(allocator);
    for (humidities.items) |humidity| {
        try humidity_to_location.getRanges(humidity, &locations);
    }
    var min_location: u64 = std.math.maxInt(u64);
    for (locations.items) |location| {
        if (location.start < min_location) min_location = location.start;
    }
    std.debug.print("{}\n", .{min_location});
}
