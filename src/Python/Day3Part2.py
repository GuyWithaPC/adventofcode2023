#!/bin/python3

class Gear:
    def __init__ (self):
        self.adjacent = 0
        self.ratio = 1
    
    adjacent: int
    ratio: int

    def __str__ (self):
        return f"Gear: {self.adjacent} {self.ratio}"
    
    def __repr__ (self):
        return str(self)

class PartNumber:
    def __init__ (self, buffer: str):
        self.size = len(buffer)
        self.num = int(buffer)
        self.adjacent_to = {}

    size: int
    num: int

    def add_adjacent(self, gears: {(int, int): Gear}):
        for key, value in gears.items():
            self.adjacent_to[key] = value

    adjacent_to: {(int, int): Gear}

    def __str__ (self):
        return f"PartNumber: {self.num} adjacent to {self.adjacent_to}"
    
    def __repr__ (self):
        return str(self)


def to_gears(data: [str]) -> {(int, int): Gear}:
    gears = {}
    for row, line in enumerate(data):
        for col, char in enumerate(line):
            if char == '*':
                gears[(col, row)] = Gear()
    return gears

def search_around(pos: (int, int), gears: {(int, int): Gear}) -> bool:
    pattern = [(pos[0] + x, pos[1] + y) for x in range(-1,2) for y in range(-1,2)]
    return any([gears.get(pos) != None for pos in pattern])

def gears_around(pos: (int, int), gears: {(int, int): Gear}) -> {(int, int): Gear}:
    pattern = [(pos[0] + x, pos[1] + y) for x in range(-1,2) for y in range(-1,2)]
    gearsAround = {}
    for pos in pattern:
        if gears.get(pos) != None:
            gearsAround[pos] = gears.get(pos)
    return gearsAround

def read_part_numbers(data: [str], gears: {(int, int): Gear}) -> {(int,int): PartNumber}:
    part_numbers = {}
    for row, line in enumerate(data):
        start_position: (int,int) = None
        gear_adjacent = False
        gears_adjacent = {}
        number_buffer = ""
        for col, char in enumerate(line):
            if char.isdigit():
                if start_position == None:
                    start_position = (col, row)
                    number_buffer += char
                else:
                    number_buffer += char
                gear_adjacent = gear_adjacent or search_around((col, row), gears)
                gears_adjacent.update(gears_around((col, row), gears))
            else:
                if start_position != None and not gear_adjacent:
                    start_position = None
                    number_buffer = ""
                elif start_position != None:
                    part_numbers[(col, row)] = PartNumber(number_buffer)
                    part_numbers[(col, row)].add_adjacent(gears_adjacent)
                    start_position = None
                    number_buffer = ""
                    gear_adjacent = False
                    gears_adjacent = {}
        if gear_adjacent and start_position != None:
            part_numbers[(col,row)] = PartNumber(number_buffer)
            part_numbers[(col,row)].add_adjacent(gears_adjacent)
    return part_numbers


with (open("input/day3.txt", "r")) as file:
    data = file.read().splitlines()
    gears = to_gears(data)
    part_numbers = read_part_numbers(data, gears)
    for part_number in part_numbers.values():
        for gear in part_number.adjacent_to.values():
            gear.adjacent += 1
            gear.ratio *= part_number.num
    print(sum([gear.ratio for gear in gears.values() if gear.adjacent == 2]))