
class Map:
    rows: [bool]
    cols: [bool]
    def __init__(self, data: [[bool]]):
        self.rows = [any(row) for row in data]
        self.cols = [any(col) for col in zip(*data)]
    
    def dist_between(self, a: (int, int), b: (int, int)) -> int:
        base = abs(a[0] - b[0]) + abs(a[1] - b[1])
        for x in range(min(a[0], b[0]), max(a[0], b[0])):
            if not self.cols[x]:
                base += 1
        for y in range(min(a[1], b[1]), max(a[1], b[1])):
            if not self.rows[y]:
                base += 1
        return base


with (open("input/day11.txt", "r")) as file:
    lines = file.read().splitlines()
    bools = [[c == "#" for c in line] for line in lines]
    m = Map(bools)
    positions = []
    for y in range(len(bools)):
        for x in range(len(bools[y])):
            if bools[y][x]:
                positions.append((x, y))
    total = 0
    for i in range(len(positions)):
        for j in range(i + 1, len(positions)):
            total += m.dist_between(positions[i], positions[j])
    
    print(total)