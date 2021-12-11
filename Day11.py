octopodes = list(map(lambda row: list(map(int, row)), """6744638455
3135745418
4754123271
4224257161
8167186546
2268577674
7177768175
2662255275
4655343376
7852526168""".split("\n")))

offsets = ((0,1),(0,-1),(1,0),(-1,0),(1,1),(1,-1),(-1,-1),(-1,1))
flashes = 0
for _ in range(100):
    flashed = set()
    done = False
    while not done:
        done = True
        toFlash = set()
        for x in range(len(octopodes)):
            for y in range(len(octopodes[x])):
                if octopodes[x][y] >= 9:
                    toFlash.add((x,y))
                    octopodes[x][y] = 0
                    flashed.add((x, y))
                    flashes+=1
        
        for x, y in toFlash:
            for xOff, yOff in offsets:
                newX = x+xOff
                newY = y+yOff
                if 0 <= newX < len(octopodes) and 0 <= newY < len(octopodes[newX]) and (newX, newY) not in flashed:
                    done = False
                    octopodes[newX][newY] += 1
    for x in range(len(octopodes)):
        for y in range(len(octopodes[x])):
            if (x, y) not in flashed:
                octopodes[x][y] += 1
print(flashes)

# Assuming they do not flash simultaneously in the first 100 steps:
step = 100
while True:
    step+=1
    flashed = set()
    done = False
    while not done:
        done = True
        toFlash = set()
        for x in range(len(octopodes)):
            for y in range(len(octopodes[x])):
                if octopodes[x][y] >= 9:
                    toFlash.add((x,y))
                    octopodes[x][y] = 0
                    flashed.add((x, y))
        
        for x, y in toFlash:
            for xOff, yOff in offsets:
                newX = x+xOff
                newY = y+yOff
                if 0 <= newX < len(octopodes) and 0 <= newY < len(octopodes[newX]) and (newX, newY) not in flashed:
                    done = False
                    octopodes[newX][newY] += 1
    if len(flashed) == len(octopodes) * len(octopodes[x]):
        break
    for x in range(len(octopodes)):
        for y in range(len(octopodes[x])):
            if (x, y) not in flashed:
                octopodes[x][y] += 1
print(step)
