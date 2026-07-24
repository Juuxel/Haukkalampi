def has_sponge_around(x, y, z):
    for xo in range(-2, 3):
        for yo in range(-2, 3):
            for zo in range(-2, 3):
                if level.is_within_bounds(x + xo, y + yo, z + zo) and level.get_tile(x + xo, y + yo, z + zo) == 19:
                    return True
    return False

def is_water(x, y, z):
    return level.is_within_bounds(x, y, z) and level.get_tile(x, y, z) == 8

def flow(x, y, z):
    if not level.is_within_bounds(x, y, z) or level.get_tile(x, y, z) != 0:
        return
    if (is_water(x, y + 1, z) or is_water(x - 1, y, z) or is_water(x + 1, y, z) or is_water(x, y, z - 1) or is_water(x, y, z + 1)) and not has_sponge_around(x, y, z):
        level.set_tile(x, y, z, 8)

def clear_water_around(x, y, z):
    for xo in range(-2, 3):
        for yo in range(-2, 3):
            for zo in range(-2, 3):
                if level.is_within_bounds(x + xo, y + yo, z + zo) and level.get_tile(x + xo, y + yo, z + zo) == 8:
                    level.set_tile(x + xo, y + yo, z + zo, 0)

def on_tile_change(x, y, z, old_tile, tile):
    if tile == 0:
        server.schedule_tick(lambda: flow(x, y, z))

        if old_tile == 19:
            for xo in range(-2, 3):
                for yo in range(-2, 3):
                    for zo in range(-2, 3):
                        if abs(xo) == 2 or abs(yo) == 2 or abs(zo) == 2:
                            flow(x + xo, y + yo, z + zo)

    elif tile == 19:
        server.schedule_tick(lambda: clear_water_around(x, y, z))

def on_neighbor_change(x, y, z, nx, ny, nz, tile):
    if tile == 8 and y <= ny and level.get_tile(x, y, z) == 0:
        server.schedule_tick(lambda: level.set_tile(x, y, z, tile))

print("Loading flowing water script")
level.tile_changed.subscribe(on_tile_change)
level.neighbor_changed.subscribe(on_neighbor_change)
