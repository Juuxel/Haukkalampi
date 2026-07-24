def is_sand_or_gravel(tile):
    return tile == 12 or tile == 13

def fall(x, y, z, tile):
    if level.is_within_bounds(x, y - 1, z) and level.get_tile(x, y, z) == tile and level.is_air(x, y - 1, z):
        level.set_tile(x, y, z, 0)
        level.set_tile(x, y - 1, z, tile)
        server.schedule_tick(lambda: fall(x, y - 1, z, tile))

def on_tile_change(x, y, z, old_tile, tile):
    if is_sand_or_gravel(tile):
        server.schedule_tick(lambda: fall(x, y, z, tile))

def on_neighbor_change(x, y, z, nx, ny, nz, neighbor_tile):
    if ny < y and neighbor_tile == 0:
        tile = level.get_tile(x, y, z)
        if is_sand_or_gravel(tile):
            server.schedule_tick(lambda: fall(x, y, z, tile))

print("Loading falling tiles script")
level.tile_changed.subscribe(on_tile_change)
level.neighbor_changed.subscribe(on_neighbor_change)
