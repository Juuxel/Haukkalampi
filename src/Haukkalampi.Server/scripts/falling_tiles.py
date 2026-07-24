def fall(x, y, z, tile):
    if level.is_within_bounds(x, y - 1, z) and level.get_tile(x, y, z) == tile and level.get_tile(x, y - 1, z) == 0:
        level.set_tile(x, y, z, 0)
        level.set_tile(x, y - 1, z, tile)
        server.schedule_tick(lambda: fall(x, y - 1, z, tile))

def on_tile_change(x, y, z, old_tile, tile):
    if tile == 12 or tile == 13:
        server.schedule_tick(lambda: fall(x, y, z, tile))

print("Loading falling tiles script")
level.tile_changed.subscribe(on_tile_change)
