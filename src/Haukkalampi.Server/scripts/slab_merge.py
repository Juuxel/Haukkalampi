def on_tile_change(x, y, z, old_tile, tile):
    if tile == 44 and level.is_within_bounds(x, y - 1, z) and level.get_tile(x, y - 1, z) == 44:
        def next_tick():
            level.set_tile(x, y - 1, z, 43)
            level.set_tile(x, y, z, 0)
        server.schedule_tick(next_tick)

print("Loading slab merge script")
level.tile_changed.subscribe(on_tile_change)
