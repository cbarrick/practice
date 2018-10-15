def colors(arr):
    counts = {}

    # Count 'em up
    for color in arr:
        if color in counts:
            counts[color] += 1
        else:
            counts[color] = 1

    # Get the most popular
    best = None
    best_count = 0
    for color, count in counts.items():
        if best_count < count:
            best = color
            best_count = count

    return best, best_count
