/* flood.c  --  by u/skeeto
 *
 * I copied this submission from u/skeeto to compare against my implementation.
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

/* Parameterize for 4x4. */
#define N          4
#define WL         UINT16_C(~0x1111)  // "wall left"
#define WR         UINT16_C(~0x8888)  // "wall right"
#define STEP_MAX   17
typedef uint16_t intmask;

/* Is group A adjacent to group B? */
static _Bool
is_adj(intmask a, intmask b)
{
    return ((a << N) & b) ||
           ((a >> N) & b) ||
           ((a << 1) & WL & b) ||
           ((a >> 1) & WR & b);
}

/* Merge group i into matching adjacent groups. */
static int
merge(int *values, intmask *masks, int n, int i)
{
    for (int j = 0; j < n; j++) {
        if (i != j && values[i] == values[j] && is_adj(masks[i], masks[j])) {
            int t = j < i ? j : i;
            int d = j < i ? i : j;
            masks[t] = masks[i] | masks[j];
            masks[d] = masks[n - 1];
            values[d] = values[n - 1];
            i = t;
            n--;
        }
    }
    return n;
}

static int
solve(const int *values, const intmask *masks, int n, int s, int best)
{
    if (n == 1)
        return s;    // solution found

    for (int i = 0; i < n; i++) {
        /* Try merging with each adjacent group. */
        for (int j = 0; j < n; j++) {
            if (i != j && is_adj(masks[i], masks[j])) {
                /* d = steps needed to merge with this adjacent group. */
                int d = abs(values[i] - values[j]);
                if (s + d < best) {
                    int cvalues[N * N];
                    intmask cmasks[N * N];
                    memcpy(cvalues, values, sizeof(*values) * n);
                    memcpy(cmasks, masks, sizeof(*masks) * n);
                    cvalues[i] = cvalues[j];
                    int cn = merge(cvalues, cmasks, n, i);
                    best = solve(cvalues, cmasks, cn, s + d, best);
                }
            }
        }
    }
    return best;
}

static const int dirs[] = {-1, +0, +1, +0, +0, -1, +0, +1};

/* Compute the mask for group at (x, y) and destroy it. */
static intmask
find_mask(int *grid, int x, int y)
{
    intmask mask = (intmask)1 << (y * N + x);
    int value = grid[y * N + x];
    grid[y * N + x] = 0;
    for (int i = 0; i < 4; i++) {
        int xx = x + dirs[i * 2 + 0];
        int yy = y + dirs[i * 2 + 1];
        if (xx >= 0 && xx < N && yy >= 0 && yy < N)
            if (grid[yy * N + xx] == value)
                mask |= find_mask(grid, xx, yy);
    }
    return mask;
}

int
main(void)
{
    /* Load input into array. */
    int grid[N * N];
    for (int i = 0; i < N * N; i++)
        scanf("%d", grid + i);

    /* Convert into bitmask representation. */
    int values[N * N];
    intmask masks[N * N];
    int n = 0;
    for (int y = 0; y < N; y++) {
        for (int x = 0; x < N; x++) {
            if (grid[y * N + x]) {
                values[n] = grid[y * N + x];
                masks[n++] = find_mask(grid, x, y);
            }
        }
    }

    int best = solve(values, masks, n, 0, STEP_MAX);
    printf("%d\n", best);
    return 0;
}
