package main

import (
	"container/heap"
	"flag"
	"fmt"
	"io"
	"time"
)

func main() {
	var (
		dfs = flag.Bool("dfs", false, "depth first solver")
		bfs = flag.Bool("bfs", false, "parallel breadth first solver (slow af)")
		as  = flag.Bool("as", false, "parallel A* solver")
		w   = flag.Int("w", 3, "the number of workers for parallel solvers")
		buf = flag.Int("buf", 8, "the channel buffer size for parallel solvers")
	)

	flag.Parse()
	b, ctx := Scan()

	switch {
	case *dfs:
		fmt.Println("solver: DFS")
		start := time.Now()
		n := ctx.DFS(b, 0, 0)
		dur := time.Since(start)
		fmt.Println("answer:", n)
		fmt.Println("time:", dur)

	case *bfs:
		fmt.Println("solver: BFS")
		fmt.Println("workers: ", *w)
		fmt.Println("buffer size:", *buf)
		start := time.Now()
		n := ctx.BFS(b, *w, *buf)
		dur := time.Since(start)
		fmt.Println("answer:", n)
		fmt.Println("time:", dur)

	case *as:
		fmt.Println("solver: A*")
		fmt.Println("workers:", *w)
		fmt.Println("buffer size:", *buf)
		start := time.Now()
		n := ctx.AStar(b, *w, *buf)
		dur := time.Since(start)
		fmt.Println("answer:", n)
		fmt.Println("time:", dur)

	default:
		fmt.Println("Must specify a solver:")
		flag.PrintDefaults()
	}
}

// Input
// --------------------------------------------------

// Scan reads a grid of numbers in from standard input.
func Scan() (Board, *Context) {
	var (
		b    Board
		grid [][]int
		ns   []int
		err  error
		size int
	)

	// Scan integers until EOF
	for {
		var n int
		_, err = fmt.Scan(&n)
		if err != nil {
			break
		}
		ns = append(ns, n)
	}
	if err != io.EOF {
		panic(err.Error())
	}

	// Compute the size of the board
	switch len(ns) {
	case 4:
		size = 2
	case 9:
		size = 3
	case 16:
		size = 4
	case 25:
		size = 5
	case 36:
		size = 6
	case 49:
		size = 7
	case 64:
		size = 8
	default:
		panic("invalid shape")
	}

	// Convert grid into a Board
	for i := 0; i < len(ns); i += size {
		grid = append(grid, ns[i:i+size])
	}
	for y := range grid {
		for x := range grid[y] {
			if grid[y][x] != 0 {
				b = append(b, extractMask(grid, y, x))
			}
		}
	}

	return b, GetContext(size)
}

func extractMask(grid [][]int, y, x int) Mask {
	size := len(grid)
	m := Mask{
		V: grid[y][x],
		M: uint64(1) << uint(size*y+x),
	}

	grid[y][x] = 0

	for _, dir := range [4][2]int{{+1, 0}, {-1, 0}, {0, +1}, {0, -1}} {
		xx := int(x) + dir[0]
		yy := int(y) + dir[1]
		if 0 <= xx && xx < size &&
			0 <= yy && yy < size &&
			grid[yy][xx] == m.V {
			m.M |= extractMask(grid, yy, xx).M
		}
	}

	return m
}

// Game State
// --------------------------------------------------

// A Board represents the game state at some point in time.
type Board []Mask

// A Mask is a bitmask for a particular value. The bits indicate which cells
// contain the value. A mask is created for each connected component.
type Mask struct {
	V int    // value
	M uint64 // position mask
}

// Bounds returns the minimum and maximum values on the board.
func (b Board) Bounds() (min, max int) {
	const maxInt = int(^uint(0) >> 1)
	min = maxInt
	max = -maxInt
	for i := range b {
		if b[i].V < min {
			min = b[i].V
		}
		if max < b[i].V {
			max = b[i].V
		}
	}
	return min, max
}

// Context
// --------------------------------------------------

// A Context is the metadata needed to work with grids of various sizes.
type Context struct {
	Size      uint
	WallLeft  uint64
	WallRight uint64
}

var CTX = [8]Context{
	Context{1, ^uint64(0x1), ^uint64(0x1)},
	Context{2, ^uint64(0x5), ^uint64(0xA)},
	Context{3, ^uint64(0x49), ^uint64(0x124)},
	Context{4, ^uint64(0x1111), ^uint64(0x8888)},
	Context{5, ^uint64(0x108421), ^uint64(0x1084210)},
	Context{6, ^uint64(0x41041041), ^uint64(0x820820820)},
	Context{7, ^uint64(0x40810204081), ^uint64(0x1020408102040)},
	Context{8, ^uint64(0x101010101010101), ^uint64(0x8080808080808080)},
}

// GetContext returns the Context for the given grid size.
// The size must be an integer between 1 and 8 inclusive.
func GetContext(size int) *Context {
	if 0 <= size && size < 8 {
		return &CTX[size-1]
	}
	panic("invalid size")
}

// Adjacent returns true if the Masks a and b contain any adjacent cells.
func (ctx *Context) Adjacent(a, b Mask) bool {
	m := ((a.M << ctx.Size) & b.M)
	m |= ((a.M >> ctx.Size) & b.M)
	m |= ((a.M << 1) & ctx.WallLeft & b.M)
	m |= ((a.M >> 1) & ctx.WallRight & b.M)
	return m != 0
}

// Fix merges the ith Mask of the board with any adjacent, same-valued Mask.
func (ctx *Context) Fix(b Board, i int) Board {
	n := len(b)
	for j := 0; j < n; j++ {
		if j != i && b[i].V == b[j].V && ctx.Adjacent(b[i], b[j]) {
			t, d := cmp(i, j)
			b[t].M = b[i].M | b[j].M
			b[d] = b[n-1]
			b = b[:n-1]
			i = t
			n--
		}
	}
	return b
}

// DFS Solver
// --------------------------------------------------

// DFS solves the board using depth-first search and returns the number of
// moves required for the solution. The value n is added to the result, and the
// return value will not exceed maxDepth. If maxDepth is less than or equal
// to zero, no maximum depth is enforced.
func (ctx *Context) DFS(b Board, n int, maxDepth int) int {
	if len(b) == 1 {
		return n
	}

	for i := range b {
		for j := range b {
			if i != j && ctx.Adjacent(b[i], b[j]) {
				d := abs(b[i].V - b[j].V)
				if maxDepth <= 0 || n+d < maxDepth {
					cpy := make(Board, len(b))
					copy(cpy, b)
					cpy[i].V = cpy[j].V
					cpy = ctx.Fix(cpy, i)
					maxDepth = ctx.DFS(cpy, n+d, maxDepth)
				}
			}
		}
	}

	return maxDepth
}

// BFS and A* Solvers
// --------------------------------------------------

// BFS solves the board using a parallel breadth-first search
// and returns the number of moves required for the solution.
func (ctx *Context) BFS(b Board, workers int, buf int) int {
	h := func(Board) int {
		return 0
	}

	q := MakeQueue(buf)
	ret := make(chan int)
	q.Insert(Item{b, 0, h(b)})
	for i := 0; i < workers; i++ {
		go ctx.worker(i, q, h, ret)
	}
	return <-ret
}

// AStar solves the board using a parallel A* search
// and returns the number of moves required for the solution.
func (ctx *Context) AStar(b Board, workers int, buf int) int {
	h := func(b Board) int {
		min, max := b.Bounds()
		return max - min
	}

	q := MakeQueue(buf)
	ret := make(chan int)
	q.Insert(Item{b, 0, h(b)})
	for i := 0; i < workers; i++ {
		go ctx.worker(i, q, h, ret)
	}
	return <-ret
}

// worker implements the common worker logic for A*/BFS searches.
// - id is an identifier for the worker, useful for debugging.
// - q is the search Queue.
// - h is the heuristic function for search order.
// - ret is the channel that will recieve the answer.
func (ctx *Context) worker(id int, q Queue, h func(Board) int, ret chan<- int) {
	for {
		itm := q.Next()
		b := itm.Board

		if len(itm.Board) == 1 {
			// TODO: stop other goroutines cleanly
			ret <- itm.D
			return
		}

		for i := range b {
			for j := range b {
				if i != j && ctx.Adjacent(b[i], b[j]) {
					d := abs(b[i].V - b[j].V)
					cpy := make(Board, len(b))
					copy(cpy, b)
					cpy[i].V = cpy[j].V
					cpy = ctx.Fix(cpy, i)
					q.Insert(Item{cpy, itm.D + d, h(cpy)})
				}
			}
		}
	}
}

// Queue implements an A* queue of Items.
// Queue is essentially thread-safe wrapper for Heap.
type Queue struct {
	in  chan<- Item
	out <-chan Item
}

// Heap implements the heap.Interface for Items in A* order.
type Heap []Item

// An Item combines a Board state with a distance and heuristic measures.
// Items are used to sort Boards in A* order.
type Item struct {
	Board
	D int
	H int
}

// MakeQueue creates a Queue and spawns a goroutine to manage it. The channels
// used to communicate with the Queue may be buffered, but note that AStar is
// not guaranteed to return an optimal solution when using buffering. However,
// in practice,the solution will be optimal even with buffering.
func MakeQueue(buffering int) Queue {
	in := make(chan Item, buffering)
	out := make(chan Item, buffering)
	q := Queue{in, out}

	go func(in <-chan Item, out chan<- Item) {
		h := new(Heap)
		heap.Push(h, <-in)
		for {
			select {
			case x := <-in:
				heap.Push(h, x)
			case out <- (*h)[0]:
				heap.Pop(h)
				if h.Len() == 0 {
					heap.Push(h, <-in)
				}
			}
		}
	}(in, out)

	return q
}

// Insert adds an item to the Queue.
func (q Queue) Insert(x Item) {
	q.in <- x
}

// Next remove the Item from the Queue with
// the lowest sum of distance and heuristic.
func (q Queue) Next() Item {
	return <-q.out
}

func (h Heap) Len() int {
	return len(h)
}

func (h Heap) Less(i, j int) bool {
	a := h[i].D + h[i].H
	b := h[j].D + h[j].H
	return a < b
}

func (h Heap) Swap(i, j int) {
	h[i], h[j] = h[j], h[i]
}

func (h *Heap) Push(x interface{}) {
	*h = append(*h, x.(Item))
}

func (h *Heap) Pop() interface{} {
	n := len(*h)
	x := (*h)[n-1]
	*h = (*h)[:n-1]
	return x
}

// Helpers
// --------------------------------------------------

func abs(a int) int {
	if a < 0 {
		return -a
	}
	return a
}

func cmp(a, b int) (int, int) {
	if a < b {
		return a, b
	}
	return b, a
}
