// IDEA: make list of Point that have + \ /
// cards are tuple of (coords, dir, intersection) e.g. (Point(1,1), (1,0), (0,0,1)) --> moving right (x + 1, y + 0) each tick
// / --> transforms (x+1, y-1)
// \ --> transforms (x-1, y+1)
// + --> transforms(x+0, y+0) or / \
//
// Use the State monad from scalaZ to implement the cart state for intersections
// perhaps also make: type Cart[A, S] or something like this
//
// TODO: make function that reads in file as grid
//
// compute the whole lot. If any coord point of card is identical, then return crash coords
// track cards only, + / \ are fixed and only operate if card has same coords