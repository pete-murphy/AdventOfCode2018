"use strict"

const findRepeat = (xs, acc = 0, set = new Set([0])) => {
  const go = ([y, ...ys], acc, set) =>
    y === undefined
      ? findRepeat(xs, acc, set)
      : (y_ => (set.has(y_) ? y_ : go(ys, y_, set.add(y_))))(y + acc)
  return go(xs, acc, set)
}
