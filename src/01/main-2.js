const fs = require("fs")

const data = fs
  .readFileSync(__dirname + "/input.txt", "utf-8")
  .split("\n")
  .map(parseFloat)
  .slice(0, -1)

// fs.writeFileSync(
//   __dirname + "/input.json",
//   JSON.stringify(data, null, 2),
//   "utf-8"
// )

// const data = [7, 7, -2, -7, -4]

// const data = [3, 3, 4, -2, -4]

// data.map(x => typeof x).filter(x => x !== "number") //?
const findRepeat = (xs, acc, set) => {
  const go = ([y, ...ys], acc, set) => {
    return y === undefined
      ? findRepeat(xs, acc, set)
      : set.has(y + acc)
        ? y + acc
        : go(ys, y + acc, set.add(y + acc))
  }
  return go(xs, acc, set)
}

findRepeat(data, 0, new Set([0])) //?
