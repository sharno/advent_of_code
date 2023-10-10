input =
  File.read!("3.txt")
  |> String.split(["\n", "\r", "\r\n"])

# part 1
defmodule Day3 do
  def solve(grid, sloper, sloped) do
    height = length(grid)
    width = String.length(hd(grid))

    Stream.iterate({0, 0}, fn {r, d} -> {rem(r + sloper, width), d + sloped} end)
    |> Stream.take_while(fn {_r, d} -> d < height end)
    |> Stream.map(fn {x, y} -> Enum.at(grid, y) |> String.at(x) end)
    |> Enum.count(fn s -> s == "#" end)
  end
end

Day3.solve(input, 3, 1)
|> IO.inspect()

# part 2

Enum.product([
  Day3.solve(input, 1, 1),
  Day3.solve(input, 3, 1),
  Day3.solve(input, 5, 1),
  Day3.solve(input, 7, 1),
  Day3.solve(input, 1, 2)
])
|> IO.inspect()
