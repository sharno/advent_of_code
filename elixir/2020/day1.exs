nums = File.read!("1.txt") |> String.split() |> Enum.map(&String.to_integer/1)
nums_set = MapSet.new(nums)

# part1
Enum.find(nums, fn x -> MapSet.member?(nums_set, 2020 - x) end)
|> (fn x -> x * (2020 - x) end).()
|> IO.inspect()

# part2
for n <- nums,
    m <- nums,
    n + m < 2020,
    into: %{} do
  {n + m, {n, m}}
end
|> Enum.find(fn {pair, _xy} -> MapSet.member?(nums_set, 2020 - pair) end)
|> (fn {pair, {x, y}} -> x * y * (2020 - pair) end).()
|> IO.inspect()
