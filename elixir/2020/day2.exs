input =
  File.read!("2.txt")
  |> String.split(["\n", "\r", "\r\n"])
  |> Enum.map(fn s -> String.split(s, ["-", ": ", " "]) end)
  |> Enum.map(fn [a, b, c, d] -> {String.to_integer(a), String.to_integer(b), c, d} end)

# part 1
input
|> Enum.count(fn {a, b, c, s} ->
  String.graphemes(s)
  |> Enum.count(&(&1 == c))
  |> (&(&1 >= a and &1 <= b)).()
end)
|> IO.inspect()

# part 2
input
|> Enum.count(fn {a, b, c, s} ->
  not (String.at(s, a - 1) == c and String.at(s, b - 1) == c) and
    (String.at(s, a - 1) == c or
       String.at(s, b - 1) == c)
end)
|> IO.inspect()
